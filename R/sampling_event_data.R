dataset_details <- function(key) {
  url <- paste0("http://api.gbif.org/v1/dataset/", key)
  hits <- jsonlite::fromJSON(url)
  overview <- tibble::as_tibble(hits[c(
    "doi", "title", "description",
    "language", "license"
  )])
  endpoints <- tibble::as_tibble(hits$endpoints)
  return (list(overview = overview, endpoints = endpoints))
}

# download and parse sample event data from DwcA-file
dwca_download <- function(url) {
  dwca_file <- tempfile(pattern = "sampling-event-dwca-", fileext = ".zip")
  dwca_dir <- dirname(dwca_file)
  if (download.file(url, destfile = dwca_file, mode = "wb", quiet = TRUE) != 0)
    stop("External error downloading from ", url)
  return (dwca_file)
}

dwca_parse <- function(dwca_file) {

  # TODO use library(finch) in the next iteration

  stopifnot(file.exists(dwca_file), tools::file_ext(dwca_file) == "zip")
  filez <- unzip(dwca_file, list = TRUE, junkpaths = TRUE)$Name
  meta <- eml <- event_core <- occurrence_ext <- mof <- issues <- NULL

  if ("meta.xml" %in% filez) {
    # TODO parse meta.xml for encoding, field/line terminators,
    # quoting char, skiplines etc and use this in further parsing
    meta <- xml2::read_xml(unz(dwca_file, "meta.xml")) %>%
      xml2::as_list()
  } else {
    warning("No meta.xml found in ", dwca_file)
  }

  if ("eml.xml" %in% tolower(filez)) {
    eml <- xml2::read_xml(unz(dwca_file, "eml.xml")) %>%
      xml2::as_list()
  } else {
    warning("No eml.xml found in ", dwca_file)
  }

  if ("event.txt" %in% filez) {
    event_core <- suppressWarnings(
      suppressMessages(readr::read_tsv(unz(dwca_file, "event.txt"))) %>%
      select(-id))
  } else {
    warning("No event_core table found in ", dwca_file)
  }

  if ("occurrence.txt" %in% filez) {
    occurrence_ext <- suppressWarnings(
      suppressMessages(readr::read_tsv(unz(dwca_file, "occurrence.txt"))) %>%
      select(-id))
  } else {
    message("No occurrence extension table found in ", dwca_file)
  }

  if ("measurementorfact.txt" %in% filez) {
    mof <- suppressWarnings(
      suppressMessages(readr::read_tsv(unz(dwca_file, "measurementorfact.txt"))) %>%
      rename(eventID = id))
  } else {
    message("No 'measurement or facts' found in ", dwca_file)
  }

  res <- list(
    "event_core" = as_tibble(event_core),
    "occurrence_ext" = as_tibble(occurrence_ext),
    "measurementorfact" = as_tibble(mof)
  )

  n_probs <- function(x) {
    probs <- attr(suppressWarnings(x), "problems")
    if (is.null(probs))
        0
    else nrow(probs)
  }

  tables_with_probs <- purrr::map_lgl(res,
    function(x) n_probs(x) > 0
  )

  if (any(tables_with_probs)) {
    res$has_parsing_issues <- TRUE
    res$parsing_issues <- purrr::map(res[tables_with_probs], readr::problems) %>%
      bind_rows(.id = "table")
    res$parsing_issues$file <- dwca_file
    warning("Found parsing issues in ", dwca_file,
      ", details are in result$parsing_issues", "\n")
  } else {
    res$parsing_issues <- NULL
    res$has_parsing_issues <- FALSE
  }

  return (res)
}

# get EML at IPT and generate suitable citation
eml_download <- function(url) {
  tmp <- tempfile()
  download.file(url = url, destfile = tmp, quiet = TRUE)
  meta <- xml2::read_xml(tmp) %>% xml2::as_list()
  gbif_citation <- meta$additionalMetadata$metadata$gbif$citation[[1]]
  citation <- gsub("GBIF.org", paste(url), gbif_citation)
  res <- list(
    eml = meta,
    citation = citation
  )
  return (res)
}


# combine and add the above to a function that could be exported
# this function needs proper documentation

#' Sampling Event Data Download from IPT
#'
#' This function downloads DwC-A directly from the IPT. The GBIF API does not
#' yet throw back empty events and potential hierarchical
#' structures which may be essential for inferring sampling effort. This is under
#' implementation at GBIF and we hope that this function will soon be redundant.
#'
#' @param key character string with sampling event dataset identifier from GBIF
#' @return A list with slots for metadata (`meta`), for the DwcA tables (`dwca`) and all the data from the various sampling event data tables joined into one data frame (`data`)
#' @examples
#' \dontrun{
#' sed <- sampling_event_data("78360224-5493-45fd-a9a0-c336557f09c3")
#' }
#' @import tidyverse tidyr dplyr jsonlite readr tibble xml2
#' @importFrom utils download.file unzip
#' @export
#'
sampling_event_data <- function(key) {

  url_dwca <-
    dataset_details(key)$endpoints %>%
    filter(type == "DWC_ARCHIVE") %>%
    .$url

  dwca_file <- dwca_download(url_dwca)
  dwca <- dwca_parse(dwca_file)
  unlink(dwca_file)

  url_eml <-
    dataset_details(key)$endpoints %>%
    filter(type == "EML") %>%
    .$url

  eml <- eml_download(url_eml)

  df <- dwca$event_core

  if (nrow(dwca$occurrence_ext) > 0) {
    if ("eventID" %in% names(dwca$occurrence_ext)) {
      df <- df %>% left_join(dwca$occurrence_ext, by = "eventID")
    } else {
      warning("occurrence extension table lacks eventID for key ", key)
    }
  }

  if (nrow(dwca$measurementorfact) > 0) {
    if (all(c("eventID", "measurementValue", "measurementType") %in%
        names(dwca$measurementorfact))) {
      mof_wide <-
        dwca$measurementorfact %>%
          select(eventID, measurementValue, measurementType, -contains("measurementID")) %>%
          spread(key = measurementType, value = measurementValue)
      df <- df %>% left_join(mof_wide, by = "eventID")
    } else {
      warning("measurementorfacts table lacks eventID, ",
        "measurementValue or measurementType for key ", key)
    }
  }

  res <- list(
    meta = eml,
    dwca = dwca,
    data = df
  )

  return (res)
}

# add this to fix some R CMD check complaints

#' @importFrom utils globalVariables
if (getRversion() >= "2.15.1")
  globalVariables(names = unlist(strsplit(split = " ",
 paste(". eventID measurementType measurementValue type"))))
