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
dl_dwca <- function(url) {

  dwca_file <- tempfile(fileext = ".zip")
  dwca_dir <- dirname(dwca_file)
  download.file(url, destfile = dwca_file, mode = "wb", quiet = TRUE)
  unzip(dwca_file, exdir = dwca_dir)

  event_file <- file.path(dwca_dir, "event.txt")
  occ_file <- file.path(dwca_dir, "occurrence.txt")
  mof_file <- file.path(dwca_dir, "measurementorfact.txt")

  occurrence_ext <-
    suppressMessages(readr::read_tsv(occ_file)) %>%
    select(-id)

  event_core <- NULL
  mof <- NULL

  if (file.exists(event_file)) {
    event_core <-
      suppressMessages(readr::read_tsv(event_file)) %>%
      select(-id)
  } else {
    warning("No event_core table found in ", url)
  }
  if (file.exists(mof_file)) {
    mof <-
      suppressMessages(readr::read_tsv(mof_file)) %>%
      rename(eventID = id)
  } else {
    message("No 'measurement or facts' found in ", url)
  }

  res <- list(
    event_core = event_core,
    occurrence_ext = occurrence_ext,
    measurementorfact = mof
  )

  return (res)
}

# get EML at IPT and generate suitable citation
dl_eml <- function(url) {
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

  dwca <- dl_dwca(url_dwca)

  url_eml <-
    dataset_details(key)$endpoints %>%
    filter(type == "EML") %>%
    .$url

  eml <- dl_eml(url_eml)

  df <-
    dwca$event_core %>%
    left_join(dwca$occurrence_ext, by = "eventID")

  if (!is.null(dwca$mof)) {
    mof_wide <-
      dwca$measurementorfact %>%
      select(eventID, measurementValue, measurementType) %>%
      spread(key = measurementType, value = measurementValue)
    df <- df %>% left_join(mof_wide, by = "eventID")

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
