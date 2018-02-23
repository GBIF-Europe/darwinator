library(readr)
library(dplyr)
library(stringr)
library(purrr)
library(devtools)

dwc_terms_recommended <- function() {

  url_dwc <-
    paste0("https://raw.githubusercontent.com/tdwg/dwc/master", c(
      "/standard/vocabularies/term_versions.csv",
      "/dist/simple_dwc_vertical.csv")
    )

  dwc_full <-
    read_csv(url_dwc[1], col_types = "cDcccccc")

  dwc_simple <-
    read_csv(url_dwc[2], col_types = "c") %>%
    rename(label = type) %>%
    mutate(is_simple = TRUE)

  terms <-
    dwc_full %>%
    filter(status == "recommended") %>%
    filter(grepl("terms", term_iri)) %>%
    left_join(dwc_simple, by = "label") %>%
    mutate(is_simple = ifelse(is.na(is_simple), FALSE, TRUE)) %>%
    select(label, is_simple, definition, term_iri,
      everything(), -status, -replaces) %>%
    arrange(desc(is_simple), -desc(term_iri))

  return (terms)
}

dwc_terms <- dwc_terms_recommended()
devtools::use_data(dwc_terms, overwrite = TRUE)


# investingating dupes etc

dwc_dupes <-
  dwc_full %>%
  filter(status == "recommended") %>%
  filter(grepl("terms", term_iri)) %>%
  mutate(label = str_extract(term_iri, "[^/]*?$")) %>%
  left_join(dwc_simple, by = "label") %>%
  group_by(label) %>%
  summarize(count = n()) %>%
  filter(count > 1) %>%
  select(label)

iri_dupes <-
  dwc_full %>%
  filter(status == "recommended") %>%
  mutate(label = str_extract(term_iri, "[^/]*?$")) %>%
  inner_join(dwc_dupes, by = "label") %>%
  arrange(label) %>%
  select(label, term_iri)

terms <-
  dwc_full %>%
  filter(status == "recommended") %>%
  filter(grepl("terms", term_iri)) %>%
  mutate(term_type = str_extract(term_iri, "[^/]*?$")) %>%
  left_join(dwc_simple %>% rename(term_type = label), by = "term_type") %>%
  mutate(is_match = label == term_type)

#map_chr(str_split(terms, pattern = "/"), function(x) rev(x)[1])
