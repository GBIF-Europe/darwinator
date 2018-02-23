#!/usr/bin/Rscript

library(rgbif)
library(purrr)
library(dplyr)
library(readr)
library(darwinator)

# make a search using rgbif for:
# all Norwegian sampling event dataset identfiers

search <-
  dataset_search(
    type = "SAMPLING_EVENT",
    publishingCountry = "NO"
  )

norway_keys <- search$data$datasetKey

psed <- possibly(function(key) sampling_event_data(key), NULL)
norway <- map(norway_keys, psed)

# exclude failed requests
fails <- unlist(map(norway, is.null))
stopifnot(any(fails))
message("crawl went well!")

# persist data locally
failed_keys <- norway_keys[fails]
successful_downloads <- norway[!fails]
message("saving all data locally, count of datasets: ",
  length(successful_downloads))
saveRDS(successful_downloads, "norway.Rds")

parsing_issues <- map_df(successful_downloads, c("dwca", "parsing_issues"))
message("saving csv with parsing issues locally, count of parsing issues: ",
  nrow(parsing_issues))
write_excel_csv(parsing_issues, "issues-norway.csv")
q(status = 0)
