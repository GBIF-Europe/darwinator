<!-- README.md is generated from README.Rmd. Please edit that file -->
[![Build Status](https://travis-ci.org/GBIF-Europe/darwinator.svg?branch=master)](https://travis-ci.org/GBIF-Europe/darwinator)

Introduction
------------

`darwinator` is an R package that can download sampling event datasets published at GBIF.org from IPT installations.

Installing from github
----------------------

If you want to install the latest version of the `darwinator` package from github, you can do it like so:

``` r
# First make sure you have the devtools package
# which simplifies installations from github
# Note: Windows users have to first install Rtools to use devtools

install.packages("devtools") 
library(devtools)
install_github("GBIF-Europe/darwinator")
```

Quick start
-----------

To see some quick usage examples to get you started, open the Vignette.

Load the package in your R environment:

``` r

library(darwinator)
sed <- sampling_event_data("78360224-5493-45fd-a9a0-c336557f09c3")

df <- sed$data
dwca <- sed$dwca
citation <- sed$meta$citation
```

Another example - get all GBIF dataset identifiers for Norwegian sampling event based datasets using `rgbif`. Then download all sampling event data from Norway saving it locally to enable off-line work.

``` r
library(tidyverse)
library(rgbif)
library(darwinator)

# make a search for all Norwegian sampling event dataset identfiers
search <-
  dataset_search(
    type = "SAMPLING_EVENT",
    publishingCountry = "NO"
  )
keys <- search$data$datasetKey

# use dplyr::possibly to wrap the sampling_event_data function
# so that it returns NULL if it fails on a particular dataset
library(purrr)
psed <- possibly(function(key) sampling_event_data(key), NULL)
norway <- map(keys, psed)

# exclude failed requests
fails <- unlist(map(norway, is.null))
failed_keys <- keys[fails]
successful_downloads <- norway[!fails]

# save all successful downloads
saveRDS(dl_success, "norway-sed.Rds")

# a report of parsing issues can be generated like this:
parsing_issues <- map_df(successful_downloads, c("dwca", "parsing_issues"))
readr::write_excel_csv(issues_norway, "issues-norway.csv")
```

For more usage examples, please see the vignette.

Meta
----

Please note that this project is released with a [Contributor Code of Conduct](CONDUCT.md). By participating in this project you agree to abide by its terms.

-   Please [report any issues or bugs](https://github.com/GBIF-Europe/darwinator/issues).
-   License: AGPL
