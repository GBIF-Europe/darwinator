library(darwinator)

context("sampling event data tests for a couple of datasets")

test_that("citation is correct", {
  skip_on_travis()
  key <- "78360224-5493-45fd-a9a0-c336557f09c3"
  citation <- sampling_event_data(key)$meta$citation
  expect_equal(
    citation,
    "Finstad A G, Hendrichsen D K, Nilsen E (2015): Lepidurus arcticus survey Northeast Greenland 2013. v1.8. NTNU University Museum. Dataset/Samplingevent. http://gbif.vm.ntnu.no/ipt/resource?r=lepidurus-arcticus-survey_northeast-greenland_2013&v=1.8"
  )
})

test_that("31 rows of data ", {
  skip_on_travis()
  key <- "78360224-5493-45fd-a9a0-c336557f09c3"
  df <- sampling_event_data(key)
  expect_equal(
    nrow(sampling_event_data(key)$data),
    31
  )
})

test_that("a sampling dataset without MOF table works", {
  skip_on_travis()
  key <- "c47f13c1-7427-45a0-9f12-237aad351040"
  expect_gt(
    nrow(sampling_event_data(key)$data),
    0
  )
})

test_that("parsing errors are found", {
  skip_on_travis()
  library(purrr)
  test_pe_keys <- c(
    "9c689543-6bf1-42e1-b280-b61d739e18c3",
  #  "a855a639-cccb-4286-af92-f7567d74a887",
  #  "33591b80-0e31-480c-82ce-2f57211b10e6",
    "57d403f4-9d00-4651-8e26-1f4e81a21181"
  )
  pe <- suppressWarnings(map(test_pe_keys, sampling_event_data))
  issues <- map_df(pe, c("dwca", "parsing_issues"))
  expect_lt(1, nrow(issues))
})

test_that("citation EML from IPT can be parsed", {
  skip_on_travis()
  #key <- "78360224-5493-45fd-a9a0-c336557f09c3"
  ipt_url <- paste0("https://gbif.vm.ntnu.no/ipt/eml.do",
    "?r=lepidurus-arcticus-survey_northeast-greenland_2013")
  eml <- darwinator:::eml_download(ipt_url)
  citation <- eml$citation
  expect_equal(
    citation,
    "Finstad A G, Hendrichsen D K, Nilsen E (2015): Lepidurus arcticus survey Northeast Greenland 2013. v1.8. NTNU University Museum. Dataset/Samplingevent. http://gbif.vm.ntnu.no/ipt/resource?r=lepidurus-arcticus-survey_northeast-greenland_2013&v=1.8"
  )
})

