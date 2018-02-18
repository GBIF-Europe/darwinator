context("sampling event data tests for a couple of datasets")

test_that("citation is correct", {
  key <- "78360224-5493-45fd-a9a0-c336557f09c3"
  expect_equal(
    sampling_event_data(key)$meta$citation,
    "Finstad A G, Hendrichsen D K, Nilsen E (2015): Lepidurus arcticus survey Northeast Greenland 2013. v1.8. NTNU University Museum. Dataset/Samplingevent. http://gbif.vm.ntnu.no/ipt/resource?r=lepidurus-arcticus-survey_northeast-greenland_2013&v=1.8"
  )
})

test_that("31 rows of data ", {
  key <- "78360224-5493-45fd-a9a0-c336557f09c3"
  expect_equal(
    nrow(sampling_event_data(key)$data),
    31
  )
})

test_that("a sampling dataset without MOF table works", {
  key <- "c47f13c1-7427-45a0-9f12-237aad351040"
  expect_gt(
    nrow(sampling_event_data(key)$data),
    0
  )
})
