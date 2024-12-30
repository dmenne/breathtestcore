# Additional tests in test_cleanup_data

# Create hook function to deselect first record
choose_record = function(records) {
  r  = rep(TRUE, length(records))
  r[1] = FALSE
  r
}

test_that("read_breathid_xml handles hook", {
  filename = btcore_file("NewBreathID_multiple.xml")
  # Reset hook, just to be sure
  options(breathtestcore.choose_record = NULL)
  bids = read_breathid_xml(filename)
  expect_s3_class(bids, "breathtest_data_list")
  expect_equal(length(bids), 3)
  # Set hook
  options(breathtestcore.choose_record = choose_record)
  bids = read_breathid_xml(filename)
  expect_equal(length(bids), 2)
  # Deactivate hook
  options(breathtestcore.choose_record = NULL)
  bids = read_breathid_xml(filename)
  expect_equal(length(bids), 3)
})

test_that("read_breathid_xml hook not called when there is only one record", {
  filename = btcore_file("NewBreathID_01.xml")
  # Set hook
  # Reset hook, just to be sure
  options(breathtestcore.choose_record = NULL)
  bids = read_breathid_xml(filename)
  expect_equal(length(bids), 1)
  # Set hook
  options(breathtestcore.choose_record = choose_record)
  bids1 = read_breathid_xml(filename)
  expect_equal(length(bids1), 1)
  expect_identical(bids, bids1)
  options(breathtestcore.choose_record = NULL)
})

test_that("fitting difficult xml returns data",{
  filename = btcore_file("invalidnls.xml")
  data = cleanup_data(read_breathid_xml(filename))
  fit = nls_fit(data)
  expect_s3_class(fit, "breathtestfit")
  expect_match(comment(fit$data), "no valid fit")
})

test_that("Read December 2024 sample with errors has message in attribute",{
  # This sample had a record with only 2 data points
  filename = btcore_file("short_record.xml")
  xml_data = read_breathid_xml(filename)
  expect_match(attr(xml_data, "errors"), "Empty")
  data = cleanup_data(xml_data)
  expect_s3_class(data, "tbl")
}  )
