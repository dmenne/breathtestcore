context("Read breathid xml format")
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
  expect_is(bids, "breathtest_data_list")
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
