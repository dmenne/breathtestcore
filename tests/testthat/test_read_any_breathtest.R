test_that("Reading a single file returs a list of class breathtest_data",{
  file = btcore_file("IrisCSV.TXT")
  bt = read_any_breathtest(file)
  expect_s3_class(bt, "breathtest_data_list")
  expect_equal(length(bt), 1)  
  expect_s3_class(bt[[1]], "breathtest_data")
  expect_equal(bt[[1]]$patient_id, "123456")
})

test_that("Repeated groups are joined into one", {
  files1 = c(
    group_a = btcore_file("IrisCSV.TXT"),
    group_a = btcore_file("Standard.TXT"),
    group_b = btcore_file("350_20043_0_GER.txt")
  )
  bt = read_any_breathtest(files1)
  bt1 = cleanup_data(bt)
  expect_equal(unique(bt1$group), c("group_a", "group_b"))
})

test_that("Reading a composite file returns a list of class breathtest_data_list",{
  file = btcore_file("NewBreathID_multiple.xml")
  bt = read_any_breathtest(file)
  expect_s3_class(bt, "breathtest_data_list")
  expect_equal(length(bt), 3)  
  expect_s3_class(bt[[1]], "breathtest_data")
  expect_equal(bt[[1]]$patient_id, "07951400")
})

test_that("Reading multiple files returns a list with multiple items",{
  files = c(
    btcore_file("IrisCSV.TXT"),
    btcore_file("350_20043_0_GER.txt"),
    btcore_file("IrisMulti.TXT"),
    btcore_file("NewBreathID_multiple.xml"),
    btcore_file("ExcelSamples.xlsx")  
  )
  bt = read_any_breathtest(files)
  expect_s3_class(bt, "breathtest_data_list")
  expect_equal(length(bt), 7)  
  expect_s3_class(bt[[1]], "breathtest_data")
  expect_s3_class(bt[[7]], "tbl_df")
  expect_equal(bt[[1]]$patient_id, "123456")

  bt_c = cleanup_data(bt)
  expect_equal(ncol(bt_c), 4)
  expect_equal(nrow(bt_c), 223)
  expect_equal(bt_c$patient_id[1], "123456" )

  bt_c = cleanup_data(bt, use_filename_as_patient_id = TRUE)
  pat_names = unique(bt_c$patient_id)
  expect_equal(pat_names[1], "IrisCSV" )
  expect_equal(pat_names[3], "IrisMulti" )
})


