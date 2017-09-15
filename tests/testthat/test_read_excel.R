context("Reading excel files")

filename = btcore_file("ExcelSamples.xlsx")
expected_names = c("patient_id", "group", "minute", "pdr")

test_that("Several Excel formats are consistent after passing cleanup_data" , {
  library(readxl)
  sheets = excel_sheets(filename)
  # Four-columns sheets
  for (sheet in sheets[1:4]) {
    data = read_breathtest_excel(filename, sheet = sheet)
    cleanup_data(data)
    bt = cleanup_data(read_breathtest_excel(filename, sheet = sheet))
    expect_false(all(stringr::str_detect(bt$group, " ")))
    expect_equal(names(bt), expected_names, 
                 info = paste0("in sheet ", sheet, " of ", basename(filename))) 
  }  
  # 3-col Sheet without group
  sheet = 5
  bt = read_breathtest_excel(filename, sheet = sheet)
  expect_equal(names(bt[[1]]), expected_names[-2], 
               info = paste0("in sheet ", sheet, " of ", basename(filename))) 
  bt_c = cleanup_data(bt)
  expect_equal(names(bt_c), expected_names, 
                 info = paste0("in sheet ", sheet, " of ", basename(filename))) 

  # 2col sheet
  sheet = 6
  bt = read_breathtest_excel(filename, sheet = sheet)
  expect_equal(names(bt[[1]]), expected_names[-(1:2)], 
               info = paste0("in sheet ", sheet, " of ", basename(filename))) 
  bt_c = cleanup_data(bt)
  expect_equal(names(bt_c), expected_names, 
                 info = paste0("in sheet ", sheet, " of ", basename(filename)))
})
  

test_that("Invalid sheets rise error",{
  sheet = "bad_header"
  expect_error(read_breathtest_excel(filename, sheet = sheet),
               "column names should be")
  
  sheet = "bad_order"
  expect_error(read_breathtest_excel(filename, sheet = sheet),
               "column names should be")

  sheet = "bad_columns"
  expect_error(read_breathtest_excel(filename, sheet = sheet),
               "has 1 column")
})

