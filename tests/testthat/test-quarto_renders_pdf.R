test_that("quarto renders pdf", {
  skip_if_not_pandoc("2.0")
  the_path <- fs::file_temp(pattern = "quarto_pdf")
  scoped_temporary_project(dir = the_path)
  create_quarto_doc("untitled", open = FALSE)
  f <- "untitled/untitled.qmd"
  add_to_quarto_file(f)
  quarto::quarto_render(f)
  expect_true(file.exists("untitled/untitled.html"))
  expect_true(file.exists("untitled/untitled.pdf"))

})
