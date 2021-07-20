
require(bookdown)


bookdown::render_book("index.Rmd", "bookdown::gitbook")


bookdown::render_book("index.Rmd", "bookdown::pdf_book")


