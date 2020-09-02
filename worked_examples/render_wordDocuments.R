library(purrr)
library(rmarkdown)
files <- list.files("rmarkdown", pattern = "^[56].*\\.Rmd$", full.names = T)
walk(files, render, output_format = "word_document", output_dir = "word", envir = new.env())
