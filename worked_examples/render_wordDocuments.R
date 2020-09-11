library(purrr)
library(rmarkdown)
files <- list.files("rmarkdown", pattern = "^[56].*\\.Rmd$", full.names = T)
files <- files[c(8)]
walk(files, render, output_format = "word_document", output_dir = "word", envir = new.env())
