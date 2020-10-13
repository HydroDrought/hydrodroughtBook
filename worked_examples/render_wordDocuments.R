library(purrr)
library(rmarkdown)
files <- list.files("rmarkdown", pattern = "^[567].*\\.Rmd$", full.names = T)
#files <- files[c(8)]
files <- files[c(seq(1,11),13)]
walk(files, render, output_format = "word_document", output_dir = "word", envir = new.env())
