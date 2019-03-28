# quickly build html documents for all markdown files in folder

files <- list.files("worked_examples/", pattern = "\\.Rmd", full.names = TRUE)

library(rmarkdown)
walk(files, render, output_format = html_document())
