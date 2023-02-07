library(purrr)
library(rmarkdown)

### Find only the R Markdown files
files <- list.files("rmarkdown", pattern = "^[1567].*\\.Rmd$", full.names = T)
files

### THis can save time if you only want to run a few of the files
files <- files[seq(2, 15)]

### To create word documents
walk(files, render, output_format = "word_document", output_dir = "word", envir = new.env())
