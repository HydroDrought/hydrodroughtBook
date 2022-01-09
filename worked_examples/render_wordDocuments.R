library(purrr)
library(rmarkdown)
files <- list.files("rmarkdown", pattern = "^[1567].*\\.Rmd$", full.names = T)
#files <- list.files("rmarkdown", pattern = ".*\\.Rmd$", full.names = T)
#files <- files[[1]]
#files <- files[c(8)]
#files <- files[c(seq(1,11),13)]
files <- files[seq(2, 15)]

files <- files[c(14,15)]

files <- files[seq(14,17)]

### To create word document
walk(files, render, output_format = "word_document", output_dir = "word", envir = new.env())

### To create web site
rmarkdown::render_site("rmarkdown")

### Other old code
#rmarkdown::render_site(input = "rmarkdown", output)
#rmarkdown::render_site()


