# quickly build html documents for all markdown files in folder

files <- list.files("worked_examples/files/", pattern = "\\.Rmd", full.names = TRUE)

library(rmarkdown)
walk(files, render, output_format = tufte::tufte_html(tufte_features = c("fonts"),  css = "style.css"))
