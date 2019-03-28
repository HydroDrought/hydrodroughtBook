library(tidyverse)

dir <- "worked_examples/skeleton"
dir.create(dir, showWarnings = FALSE)

tbl <- tribble(
    ~number, ~title, ~comment, ~status,

    # Hege
    "5.1", "Flow duration curve",
    "new R-script and updated time series, worked example changed accordingly",
    "almost completed",

    "5.2", "Mean annual minimum flow",
    "new R-script and updated time series, worked example changed accordingly",
    "",

    "5.3", "Base Flow Index",
    "new R-script and updated time series, worked example changed accordingly",
    "",

    "5.4", "Threshold level method",
    "Nizowka replaced by new R-script (+updated time series), worked example changed accordingly",
    "",

    "5.5", "Sequent Peak Algorithm",
    "Decided to keep as the method is applied operationally by the hydropower sector. New R-script and updated time series",
    "",

    "5.6", "Standardized Groundwater Index",
    "to be made by John in collaboration with Tobias",
    "",

    "5.7", "Rank and correlation coefficients",
    "To be implemented in R + new regional dataset – updated by Nico –in collaboration with Tobias",
    "",


    # Jim
    "7.1", "Linear regression and residuals",
    "", "",

    "7.2", "Multiple regression",
    "incl. variable selection", "",

    "7.3", "Regression extensions",
    "logistic, poisson, GLMs", "",

    "7.4", "Trend analysis",
    "", "",

    "7.5", "Structural change analysis",
    "", "",

    "7.6", "PCA fundamentals",
    "", "",

    # Kerstin
    "12.1",  "EDII text report",
    "This will not have any R Code, it will consist of an example text and our instructions.",
    "",

    "12.2",  "Likelihood of impact occurrence",
    "will be based on data and code from Jim’s JoH paper",
    "",

    # Christel
    "13.1", "Forecast skill",
    "Lambourne catchment and will use GR4 model, calibrated using the airGR4 R toolset",
    ""
)

from_template <- function(number, title,
                          template = "./worked_examples/skeleton/template.Rmd")
{
    filename <- paste0(sub(".", "-", x = number, fixed = TRUE),
                       "_", gsub(" ", "_", x = tolower(title)), ".Rmd")

    x <- readLines(template) %>%
        sub(pattern = "@number", replacement = number, fixed = TRUE) %>%
        sub(pattern = "@title", replacement = title, fixed = TRUE)

    writeLines(x, con = file.path(dirname(template), filename))
}

walk2(tbl$number, tbl$title, from_template)


list_entry <- function(number, title, comment = "", ...)
{
    filename <- paste0(sub(".", "-", x = number, fixed = TRUE),
                       "_", gsub(" ", "_", x = tolower(title)), ".html")
    paste0(" * Worked example ", number, ": [", title, "]",
           "(https://combinatronics.com/jstagge/hydroDrought/master/worked_examples/",
           filename, ") ",
           ifelse(comment != "", paste0("(", comment, ")"), ""))
}

pmap_chr(tbl, list_entry) %>%
    cat(fill = TRUE)

