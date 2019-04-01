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

    "8.1", "Regional Regression",
    "", "",

    "8.2", "Top Kriging",
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
           "(https://combinatronics.com/jstagge/hydroDrought/master/worked_examples/files/",
           filename, ") ",
           ifelse(comment != "", paste0("(", comment, ")"), ""))
}

pmap_chr(tbl, list_entry) %>%
    cat(fill = TRUE)


chapters <- tribble(
    ~title, ~author,
    "Introduction", c("Lena Tallaksen", "Henny van Lanen"),
    "Hydroclimatology", c("Daniel Kingston", "Monica Ionita", "Kerstin Stahl"),
    "Drought Generating Processes", c("Henny van Lanen", "Anne van Loon Miriam Fendeková", "John Bloomfield"),
    "Hydrological Data", c("Jamie Hannaford", "Matt Fry", "Gregor Laaha", "Katie Muchan", "Gwyn Rees", "Henny van Lanen"),
    "Hydrological Drought Characteristics", c("Hege Hisdal", "John Bloomfield", "Tobias Gauster", "Simon Parry", "Lena Tallaksen", "Niko Wanders"),
    "Frequency Analysis", c("Lena M Tallaksen", "Hege Hisdal", "Gregor Laaha", "and Henrik Madsen"),
    "Statistical modelling", c("Jim Stagge", "Monica Ionita", "Daniel Kingston", "Lena Tallaksen"),
    "Regionalization Procedures", c("Gregor Laaha", "Tobias Gauster", "Eric Sauquet", "Kolbjørn Engeland"),
    "Process-based modelling", c("Henny van Lanen", "Anne van Loon", "Niko Wanders and Christel Prudhomme"),
    "Human Influences", c("Anne F. Van Loon", "Niko Wanders", "John Bloomfield", "Miriam Fendeková", "Henny A.J. Van Lanen"),
    "Past and Future Droughts", c("Niko Wanders", "Christel Prudhomme", "Katie Smith", "Jim Stagge", "Jean-Philippe Vidal"),
    "Drought Impacts", c("Kerstin Stahl", "Lucy Barker", "Veit Blauhut", "Jim Stagge"),
    "Drought Early warning", c("Christel Prudhomme", "Lucy Barker", "Carmelo Cammalleri", "Shaun Harrigan", "Monica Ionita", "Jürgen Vogt"),
    "Perspectives", c("Henny van Lanen", "Albert van Dijk", "Mark Svoboda", "Lena Tallaksen")
)

chapters  <- chapters %>%
    mutate(no = row_number(),
           lead = map_chr(author, 1))

fmt_chapter <- function(no, title, lead, ...)
{
    x <- paste(
        "  <tr>",
        paste0("    <td> <b> ", no, ". ", title, "</b> </td>"),
        paste0("    <td> <i>", lead, "</i> </td>"),
        "    <td>  </td>",
        "  </tr>\n", sep = "\n")

    cat(x)
}

fmt_listentry <- function(number, title, comment = "", ...)
{
    prefix <- "https://combinatronics.com/jstagge/hydroDrought/master/worked_examples/files/"
    html <- paste0(sub(".", "-", x = number, fixed = TRUE),
                   "_", gsub(" ", "_", x = tolower(title)), ".html")
    rscript <- paste0(sub(".", "-", x = number, fixed = TRUE),
                      "_", gsub(" ", "_", x = tolower(title)), ".R")

    x <- paste(
        paste0("  <li>", number, " ", title, ": "),
        paste0('    <a href="', prefix, html, '">Worked Example</a>,  '),
        paste0('    <a href="', prefix, rscript, '">R-script</a>'),
        ifelse(comment == "" || length(comment) == 0, "", paste0("    <br><i>", comment, "</i>")),
        "  </li>\n\n", sep = "\n"
    )

    cat(x)
}

pwalk(chapters, fmt_chapter)
pwalk(tbl, fmt_listentry)

tbl %>%
    mutate(no = as.numeric(sub("\\..*", "", number))) %>%
    right_join(chapters, by = "no")



# extract R code from Rmd files
dir <- "../../worked_examples/files"
files <- data_frame(filename = list.files(path = dir, pattern = "\\.Rmd$",
                                          full.names = TRUE)) %>%
    extract(col = filename, into = c("chapter", "number"),
            regex = c("(\\d+)-(\\d+)"), convert = TRUE, remove = FALSE) %>%
    arrange(chapter, number)

rmd <- files$filename[2]
knitr::purl(input = rmd, output = sub(".Rmd", ".R", rmd, fixed = TRUE))
