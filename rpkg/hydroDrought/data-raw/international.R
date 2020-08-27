library(tidyverse)
library(readxl)

path <- "../../data/intl_dataset/xls"

# Import station meta data ----
meta <- file.path(path, "metadata_provided.xlsx") %>%
    read_xlsx(na = c("NA", "")) %>%
    select(
        id, river = "River", site = "Site", country = "Country", start = "Start",
        end = "End", catchment = "Catchment Area(km2)",
        site.altitude = "Station Altitude  (m.a.s.l)", summer.start, summer.end,
        mean.flow = "Mean flow (m3s-1)", comment = "Basis of Selection",
        qmin = "Qmin", qmean = "Qmean", qmax = "Qmax", provider
    ) %>%
    mutate(
        river = factor(river, levels = river),
        summer.start = hydroDrought:::.origin(summer.start),
        summer.end = hydroDrought:::.origin(summer.end)
    )

computable <- c("start", "end", "mean.flow",  "qmin", "qmean", "qmax")

meta.provided <- meta %>%
    select(river, all_of(computable))

meta <- meta %>%
    select(-all_of(computable))



# Import Discharges from Excel Files ----
read_intl_xlsx <- function(filename)
{
    x <- read_xlsx(path = filename, sheet = 1,
                   range = cell_cols(1:2), col_names = c("time", "discharge"),
                   col_types = c("guess", "numeric"), na = "-0.999")

    if (any(is.character(x$time))) {
        x <- x %>% mutate(time = as.Date(time, format = "%d-%m-%Y"))
    }

    x <- x %>%
        mutate(time = as.Date(time))

    if (any(is.na(x$time))) {
        message("Datum falsch in: ", filename)
    }

    x <- hydroDrought::sanitize_ts(x, id = filename, force_positive = TRUE)

    return(x)
}


flowdata <- tibble(
    filename = list.files(path = path, pattern = "^\\d+[ab]?\\..*\\.xlsx")
) %>%
    extract(
        filename, into = c("id", "text"),
        regex = "^([0-9]+[ab]?). (.*)$",
        remove = FALSE, convert = TRUE
    ) %>%
    separate(
        col = text,
        into = c("river", "site", "country", "year", NA), sep = "[,.]"
    ) %>%
    mutate(across(where(is.character), .fns = trimws)) %>%
    select(id, filename) %>%
    full_join(select(meta, river, id), by = "id") %>%
    arrange(river) %>%
    mutate(
        data = map(file.path(path, filename), read_intl_xlsx),
    ) %>%
    select(river, data)


meta.computed <- flowdata %>%
    mutate(
        start = map_dbl(data, ~year(min(.x$time))),
        end = map_dbl(data, ~year(max(.x$time))),
        years = map_other(data, ~diff(range(.x$time))),
        years = as.double(years, unit = "days") / 365,
        qmin = map_dbl(data, ~min(.x$discharge, na.rm = TRUE)),
        qmean = map_dbl(data, ~mean(.x$discharge, na.rm = TRUE)),
        qmax = map_dbl(data, ~max(.x$discharge, na.rm = TRUE))
    ) %>%
    select(-data)


# compare computed and provided metadata
# they are identical
m <- lst(meta.computed, meta.provided) %>%
    enframe() %>%
    unnest(value) %>%
    select(-mean.flow, -years) %>%
    pivot_longer(cols = -c(name, river), names_to = "metric") %>%
    pivot_wider()

# ggplot(m, aes(meta.computed, meta.provided)) +
#     geom_point() +
#     ggrepel::geom_text_repel(aes(label = river), point.padding = 0.2, direction = "x") +
#     scale_x_log10() +
#     scale_y_log10() +
#     geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
#     facet_wrap(vars(metric), scales = "free")
#

# data gaps ----

gaps <- flowdata %>%
    unnest(data) %>%
    group_by(river) %>%
    mutate(gap.no = group_const_value(is.na(discharge))) %>%
    filter(is.na(discharge)) %>%
    group_by(river, gap.no) %>%
    summarise(start = first(time), end = last(time), days = n(), .groups = "drop_last") %>%
    mutate(gap.no = order(gap.no))

# combine data and meta data
intl <- flowdata %>%
    full_join(meta, by = "river") %>%
    full_join(meta.computed, by = "river") %>%
    select(-id)

usethis::use_data(intl, overwrite = TRUE)
