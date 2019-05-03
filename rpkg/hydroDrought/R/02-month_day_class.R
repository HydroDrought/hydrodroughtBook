as.monthDay <- function(x, origin = "-01-01")
{
    x <- as.Date(x)
    origin <- regmatches(origin, regexpr("-.*", origin))

    # origin must exist in non-leap years
    i <- tryCatch(as.Date(paste0("1971", origin)),
                  error = function(e) stop(origin, " is not a valid origin.",
                                           call. = FALSE))

    year(x) <- 1972

    # year has to conatin Feb 29th.
    # if origin is after Feb 29th: take the previous or next year
    o <- paste0("1972", origin)
    if (o > as.Date("1972-02-29")) {
        year(x[x >= o]) <- 1971
        origin <- as.Date(paste0("1971", origin))
    } else {
        year(x[x < o]) <- 1973
        origin <- as.Date(paste0("1972", origin))
    }

    attr(x, "origin") <- origin
    class(x) <- c("monthDay", "Date")
    return(x)
}

pillar_shaft.monthDay <- function(x, ...) {
    out <- format(x)
    out[is.na(x)] <- NA
    pillar::new_pillar_shaft_simple(out, align = "right")
}

type_sum.monthDay <- function(x) {
    "month-day"
}


# df <- data_frame(input = seq(as.Date("2011-01-01"), as.Date("2012-01-01"), by = "1 day"),
#                  md = as.monthDay(input, origin = "-04-01"),
#                  year = as.factor(year(md)))
#
# df %>%
#     ggplot(aes(md, year)) +
#     geom_raster(fill = "black")
#


# as.monthDay(today())

print.monthDay <- function(x, format = "-%m-%d", ...)
{
    print(format.Date(x, format = format))
}

format.monthDay <- function(x, format = "-%m-%d", ...)
{
    format.Date(x, format = format)
}

# doesn't work
# filter(ng, md == "-02-29")

# ggplot doesn't work with as.double method
# as.double.monthDay <- function(x, ...) {
#     unclass(x) - as.numeric(attr(x, "origin")) + 1
# }


as.integer.monthDay <- function(x, ...) {
    as.integer(unclass(x) - as.numeric(attr(x, "origin")) + 1)
}

#
# ng <- filter(international, River == "Ngaruroro") %>%
#     select(discharge) %>%
#     unnest() %>%
#     append_group("year", start = "-09-01") %>%
#     mutate(md = as.monthDay(time, origin = "-09-01"),
#            day = as.integer(md),
#            date = md)
#
# class(ng$date) <- "Date"
#
# ggplot(ng, aes(x = md, y = year)) +
#     geom_raster() +
#     scale_x_date(date_breaks = "1 months", date_labels = "%b")
