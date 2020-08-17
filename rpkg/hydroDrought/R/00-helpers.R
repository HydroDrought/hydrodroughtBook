#' @export

moving_average <- function(x, n, sides = "past")
{
    dict <- c("past" = 1, "center" = 2, "future" = 3)
    if (is.character(sides)) {
        sides <- pmatch(sides, names(dict))
    } else if (is.numeric(sides)) {
        sides <- match(sides, dict)
    }
    if (is.na(sides)) stop("content of argument 'sides' is invalid.")

    sides <- dict[sides]

    if (sides == 3) {
        sides <- 1
        y <- rev(stats::filter(rev(x), filter = rep(x = 1/n, times = n), sides = sides))
    } else {
        y <- stats::filter(x, filter = rep(x = 1/n, times = n), sides = sides)
    }

    # filter() returns a ts-object
    return(as.numeric(y))
}

# .jday <- function(x) {
#     #as.numeric(format(as.Date(x), "%j"))
#
#     # set the year to year 1972 (which is a leap year)
#     year(x) <- 1972
#     return(x)
# }

#' @export
append_group <- function(x, by = c("day", "week", "month", "season", "year"),
                         start = "-01-01", unique.id = FALSE)
{
    by <- match.arg(by, several.ok = TRUE)
    x$time <- as.Date(x$time)
    start <- regmatches(start, regexpr("-.*", start))

    # always calculate the hydrological year, only id is meaningful
    x$year <- as.integer(substring(group_id(x$time, start[1]), 1L, 4L))

    if ("season" %in% by) {
        if (length(start) < 2) {
            warning("There have to be a least two seasons. Specify argument 'start' accordingly.")
        }
        season.id <- group_id(x$time, start)

        x$season <- factor(substr(season.id, 5L, 10L), levels = start)

        # if existing, use names of the group
        nam <- names(start)
        is.named <- length(nam) == length(start) && all(!is.na(nam)) && all(nam != "")
        if (is.named) levels(x$season) <- nam

        if (unique.id) x$season.id <- season.id
    }

    # only week and month are trivial
    f <- c(week = week, month = month)
    for (i in setdiff(by, c("year", "season", "day"))) {
        #for (i in setdiff(by, c("year", "season"))) {
        x <- mutate(x, !!i := f[[i]](x$time))
        if (unique.id) x[[paste0(i, ".id")]] <- paste(x$year, x[[i]], sep = "-")
    }

    # treat day differently, we need to pass the start of the year
    if ("day" %in% by) {
        x <- mutate(x, day = monthDay(x$time, origin = start[1]))
        if (unique.id)  x <- mutate(x, day.id = as.integer(x$day))
    }

    return(x)
}

#' @export
group_id <- function(time, starts)
{
    starts <- regmatches(starts, regexpr("-.*", starts))

    # all relevant years of the time series
    # first season break could be in the year before
    year <- sort(unique(year(time)))
    year <- c(year, min(year) - 1, max(year) + 1)

    # paste years with season starts
    # vectorized comparison is quite fast
    breaks <- sort(as.Date(outer(year, starts, paste0)))
    season <- outer(time, breaks, ">=")

    return(breaks[rowSums(season)])
}

#' @export
water_year <- function(time, start = "-01-01")
{
    year(group_id(time = time, starts = start))
}

#' @export
var_threshold <- function(x, vary.by = c("day", "week", "month", "season", "year"),
                          fun, start = "-01-01", append = FALSE, ...)
{
    vary.by <- match.arg(vary.by)
    y <- append_group(x, by = vary.by, start = start)

    if (vary.by == "day") {
        # interpolate Feb 29th with "surrounding" days if not a leap year
        leapday <- monthDay(as.Date("1972-02-29"), origin = start)

        leapdays <- filter(y, day == leapday - 1 | day == leapday + 1,
                           !leap_year(time)) %>%
            group_by(year) %>%
            summarise(discharge = mean(discharge),
                      day = leapday) %>%
            # surrounding values could be NA
            filter(!is.na(discharge))

        y <- bind_rows(y, leapdays)
    }

    threshold <- y %>%
        # summaries with NA values do not make sense, avoids to always specify na.rm = TRUE
        filter(!is.na(discharge)) %>%
        group_by(.dots = vary.by) %>%
        summarise(threshold = fun(discharge, ...))

    if (append) {
        res <- left_join(y, threshold, by = vary.by)
    } else {
        res <- threshold
    }

    attr(res, "threshold") <- list(vary.by = vary.by, start = start)

    return(res)
}


#' @export
const_threshold <- function(x, fun, append = FALSE, ...)
{
    threshold <- x %>%
        # summaries with NA values do not make sense, avoids to always specify na.rm = TRUE
        filter(!is.na(discharge)) %>%
        summarise(threshold = fun(discharge, ...))

    if (append) {
        return(mutate(x, threshold = threshold$threshold))
    } else {
        return(threshold)
    }
}


# gleiche ID, solange sich die Werte nicht ändern
.rle_id <- function(x)
{
    cumsum(x != lag(x, default = x[1]))
}

#' @export
.group_const_value <- .rle_id

# gleiche ID, solange die Änderung konstant ist
.group_const_change <- function(x, change = median(diff(x), na.rm = TRUE))
{
    d <- c(change, diff(x))
    cumsum(d != change) + 1
}



# .time_difference <- function(time)
# {
#     dt <- .guess_dt(time)
#
#     if (!is.na(dt)) {
#         # for regular time series we can estimate the duration of the
#         # last interval
#         inc <- as.double(diff(c(time, tail(time, 1) + dt)), units = "secs")
#     } else {
#         # for irregular time series it will be NA
#         inc <- c(as.double(diff(time), units = "secs"), NA)
#     }
#
#     return(inc)
# }

.guess_dt <- function(time)
{
    u <- c("second", "minute", "hour", "day", "month", "year")

    inc <- int_diff(time)
    for (i in u) {
        # cat(i, ", ")
        #rounded <- floor_date(time, unit = i, week_start = week_start)

        span <- as.period(inc, unit = i)

        # cannot use unique(): removes class
        s <- span[!duplicated(span)]
        regular <- length(s) == 1

        # if(regular && s == period(1, units = i)) {
        if (regular) {
            return(s)
        }
    }
    # irregular time series
    return(NA)
}


#' @export
sanitize_ts <- function(x, # approx.missing = 0,
                        time_col = "time", value_col = "discharge",
                        id = NA_character_, message = TRUE,
                        force_positive = FALSE, add_implicit_NA = TRUE,
                        remove_duplicates = TRUE, sort = TRUE)
{
    id <- as.character(id)
    xx <- x %>%
        select(time = !!time_col, value = !!value_col) %>%
        mutate(
            # todo: works currently only for daily data
            # todo: generalize for POSIXct
            time = as.Date(time),
            value = as.numeric(value)
        )

    msg <- msg_collector(n_in = nrow(x))

    ## checks on the DATA VALUES
    # explicitly missing values
    bad <- filter(xx, !is.finite(value))
    msg$append(what = "explicitly missing values", data = bad, id = id,
               n_total = nrow(xx))

    # negative values
    bad <- filter(xx, value < 0)
    if(force_positive) {
        xx$value[xx$value < 0] <- NA
        action <- "Setting them to NA."
    } else {
        action <-  ""
    }
    msg$append(what = "values below zero", data = bad, id = id,
               n_total = nrow(xx), action = action)


    # zeros
    bad <- filter(xx, abs(value) < sqrt(.Machine$double.eps))
    msg$append(what = "values equal to zero", data = bad, id = id,
               n_total = nrow(xx))

    # checks on the TIME INDICES
    # missing values in time index
    bad <- filter(xx, is.na(time))
    xx <- filter(xx, !is.na(time))
    msg$append(what = "invalid time indices", data = bad, id = id,
               n_total = nrow(xx), action = "Removing these records.")


    # exact duplicated time indices
    dups <- xx %>%
        group_by(time, value) %>%
        filter(n() > 1)
    msg$append(what = "completely identical input records", data = dups, id = id,
               n_total = nrow(xx), action = "Removing them.")
    xx <- distinct(xx)

    # duplicated time indices
    dups <- xx %>%
        group_by(time) %>%
        filter(n() > 1)
    if (remove_duplicates) {
        idx <- duplicated(xx$time)
        xx <- xx[!idx, ]
        action <-  "Keeping first occurance."
    } else {
        action <-  ""
    }

    msg$append(what = "duplicated time indices", data = dups, id = id,
               n_total = nrow(xx), action = action)

    # ordered/unordered
    bad <- filter(xx, time < lag(time, default = min(time, na.rm = TRUE)))
    if (sort) {
        xx <- arrange(xx, time)
        action <-  "Sorting time series."
    } else {
        action <-  ""
    }

    msg$append(what = "time indices jumping to the past (unordered time series)",
               data = bad, action = action,
               id = id, n_total = nrow(xx))

    # gaps
    # gaps are implicitly missing values
    gaps <- tibble(
        time = seq(from = min(xx$time), to = max(xx$time), by = "day"),
    ) %>%
        anti_join(xx, by = "time") %>%
        mutate(
            gap = .group_const_change(time),
            value = NA_real_
        )

    if(nrow(gaps)) {
        if (add_implicit_NA) {
            xx <- bind_rows(xx, select(gaps, time, value))
            action <- "Adding NA values."
            if (sort) {
                xx <- arrange(xx, time)
                action <- "Adding NA values and sorting."
            }
        } else {
            action <- ""
        }

        txt <- paste("implicitly missing values (", max(gaps$gap), "gaps)")
        msg$append(what = txt, data = gaps, n = nrow(gaps),
                   id = id, n_total = nrow(xx), action = action)


    }




    msg$final(n_out = nrow(xx))

    if (message) msg$print()

    res <- xx %>%
        select(!!time_col := time, !!value_col := value)

    attr(res, "message") <- msg$export()
    return(res)
}

msg_collector <- function(n_in) {

    m <- tibble(id = character(), what = character(), data = list(), n = integer())
    n_in <- n_in
    n_out <- 0

    return(list(
        append = function(what, data, id = NA, n = nrow(data), action = "",
                          n_total = NA, force_message = FALSE)
        {
            if(nrow(data) > 0 | force_message) {
                m <<- bind_rows(m, tibble(id = id,
                                          what = what,
                                          data = list(data),
                                          n = n, action = action,
                                          n_total = n_total))
            }
        },

        print = function()
        {
            .print_one_message <- function(m, id = NA)
            {
                n_total <- max(m$n_total, na.rm = TRUE)
                perc <- paste0(" (",
                               format(m$n / n_out * 100, nsmall = 1, digits = 1),
                               "%)")

                n <- list("in" = n_in, "out" = n_out)
                n <- if(n_in == n_out) {
                    paste("  Importing and exporting:", n_in, "rows.")
                } else {
                    d <-  n_out - n_in
                    paste0("  Importing ", n_in, ", and exporting ", n_out,
                           " rows (", abs(d), ifelse(d < 0, " fewer", " more"), " rows).")
                }

                txt <- paste(
                    c(paste0("Dataset: ", ifelse(!is.na(id), id, "")),
                      n,
                      paste0("  ", format(m$n), perc,
                             " ", m$what, ". ", m$action)), collapse = "\n"
                )
                message(txt)
            }

            mesg <- m %>%
                nest(data = c(-id))

            walk2(mesg$data, mesg$id, .print_one_message)
        },

        export = function() return(m),
        final = function(n_out) n_out <<- n_out
    ))
}

# damit map() auch für difftime und POSIX Objekte funktioniert
#' @export
map_other <- function(.x, .f, ...)
{
    reduce(map(.x, .f, ...), c)
}

