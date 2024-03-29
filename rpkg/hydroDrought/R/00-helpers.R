#' @importFrom rlang .data
#' @importFrom dplyr mutate %>% slice filter summarise group_by if_else select case_when lag lead ungroup pull left_join first last n rename bind_rows across transmute slice_max slice_min count arrange semi_join distinct arrange anti_join
#' @importFrom tidyselect all_of everything
#' @importFrom ggplot2 ggplot aes geom_line geom_point coord_cartesian scale_x_date scale_y_continuous expansion geom_hline scale_y_log10 geom_tile labs theme_bw element_rect facet_wrap theme element_blank geom_step expand_limits scale_fill_viridis_d scale_linetype_discrete  geom_ribbon labs guide_legend element_line scale_x_datetime
#' @importFrom scales squish_infinite
#' @importFrom tibble tibble lst
#' @importFrom tidyr pivot_longer pivot_wider nest unnest
#' @importFrom purrr walk2 map


#' @importFrom  stats quantile approx
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


#' @importFrom dplyr mutate %>% slice filter summarise group_by if_else select
#' @importFrom rlang :=
#' @export
group_ts <- function(time, by = c("day", "week", "month", "season", "year"),
                     origin = "-01-01", unique.id = FALSE)
{
    by <- match.arg(by)
    time <- as.Date(time)
    origin <- regmatches(origin, regexpr("-.*", origin))

    x <- .group_id_list(time = time, starts = origin)

    # only week and month are trivial
    if (by == "year") {
        group <- x$year
    } else if (by == "season") {
        group <- x$season
    } else if (by %in% c("week", "month")) {
        fun <- list(week = lubridate::week, month = lubridate::month)
        group <- fun[[by]](time)
    } else if(by == "day") {
        # treat day differently, we need to pass the origin of the year
        group <- monthDay(time, origin = origin[1])
    }

    # year is already a group for itself
    if (unique.id & by != "year") {
        if (is.unsorted(time)) warning("Time series is not sorted.")

        group <- paste(x$year, group_const_value(group), sep = ".")
    }

    return(group)
}


#' @importFrom lubridate year leap_year
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

.group_id_list <- function(time, starts)
{
    x <- group_id(time = time, starts = starts)
    season <- factor(substr(x, 5L, 10L), levels = starts)

    # if existing, use names of the group
    nam <- names(starts)
    is.named <- length(nam) == length(starts) && all(!is.na(nam)) && all(nam != "")
    if (is.named) levels(season) <- nam

    list(
        start = x,
        year = as.integer(substr(x, start = 1L, stop = 4L)),
        season = season
    )
}

#' @export
water_year <- function(time, origin = "-01-01")
{
    year(group_id(time = time, starts = origin))
}


#' @export
var_threshold <- function(x, vary.by = c("day", "week", "month", "season", "year"),
                          fun, start = "-01-01", append = FALSE, ...)
{
    # todo: var_threshold() should have arguments discharge = , time =,
    vary.by <- match.arg(vary.by)
    y <- x %>%
        mutate(
            year = water_year(time = .data$time, origin = start),
            group = group_ts(time = .data$time, by = vary.by, origin = start)
        )

    if (vary.by == "day") {
        # interpolate Feb 29th with "surrounding" days if not a leap year
        leapday <- monthDay(as.Date("1972-02-29"), origin = start)

        leapdays <- y %>%
            filter(.data$group == leapday - 1 | .data$group == leapday + 1,
                   !leap_year(.data$time)) %>%
            group_by(.data$year) %>%
            summarise(
                discharge = mean(.data$discharge),
                group = leapday
            ) %>%
            # surrounding values could be NA
            filter(!is.na(.data$discharge))

        y <- bind_rows(y, leapdays)
    }

    threshold <- y %>%
        # summaries with NA values do not make sense, avoids to always specify na.rm = TRUE
        filter(!is.na(.data$discharge)) %>%
        group_by(.data$group) %>%
        summarise(threshold = fun(.data$discharge, ...)) %>%
        rename(!!vary.by := .data$group)

    if (append) {
        res <- y %>%
            rename(!!vary.by := .data$group) %>%
            left_join(threshold, by = vary.by)
    } else {
        res <- threshold
    }

    attr(res, "threshold") <- list(vary.by = vary.by, origin = start)

    return(res)
}


#' @export
const_threshold <- function(x, fun, append = FALSE, ...)
{
    threshold <- x %>%
        # summaries with NA values do not make sense, avoids to always specify na.rm = TRUE
        filter(!is.na(.data$discharge)) %>%
        summarise(threshold = fun(.data$discharge, ...))

    if (append) {
        return(mutate(x, threshold = threshold$threshold))
    } else {
        return(threshold)
    }
}


# assign same IDs as log as values do not change
#' @export
group_const_value  <- function(x)
{
    cumsum(x != lag(x, default = x[1]))
}


#' @importFrom stats median
#' @export
# assign same IDs as log as change is constant
group_const_change <- function(x, change = median(diff(x), na.rm = TRUE))
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

#' @importFrom lubridate int_diff as.period
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
                        remove_duplicates = TRUE, sort = TRUE,
                        approx.missing = 0)
{
    id <- as.character(id)
    xx <- x %>%
        select(time = !!time_col, value = !!value_col) %>%
        mutate(
            # todo: works currently only for daily data
            # todo: generalize for POSIXct
            time = as.Date(.data$time),
            value = as.numeric(.data$value)
        )

    msg <- msg_collector(n_in = nrow(x))

    ## checks on the DATA VALUES
    # explicitly missing values
    bad <- filter(xx, !is.finite(.data$value))
    msg$append(what = "explicitly missing values", data = bad, id = id,
               n_total = nrow(xx))

    # negative values
    bad <- filter(xx, .data$value < 0)
    if(force_positive) {
        xx$value[xx$value < 0] <- NA
        action <- "Setting them to NA."
    } else {
        action <-  ""
    }
    msg$append(what = "values below zero", data = bad, id = id,
               n_total = nrow(xx), action = action)


    # zeros
    bad <- filter(xx, abs(.data$value) < sqrt(.Machine$double.eps))
    msg$append(what = "values equal to zero", data = bad, id = id,
               n_total = nrow(xx))

    # checks on the TIME INDICES
    # missing values in time index
    bad <- filter(xx, is.na(.data$time))
    xx <- filter(xx, !is.na(.data$time))
    msg$append(what = "invalid time indices", data = bad, id = id,
               n_total = nrow(xx), action = "Removing these records.")


    # exact duplicated time indices
    dups <- xx %>%
        group_by(.data$time, .data$value) %>%
        filter(n() > 1)
    msg$append(what = "completely identical input records", data = dups, id = id,
               n_total = nrow(xx), action = "Removing them.")
    xx <- distinct(xx)

    # duplicated time indices
    dups <- xx %>%
        group_by(.data$time) %>%
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
    bad <- filter(xx, .data$time < lag(.data$time, default = min(.data$time, na.rm = TRUE)))
    if (sort) {
        xx <- arrange(xx, .data$time)
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
            gap = group_const_change(.data$time),
            value = NA_real_
        )

    if(nrow(gaps)) {
        if (add_implicit_NA) {
            xx <- bind_rows(xx, select(gaps, .data$time, .data$value))
            action <- "Adding NA values."
            if (sort) {
                xx <- arrange(xx, .data$time)
                action <- "Adding NA values and sorting."
            }
        } else {
            action <- ""
        }

        txt <- paste("implicitly missing values (", max(gaps$gap), "gaps)")
        msg$append(what = txt, data = gaps, n = nrow(gaps),
                   id = id, n_total = nrow(xx), action = action)
    }

    interpolated  <- .fill_na(xx$value, max.len = approx.missing)
    xx$value <- interpolated
    idx <- attr(interpolated, "index.interpolated")
    if (length(idx)) {
        gaps <- xx %>%
            slice(idx) %>%
            mutate(group = group_const_change(idx)) %>%
            nest(data = c(.data$time, .data$value))
        txt <- paste("gaps with length <=", approx.missing, "days")
        action <- "Filling with linear interpolation."

        msg$append(what = txt, data = gaps, n = nrow(gaps),
                   id = id, n_total = nrow(xx), action = action)

    }

    msg$final(n_out = nrow(xx))
    if (message) msg$print()

    res <- xx %>%
        select(!!time_col := .data$time, !!value_col := .data$value)

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
                    paste("  Reading and writing:", n_in, "rows.")
                } else {
                    d <-  n_out - n_in
                    paste0("  Reading ", n_in, ", and writing ", n_out,
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
                nest(data = c(-.data$id))

            walk2(mesg$data, mesg$id, .print_one_message)
        },

        export = function() return(m),
        final = function(n_out) n_out <<- n_out
    ))
}

.fill_na <- function(x, max.len = Inf, ...)
{
    g <- group_const_value(is.na(x))
    rl <- rle(g)
    len <- rep(rl$lengths, rl$lengths)

    # indices, for which interpolation is required
    mask <- is.na(x) & len <= max.len
    # idx <- seq_along(x)[mask]
    idx <- which(mask)
    x[idx] <- approx(seq_along(x), x, xout = idx, rule = 2, ...)$y

    # gaps are periods of missing values
    attr(x, "index.interpolated") <- if (length(idx)) idx else logical()

    return(x)
}


# damit map() auch für difftime und POSIX Objekte funktioniert
map_other <- function(.x, .f, ...)
{
    purrr::reduce(purrr::map(.x, .f, ...), c)
}


numbers_english <- function(x)
{
    x <- as.numeric(x)
    text <- c("one", "two", "three", "four", "five", "six", "seven", "eight",
              "nine", "ten", "eleven", "twelve")[x]

    ifelse(x <= 12, text, x)
}

paste_with_and <- function(x , sep = ", ", final = " and ")
{
    paste(
        paste(utils::head(x, -1), collapse = sep),
        utils::tail(x, 1), sep = final
    )
}


filter_season <- function(x, range)
{
    range <- lubridate::yday(.origin(range))
    jday <- lubridate::yday(x$time)

    if (range[1] < range[2]) {
        mask <- jday >= range[1] & jday <= range[2]
    } else {
        mask <- jday >= range[1] | jday <= range[2]
    }

    return(x[mask, ])
}
