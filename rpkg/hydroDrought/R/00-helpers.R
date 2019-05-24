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
var_threshold <- function(x, vary.by = c("day", "week", "month", "season", "year"),
                          fun, start = "-01-01", append = FALSE, ...)
{
    vary.by <- match.arg(vary.by)
    y <- append_group(x, by = vary.by, start = start)

    if (vary.by == "day") {
        # interpolate Feb 29th with "surrounding" days if not a leap year
        leapday <- as.Date("1972-02-29")

        leapdays <- filter(y, day == leapday - 1 | day == leapday + 1,
                           !leap_year(time)) %>%
            group_by(year) %>%
            summarise(discharge = mean(discharge),
                      day = leapday) %>%
            # surrounding values could be NA
            filter(!is.na(discharge))

        y <- bind_rows(y, leapdays) %>%
            # somehow, class gets lost...
            mutate(day = monthDay(day, origin = start))
    }

    threshold <- y %>%
        # summaries with NA values do not make sense, avoids to always specify na.rm = TRUE
        filter(!is.na(discharge)) %>%
        group_by(.dots = vary.by) %>%
        summarise(threshold = fun(discharge, ...))

    if (append) {
        return(left_join(y, threshold, by = vary.by))
    } else {
        return(threshold)
    }
}


#' @export
const_threshold <- function(x, fun, append = FALSE, ...)
{
    threshold <- x %>%
        # summaries with NA values do not make sense, avoids to always specify na.rm = TRUE
        filter(!is.na(discharge)) %>%
        summarise(threshold = fun(discharge, ...))

    if (append) {
        return(left_join(x, threshold, by = vary.by))
    } else {
        return(threshold)
    }
}


.rle_id <- function(x)
{
    cumsum(x != lag(x, default = x[1]))
}
