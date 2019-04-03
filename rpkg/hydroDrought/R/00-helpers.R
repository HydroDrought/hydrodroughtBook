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

#' @export
append_group <- function(x, by = c("day", "week", "month", "season", "year"),
                         start = "-01-01", unique.id = FALSE)
{
    by <- match.arg(by, several.ok = TRUE)
    x$time <- as.Date(x$time)
    start <- regmatches(start, regexpr("-.*", start))

    .jday <- function(x) as.numeric(format(as.Date(x), "%j"))

    # always calculate the hydrological year, only id is meaningful
    x$year <- as.numeric(substring(group_id(x$time, start[1]), 1L, 4L))

    if ("season" %in% by) {
        if (length(start) < 2) {
            warning("There have to be a least two seasons. Specify argument 'start' accordingly.")
        }
        season.id <- group_id(x$time, start)

        x$season <- factor(substr(season.id, 5L, 10L), levels = start)

        # if existing, use names of the group
        nam <- names(start)
        is.named <- length(nam) == length(start) && all(!is.na(nam)) && all(nam != "")
        if (is.named) x$season <- fct_recode(x$season, nam)


        if (unique.id) x$season.id <- season.id
    }

    # only week and month are trivial
    f <- c(week = week, month = month)
    for (i in setdiff(by, c("year", "season", "day"))) {
        x <- mutate(x, !!i := f[[i]](x$time))
        if (unique.id) x[[paste0(i, ".id")]] <- paste(x$year, x[[i]], sep = "-")
    }

    # calculate day at the end, so that we can copy week and season for day 366


    return(x)
}

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

var_threshold <- function(x, by = c("day", "week", "month", "season", "year"),
                          fun, start = "-01-01", ...)
{
    by <- match.arg(by)
    y <- append_group(x, by = by, start = start)

    if (by == "day") {
        # interpolate discharge value on day 366
        # if year is not a leap year as the mean of the surrounding days
        # get list of non leap-years from data
        leap <- filter(y, day >= 365) %>%
            group_by(year) %>%
            filter(n() == 1) %>%
            mutate(time = as.Date(NA), day = 366, discharge = NA)

        y <- bind_rows(y, leap) %>%
            arrange(year, time) %>%
            mutate(discharge = if_else(
                day == 366 & is.na(discharge),
                (lag(discharge) + lead(discharge)) / 2,
                discharge
            ))
    }

    y %>%
        # summaries with NA values do not make sense, avoids to always specify na.rm = TRUE
        filter(!is.na(discharge)) %>%
        group_by(.dots = by) %>%
        summarise(threshold = fun(discharge, ...))
}


const_threshold <- function(x, fun, ...)
{
    x %>%
        # summaries with NA values do not make sense, avoids to always specify na.rm = TRUE
        filter(!is.na(discharge)) %>%
        summarise(threshold = fun(discharge, ...))
}

#' @export
drought_events <- function(x, threshold,
                           pooling = c("none", "moving-average", "sequent-peak", "inter-event"),
                           pooling.pars = list(n = 10, sides = "center",
                                               min.duration = 5, vol.ratio = 0.1),
                           drop = TRUE, full.table = FALSE)
{
    pooling <- match.arg(pooling)

    # give warnings if using default parameters
    # specifing them explicitly silences it

    # ? use deficit instead of volume

    # make function resislient against NAs
    # NAs always terminate a drought, never pooled over NAs..

    if (pooling == "moving-average") {
        # remove NAs introduced by moving average
        x <- x %>%
            mutate(discharge = moving_average(discharge, n = pooling.pars$n,
                                              sides = pooling.pars$sides))
    }

    if (is.numeric(threshold) && length(threshold == 1)) {
        # append constant threshold to data
        x$threshold <- threshold
    } else {
        # compute the same groups than threshold and join it
        by <- setdiff(colnames(threshold), "threshold")
        x <- x %>%
            append_group(by = by) %>%
            left_join(threshold, by = by)
    }

    x <- x %>%
        mutate(volume = (discharge - threshold) * 86400,
               below.threshold = discharge < threshold,
               under.drought = below.threshold)

    if (pooling == "sequent-peak") {
        # the real difference is the day the drought ends
        x <- x %>%
            mutate(cumvolume = cumsum(volume))

        n <- nrow(x)
        repeat {
            start <- head(which(x$volume < 0 & x$cumvolume >= 0), 1)
            if (length(start) == 0) break

            mask <- seq(from = start, to = n)
            x$cumvolume[mask] <- x$cumvolume[mask] - x$cumvolume[start - 1]
        }

        # we are under drought as long as there is a cumulative deficit
        x <- x %>%
            mutate(under.drought = cumvolume < 0)
    }

    x <- x %>%
        mutate(change = under.drought != lag(under.drought))
    x$change[1] <- FALSE

    x <- x %>%
        # assign event numbers, only every second event is drought
        mutate(event = cumsum(change))

    if (pooling == "inter-event") {

    }

    # # only keep drought events?
    # if (drop) {
    x <- x  %>%
        filter(under.drought)
    # }

    # summarizing each event
    x <- x %>%
        group_by(event) %>%
        summarise(start = time[1], duration = n(), end = time[duration],
                  qmin = min(discharge), tqmin = time[which.min(discharge)[1]],
                  vbt = sum(volume[below.threshold]), volume = sum(volume),
                  dbt = sum(below.threshold),
                  under.drought = unique(under.drought)) %>%
           mutate(event = order(event))        # relabel events

    # in theory volume must be zero when using sequent peak algorithm
    # because droughts most likely terminate during a day
    # we will obtain a (small) negative value as the upcrossing is on
    # first day after
    if (pooling == "sequent-peak") x$volume[x$under.drought] <- 0

    # retain full table?
    if (!full.table) {
        x <- x  %>%
            select(event, start, end, duration, dbt, volume, vbt, tqmin, qmin)
    }

    return(x)
}
