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
