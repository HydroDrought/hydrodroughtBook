#' @importFrom lubridate days
#' @importFrom dygraphs dygraph dyRangeSelector dySeries dyShading dyAnnotation
#' @export
drought_events <- function(x, threshold,
                           pooling = c("none", "moving-average", "sequent-peak", "inter-event"),
                           pooling.pars = list(n = 10, sides = "center",
                                               min.duration = 5, min.vol.ratio = 0.1),
                           full.table = FALSE, relabel.events = TRUE,
                           plot = FALSE)
{
  pooling <- match.arg(pooling)

  # find drought events, compute columns
  # volume, storage (zero when not using sequent peak algorithm),
  # below.threshold, under.drought, event
  #
  x <- .drought_events(x = x, threshold = threshold,
                       pooling = pooling, pooling.pars = pooling.pars)

  # aggregate over events, still keeping non-drought events
  events <- x %>%
    group_by(.data$event) %>%
    summarise(
      first.day = min(.data$time), last.day = max(.data$end) - days(1),
      # duration for SPA is duration until max storage
      # only meaningful if pooling == "sequent-peak"
      d.smax = which.max(.data$storage),
      d.smax = as.difftime(as.double(.data$d.smax), units = "days"),
      # duration = `units<-`(x = end - first.day, value = "days") - 1,
      duration = .data$last.day - .data$first.day + as.difftime(1, units = "days"),
      dbt = as.difftime(as.double(sum(.data$under.drought)), units = "days"),
      qmin = min(.data$discharge),
      tqmin = .data$time[which.min(.data$discharge)],
      # volume for SPA is max storage
      volume = sum(.data$volume),
      v.smax = max(.data$storage),
      under.drought = unique(.data$under.drought),
      # mean.flow = mean(discharge),
      .groups = "drop"
    )

  if (pooling == "inter-event") {
    events <- .pool_inter_event(
      x = events,
      min.duration = pooling.pars$min.duration,
      min.vol.ratio = pooling.pars$min.vol.ratio
    )
  }

  # drop non-drought events now, after inter-event pooling
  # only keeping drought events
  events <- filter(events, .data$under.drought)

  if (relabel.events) events <- mutate(events, event = seq_along(.data$event))

  if (plot) {
    df <- x %>%
      mutate(lwr = if_else(.data$under.drought, .data$discharge, .data$threshold),
             upr = .data$threshold) %>%
      select(.data$discharge, .data$threshold, .data$lwr, .data$upr)

    tbl <- events %>%
      mutate(ttip = paste0("volume: ", signif(.data$volume, 3), "\n",
                           "duration: ", .data$duration, " days\n"))

    p <- xts::xts(df, order.by = x$time) %>%
      dygraph() %>%
      dyRangeSelector() %>%
      dySeries("discharge", stepPlot = .data$step, drawPoints = TRUE, color = "darkblue") %>%
      dySeries(c("lwr", "threshold", "upr"), stepPlot = .data$step, color = "red",
               strokePattern = "dashed")


    for (i in seq_len(nrow(tbl))) {
      p <- dyShading(p, from = tbl$first.day[i], to = tbl$last.day[i], color = "lightgrey")
      p <- dyAnnotation(p, x = tbl$first.day[i], text = tbl$event[i],
                        tooltip = tbl$ttip[i],
                        width = 30, attachAtBottom = TRUE)
    }

    print(p)
  }

  # retain full table?
  if (!full.table) {
    cols <- case_when(
      pooling == "inter-event" ~ list(c("event", "first.day", "last.day", "duration", "dbt", "volume", "qmin", "tqmin",
                                        "pooled")),
      pooling == "sequent-peak" ~ list(c("event", "first.day", "last.day", duration = "d.smax", "dbt", volume = "v.smax",
                                         "qmin", "tqmin")),
      TRUE ~ list(c("event", "first.day", "last.day", "duration",  "volume", "qmin", "tqmin"))
    )[[1]]

    events <- select(events, all_of(cols))
  }

  return(events)
}

#' @importFrom rlang .data
.drought_events <- function(x, threshold,
                            pooling = c("none", "moving-average", "sequent-peak", "inter-event"),
                            pooling.pars = list(n = 10, sides = "center",
                                                min.duration = 5, min.vol.ratio = 0.1))
{
  pooling <- match.arg(pooling)
  att <- attr(threshold, "threshold")

  # give warnings if using default parameters
  # specifing them explicitly silences it

  # todo? use deficit instead of volume

  # todo: make function resislient against NAs
  # never pooled over NAs..

  if (pooling == "moving-average") {
    # todo: remove NAs introduced by moving average
    x <- x %>%
      mutate(discharge = moving_average(.data$discharge, n = pooling.pars$n,
                                        sides = pooling.pars$sides))
  }

  if (is.numeric(threshold) && length(threshold == 1)) {
    # append constant threshold to data
    x$threshold <- threshold
  } else {
    if (is.null(att)) {
      warning("Could not derive start of the hydrological year from 'threshold' object. Defaulting to '-01-01'.")
      by <- setdiff(colnames(threshold), "threshold")
      origin <- "-01-01"
    } else {
      by <- att$vary.by
      origin <- att$start
    }

    # compute the same groups than threshold and join it
    x <- x %>%
      mutate(
        group = group_ts(time = .data$time, by = by, start = origin)
      ) %>%
      rename(!!by := .data$group) %>%
      left_join(threshold, by = by)
  }

  # append the "end" of the day
  # it is an interval in a mathematical sense [start, end)
  x <- mutate(x, end = lead(.data$time, 1))

  # for regular time series we can estimate the duration of the last interval
  dt <- .guess_dt(x$time)
  if (!is.na(dt)) {
    x$end[nrow(x)] <- last(x$time) + dt
  }

  x <- x %>%
    mutate(
      volume = -(.data$discharge - .data$threshold) *
        as.double(.data$end - .data$time, units = "secs"),
      below.threshold = .data$discharge < .data$threshold,
      under.drought = .data$below.threshold,
      storage = 0)

  if (pooling == "sequent-peak") {
    # overwriting the column 'under.drought'
    x <- x %>%
      mutate(storage = storage(.data$discharge, threshold = .data$threshold),
             # akwardly, storage is not defined as a volume, but as discharge
             # storate = storage * as.double(end - time, units = "secs"),
             under.drought = .data$storage > 0)
  }

  .warn_na(x)

  # assign event numbers, only every second event is drought
  # periods with NAs get negative event numbers and are relabeled
  x <- mutate(
    x,
    event = group_const_value(.data$under.drought | is.na(.data$discharge)),
    event = if_else(is.na(.data$discharge), -.data$event, .data$event),
    event = as.integer(group_const_value(.data$event) + 1)
  )

  return(x)
}

.warn_na <- function(x)
{
  na.pos <- which(is.na(x$discharge))
  if (length(na.pos)) {
    # test if values around NAs are below threshold
    neighbors <- unique(c(na.pos - 1, na.pos + 1))
    candidates <- x %>%
      slice(neighbors) %>%
      filter(!is.na(.data$discharge), .data$discharge <= .data$threshold)

    if (nrow(candidates)) {
      warning("Missing values adjacent to flows below threshold.\nNAs always terminate drought event. To avoid this interpolate first.")
      return(TRUE)
    }
  }
  return(FALSE)
}

.pool_inter_event <- function(x, min.duration, min.vol.ratio)
{
  p <- 1
  x$pool <- 0L

  row <- if (x$under.drought[1]) 1 else 2   # start with first drought

  # skip last row because we only want pairs
  while (row <= nrow(x) - 1) {
    # iterate over pairs of drought event and the
    # anteceding non-drought event
    x$pool[c(row, row + 1)] <- p

    ie.time <- x$duration[row + 1]

    # interevent volume does not increase the pooled drought volume
    cumvol <- sum(x$volume[x$pool == p & x$under.drought])
    vol.ratio <- cumvol / x$volume[row + 1]

    depended <- ie.time < min.duration &&
      vol.ratio < min.vol.ratio

    if (!depended) p <- p + 1

    row <- row + 2
  }

  # sum up events that got pooled
  x <- x %>%
    filter(.data$under.drought) %>%
    group_by(.data$pool) %>%
    summarise(
      first.day = first(.data$first.day),
      last.day = last(.data$last.day),
      duration = `units<-`(x = .data$last.day - .data$first.day + 1, value = "days"),
      dbt = sum(.data$dbt),
      under.drought = first(.data$under.drought),
      volume = sum(.data$volume),
      tqmin = .data$tqmin[first(which.min(.data$qmin))],
      qmin = min(.data$qmin),
      pooled = n() - 1
    ) %>%
    rename(event = .data$pool)

  return(x)
}

#' @export
storage <- function(discharge, threshold)
{
  x <- tibble(discharge = discharge,
              deficit = threshold - discharge,
              storage = 0)

  x$storage[1] <- if (x$deficit[1] > 0 & !is.na(x$deficit[1])) x$deficit[1] else 0
  for (i in seq(2, nrow(x))) {
    s <- x$storage[i - 1] + x$deficit[i]
    x$storage[i] <- if (s > 0 & !is.na(x$deficit[i])) s else 0
  }

  return(x$storage)
}



