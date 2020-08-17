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
    group_by(event) %>%
    summarise(
      start = min(time), end = max(end),
      # duration for SPA is duration until max storage
      d.smax = as.difftime(as.double(which.max(storage)), units = "days"),
      duration = `units<-`(x = end - start, value = "days"),
      qmin = min(discharge),
      tqmin = time[which.min(discharge)],
      # volume for SPA is max storage
      volume = sum(volume),
      v.smax = max(storage),
      under.drought = unique(under.drought)
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
  events <- filter(events, under.drought)

  if (relabel.events) events <- mutate(events, event = order(event))

  if (plot) {
    df <- x %>%
      mutate(lwr = if_else(under.drought, discharge, threshold),
             upr = threshold) %>%
      select(discharge, threshold, lwr, upr)

    tbl <- events %>%
      mutate(ttip = paste0("volume: ", signif(volume, 3), "\n",
                           "duration: ", duration, " days\n"))

    p <- xts::xts(df, order.by = x$time) %>%
      dygraph() %>%
      dyRangeSelector() %>%
      dySeries("discharge", stepPlot = step, drawPoints = TRUE, color = "darkblue") %>%
      dySeries(c("lwr", "threshold", "upr"), stepPlot = step, color = "red",
               strokePattern = "dashed")


    for (i in seq_len(nrow(tbl))) {
      p <- dyShading(p, from = tbl$start[i], to = tbl$end[i], color = "lightgrey")
      p <- dyAnnotation(p, x = tbl$start[i], text = tbl$event[i],
                        tooltip = tbl$ttip[i],
                        width = 30, attachAtBottom = TRUE)
    }

    print(p)
  }

  # retain full table?
  if (!full.table) {
    cols <- case_when(
      pooling == "inter-event" ~ list(c("event", "start", "end", "duration", "volume", "qmin", "tqmin",
                                   "pooled")),
      pooling == "sequent-peak" ~ list(c("event", "start", "end", duration = "d.smax", volume = "v.smax",
                                    "qmin", "tqmin")),
      TRUE ~ list(c("event", "start", "end", "duration",  "volume", "qmin", "tqmin"))
    )[[1]]

    events <- select(events, all_of(cols))
  }

  return(events)
}


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
  # NAs always terminate a drought, never pooled over NAs..

  if (pooling == "moving-average") {
    # todo: remove NAs introduced by moving average
    x <- x %>%
      mutate(discharge = moving_average(discharge, n = pooling.pars$n,
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
      append_group(by = by, start = origin) %>%
      left_join(threshold, by = by)
  }

  # append the "end" of the day
  # it is an interval in a mathematical sense [start, end)
  x <- mutate(x, end = lead(time, 1))

  # for regular time series we can estimate the duration of the last interval
  dt <- .guess_dt(x$time)
  if (!is.na(dt)) {
    x$end[nrow(x)] <- last(x$time) + dt
  }

  x <- x %>%
    mutate(
      volume = -(discharge - threshold) *
        as.double(end - time, units = "secs"),
      below.threshold = discharge < threshold,
      under.drought = below.threshold,
      storage = 0)

  if (pooling == "sequent-peak") {
    # overwriting the column 'under.drought'
    x <- x %>%
      mutate(storage = .storage(x$discharge, threshold = threshold) *
               as.double(end - time, units = "secs"),
             under.drought = storage > 0)
  }

  # assign event numbers, only every second event is drought
  x <- mutate(x, event = .rle_id(under.drought))

  return(x)
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

    depended <- ie.time <= min.duration &&
      vol.ratio < min.vol.ratio

    if (!depended) p <- p + 1

    row <- row + 2
  }

  # sum up events that got pooled
  x <- x %>%
    filter(under.drought) %>%
    group_by(pool) %>%
    summarise(
      start = first(start),
      end = last(end),
      duration = `units<-`(x = end - start + 1, value = "days"),
      under.drought = first(under.drought),
      volume = sum(volume),
      tqmin = tqmin[first(which.min(qmin))],
      qmin = min(qmin),
      pooled = n() - 1
    ) %>%
    rename(event = pool)

  return(x)
}

.storage <- function(discharge, threshold)
{
  x <- tibble(discharge = discharge,
              deficit = threshold - discharge,
              storage = 0)

  x$storage[1] <- if (x$deficit[1] > 0) x$deficit[1] else 0
  for (i in seq(2, nrow(x))) {
    s <- x$storage[i - 1] + x$deficit[i]
    x$storage[i] <- if (s > 0) s else 0
  }

  return(x$storage)
}

# tibble(d = c(1, 1, 0, 0, -1, 0, -1, -1, -1, 1, 1, 1, 1),
#        t = 0,
#        s = .storage(d, t))




inspect_spa <- function(x)
{
  discharge <- ggplot(x, aes(time, discharge)) +
    geom_line() +
    geom_point(size = 0.5) +
    # possible bug: q90 might not be defined
    geom_hline(yintercept = q90, col = 2, linetype = "dashed", size = 0.2) +
    facet_wrap(~event, scales = "free", nrow = 1)

  storage <- ggplot(x, aes(time, storage)) +
    geom_line() +
    geom_point(size = 0.5) +
    expand_limits(y = 0) +
    facet_wrap(~event, scales = "free", nrow = 1)

  cowplot::plot_grid(discharge, storage, align = "v", ncol = 1)
}
