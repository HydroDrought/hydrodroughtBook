
#' @export
days_in_year <- function(x, origin = "-01-01")
{
  from <- if (is.numeric(x)) as.Date(paste0(x, origin)) else x
  to <- as.Date(paste0(year(from) + 1, origin))

  days <- as.double(to - from, units = "days")
  return(as.integer(days))
}


#' @export
coverage_yearly <- function(x, origin = "-01-01")
{
  tibble(time = x) %>%
    mutate(
      year = water_year(.data$time, start = origin)
    ) %>%
    count(.data$year, name = "days.with.data") %>%
    mutate(
      days.in.year = days_in_year(.data$year, origin = origin),
      days.missing = .data$days.in.year - .data$days.with.data,
      coverage = .data$days.with.data / .data$days.in.year
    ) %>%
    arrange(.data$year)
}


remove_incomplete_first_last <- function(x, percent = 0.99, origin = "-01-01")
{
  has.year <- "year" %in% colnames(x)
  if (!has.year) x <- mutate(
    year = water_year(.data$time, start = origin)
  )

  coverage <- x %>%
    count(year) %>%
    mutate(coverage = n / days_in_year(year, origin = origin)) %>%
    arrange(year)

  firstyear <- first(coverage$year)
  lastyear <- last(coverage$year)

  complete <- coverage %>%
    filter(year %in% seq(firstyear + 1, lastyear - 1) |
             (coverage >= percent))

  res <- semi_join(x, complete, by = "year")

  if (!has.year) res <- select(res, -year)

  return(res)
}

#' @importFrom lubridate year floor_date ceiling_date days
month_midpoints <- function(limits)
{
  l <- c(floor_date(limits[1], unit = "month"),
         ceiling_date(limits[2], unit = "month"))

  nmonths <- (as.double(l[2] - l[1], unit = "days") + 31) / 30
  s <- l[1] + days(14) + months(seq(0, nmonths - 1))

  s
}

month_breaks <- function(limits)
{
  l <- c(floor_date(limits[1], unit = "month"),
         ceiling_date(limits[2], unit = "month"))

  nmonths <- (as.double(l[2] - l[1], unit = "days") + 31) / 30
  s <- l[1] + months(seq(0, nmonths))

  s
}


#' @importFrom lubridate month
month_localized <- function(x, locale = "en_US.utf8")
{
  month(x, label = TRUE, abbr = TRUE, locale = locale)
}

#' @export
scale_x_month <- function(..., locale = "en_US.utf8")
{
  scale_x_date(breaks = month_midpoints,
               labels = function(x) month_localized(x, locale = locale),
               minor_breaks = month_breaks, ...)
}

#' @export
scale_x_datetime_month <- function(..., locale = "en_US.utf8")
{
  scale_x_datetime(breaks = month_midpoints,
                   labels = function(x) month_localized(x, locale = locale),
                   minor_breaks = month_breaks, ...)
}




log_breaks_covered <- function(x, candidates = 1 * 10^(-4:5))
{
  candidates[candidates > min(x) & candidates < max(x)]
}

#' @export
plot_daily_flow <- function(x, exc.freq = c(0.95, 0.9, 0.8, 0.5),
                            title = "", subtitle = "", origin = "-01-01")
{
  x <- x %>%
    mutate(
      year = water_year(.data$time, start = origin),
      day = monthDay(.data$time, origin = origin)
    ) %>%
    remove_incomplete_first_last(origin = origin)

  f <- function(x)
  {
    list(
      tibble(
        discharge = lfquantile(x, exc.freq = exc.freq),
        exc.freq = names(.data$discharge)
      ) %>%
        mutate(exc.freq = factor(.data$exc.freq, levels = .data$exc.freq))
    )
  }

  quantiles <- x %>%
    var_threshold(vary.by = "day", fun = f, start = origin) %>%
    unnest(.data$threshold) %>%
    group_by(.data$exc.freq) %>%
    mutate(
      smoothed = pracma::whittaker(.data$discharge),
      smoothed = case_when(.data$smoothed < 0.00025 ~ 0,
                           .data$smoothed < 0.0005 ~ 0.0005,
                           TRUE ~ smoothed)
    )

  q.const <- x %>%
    const_threshold(fun = f) %>%
    unnest(.data$threshold)

  envelope <- x %>%
    group_by(.data$day) %>%
    summarise(across(.data$discharge, lst(min, max), na.rm = T), .groups = "drop")

  extremes <- x %>%
    group_by(year) %>%
    summarise(
      resid.dry = sum((.data$discharge[.data$discharge < median(.data$discharge)])^2),
      resid.wet = sum((.data$discharge[.data$discharge > median(.data$discharge)])^2),
      .groups = "drop") %>%
    nest(data = everything()) %>%
    transmute(
      wet = slice_max(.data$data[[1]], .data$resid.wet, n = 1, with_ties = FALSE)$year,
      dry = slice_min(.data$data[[1]], .data$resid.dry, n = 1, with_ties = FALSE)$year
    ) %>%
    pivot_longer(everything(), names_to = "state", values_to = "year") %>%
    left_join(x, by = "year")

  labels <- extremes %>%
    select(.data$state, .data$time, .data$discharge, .data$day) %>%
    nest(data = c(.data$discharge, .data$time, .data$day)) %>%
    mutate(
      min = map(.data$data, ~slice_min(.x, discharge, n = 1, with_ties = FALSE)),
      max = map(.data$data, ~slice_max(.x, discharge, n = 1, with_ties = FALSE))
    ) %>%
    select(-.data$data) %>%
    pivot_longer(-.data$state, names_to = "extreme", values_to = "data") %>%
    filter(.data$state == "wet" & .data$extreme == "max" | .data$state == "dry" & .data$extreme == "min") %>%
    unnest(.data$data) %>%
    mutate(text = paste0(.data$time, ": ", round(.data$discharge, 3), " m\u00B3/s"))

  subtitle <- paste("Gauged daily flow for", year(min(x$time)), "-", year(max(x$time)),
                    subtitle)

  #  muted(hue_pal()(2), l = 90, c = 30)
  ggplot(x, aes(x = .data$day)) +

    # envelopes
    geom_ribbon(data = envelope, aes(ymax = .data$discharge_min), ymin = -Inf, fill = "#FFD7D5") +
    geom_ribbon(data = envelope, aes(ymin = .data$discharge_max), ymax = Inf, fill =  "#B3EDF0") +

    # yearly discharge
    geom_line(mapping = aes(y = .data$discharge, group = .data$year), alpha = 0.2, size = 0.1) +

    # quantiles
    geom_line(data = quantiles, mapping = aes(y = .data$smoothed, linetype = exc.freq), size = 0.2) +
    geom_hline(data = q.const, aes(yintercept = .data$discharge, linetype = exc.freq), size = 0.2) +

    # extreme years
    geom_point(data = labels, mapping = aes(y = .data$discharge), shape = 1, size = 2) +
    geom_line(data = extremes, mapping = aes(y = .data$discharge, col = .data$state), show.legend = FALSE) +
    ggrepel::geom_text_repel(data = labels, mapping = aes(y = .data$discharge, label = .data$text),
                             direction = "both", point.padding = 0.5, size = 3) +

    labs(title = title, subtitle = subtitle) +
    scale_y_log10(label = function(x) paste(format(x, scientific = F, drop0trailing = T), " m\u00B3/s"),
                  breaks = log_breaks_covered, oob = squish_infinite) +
    scale_x_date(date_labels = "%b", breaks = month_midpoints,
                 expand = expansion(mult = 0, add = 0)) +
    scale_linetype_discrete("", guide = guide_legend(reverse = TRUE)) +

    theme_bw(base_size = 10) +
    theme(panel.grid = element_line(color = "black"),
          panel.grid.minor.x = element_line(size = 0.1),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.y = element_blank(), #element_line(size = 0.1),
          panel.grid.major.y = element_blank(), #element_line(size = 0.2),
          axis.ticks.x = element_blank(),
          axis.title = element_blank(),
          panel.background = element_rect(fill = NA),
          panel.ontop = FALSE)
}


#' @export
plot_coverage <- function(x, title = "", origin = "-01-01")
{

  subtitle <- paste("Data coverage for", year(min(x$time)), "-", year(max(x$time)))

  x <- x %>%
    mutate(
      year = water_year(.data$time, start = origin)
    ) %>%
    mutate(
      day = monthDay(.data$time, origin = origin),
      coverage = if_else(is.na(.data$discharge), "data missing", "data available"),
      coverage = factor(.data$coverage, levels = c("data missing", "data available"))
    )

  p <- ggplot(x, aes(.data$day, .data$year, fill = .data$coverage)) +
    geom_tile() +

    labs(title = title, subtitle = subtitle) +
    scale_x_date(date_labels = "%b", breaks = month_midpoints,
                 expand = expansion(mult = 0, add = 0)) +
    scale_y_continuous(minor_breaks = function(x) seq(x[1], x[2], 1),
                       expand = expansion(mult = 0, add = 0)) +
    scale_fill_viridis_d(drop = FALSE, begin = 0.4, end = 0.8) +

    theme_bw(base_size = 10) +
    theme(panel.grid.major.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.title = element_blank(),
          panel.background = element_rect(fill = NA),
          panel.ontop = TRUE)

  p
}


#' @export
integer_breaks <- function(n = 5, ...) {
  fxn <- function(x) {
    breaks <- floor(pretty(x, n, ...))
    names(breaks) <- attr(breaks, "labels")
    breaks
  }
  return(fxn)
}


inspect_spa <- function(x)
{
  discharge <- ggplot(x, aes(.data$time, .data$discharge)) +
    geom_line() +
    geom_point(size = 1) +
    coord_cartesian(ylim = c(NA, x$threshold[1])) +
    scale_x_date(date_labels = "%Y-%m-%d") +
    scale_y_continuous(expand = expansion(c(0.04, 0.1))) +
    geom_hline(yintercept = x$threshold[1], col = 2, linetype = "dashed", size = 0.2) +
    facet_wrap(~event, scales = "free", nrow = 1) +
    theme(axis.title.x = element_blank())

  storage <- ggplot(x, aes(.data$time, .data$storage)) +
    geom_step() +
    geom_point(size = 1) +
    scale_x_date(date_labels = "%Y-%m-%d") +
    expand_limits(y = 0) +
    facet_wrap(~event, scales = "free", nrow = 1) +
    theme(axis.title.x = element_blank())

  cowplot::plot_grid(discharge, storage, align = "v", ncol = 1)
}
