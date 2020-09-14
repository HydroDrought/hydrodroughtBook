.extract_slope <- function(x, y)
{
  model <- stats::lm(y ~ x + 0, tibble(x, y))
  co <- stats::coef(model)

  return(co["x"])
}

peaks <- function(x, halfWindowSize) {

  windowSize <- halfWindowSize * 2 + 1
  windows <- stats::embed(x, windowSize)
  localMaxima <- max.col(windows, "first") == halfWindowSize + 1

  return(c(rep(FALSE, halfWindowSize), localMaxima, rep(FALSE, halfWindowSize)))
}

#' @export
recession <- function(time, discharge, qref = lfquantile(discharge, 0.7),
                      smooth = 0, zeroslope = 0, minlen = 5, plot = FALSE)
{
  segment <- tibble(time, discharge) %>%
    mutate(
      # slope is computed from current point to next point
      slope = (lead(discharge) - discharge) / as.double(lead(time) - time, units = "days"),
      falling = slope <= zeroslope,
      isFloodpeak = peaks(as.vector(discharge), halfWindowSize = 2) & discharge > qref,
      disturbed = isFloodpeak | lag(isFloodpeak, n = 1, default = F) | lag(isFloodpeak, n = 2, default = F)
    )  %>%
    mutate (
      event = group_const_value(falling)
    )
  segment <- segment %>%
    filter( !is.na(event),
           # !disturbed, discharge <= qref
           ) %>%
    select(-isFloodpeak, -disturbed) %>%
    nest(data = c(-event, -falling)) %>%
    mutate(
      len = purrr::map_int(data, nrow),
      falling = if_else(len <= smooth & !falling, TRUE, falling),
      event = group_const_value(falling)
    ) %>%
    filter(falling) %>%
    select(-len) %>%
    unnest(data) %>%
    nest(data = c(-event)) %>%
    mutate(
      # data = map(data, mutate, day = seq_len(n())),
      len = purrr::map_int(data, nrow)
    )

  recession <- segment %>%
    filter(.data$len >= minlen) %>%
    unnest(data) %>%
    group_by(.data$event) %>%
    mutate(day = dplyr::row_number())

  pairs <- recession %>%
    group_by(event) %>%
    mutate(
      discharge.leaded = lead(discharge)
    ) %>%
    stats::na.omit()



  k <- .extract_slope(pairs$discharge, pairs$discharge.leaded)
  alpha <- -log(k)

  if(plot) {
    q0 <- max(pairs$discharge)

    time_offset <- function(q0, discharge, slope)
    {
      (log(discharge) - log(q0)) / log(slope)
    }

    master_curve <- function(time, q0, k, t0 = 0, dt = 86400, ...)
    {
      q0 *  exp((t0 + as.double(time, ...)) * log(k) / dt)
    }

    pairs <- pairs %>%
      group_by(event) %>%
      mutate(
        shift = time_offset(q0 = q0, discharge = discharge[1], slope = k),
        day.shifted = day + shift - 1
      )

    ggplot(pairs, aes(x = lubridate::as_datetime(day.shifted * 86400), y = discharge, group = event)) +
      geom_line(size = 0.2, alpha = 0.4) +
      geom_hline(yintercept = qref, linetype = "dashed", size = 0.5) +
      geom_function(fun = ~master_curve(.x, q0 = q0, k = k),
                    inherit.aes = FALSE, col = "white") +
      geom_function(fun = ~master_curve(.x, q0 = q0, k = k),
                    inherit.aes = FALSE, linetype = "dashed") +
      labs(x = "Time in days", y = "Discharge") +
      scale_x_datetime(date_breaks = "7 days", date_minor_breaks = "1 days",
                       date_labels = "%j", expand = expansion()) +
      theme_bw(base_size = 8)
  }

  # attr(k, "segments") <- recession
  return(unname(alpha))
}

