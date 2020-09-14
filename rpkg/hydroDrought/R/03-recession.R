library(hydroDrought)
library(tidyverse)

x <- international %>%
  filter(river == "Ngaruroro") %>%
  select(data) %>%
  unnest(data)


.extract_slope <- function(x, y)
{
  model <- lm(y ~ x + 0, tibble(x, y))
  co <- coef(model)

  return(co["x"])
}

peaks <- function(x, halfWindowSize) {

  windowSize <- halfWindowSize * 2 + 1
  windows <- embed(x, windowSize)
  localMaxima <- max.col(windows, "first") == halfWindowSize + 1

  return(c(rep(FALSE, halfWindowSize), localMaxima, rep(FALSE, halfWindowSize)))
}

recession <- function(time, discharge, qref = lfquantile(discharge, 0.5),
                      smooth = 0, zeroslope = 0, minlen = 2, plot = FALSE)
{
  segment <- tibble(time, discharge) %>%
    mutate(
      # slope is computed from current point to next point
      slope = (lead(discharge) - discharge) / as.double(lead(time) - time, units = "days"),
      falling = slope <= zeroslope,
      isFloodpeak = peaks(as.vector(discharge), halfWindowSize = 2),
      disturbed = isFloodpeak | lag(isFloodpeak, n = 1, default = F) | lag(isFloodpeak, n = 2, default = F)
    )  %>%
    filter(
      !disturbed,
      discharge <= qref
    ) %>%
    select(-isFloodpeak, -disturbed) %>%
    mutate (
      event = group_const_value(falling)
    ) %>%
    filter(!is.na(event)) %>%
    nest(data = c(-event, -falling)) %>%
    mutate(
      len = map_int(data, nrow),
      falling = if_else(len <= smooth & !falling, TRUE, falling),
      event = group_const_value(falling)
    ) %>%
    filter(falling) %>%
    select(-len) %>%
    unnest(data) %>%
    nest(data = c(-event)) %>%
    mutate(
      # data = map(data, mutate, day = seq_len(n())),
      len = map_int(data, nrow)
    )

  recession <- segment %>%
    filter(len >= minlen) %>%
    unnest(data) %>%
    group_by(event) %>%
    mutate(day = row_number())

  if(plot) {
    ggplot(recession, aes(x = day, y = discharge, group = event)) +
      geom_line()
  }

  pairs <- recession %>%
    group_by(event) %>%
    mutate(
      discharge.leaded = lead(discharge)
    ) %>%
    na.omit()

  k <- .extract_slope(pairs$discharge, pairs$discharge.leaded)
  alpha <- -ln(k)
  # attr(k, "segments") <- recession
  return(unname(alpha))
}

recession(x$time, x$discharge)
