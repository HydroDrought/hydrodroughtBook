#' @importFrom stats quantile
#' @export
lfquantile <- function(x, exc.freq = c(0.95, 0.9, 0.8, 0.5), na.rm = TRUE, ...)
{
  q <- quantile(x = x, probs = 1 - exc.freq, names = FALSE, na.rm = na.rm, ...)
  names(q) <- paste0("Q", round(exc.freq * 100, digits = 1))

  return(q)
}

# consider the following, automatically completing seasons
# low_flow_percentile <- function(x, prob, start = NA, end = NA)
# {
#   x %>%
#     complete_seasons(start = start, end = end)  %>% # prototype in hd-book code
#     ungroup() %>%
#     summarise(q = quantile(discharge, probs = prob, na.rm = TRUE)) %>%
#     pull(q)
# }


#' @importFrom lfstat baseflow
#' @export
lfstat::baseflow

#' @importFrom stats qnorm
#' @export
sgi <- function(x, na.rm = TRUE)
{
    if (na.rm) x <- x[is.finite(x)]

    qnorm(rank(x) / (length(x) + 1))
}

#' @export
mean_annual_minimum <- function(discharge, time, start = "-01-01", n = 1,
                                na.rm = FALSE, omit.missing.years = TRUE)
{
  x <- tibble(
    time = time,
    year = water_year(time, start = start),
    discharge = discharge
  ) %>%
    mutate(
      smoothed = moving_average(.data$discharge, n = n),
      # replace NAs introduced by smoothing with Inf so they do not affect the min()
      smoothed = replace(.data$smoothed, seq_len(n - 1), Inf)
    ) %>%
    group_by(.data$year) %>%
    summarise(am = min(.data$smoothed, na.rm = na.rm), .groups = "drop")

  if (omit.missing.years) {
    x <- x %>%
      filter(is.finite(.data$am))
  }

  x <- x %>%
    summarise(mam = mean(.data$am)) %>%
    pull(.data$mam)

  names(x) <- paste0("MAM(", n, ")")

  return(x)
}
