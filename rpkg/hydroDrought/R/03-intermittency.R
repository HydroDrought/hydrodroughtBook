# todo

# 0) append desired groups
# - major = min(minor)
# - minor = day, week, month, season
# check compatibility of minor and major interval

#!! 1) treat NAs drop = c("keep", "drop_values", "drop_group", "drop_minor", "drop_major"),
# 2) compute spells
#!! 3) treat spells spanning several groups
#- rule = c("cut", "duplicate", "onset", "termination"),

# pooling of events

# functions aggregation spells need default values
# e.g. duration = 0 for "no-flow" in case there is always flow in a given year
# the default default value could be NA
# f.spell <- lst(
#   duration = list(
#     fun = function(x) length(x),
#     column = "time",
#     default = 0),
#   # interval = list(function(x) interval(min(x), max(x)), "time"),
#   # first.day = function(x) min(x$time),
#   # last.day = function(x) max(x$time),
#   # duration = list(function(x) diff(range(x)), "time"),
#   # min.flow = list(min, "discharge"),
#   # max.flow = function(x) max(x$discharge)
# )

.simplify_output <- function(data, simplify = TRUE)
{
  if(simplify) {
    if (length(unique(map(data$value, class))) == 1) {
      data <- unnest(data, value)
    }
  }

  return(data)
}

#' @importFrom rlang exec
#' @importFrom purrr map2
.summarize_and_enframe <- function(data, funs, name, default_col = "value")
{
  if (all(lengths(funs) == 1)) {
    col <- default_col
  } else {
    col <- purrr::map(funs, 2)
    funs <- purrr::map(funs, 1)
  }

  # hack in order to operate on vectors and on lists
  l <- map2(col, funs, function(column, fun) fun(exec(c, !!!data[[column]])))

  tibble(!!name := names(funs), value = l)
}

#' @importFrom purrr map_lgl
#' @importFrom tidyselect any_of
#' @export
# agg.spell aggregates all values with the same spell id
# after aggregation there will be one value for every spell
ires_metric <- function(time, flow, threshold = 0.001,
                        group.major = water_year(time),
                        group.minor = month(time, label = TRUE, abbr = TRUE),
                        agg.spell = NULL, agg.group = NULL, agg.minor = NULL,
                        agg.major = NULL, agg.total= NULL)
{

  spells <- tibble(
    time,
    flow,
    major = group.major,
    minor = group.minor
  ) %>%
    mutate(
      group = group_const_value(paste(major, minor)) + 1,
      state = factor(flow <= threshold,
                     exclude = character(),
                     levels = c(TRUE, FALSE, NA),
                     labels = c("no-flow", "flow", "no data")),
      spell = group_const_value(state) + 1
    ) %>%
    select(time, flow, spell, group, minor, major, state)

  # ggplot(spells, aes(monthDay(time), major, fill = state)) + geom_tile()

  # todo:  agg.spell must not be NULL
  # todo: check all agg functions must be named lists

  # holds the hierarchy of the aggregation process
  agg <- list(
    "time" = NULL,
    "flow" = NULL,
    "spell" = agg.spell,
    "group" = agg.group,
    "minor" = agg.minor,
    "major" = agg.major,
    "total" = agg.total
  ) %>%
    tibble::enframe(name = "level", value = "fun") %>%
    mutate(name = paste0("fun.", level))

  # never skip time and flow
  remove <- setdiff(agg$level[map_lgl(agg$fun, is.null)], c("time", "flow"))

  # remove columns we do not aggregate over
  result <- spells %>%
    select(-any_of(remove))

  # todo: implement default value for first aggregation

  rows <- !(agg$level %in% c("time", "flow") | map_lgl(agg$fun, is.null))
  for (l in which(rows)) {
    if (!is.null(agg$fun[[l]])) {
      result <- result %>%
        nest(data = any_of(c(utils::head(agg$level, l - 1), "data", "value"))) %>%
        mutate(
          metric = map(data, .summarize_and_enframe,
                       funs = agg$fun[[l]], name = agg$name[l])
        ) %>%
        unnest(metric) %>%
        .simplify_output()
    }
  }

  return(result)
}


#' Checking for flow intermittency
#' @export
#' @examples
#' \dontrun{
#' library(dplyr)
#' library(purrr)
#'
#' international %>%
#' select(river, data) %>%
#'   mutate(
#'     is.intermittent = map_lgl(data, ~is_intermittent(.x$time, .x$discharge))
#'   )}
is_intermittent <- function(time, flow, threshold = 0.001,
                            ndays = 5, consecutive = TRUE) {

  # shortcut
  if(all(stats::na.omit(flow) > threshold)) return(FALSE)


  f.major <- if(consecutive) lst(max) else lst(sum)
  tbl <- ires_metric(time, flow, threshold, agg.spell = .duration,
                     agg.major = f.major) %>%
    filter(state == "no-flow")

  any(tbl$value >= ndays)

}





.duration <- lst(
  duration = list(
    fun = function(x) length(x),
    column = "time",
    default = 0)
)


# Metrics -----
cv <- function(x) {
  x <- as.double(x)
  stats::sd(x) / base::mean(x)
}

#' @importFrom rlang set_names
#' @export
no_flow_years <- function(time, flow, threshold = 0.001,
                          name = "proportion of no flow years")
{
  f.major <- lst(
    "no flow year" = list(
      fun = function(x)  {browser(); any(x == "no-flow")},
      column = "state",
      default = NA)
  )


  tbl <- ires_metric(time, flow, threshold, agg.major = .duration)

  nyears <- length(unique(tbl$major))
  noflowyears <- tbl %>% filter(state == "no-flow", value > 0) %>% nrow()

  result <- noflowyears / nyears %>%
    set_names(name)

  return(result)
}

#' @export
MAN <- function(time, flow, threshold = 0.001, name = "MAN")
{
  m <- ires_metric(time, flow, threshold,
                   agg.major = .duration, agg.total = lst(mean))

  result <- filter(m, state == "no-flow") %>%
    pull(value) %>%
    set_names(name)

  return(result)
}

#' @export
CVAN <- function(time, flow, threshold = 0.001, name = "CVAN")
{
  m <- ires_metric(time, flow, threshold,
                   agg.major = .duration, agg.total = lst(cv))

  result <- filter(m, state == "no-flow") %>%
    pull(value) %>%
    set_names(name)

  return(result)
}

#' @export
FAN <- function(time, flow, threshold = 0.001)
{
  m <- ires_metric(time, flow, threshold,
                   agg.major = .duration)

  tbl <- filter(m, state == "no-flow")

  result <- tbl$value %>%
    set_names(tbl$major)

  return(result)
}

#' @export
MAMD <- function(time, flow, threshold = 0.001, name = "MAMD")
{
  m <- ires_metric(time, flow, threshold,
                   agg.spell = .duration, agg.major = lst(max),
                   agg.total = lst(mean))

  result <- filter(m, state == "no-flow") %>%
    pull(value) %>%
    set_names(name)

  return(result)
}

# k <- function(time, flow, threshold = 0.001, name = "k")
# {
#   r <- function(...) {
#     x <-  smires:::recession(...)
#     if (is.null(x)| length(x) == 0) x <- NA_real_
#
#     return(x)
#   }
#
#   if(all(!is.na(flow))) {
#     m <- ires_metric(time, flow, threshold,
#                      agg.major =  list(
#                        recession = list(fun = function(x) r(x),
#                                         column = "flow",
#                                         default = 0)
#                      ),
#                      agg.total = list(mean = function(x) mean(x, na.rm = TRUE))
#     )
#
#     result <- filter(m, state == "flow")$value
#
#   } else {
#     result <- NA_real_
#   }
#
#     names(result) <- name
#
#   return(result)
# }

#' @export
tau0 <- function(time, flow, threshold = 0.001, name = "mean onset")
{

  f.spell <- lst(
    onset = list(
      fun = function(x) min(x),
      column = "time",
      default = 0)
  )

  m <- ires_metric(time, flow, threshold,
                   agg.spell = f.spell,
                   agg.major = lst(first), agg.total = lst(mean_day))

  result <- filter(m, state == "no-flow") %>%
    pull(value) %>%
    set_names(name)

  return(result)
}

#' @export
tauE <- function(time, flow, threshold = 0.001, name = "mean termination")
{

  f.spell <- lst(
    termination = list(
      fun = function(x) max(x),
      column = "time",
      default = 0)
  )

  m <- ires_metric(time, flow, threshold,
                   agg.spell = f.spell,
                   agg.major = lst(first = first), agg.total = lst(mean_day))

  result <- filter(m, state == "no-flow") %>%
    pull(value) %>%
    set_names(name)

  return(result)
}


# Circular statistics ----
.circular_stats <- function(x, lwr = 0, upr = 365) {
  if(any(x < lwr | x > upr))
    stop("input data not in range [", lwr, ", ", upr, "]")

  ang <- (x - lwr)/upr * 2 *pi

  m <- mean(exp(1i * ang))  # mean vector
  a <- Mod(m)               # absolute value

  phi <- Arg(m)
  if (phi < 0) phi <- Arg(m) + 2*pi
  cm <- phi * (upr - lwr)/2/pi + lwr


  cv <- ((upr - lwr)/2/pi)^2 * 2 * log(1/a)

  csd <- ((upr - lwr)/2/pi) * sqrt(-2* log(a))

  return(c(mean = cm, var = cv, sd = csd, abs = a))
}

circular_mean <- function(x, lwr = 0, upr = 365)
  .circular_stats(x = x, lwr = lwr, upr = upr)["mean"]

circular_r <- function(x, lwr = 0, upr = 365)
  .circular_stats(x = x, lwr = lwr, upr = upr)["abs"]

circular_sd <- function(x, lwr = 0, upr = 365)
  .circular_stats(x = x, lwr = lwr, upr = upr)["sd"]


mean_day <- function(x, lwr = 0, upr = 365)
{
  if (inherits(Sys.Date(), c("Date", "POSIXct"))) {
    x <- lubridate::yday(.correct_leapyear(x))
  }

  d <- circular_mean(x = x, lwr = lwr, upr = upr)
  d <- monthDay(round(d))

  return(d)
}


# class jday ----

# .ordinal_suffix <- function(x) {
#   unit.pos <- x %% 10
#
#   # suffix for values > 10 has to be th
#   decade <- (x %% 100) %/% 10
#
#   suffix <- rep_len("th", length(x))
#   mask <- unit.pos %in% 1:3 & decade != 1
#   suffix[mask] <- c("st", "nd", "rd")[unit.pos[mask]]
#
#   return(suffix)
# }
#
#
# print.jday <- function(x, ...)
# {
#   y <- format.jday(x)
#   y <- ifelse(is.na(y),  y <- NA_character_, y)
#   print(y)
# }
#
#
# format.jday <- function(x, ...)
# {
#   nam <- names(x)
#   if(is.numeric(x)) {
#     if(all(x > 0 & x < 366)){
#       x <- as.Date(as.numeric(x) - 1, origin = "1970-01-01")
#     } else {
#       stop("Argument `x` must be either date or an integer inside [1, 365].")
#     }
#   }
#
#   x <- as.Date(x)
#   month <- month.abb[as.numeric(format(x, "%m"))]
#
#   day <- as.numeric(format(x, "%d"))
#   suffix <- .ordinal_suffix(day)
#   day <- format(paste0(day, suffix), width = 4, justify = "right")
#
#   y <- paste0(month, " ", day, " (day ",
#               format(as.numeric(format(x, "%j")), width = 3, justify = "right"), ")")
#   names(y) <- nam
#
#   return(y)
# }

# ensures that x is always between 1 and 365 by shifting all dates in a leap
# year after Feb 28 by -1
.correct_leapyear <- function(x)
{
  x <- as.Date(x)
  day <- lubridate::yday(x)

  wrong <- .is_leapyear(x) & day >= 60
  if(any(wrong)) {
    # we are in a leap year and after Feb 28th
    #  warning("Date falls into a leap year. Using the previous day.")
    x[wrong] <- x[wrong] - 1
  }

  return(x)
}

.is_leapyear <- function(x)
{
  year <- year(x)
  (year%%4 == 0) & ((year%%100 != 0) | (year%%400 == 0))
}
