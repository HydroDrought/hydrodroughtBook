# todo

# 0) append desired groups
# - year = min(minor)
# - minor = day, week, month, season
# check compatibility of minor and year interval

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
      data <- unnest(data, .data$value)
    }
  }

  return(data)
}

#' @importFrom rlang exec
#' @importFrom purrr map2
.summarize_and_enframe <- function(data, funs, level)
{
  fun.level <-  paste0("fun.", level)

  col <- purrr::map(funs, "column")
  funs <- purrr::map(funs, "fun")

  # hack in order to operate on vectors and on lists
  l <- map2(col, funs, function(column, fun) fun(exec(c, !!!data[[column]])))

  tibble(!!fun.level := names(funs), value = l)
}

# x <- tibble(
#   vec1 = c(NA, 1:10),
#   vec2= c(as.Date(NA), Sys.Date() + lubridate::days(1:10)),
#   list1 = as.list(vec1),
#   list2 = as.list(vec2),
#   # list3 = c(list(NULL), tail(list1, -1))
# ) %>%
#   print()
#
# f <- function(x) x[3]
# f <- function(x) max(x, na.rm = TRUE)
#
# x %>%
#   mutate(across(where(is.list), unlist, use.names = F)) %>%
#   summarize(across(everything(), f))


# x <- tibble(
#   time = 1:10,
#   discharge = -1:-10,
#   state = sample(c("flow", "no-flow"), 10, replace = T),
#   value = 101:110
# )
#
# a_fun <- function(fun) {
#   fun <- rlang::enexpr(fun)
#
#   # if supplied without argument use "value" as default argument
#   if (!rlang::is_call(fun)) fun <- rlang::call2(fun, rlang::expr(value))
#
#   return(fun)
# }
#
# # becoming function (x) mean(x$value)
# eval_tidy(a_fun(mean), as_data_mask(x))
#
# # becoming function (x) mean(x$discharge)
# eval_tidy(a_fun(mean(discharge)), as_data_mask(x))
#
# # becoming function (x) complex_fun(t = x$time, q = x$discharge)
# complex_fun <- function(x, y) x * y
# eval_tidy(a_fun(complex_fun(time, discharge)), as_data_mask(x))
#

agg_fun <- function(fun, name = deparse(substitute(fun)), column = "value", default = NA)
{
  # todo: check that default and return value of fun are of same type
  # eg. when using time NA_Date
  lst(!!name := lst(fun, column, default))
}

.duration <- agg_fun(function(x) length(x),
                     name = "duration", column = "time", default = 0L)

#' @importFrom purrr map_lgl
#' @importFrom tidyselect any_of
#' @export
# agg.spell aggregates all values with the same spell id
# after aggregation there will be one value for every spell
ires_metric <- function(time, flow, threshold = 0.001,
                        na = c("none", "fill", "drop_group", "drop_minor", "drop_year"),
                        rule = c("cut", "duplicate", "onset", "termination", "majority"),
                        group.year = water_year(time),
                        group.minor = month(time, label = TRUE, abbr = TRUE),
                        agg.spell = NULL, agg.group = NULL, agg.minor = NULL,
                        agg.year = NULL, agg.total= NULL)
{
  na <- match.arg(na)
  rule <- match.arg(rule)

  x <- tibble(
    time,
    flow,
    year = group.year,
    minor = group.minor,
    group = group_const_value(paste(.data$year, .data$minor)) + 1
  )  %>%
    treat_missing_values(na = na)

  spells <- x %>%
    mutate(
      state = factor(flow <= threshold,
                     exclude = character(),
                     levels = c(TRUE, FALSE, NA),
                     labels = c("no-flow", "flow", "no data")),
      spell = group_const_value(.data$state) + 1
    )

  if(all(spells$state != "no-flow")) warning("Every flow value < threshold. River is not intermittent.")

  spells <- spells %>%
    allocate_spell(rule = rule) %>%
    select(time, flow, spell, group, minor, year, state)

  # ggplot(spells, aes(monthDay(time), year, fill = state)) + geom_tile()

   # holds the hierarchy of the aggregation process
  agg <- list(
    "time" = NULL,
    "flow" = NULL,
    "spell" = agg.spell,
    "group" = agg.group,
    "minor" = agg.minor,
    "year" = agg.year,
    # last group is equal to aggregating per state
    "state" = agg.total
  ) %>%
    tibble::enframe(name = "level", value = "fun") %>%
    mutate(
      level = factor(level, levels = level, ordered = T)
    )

  # remove columns we do not aggregate over, never skip time and flow
  remove <- setdiff(agg$level[map_lgl(agg$fun, is.null)], c("time", "flow", "state", "spell"))

  result <- spells %>%
    select(-any_of(remove))

  rows <- which(!(agg$level %in% c("time", "flow") | map_lgl(agg$fun, is.null)))
  for (l in rows) {
    level <- agg$level[l]
    nest.vars <- filter(agg, level < !!level) %>%
      pull(level) %>%
      as.character() %>%
      c("data", "value")

    complete.vars <- agg %>%
      filter(!map_lgl(.data$fun, is.null), level > !!level) %>%
      pull(level) %>%
      as.character()

    result <- result %>%
      nest(data = any_of(nest.vars)) %>%
      mutate(
        metric = map(data, .summarize_and_enframe,
                     funs = agg$fun[[l]], level = as.character(level))
      ) %>%
      unnest(.data$metric) %>%
      .complete_spells(level = as.character(level), funs = agg$fun[[l]],
                       complete = complete.vars) %>%
      .simplify_output()
  }

  return(result)
}

.complete_spells <- function(x, level, funs, complete)
{
  fun.name <- paste0("fun.", level)
  complete.vars <- c(rlang::ensyms(fun.name), rlang::syms(complete))

  defaults <- tibble::enframe(map(funs, "default"), name = fun.name, value = "default")

  x <- x %>%
    complete(!!!complete.vars, fill = list(value = list(NULL))) %>%
    left_join(defaults, by = fun.name) %>%
    mutate(
      #value = modify2(value, default, ~(if(is.null(.x)) .y else .x))
      value = if_else(map_lgl(value, is.null), default, value)
    ) %>%
    select(-default) %>%
    filter(!(state == "no data" & is.na(.data[[level]])))

  return(x)
}

allocate_spell <- function(x, rule = c("cut", "duplicate", "onset", "termination", "majority"))
{
  rule <- match.arg(rule)

  # todo: only one level can have rule = "cut" because it modifies
  # the spell number, not the level

  # todo: allocating minor is not meaningful, as minor is cyclic (repetitive)
  # always use group instead?
  if (length(rule) == 1) rule <- rep(rule, 3)

  x <- .allocate_spell_to_level(x, rule = rule[1], level = "year")
  x <- .allocate_spell_to_level(x, rule = rule[2], level = "minor")
  x <- .allocate_spell_to_level(x, rule = rule[3], level = "group")

  return(x)
}

.allocate_spell_to_level <- function(x, rule, level)
{

  # find spells spanning several intervals
  spanning <- x %>%
    count(spell, .data[[level]], name = "n.obs") %>%
    dplyr::add_count(spell) %>%
    filter(n > 1) %>%
    select(spell, !!level) %>%
    group_by(spell)

  if(nrow(spanning) == 0) return(x)

  # rule = "cut" is the default behavior because we group on
  # spell, group, minor, year before computing the metric
  # we could just do return(x)
  # but to demonstrate the behavior more explicitly add fractions to spell no
  if (rule == "cut") {
    # s <- mutate(spanning, spell.new = spell + (row_number() - 1) / n())
    #
    # res <- x %>%
    #   full_join(s, by = c("spell", level)) %>%
    #   mutate(
    #     spell = if_else(!is.na(spell.new), spell.new, spell)
    #   ) %>%
    #   select(-spell.new)
    #
    # # here we are modifying the spell.no thats why we return early
    # return(res)

    return(x)

  } else if(rule == "duplicate") {
    s <- rename(spanning, level.new = !!level)
  } else if(rule == "onset") {
    s <- mutate(spanning, level.new = first(.data[[level]]))
  } else if(rule == "termination") {
    s <- mutate(spanning, level.new = last(.data[[level]]))
  } else if(rule == "majority") {
    Mode <- function(x) {
      ux <- unique(x)
      ux[which.max(tabulate(match(x, ux)))]
    }

    s <- mutate(spanning, level.new = Mode(.data[[level]]))
  }

  res <- x %>%
    dplyr::full_join(s, by = if(rule == "duplicate") "spell" else c("spell", level)) %>%
    mutate(
      !!level := if_else(!is.na(level.new), level.new, .data[[level]])
    ) %>%
    select(-level.new)

  return(res)
}

treat_missing_values <- function(x, na = c("none", "fill", "drop_group", "drop_minor", "drop_year") )
{
  if( all(!is.na(x$flow))) return(x)

  if (na == "none") {
    warning("NA flow values found. Derived metrics may be wrong.\n Consider using 'na = \"drop_year\"'.")
    return(x)
  }

  if (na == "fill")  {
    x <- tidyr::fill(x, -time, .direction = "down")
  } else {
    drop <- stringr::str_remove(na, "drop_")

    incomplete <- x %>%
      filter(is.na(.data$flow)) %>%
      select(tidyselect::one_of(drop))

    x <- x %>%
      anti_join(incomplete, by = drop)
  }

  return(x)
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


  f.year <- if(consecutive) agg_fun(max) else agg_fun(sum)
  tbl <- ires_metric(time, flow, threshold, agg.spell = .duration,
                     agg.year = f.year) %>%
    filter(state == "no-flow")

  any(tbl$value >= ndays)

}








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
  tbl <- ires_metric(time, flow, threshold, agg.year = .duration)

  nyears <- length(unique(tbl$year))
  noflowyears <- tbl %>% filter(state == "no-flow", value > 0) %>% nrow()

  result <- noflowyears / nyears %>%
    set_names(name)

  return(result)
}

#' @export
MAN <- function(time, flow, threshold = 0.001, name = "MAN")
{
  m <- ires_metric(time, flow, threshold, na = "drop_year",
                   agg.year = .duration, agg.total = agg_fun(mean))

  result <- filter(m, state == "no-flow") %>%
    pull(value) %>%
    set_names(name)

  return(result)
}

#' @export
CVAN <- function(time, flow, threshold = 0.001, name = "CVAN")
{
  m <- ires_metric(time, flow, threshold, na = "drop_year",
                   agg.year = .duration, agg.total = agg_fun(cv))

  result <- filter(m, state == "no-flow") %>%
    pull(value) %>%
    set_names(name)

  return(result)
}

#' @export
FAN <- function(time, flow, threshold = 0.001)
{
  m <- ires_metric(time, flow, threshold, na = "drop_year",
                   agg.year = .duration)

  tbl <- filter(m, state == "no-flow")

  result <- tbl$value %>%
    set_names(tbl$year)

  return(result)
}

#' @export
MAMD <- function(time, flow, threshold = 0.001, name = "MAMD")
{
  m <- ires_metric(time, flow, threshold, na = "drop_year",
                   agg.spell = .duration, agg.year = agg_fun(max),
                   agg.total = agg_fun(mean))

  result <- filter(m, state == "no-flow") %>%
    pull(value) %>%
    set_names(name)

  return(result)
}

# k <- function(time, flow, threshold = 0.001, name = "k")
# {
#   r <- function(...) {
#     x <-  recession(...)
#     if (is.null(x) | length(x) == 0) x <- NA_real_
#
#     return(x)
#   }
#
#   if(all(!is.na(flow))) {
#
#     m <- ires_metric(time, flow, threshold,
#                      # using two columns does not work
#                      agg.year = agg_fun(fun = r, name = "recession", column = c("time", "flow")),
#                      agg.total = agg_fun(fun = function(x) mean(x, na.rm = TRUE), name = "mean", default = NA_real_)
#                      )
#
#     result <- filter(m, state == "flow")$value
#
#   } else {
#     result <- NA_real_
#   }
#
#   browser()
#   result <- filter(m, state == "no-flow") %>%
#     pull(value) %>%
#     set_names(name)
#
#   return(result)
# }

#' @export
tau0 <- function(time, flow, threshold = 0.001, name = "mean onset")
{
  f.spell <- agg_fun(min, name = "onset", column = "time", default = as.Date(NA))

  m <- ires_metric(time, flow, threshold, na = "drop_year",
                   agg.spell = f.spell,
                   agg.year = agg_fun(first), agg.total = agg_fun(mean_day))

  result <- filter(m, state == "no-flow") %>%
    pull(value) %>%
    set_names(name)

  return(result)
}

#' @export
tau0r <- function(time, flow, threshold = 0.001, name = "strength onset")
{
  f.spell <- agg_fun(min, name = "onset", column = "time", default = as.Date(NA))

  m <- ires_metric(time, flow, threshold, na = "drop_year",
                   agg.spell = f.spell,
                   agg.year = agg_fun(first), agg.total = agg_fun(strength_day))

  result <- filter(m, state == "no-flow") %>%
    pull(value) %>%
    set_names(name)

  return(result)
}

#' @export
tauE <- function(time, flow, threshold = 0.001, name = "mean termination")
{
  f.spell <- agg_fun(max, name = "termination", column = "time", default = as.Date(NA))

  m <- ires_metric(time, flow, threshold, na = "drop_year",
                   agg.spell = f.spell,
                   agg.year = agg_fun(first), agg.total = agg_fun(mean_day))

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

strength_day <- function(x, lwr = 0, upr = 365, na.rm = TRUE)
{
  if (all(is.na(x))) return(NA)
  if (anyNA(x)) {
    if (na.rm) x <- stats::na.omit(x) else return(NA)
  }

  if (inherits(Sys.Date(), c("Date", "POSIXct"))) {
    x <- lubridate::yday(.correct_leapyear(x))
  }

  d <- circular_sd(x = x, lwr = lwr, upr = upr)
}

mean_day <- function(x, lwr = 0, upr = 365, na.rm = TRUE)
{

  if (all(is.na(x))) return(NA)
  if (anyNA(x)) {
    if (na.rm) x <- stats::na.omit(x) else return(NA)
  }

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
