
days_in_year <- function(x, origin = "-01-01")
{
    from <- if (is.numeric(x)) as.Date(paste0(x, origin)) else x
    to <- as.Date(paste0(year(from)+1, origin))

    days <- as.double(to - from, units = "days")
    return(as.integer(days))
}

# todo: use origin instead of start in whole package

# coverage_yearly <- function(x, origin = "-01-01")
# {
#     tibble(time = x) %>%
#         append_group(by = "year", start = origin) %>%
#         count(year) %>%
#         mutate(coverage = n / days_in_year(year, origin = origin)) %>%
#         arrange(year)
# }

remove_incomplete_first_last <- function(x, percent = 0.99, origin = "-01-01")
{
    has.year <- "year" %in% colnames(x)
    if (has.year) x <- append_group(x, by = "year", start = origin)

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



month_label_midpoint <- function(limits)
{
    fs <- seq(from = limits[1], to = limits[2], by = "day")
    fs[day(fs) == 15]
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
        append_group("year", start = origin) %>%
        mutate(day = monthDay(time, origin = origin)) %>%
        remove_incomplete_first_last(origin = origin)

    f <- function(x)
    {
        list(
            tibble(
                discharge = lfquantile(x, exc.freq = exc.freq),
                exc.freq = names(discharge)
            ) %>%
                mutate(exc.freq = factor(exc.freq, levels = exc.freq))
        )
    }

    quantiles <- x %>%
        var_threshold(vary.by = "day", fun = f, start = origin) %>%
        unnest(threshold) %>%
        group_by(exc.freq) %>%
        mutate(
            smoothed = pracma::whittaker(discharge),
            smoothed = case_when(smoothed < 0.00025 ~ 0,
                                 smoothed < 0.0005 ~ 0.0005,
                                 TRUE ~ smoothed)
        )

    q.const <- x %>%
        const_threshold(fun = f) %>%
        unnest(threshold)

    envelope <- x %>%
        group_by(day) %>%
        summarise(across(discharge, lst(min, max), na.rm = T), .groups = "drop")

    extremes <- x %>%
        group_by(year) %>%
        summarise(
            resid.dry = sum((discharge[discharge < median(discharge)])^2),
            resid.wet = sum((discharge[discharge > median(discharge)])^2),
            .groups = "drop") %>%
        nest(data = everything()) %>%
        transmute(
            wet = slice_max(data[[1]], resid.wet, n = 1, with_ties = FALSE)$year,
            dry = slice_min(data[[1]], resid.dry, n = 1, with_ties = FALSE)$year
        ) %>%
        pivot_longer(everything(), names_to = "state", values_to = "year") %>%
        left_join(x, by = "year")

    labels <- extremes %>%
        select(state, time, discharge, day) %>%
        nest(data = c(discharge, time, day)) %>%
        mutate(
            min = map(data, ~slice_min(.x, discharge, n = 1, with_ties = FALSE)),
            max = map(data, ~slice_max(.x, discharge, n = 1, with_ties = FALSE))
        ) %>%
        select(-data) %>%
        pivot_longer(-state, names_to = "extreme", values_to = "data") %>%
        filter(state == "wet" & extreme == "max" | state == "dry" & extreme == "min") %>%
        unnest(data) %>%
        mutate(text = paste0(time, ": ", round(discharge, 3), " m³/s"))

subtitle <- paste("Gauged daily flow for", year(min(x$time)), "-", year(max(x$time)),
                  subtitle)

#  muted(hue_pal()(2), l = 90, c = 30)
ggplot(x, aes(x = day)) +

    # envelopes
    geom_ribbon(data = envelope, aes(ymax = discharge_min), ymin = -Inf, fill = "#FFD7D5") +
    geom_ribbon(data = envelope, aes(ymin = discharge_max), ymax = Inf, fill =  "#B3EDF0") +

    # yearly discharge
    geom_line(mapping = aes(y = discharge, group = year), alpha = 0.2, size = 0.1) +

    # quantiles
    geom_line(data = quantiles, mapping = aes(y = smoothed, linetype = exc.freq), size = 0.2) +
    geom_hline(data = q.const, aes(yintercept = discharge, linetype = exc.freq), size = 0.2) +

    # extreme years
    geom_point(data = labels, mapping = aes(y = discharge), shape = 1, size = 2) +
    geom_line(data = extremes, mapping = aes(y = discharge, col = state), show = FALSE) +
    ggrepel::geom_text_repel(data = labels, mapping = aes(y = discharge, label = text),
                             direction = "both", point.padding = 0.5, size = 3) +

    labs(title = title, subtitle = subtitle) +
    scale_y_log10(label = function(x) paste(format(x, scientific = F, drop0trailing = T), "m³/s"),
                  breaks = log_breaks_covered, oob = squish_infinite) +
    scale_x_date(date_labels = "%b", breaks = month_label_midpoint,
                  expand = expansion(mult = 0, add = 0)) +
    scale_linetype_discrete("", guide = guide_legend(reverse = TRUE)) +

    # coord_cartesian(ylim = c(max(0.0005, min(x$discharge[x$discharge > 0], na.rm = TRUE)), NA)) +

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
        append_group("year", start = origin) %>%
        mutate(
            day = monthDay(time, origin = origin),
            coverage = if_else(is.na(discharge), "data missing", "data available"),
            coverage = factor(coverage, levels = c("data missing", "data available"))
        )

    p <- ggplot(x, aes(day, year, fill = coverage)) +
        geom_tile() +

        labs(title = title, subtitle = subtitle) +
        scale_x_date(date_labels = "%b", breaks = month_label_midpoint,
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

    plot(p)
}
