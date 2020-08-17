# todo: expand POSIXct with origin and store as attribute or better as list
# this way we do not have to propagate origin = "-01-01" through every function


#' @export
monthDay <- function(x, origin = "-01-01")
{
    x <- as.Date(x)
    origin <- regmatches(origin, regexpr("-.*", origin))

    # origin must exist in non-leap years
    i <- tryCatch(as.Date(paste0("1971", origin)),
                  error = function(e) stop(origin, " is not a valid origin.",
                                           call. = FALSE))

    year(x) <- 1972

    # year has to contain Feb 29th.
    # if origin is after Feb 29th: take the previous or next year
    o <- paste0("1972", origin)
    if (o > as.Date("1972-02-29")) {
        year(x[which(x >= o)]) <- 1971
        origin <- as.Date(paste0("1971", origin))
    } else {
        year(x[which(x < o)]) <- 1973
        origin <- as.Date(paste0("1972", origin))
    }

    attr(x, "origin") <- origin
    class(x) <- c("monthDay", "Date")
    return(x)
}

#' @export
is.monthDay <- function(x) inherits(x, "monthDay")

# length, [, [<-, [[, [[<-, c

#' @export
`[.monthDay` <- function(x, ...) {
    y <- NextMethod()

    attr(y, "origin") <- attr(x, "origin")
    return(y)
}

#' @export
c.monthDay <- function(...) {
    args <- list(...)
    cl <- lapply(args, class)
    origin <- attr(args[[1]], "origin")
    y <- NextMethod()

    # all elements must have same origin
    o <- lapply(args, attr, which = "origin")
    if (length(unique(o)) > 1) stop("Not all arguments have the same origin/start.")
    attr(y, "origin") <- origin
    class(y) <- cl[[1]]
    return(y)
}

#' @export
pillar_shaft.monthDay <- function(x, ...) {
    out <- format(x)
    out[is.na(x)] <- NA
    pillar::new_pillar_shaft_simple(out, align = "right")
}

#' @export
vec_ptype_abbr.monthDay <- function(x) {
    "month-day"
}

#' @export
format.monthDay <- function(x, format = "-%m-%d", ...)
{
    NextMethod(format = format, ...)
}

#' @export
print.monthDay <- function(x, ...) {
    cat(format(x, ...), "\n")
}


# doesn't work
# filter(ng, md == "-02-29")

# ggplot doesn't work with as.double method
# as.double.monthDay <- function(x, ...) {
#     unclass(x) - as.numeric(attr(x, "origin")) + 1
# }

#' @export
as.integer.monthDay <- function(x, ...) {
    as.integer(unclass(x) - as.numeric(attr(x, "origin")) + 1)
}


.origin <- function(x)
{
    str <- ifelse(is.na(x), NA_character_, paste0("2001", x))
    monthDay(str)
}
