#' @export
lfquantile <- function(x, exc.freq = c(0.95, 0.9, 0.8, 0.5), na.rm = TRUE, ...)
{
  q <- quantile(x = x, probs = 1 - exc.freq, names = FALSE, na.rm = na.rm, ...)
  names(q) <- paste0("Q", round(exc.freq * 100, digits = 1))

  return(q)
}
