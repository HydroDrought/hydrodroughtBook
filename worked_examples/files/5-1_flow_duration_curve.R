library(hydroDrought)

# filter the dataset, just select Ngaruroro river
ngaruroro <- filter(international, River == "Ngaruroro") %>%
  select(discharge) %>%
  unnest()


# function to compute the exceedance frequency
exceedance_frequency <- function(flow)
{
  # current rank
  i <- rank(-flow, ties.method = "min", na.last = "keep")

  # largest rank in sample (= number of non-missing values)
  N <- length(na.omit(flow))

  # the exceedance frequency can be seen as the relative rank
  return(i / N)
}

# compute rank and exceedance frequency for every observation
ngaruroro <- ngaruroro %>%
  mutate(rank = rank(-discharge, ties.method = "min"),
         freq.exc = exceedance_frequency(discharge))

ngaruroro

# plot the Flow Duration curve
library(ggplot2)
ggplot(ngaruroro, aes(x = freq.exc, y = discharge)) +
  geom_line() +
  scale_y_log10() +
  scale_x_continuous(labels = percent_format(accuracy = 1)) +
  labs(x = "Exceedance Frequency",
       y = expression(paste("Flow (", m^{3}, s^{-1}, ")")))

# show the first observation with a exceedance frequency less or wqual to 0.9
ngaruroro %>%
  filter(freq.exc <= 0.9) %>%
  arrange(desc(freq.exc)) %>%
  head(1)

# compute several low flow quantiles directly
lfquantile(ngaruroro$discharge, exc.freq = c(0.95, 0.9, 0.8))
