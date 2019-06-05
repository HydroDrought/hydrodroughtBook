# *------------------------------------------------------------------
# | PROGRAM NAME: 12-2_likelihood_of_impact_occurrence
# | FILE NAME: 12-2_likelihood_of_impact_occurrence.R
# | DATE: 
# | CREATED BY:  Jim Stagge         
# *----------------------------------------------------------------
# | PURPOSE:  Fit a logistic regression equation, to relate SPI/SPEI with impact likelihood
# | 
# |
# *------------------------------------------------------------------

###########################################################################
## Set the Paths
###########################################################################
require(here)

## Path for Data and Output	
### It is a good idea to separate input data and output data folders
data_path  <- file.path(here::here(), "../data/impact_data")
output_path <- file.path(here::here(), "./output/Example_12_2")

### Set output location
### This code will create an output folder if one does not exist
write_output_base_path <- output_path
dir.create(write_output_base_path, showWarnings = FALSE)

figure_path <- file.path(write_output_base_path, "figures")
dir.create(figure_path, showWarnings = FALSE)


###########################################################################
###  Load functions
###########################################################################
### Load these functions
#require(tidyverse)

require(dplyr)
require(readr)
require(tidyr)
require(ggplot2)


require(lubridate)

require(grid)
require(gridExtra)

source(file.path(here::here(), "./files/12-2_impact/plotting_funcs.R"))

###########################################################################
## Set Initial Values
###########################################################################


###########################################################################
## Set Additional Output Folders
###########################################################################




###########################################################################
## Load impact data
###########################################################################
### Read in Impacts
impact_df <- read_csv(file.path(data_path, "MT_Central_Ic_01.csv"))

### Check the read-in 
head(impact_df)

impact_df <- impact_df %>%
	select(YYYY, HIT, NUTS_ID_x) %>%  ### Cut to only year, impact, and NUTS region
	rename("nuts" = "NUTS_ID_x", "year" = "YYYY", "impact" = "HIT") %>%   ### Rename columns
	mutate(impact = as.logical(impact))	### Make impact column TRUE/FALSE

### Check the read-in
head(impact_df)

###########################################################################
## Prepare the impact data
###########################################################################
### Prepare to cut impacts to a country level
### Create a country column using the first two letters of NUTS region
impact_df <- impact_df %>%
	mutate(country = substr(nuts, 1, 2))

### Aggregate impacts at NUTS spatial scale to country level
impact_df <- impact_df %>%
	group_by(year, country) %>%
	summarise(impact_count = sum(impact, na.rm=TRUE), nuts_n = n()) %>%	### Calculate number of impacts 
	mutate(impact = impact_count > 0) 	### Assume an impact if at least one NUTS region has impact in year

### Check results
head(impact_df)

### Visual check
ggplot(impact_df, aes(x=year, y=impact, colour=country, group=country)) + geom_line() + theme_classic(8) + facet_wrap(~ country)

###########################################################################
## Load drought indices
###########################################################################
### Read in SPI/SPEI for 3 and 12 month periods
### Cut to Germany because we will be focusing on Germany

### Read in a dataframe for SPI 3
spi_3 <- read_csv(file.path(data_path, "spi_3_combined_countries_monthly_wide.csv")) %>%
	mutate(index = "spi_3") %>%		### Create a column for the drought index
	select(X, DE, index) 	### Cut to only Germany

### Read in a dataframe for SPEI 3
spei_3 <- read_csv(file.path(data_path, "spei_3_combined_countries_monthly_wide.csv")) %>%
	mutate(index = "spei_3") %>%		### Create a column for the drought index
	select(X, DE, index) 	### Cut to only Germany

### Read in a dataframe for SPI 12
spi_12 <- read_csv(file.path(data_path, "spi_12_combined_countries_monthly_wide.csv")) %>%
	mutate(index = "spi_12") %>%
	select(X, DE, index) 

### Read in a dataframe for SPEI 12
spei_12 <- read_csv(file.path(data_path, "spei_12_combined_countries_monthly_wide.csv")) %>%
	mutate(index = "spei_12") %>%
	select(X, DE, index) 

### Combine data
spi_df <- rbind(spi_3, spei_3, spi_12, spei_12)

### Check data
head(spi_df)
tail(spi_df)

### Reorganize data columns
spi_df <- spi_df %>%
	mutate(date = as.Date(paste0(X, "-01"))) %>%    ### Convert X column to a date by adding the first day of the month
	mutate(month = month(date), year = year(date)) %>%	### Create month and year columns
	separate(index, c("index_type", "index_months"), remove=FALSE) %>%   ### Split the index column
	rename(index_val = DE) %>%	### Rename the DE column to be the droughtindex value
	select(date, year, month, index_val, index, index_type, index_months) %>%	### Reorganize the column order
	arrange(index, date)	### Sort by index and then date

### Check results of reorganization
head(spi_df)
tail(spi_df)

### Visual check
ggplot(spi_df, aes(x=date, y=index_val, colour=index_type)) + geom_line() + theme_classic(8) + facet_grid(index_months ~ . )

###########################################################################
## Cut to Germany, then Combine Impacts and Indices prior to analysis
###########################################################################
### Cut impact to only Germany
impacts_de <- impact_df %>%
	filter(country == "DE")

### Visual check
ggplot(impacts_de, aes(x=year, y=impact_count, colour=country)) + geom_line() + theme_classic(8)


### Cut SPI and SPEI to only July. Our analysis will compare SPI/SPEI -3 and -12 in July with Annual Agricultural impacts
### This was chosen to mirror the findings of Stagge et al. () without having to include a monthly term
spi_de <- spi_df %>%
	filter(month == 7)

### Merge dataframes using a full join (i.e. all records are included, even if data is missing)
spi_impacts <- spi_de %>%
	full_join(impacts_de, by = "year") %>%	### Join on year
	drop_na(impact)	### Drop rows with NA in the impact column

### Check merge
head(spi_impacts)
tail(spi_impacts)

###########################################################################
## Preliminary Checks to see if there is a likely relationship between SPI12 and Impacts
## Plot a time series
###########################################################################
### Extract a dataframe with only impacts for plotting purposes
### Add a left and right column to plot regions 0.5 a year on either side of impact

### Only consider SPEI 3
plot_df <- spi_impacts %>%
	filter(index == "spei_3")

### Another way of doing it, delete this unless you want to go this route 
### You will need this for multi variable regression
#spi_impacts %>% 
#	select(date, impact, index, index_val) %>%
#	spread(index, index_val)

plot_impacts <- plot_df %>%
	select(year, impact) %>%
	filter(impact == TRUE) %>%
	mutate(left = year - 0.5, right = year + 0.5)

### Create a time series plot
p <- ggplot(plot_df, aes(x=year)) %>%
	+ geom_hline(yintercept=0,  color = "grey70") %>%  ### Make a horizontal axis at y= 0
	+ geom_rect(data = plot_impacts, aes(xmin=left, xmax=right, ymin=-Inf, ymax=+Inf), fill='pink', alpha=0.9) %>%   ### Create a pink region around the impact
	+ geom_vline(data = plot_impacts, aes(xintercept = year), linetype="dotted", color = "red") %>%   ### Add a vertical line at the impact
	+ geom_line( aes(y = index_val), colour="#377eb8") %>%  ### Draw the time series of SPI12
	+ theme_classic(8) %>%
	+ scale_x_continuous(name = "Year", breaks = seq(1950,2100,5)) %>%
	+ scale_y_continuous(name = "SPEI-3 (July)", breaks = seq(-3,3,0.5)) 

p 

### Save plot for publication
### PDF is vector data, which means it draws lines and text, rather than png raster format (can get blurry when blown up)
### Width and height are in inches
ggsave(file.path(figure_path, "spi12_impact_timeseries.png"), p, width=7.5, height=3, dpi=600)
ggsave(file.path(figure_path, "spi12_impact_timeseries.pdf"), p, width=7.5, height=3)



###########################################################################
## I should probably cut this out, maybe a little too overly complicated, but I like it
###########################################################################
plot_all_df <- spi_impacts

plot_all_impacts <- plot_all_df %>%
	select(year, impact) %>%
	filter(impact == TRUE) %>%
	mutate(left = year - 0.5, right = year + 0.5)

### Create a time series plot
p <- ggplot(plot_all_df, aes(x=year)) %>%
	+ geom_hline(yintercept=0,  color = "grey70") %>%  ### Make a horizontal axis at y= 0
	+ geom_rect(data = plot_all_impacts, aes(xmin=left, xmax=right, ymin=-Inf, ymax=+Inf), fill='pink', alpha=0.9) %>%   ### Create a pink region around the impact
	#+ geom_vline(data = plot_all_impacts, aes(xintercept = year), linetype="dotted", color = "red") %>%   ### Add a vertical line at the impact
	+ geom_line( aes(y = index_val, colour=index_type, group = index_type)) %>%  ### Draw the time series of SPI12
	+ facet_grid(index_months ~ . ) %>%
	+ theme_classic(8) %>%
	+ scale_x_continuous(name = "Year", breaks = seq(1950,2100,5)) %>%
	+ scale_y_continuous(name = "Drought Index (July)", breaks = seq(-3,3,0.5)) %>%
	+ scale_colour_manual(name = "Index Type", values = c("#4daf4a", "#984ea3"))

p 

### Plot result
ggsave(file.path(figure_path, "spi_spei_impact_timeseries.png"), p, width=7.5, height=5, dpi=600)
ggsave(file.path(figure_path, "spi_spei_impact_timeseries.pdf"), p, width=7.5, height=5)

###########################################################################
## Preliminary Checks to see if there is a likely relationship between SPI12 and Impacts
## Plot the distribution of impact years vs non-impact years
###########################################################################
### Create a meaningful column for impacts when plotted
plot_df <-  spi_impacts %>%
	filter(index == "spei_3") %>%
	mutate(impact_label = case_when( impact == TRUE ~ "Impact Reported",
		TRUE ~ "No Impact"
		)
	) %>%
	mutate(impact_label = factor(impact_label, levels=c( "No Impact", "Impact Reported")))

###  Create a histogram showing the count in each impact category
p <- ggplot(plot_df, aes(x=index_val, fill = impact_label)) %>%   ### Plot with SPI on x axis and fill using impact labels
	+ geom_histogram(binwidth = 0.5) %>%
	+ geom_vline(xintercept=0,  color = "grey30", linetype="longdash") %>%  ### Make a vartical axis at x= 0
	+ theme_classic(8) %>%
	+ scale_x_continuous(name = "SPEI-3 (July)", breaks = seq(-5,5,0.5)) %>%
	+ scale_y_continuous(name = "Count", expand = c(0,0)) %>%
	+ scale_fill_brewer(type = "qual", palette = "Set2") %>%
	+ theme(legend.position = c(0.9, 0.9), legend.title = element_blank())

p
### Save plot
ggsave(file.path(figure_path, "spei3_impact_histogram.png"), p, width=5, height=3, dpi=600)
ggsave(file.path(figure_path, "spi3_impact_histogram.pdf"), p, width=5, height=3)


###  Create a smoothed plot showing proportion in each category
p <- ggplot(plot_df, aes(x=index_val, fill = impact_label)) %>%
	+ geom_density(position = "fill") %>%
	+ geom_vline(xintercept=0,  color = "grey30", linetype="longdash") %>%  ### Make a vartical axis at x= 0
	+ theme_classic(8) %>%
	+ scale_x_continuous(name = "SPEI-3 (July)", breaks = seq(-5,5,0.5)) %>%
	+ scale_y_continuous(name = "Proportion of Years in Impact Category", labels = scales::percent_format(accuracy = 5L), expand = c(0,0)) %>%
	+ scale_fill_brewer(type = "qual", palette = "Set2") %>%
	+ theme(legend.position = c(0.9, 0.9), legend.title = element_blank())

p
### Save plot
ggsave(file.path(figure_path, "spei3_impact_filled.png"), p, width=5, height=3, dpi=600)
ggsave(file.path(figure_path, "spei3_impact_filled.pdf"), p, width=5, height=3)


###########################################################################
## Same alternative version
###########################################################################

plot_all_df <- plot_all_df %>%
	mutate(impact_label = case_when( impact == TRUE ~ "Impact Reported",
		TRUE ~ "No Impact"
		)
	) %>%
	mutate(impact_label = factor(impact_label, levels=c( "No Impact", "Impact Reported")))

###  Create a histogram showing the count in each impact category
p <- ggplot(plot_all_df, aes(x=index_val, fill = impact_label)) %>%   ### Plot with SPI on x axis and fill using impact labels
	+ geom_histogram(binwidth = 0.5) %>%
	+ geom_vline(xintercept=0,  color = "grey30", linetype="longdash") %>%  ### Make a vartical axis at x= 0
	+ theme_classic(8) %>%
	+ scale_x_continuous(name = "Drought Index in July", breaks = seq(-5,5,0.5)) %>%
	+ scale_y_continuous(name = "Count", expand = c(0,0)) %>%
	+ scale_fill_brewer(type = "qual", palette = "Set2") %>%
	+ theme(legend.position = c(0.9, 0.9), legend.title = element_blank()) %>%
	+ facet_wrap(~ index)

p

### Save plot
ggsave(file.path(figure_path, "spi_spei_impact_histogram.png"), p, width=6, height=5, dpi=600)
ggsave(file.path(figure_path, "spi_spei_impact_histogram.pdf"), p, width=6, height=5)



###  Create a smoothed plot showing proportion in each category
p <- ggplot(plot_all_df, aes(x=index_val, fill = impact_label)) %>%
	+ geom_density(position = "fill") %>%
	+ geom_vline(xintercept=0,  color = "grey30", linetype="longdash") %>%  ### Make a vartical axis at x= 0
	+ theme_classic(8) %>%
	+ scale_x_continuous(name = "Drought Index in July", breaks = seq(-5,5,0.5)) %>%
	+ scale_y_continuous(name = "Proportion of Years in Impact Category", labels = scales::percent_format(accuracy = 5L), expand = c(0,0)) %>%
	+ scale_fill_brewer(type = "qual", palette = "Set2") %>%
	+ theme(legend.position = c(0.9, 0.9), legend.title = element_blank()) %>%
	+ facet_wrap(~ index)


p

### Save plot
ggsave(file.path(figure_path, "spi_spei_impact_filled.png"), p, width=6, height=5, dpi=600)
ggsave(file.path(figure_path, "spi_spei_impact_filled.pdf"), p, width=6, height=5)

###########################################################################
## Preliminary Checks to see if there is a likely relationship between SPI12 and Impacts
## Summary statistics for the distribution of SPI vs impacts
###########################################################################
### Calculate summary statistics for impact vs non-impact
summary_by_impact <- spi_impacts %>% 
	#select(-year, -nuts) %>% 	### Remove these columns
	group_by(index, impact) %>%	### Group by drought index and impact
	summarise(mean = mean(index_val, na.rm=TRUE), median = median(index_val, na.rm=TRUE), sd = sd(index_val, na.rm=TRUE), count = n()) %>%  ### Calculate summary statistics
	as.data.frame()

### Show result
summary_by_impact

### Calculate the same summary statistics but ignore impact grouping
summary_all <- spi_impacts %>% 
	group_by(index) %>%	### Now only group by drought index
	summarise(mean = mean(index_val, na.rm=TRUE), median = median(index_val, na.rm=TRUE), sd = sd(index_val, na.rm=TRUE), count = n()) %>%
	mutate(impact = "All") %>%  ### Need to add a column so it lines up
	select(index, impact, mean, median, sd, count) %>%
	as.data.frame()

summary_all

summary_by_impact <- rbind(summary_by_impact, summary_all) %>%
	mutate(index = factor(index, levels = c("spi_3", "spei_3", "spi_12", "spei_12"))) %>%
	arrange(index, impact )

summary_by_impact

write.csv(summary_by_impact, file.path(write_output_base_path, "impact_vs_noimpact_distribution.csv"))


###########################################################################
## Fit logistic regression for SPI12
###########################################################################
### Create data for fitting, leave only SPI12
fit_data <- spi_impacts %>%
	select(date, year, month, index, index_val, impact) %>%
	spread(index, index_val)

### Check data
fit_data

### Fit simple logistic regression using SPEI-3 as a predictor variable
### Force an intercept to be included by using + 1
spei_3_fit <- glm(impact ~ spei_3 + 1, data = fit_data, family = "binomial")
spei_3_fit_summary <- summary(spei_3_fit)

### Check fit summary
spei_3_fit_summary
confint(spei_3_fit)

### Save fit summary to file
sink(file.path(write_output_base_path, "spei_3_fit.txt"))
spei_3_fit_summary
sink()


### Fit simple logistic regression using SPI-12 as a predictor variable
### Force an intercept to be included by using + 1
spi_12_fit <- glm(impact ~ spi_12 + 1, data = fit_data, family = "binomial")
spi_12_fit_summary <- summary(spi_12_fit)

spi_12_fit_summary


### Save fit summary to file
sink(file.path(write_output_base_path, "spi_12_fit.txt"))
spi_12_fit_summary
sink()


### Notice that AIC is much lower for SPEI3 than SPI12, suggesting a better model
### Coefficient on SPEI3 is statistically significant (p ~ 0.5% << 5% (traditional, but arbitrary criterion)
### Coefficient on SPI12 is not, further proof that July SPI 12 is not a very good predictor of impacts

###########################################################################
## Plot logistic regression fit
###########################################################################
### Plot the relationship with SPEI-3
logistbarplot(log.fit = spei_3_fit, log.var = "spei_3")

### Save this plot to a PNG for later use
png(file.path(figure_path, "spei3_logist_barplot.png"),width=4.5, height=4.5, units = "in", res=600)
	logistbarplot(log.fit = spei_3_fit, log.var = "spei_3")
dev.off()

### Plot the relationship with SPI 12
logistbarplot(log.fit = spi_12_fit, log.var = "spi_12")

### Save this plot to a PNG for later use
png(file.path(figure_path, "spi12_logist_barplot.png"),width=4.5, height=4.5, units = "in", res=600)
	logistbarplot(log.fit = spi_12_fit, log.var = "spi_12")
dev.off()

### Plotting using a slightly different format
p <- logisthistplot(log.fit = spei_3_fit, log.var = "spei_3")

p
### Save plot
ggsave(file.path(figure_path, "spei3_logist_hist.png"), p, width=4.5, height=4.5, dpi=600)
#ggsave(file.path(figure_path, "spei3_logist_hist.pdf"), p, width=4.5, height=4.5)

p <- logisthistplot(log.fit = spi_12_fit, log.var = "spi_12")

p
### Save plot
ggsave(file.path(figure_path, "spei3_logist_hist.png"), p, width=4.5, height=4.5, dpi=600)
#ggsave(file.path(figure_path, "spei3_logist_hist.pdf"), p, width=4.5, height=4.5)

###########################################################################
## Plot some logistic regression theory, showing how we create the previous plots
###########################################################################
### The previous plots hold a lot of information, but have to do a lot of work in the background
### For a clearer view, here is how we create those figures

### Create a holder dataframe 
plot_df <- data.frame(spei_3 = seq(-3,3,0.1))

### Make a prediction using this new data
logodds_predict <- predict(spei_3_fit, plot_df,  se.fit = TRUE)

### Add in prediction and confidence intervals
plot_df <- plot_df %>%
	mutate(logodds = logodds_predict$fit) %>%
	mutate(logodds_upper = logodds + (qnorm(0.025) * logodds_predict$se.fit)) %>%  ### Two-tailed 95% confidence interval, or 0.025 in each tail
	mutate(logodds_lower = logodds - (qnorm(0.025) * logodds_predict$se.fit))

plot_obs <- fit_data %>%
	mutate(pred = predict(spei_3_fit)) %>%
	mutate(residuals = residuals(spei_3_fit)) %>%
	mutate(obs = pred + residuals)

### Plot Log-Odds
p <- ggplot(plot_df, aes(x=spei_3)) %>%
	+ geom_vline(xintercept=0,  color = "grey30", linetype="longdash") %>%  ### Make a vartical axis at x= 0
	+ geom_ribbon(aes(ymin = logodds_lower, ymax = logodds_upper), alpha = 0.2, fill = "grey20") %>%
	+ geom_line(aes(y=logodds), colour="black") %>%
	+ geom_point(data = plot_obs, aes(y = obs, colour=impact, shape = impact)) %>%
	+ scale_y_continuous(name = "Log-Odds") %>%
	+ scale_x_continuous(name = "SPEI-3 (July)", breaks = seq(-5,5,0.5))  %>%
	+ scale_colour_brewer(name = "Impact", type = "qual", palette = "Set2") %>%
	+ scale_shape_discrete(name = "Impact") %>%
	+ theme_classic(8) %>%
	+ theme(legend.position = c(0.9, 0.9)) 

p
### Save plot
ggsave(file.path(figure_path, "spei3_logodds.png"), p, width=5, height=3, dpi=600)
ggsave(file.path(figure_path, "spei3_logodds.pdf"), p, width=5, height=3)


### Log odds is based on the natural log, so can convert to odds ratio using exponential
plot_df <- plot_df %>%
	mutate(odds = exp(logodds)) %>%
	mutate(odds_upper = exp(logodds_upper)) %>% 
	mutate(odds_lower = exp(logodds_lower))

### Plot Odds ratio (e.g. 2 means impact is 2 x more like than non-impact 2:1)
p <- ggplot(plot_df, aes(x=spei_3)) %>%
	+ geom_vline(xintercept=0,  color = "grey30", linetype="longdash") %>%  ### Make a vartical axis at x= 0
	+ geom_ribbon(aes(ymin = odds_lower, ymax = odds_upper), alpha = 0.2, fill = "grey20") %>%
	+ geom_line(aes(y=odds), colour="black") %>%
	+ geom_point(data = plot_obs, aes(y = exp(obs), colour=impact, shape = impact)) %>%
	+ scale_y_continuous(name = "Odds Ratio") %>%
	+ scale_x_continuous(name = "SPEI-3 (July)", breaks = seq(-5,5,0.5))  %>%
	+ scale_colour_brewer(name = "Impact", type = "qual", palette = "Set2") %>%
	+ scale_shape_discrete(name = "Impact") %>%
	+ theme_classic(8) %>%
	+ theme(legend.position = c(0.9, 0.9))  %>%
	+ coord_cartesian(ylim=c(0,6))

p
### Save plot
ggsave(file.path(figure_path, "spei3_oddsratio.png"), p, width=5, height=3, dpi=600)
ggsave(file.path(figure_path, "spei3_oddsratio.pdf"), p, width=5, height=3)


### Now to convert from Odds ratio to likelihood (probability, we use the logit transform)
### Divide odds by one plus odds
plot_df <- plot_df %>%
	mutate(prob = odds / (1 + odds) ) %>%
	mutate(prob_upper = odds_upper / (1 + odds_upper)) %>% 
	mutate(prob_lower = odds_lower / (1 + odds_lower))

### Plot Probability
ggplot(plot_df, aes(x=spei_3)) %>%
	+ geom_vline(xintercept=0,  color = "grey30", linetype="longdash") %>%  ### Make a vartical axis at x= 0
	+ geom_ribbon(aes(ymin = prob_lower, ymax = prob_upper), alpha = 0.2, fill = "grey20") %>%
	+ geom_line(aes(y=prob), colour="black") %>%
	+ geom_point(data = plot_obs, aes(y = (exp(obs)) /  (1 + exp(obs)), colour=impact, shape = impact)) %>%
	+ scale_y_continuous(name = "Impact Likelihood", labels = scales::percent_format(accuracy = 5L), expand=c(0,0))  %>%
	+ scale_x_continuous(name = "SPEI-3 (July)", breaks = seq(-5,5,0.5))  %>%
	+ scale_colour_brewer(name = "Impact", type = "qual", palette = "Set2") %>%
	+ scale_shape_discrete(name = "Impact") %>%
	+ theme_classic(8) %>%
	+ theme(legend.position = c(0.9, 0.9))  %>%
	+ coord_cartesian(ylim=c(0,1))

p
### Save plot
ggsave(file.path(figure_path, "spei3_likelihood.png"), p, width=5, height=3, dpi=600)
ggsave(file.path(figure_path, "spei3_likelihood.pdf"), p, width=5, height=3)

###########################################################################
## Test multiple predictor variables
###########################################################################
### See the value of adding a second predictor, maybe SPI12 has value when combined with SPEI3
two_pred_fit <- glm(impact ~ spei_3 + spi_12 + 1, data = fit_data, family = "binomial")
two_pred_fit_summary <- summary(two_pred_fit)

two_pred_fit_summary

### Tougher to plot 2 variables. Need to make a counterfactual plot, holding each stationary.
logisthistplot(log.fit = two_pred_fit, log.var = "spei_3")
logisthistplot(log.fit = two_pred_fit, log.var = "spi_12")

###########################################################################
## Simple goodness of fit tests for the best model
###########################################################################
anova(update(spei_3_fit, ~1), spei_3_fit, two_pred_fit, test="Chisq")

### Comparison with the Null model (no predictor) shows that SPEI-3 improves the model fit significantly
### Adding SPI12 does not significantly improve the model

### You can also see this by checking the AIC values, which actually increase (get worse) with addition of SPI-12
spei_3_fit_summary
two_pred_fit_summary

###########################################################################
## Use relationship to predict likelihood of impacts and plot time series
###########################################################################
### Use the SPEI3 regression to predict the likelihood of an impact
### Keep the standard error for confidence intervals around prediction
predict_ts <- predict(spei_3_fit, newdata = fit_data, type = "link", se = TRUE)

### Add in prediction and confidence intervals
predict_df <- fit_data %>%
	mutate(predict = plogis(predict_ts$fit)) %>%
	mutate(predict_upper = plogis(predict_ts$fit + (qnorm(0.025) * predict_ts$se.fit))) %>% 
	mutate(predict_lower = plogis(predict_ts$fit - (qnorm(0.025) * predict_ts$se.fit))) %>%
	select(-spi_3, -spei_12, -spi_12)

head(predict_df)

### Create a time series plot
p <- ggplot(predict_df, aes(x=year)) %>%
	+ geom_hline(yintercept=0.5,  color = "grey70", linetype="dotted") %>%  ### Make a horizontal axis at y= 0
	+ geom_rect(data = plot_impacts, aes(xmin=left, xmax=right, ymin=-Inf, ymax=+Inf), fill='pink', alpha=0.9) %>%   ### Create a pink region around the impact
#	+ geom_vline(data = plot_impacts, aes(xintercept = year), linetype="dotted", color = "red") %>%   ### Add a vertical line at the impact
	+ geom_ribbon(aes(ymin = predict_lower, ymax = predict_upper), alpha = 0.2, fill = "grey20") %>%
	+ geom_line( aes(y = predict), colour="#377eb8") %>%  ### Draw the time series of SPI12
	+ theme_classic(8) %>%
	+ scale_x_continuous(name = "Year", breaks = seq(1950,2100,5)) %>%
	+ scale_y_continuous(name = "Impact Likelihood", breaks = seq(0,1,0.1), labels = scales::percent_format(accuracy = 5L), expand=c(0,0))

p
### Save plot
ggsave(file.path(figure_path, "impact_prediction_ts.png"), p, width=7.5, height=3, dpi=600)
ggsave(file.path(figure_path, "impact_prediction_ts.pdf"), p, width=7.5, height=3)




###########################################################################
## Other Checks
###########################################################################
### Warn about multicollinearity if including multiple indices
ggplot(fit_data, aes(x=spi_3, y=spei_3)) + geom_point() + theme_classic(8)








