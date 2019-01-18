# *------------------------------------------------------------------
# | PROGRAM NAME: 8_6_nonparametric_trend_analysis
# | FILE NAME: 8_6_nonparametric_trend_analysis.R
# | DATE: 
# | CREATED BY:  Jim Stagge         
# *----------------------------------------------------------------
# | PURPOSE:  This code runs a non-parametric trend test (Mann-Kendall and Sen Slope) 
# |
# |		
# *------------------------------------------------------------------

###########################################################################
## Set the Paths
###########################################################################
### Path for Data and Output	
data_path <- "../data"
output_path <- "./output/example_8_6"

### Set output location
write_output_base_path <- output_path
dir.create(write_output_base_path, recursive=TRUE)

write_figure_path <- file.path(write_output_base_path, "figures")
dir.create(write_figure_path, recursive=TRUE)

###########################################################################
###  Load functions
###########################################################################
### Load these functions for all code
#require(colorout)
require(tidyverse)

### Load these functions for this unique project
require(lubridate)
require(zyp)

###########################################################################
## Read in Data
###########################################################################
### Change the path to read to time series
flow_path <- file.path(data_path, "intl_dataset")

### Read in time series
goeta_daily <- read_csv(file.path(flow_path, "goeta-runoff-daily.csv"), col_names = TRUE)

### Initial plot to verify data
head(goeta_daily)
plot(goeta_daily$date, goeta_daily$flow_m3s, type="l")

###########################################################################
## Cut time series to after 1945 to avoid breakpoint
###########################################################################
### Create a new column for year and month
goeta_daily <- goeta_daily %>%
	mutate(year = year(date)) %>%
	mutate(month = month(date))

### Cut to after 1945
goeta_daily <- goeta_daily %>%
	filter(year > 1945)

###########################################################################
## Reorganize data into annual summary
###########################################################################
### Create a new dataframe, summarizing each year
goetta_annual_ts <- goeta_daily %>%
	group_by(year) %>%
	summarise(min_m3s = min(flow_m3s, na.rm=TRUE), mean_m3s=mean(flow_m3s, na.rm=TRUE))

### Preview the results
head(goetta_annual_ts)

### Save this table
write.csv(goetta_annual_ts, file.path(write_output_base_path, "goetta_annual_ts.csv"))

###########################################################################
## Reorganize data into monthly summary
###########################################################################
### Create a new dataframe, summarizing each month and year
goetta_monthly_ts <- goeta_daily %>%
	group_by(year, month) %>%
	summarise(date=min(date), min_m3s = min(flow_m3s, na.rm=TRUE), mean_m3s=mean(flow_m3s, na.rm=TRUE))

### Preview the results
head(goetta_monthly_ts)

### Save this table
write.csv(goetta_monthly_ts, file.path(write_output_base_path, "goetta_monthly_ts.csv"))

###########################################################################
## Plot Daily Flow Time Series
###########################################################################
### Create output folder
write_output_path <- file.path(write_figure_path, "goeta_ts")
dir.create(write_output_path)

### Plot daily flows
p <- ggplot(goeta_daily, aes(x=date, y=flow_m3s)) %>%
	+ geom_line(size=0.3) %>%  ## Plot as a line. Default size is normally fine, but for daily data, this is too wide
	+ theme_classic() %>%  ## Use the classic theme (vertical and horizontal axes)
	+ scale_x_date(name="Date", date_labels = "%Y", breaks=seq(as.Date("1800-01-01"), as.Date("2020-01-01"), by="10 years")) %>%
	+ scale_y_continuous(name=bquote('Daily Mean Flow'~(m^3/s)), labels = scales::comma) ## Set the y-axis title and add commas to values

### Display figure (stored in variable p)
p
### Save figure
ggsave(file.path(write_output_path, "goeta_daily_ts.png"), p, width=7, height=4, dpi=600)

### Plot flows on Log 10 scale
p <- p %>%
	+ scale_y_log10(name=bquote('Daily Mean Flow'~(m^3/s)), breaks=c(seq(20,100,20), seq(200,1000,200), seq(2000,10000,2000), seq(20000,100000,20000)), labels = scales::comma) %>%   ### Plot on a log 10 scale
	+ theme(panel.grid.major.y = element_line(colour = "grey80", size=0.1))   ### Add y gridlines

### Save figure
ggsave(file.path(write_output_path, "goeta_daily_ts_log10.png"), p, width=7, height=4, dpi=600)


###########################################################################
## Plot Monthly Flow Time Series
###########################################################################
### Plot Monthly flows
p <- ggplot(goetta_monthly_ts, aes(x=date, y=mean_m3s)) %>%
	+ geom_line(size=0.3) %>%  ## Plot as a line. Default size is normally fine, but for daily data, this is too wide
	+ theme_classic() %>%  ## Use the classic theme (vertical and horizontal axes)
	+ scale_x_date(name="Date", date_labels = "%Y", breaks=seq(as.Date("1800-01-01"), as.Date("2020-01-01"), by="10 years")) %>%
	+ scale_y_continuous(name=bquote('Monthly Mean Flow'~(m^3/s)), labels = scales::comma) ## Set the y-axis title and add commas to values

### Display figure (stored in variable p)
p
### Save figure
ggsave(file.path(write_output_path, "goeta_monthly_ts.png"), p, width=7, height=4, dpi=600)

### Plot flows on Log 10 scale
p <- p %>%
	+ scale_y_log10(name=bquote('Monthly Mean Flow'~(m^3/s)), breaks=c(seq(20,100,20), seq(200,1000,200), seq(2000,10000,2000), seq(20000,100000,20000)), labels = scales::comma) %>%   ### Plot on a log 10 scale
	+ theme(panel.grid.major.y = element_line(colour = "grey80", size=0.1))   ### Add y gridlines

### Save figure
ggsave(file.path(write_output_path, "goeta_monthly_ts_log10.png"), p, width=7, height=4, dpi=600)

###########################################################################
## Plot Annual Min Flow Time Series
###########################################################################
### Plot annual min flows
p <- ggplot(goetta_annual_ts, aes(x=year, y=min_m3s)) %>%
	+ geom_point(colour="grey50") %>%  ## Plot as points. 
	+ theme_classic() %>%  ## Use the classic theme (vertical and horizontal axes)
	+ scale_x_continuous(name="Year", breaks=seq(0,3000,10)) %>%   ## Set the x-axis title and style to be years with breaks every 10 years
	+ scale_y_continuous(name=bquote('Annual Min Flow'~(m^3/s)), labels = scales::comma) ## Set the y-axis title and add commas to values

### Display figure (stored in variable p)
p
### Save figure
ggsave(file.path(write_output_path, "goeta_annual_min_ts.png"), p, width=7, height=4, dpi=600)

###########################################################################
## Perform OLS trend analysis on Annual Min Flows
###########################################################################
### Create output folder
write_output_path <- file.path(write_figure_path, "trend_analysis")
dir.create(write_output_path)

### Create an OLS linear trend model and save to variable
trend_ols <- lm(goetta_annual_ts$min_m3s~goetta_annual_ts$year)

### Show results
summary(trend_ols)

### The trend (slope of the fitted line) appears to be significantly different from 0 (p << 0.05).
### This appears to be a significant trend. Annual mean flow increasing 7.14 cfs per year. 

### Calculate the residuals and add to dataframe
goetta_annual_ts$resid <- resid(trend_ols)

### Plot the fit
p <- ggplot(goetta_annual_ts, aes(x=year, y=min_m3s)) %>%
	+ geom_point(colour="grey50") %>%  ## Plot as points. 
	+ stat_summary(fun.data=mean_cl_normal) %>%
	+ geom_smooth(method='lm') %>%  ## Add the linear model and confidence interval
	+ theme_classic() %>%  ## Use the classic theme (vertical and horizontal axes)
	+ scale_x_continuous(name="Year", breaks=seq(0,3000,10)) %>%   ## Set the x-axis title and style to be years with breaks every 10 years
	+ scale_y_continuous(name=bquote('Annual Min Flow'~(m^3/s)), labels = scales::comma) ## Set the y-axis title and add commas to values
p
### Save figure
ggsave(file.path(write_output_path, "goeta_annual_min_trend_ols.png"), p, width=6, height=4, dpi=600)

### Plot the residuals
p <- ggplot(goetta_annual_ts, aes(x=year, y=resid)) %>%
	+ geom_point(colour="grey50") %>%  ## Plot as points. 
	+ stat_summary(fun.data=mean_cl_normal) %>%
	+ geom_smooth(method='lm') %>%  ## Add the linear model and confidence interval
	+ theme_classic() %>%  ## Use the classic theme (vertical and horizontal axes)
	+ scale_x_continuous(name="Year", breaks=seq(0,3000,10)) %>%   ## Set the x-axis title and style to be years with breaks every 10 years
	+ scale_y_continuous(name=bquote('OLS Trend Residual'~(m^3/s)), labels = scales::comma) ## Set the y-axis title and add commas to values
p
### Save figure
ggsave(file.path(write_output_path, "goeta_annual_min_trend_ols_resid.png"), p, width=6, height=4, dpi=600)

max_abs_resid <- abs(max(goetta_annual_ts$resid))

### Plot the distribution of residuals
#p <- ggplot(goetta_annual_ts, aes(x=resid)) %>%
#	+ geom_density(fill="grey70") %>%
#	+ theme_classic() %>%
#	+ scale_x_continuous(name="OLS Residuals (cfs)", labels = scales::comma, limits=c(-1.2*max_abs_resid,1.2*max_abs_resid)) %>%   ## Set the x-axis title and style to be dates. Add Expand command to make sure 0 is on axis
#	+ scale_y_continuous(name="Density", expand = c(0, 0)) ### Add Expand command to make sure 0 is on axis
	
### Display figure (stored in variable p)
#p
### Save figure
#ggsave(file.path(write_output_path, "goeta_annual_min_trend_ols_resid_dist.png"), p, width=5, height=3.5, dpi=600)

  
###########################################################################
## Perform Mann-Kendall / Sen Slope
###########################################################################
### A word of caution, the Mann-Kendall and Sen slope functions in the trend package requires a regular series, with no ties or gaps. 
### However, the Mann-Kendall and Sen slope can handle both of these cases. 
### Our data here does not have this issue, but I think it's best to get comfortable with the most flexible tool
### So, I recommend the Kendall function from the Kendall package and the zyp.sen function from the zyp package.

### Run the Mann-Kendall Test for the min annual flow
mk_test_results <- Kendall(x=goetta_annual_ts$year, y=goetta_annual_ts$min_m3s)
summary(mk_test_results)
### p = 2.22e-16 << 0.05  Reject the null, accept the alternative hypothesis that there is a significant trend

### Run the Theil-Sen slope for min annual flow
trend_sen <- zyp.sen(min_m3s ~ year, data = goetta_annual_ts)
trend_sen
confint.zyp(trend_sen)
### Sen slope = 1.318 increase in m3/s per year, less than OLS (1.441), but still significant

###########################################################################
## Plot the results to compare Non-parametric with parametric
###########################################################################
### Create a dataframe to hold the trend lines
trend_lines <- goetta_annual_ts %>% 
	select(year) %>%  ### Only select the year
	mutate(   ### Create two new columns with predictions
		ols = trend_ols$coef[1] + trend_ols$coef[2] * year,
		sen = trend_sen$coef[1] + trend_sen$coef[2] * year
	) %>% 
	gather(model, min_m3s, -year)   ### Gather these predictions into a long format for plotting

### Re-factor the model names to make it better to plot
trend_lines$model <- factor(trend_lines$model, c("ols", "sen"), labels=c("Parametric Trend\nOrdinary Least Squares (OLS)\n\n", "Non-Parametric Trend\nThiel-Sen Slope"))

### Plot the fit
p <- ggplot(goetta_annual_ts, aes(x=year, y=min_m3s)) %>%
	+ geom_point(colour="grey50") %>%  ## Plot as points. 
	+ geom_line(data = trend_lines, aes(colour=model, linetype=model)) %>%  ### Plot line fits
	+ theme_classic() %>%  ## Use the classic theme (vertical and horizontal axes)
	+ scale_colour_brewer(name="Model", type="qual", palette="Set2")  %>%  ### Set colors, use color-deficient friendly palette
	+ scale_linetype_manual(name="Model", values=c("longdash","solid"))  %>%   ### Set linetypes
	+ scale_x_continuous(name="Year", breaks=seq(0,3000,10)) %>%   ## Set the x-axis title and style to be years with breaks every 10 years
	+ scale_y_continuous(name="Annual Mean Flow (cfs)", labels = scales::comma) %>% ## Set the y-axis title and add commas to values
	+ theme(legend.position = c(0.25, 0.9))   ## Move legend to inside figure, upper left
p

### Save figure
ggsave(file.path(write_output_path, "goeta_annual_min_trend_ols_vs_sen.png"), p, width=6, height=4, dpi=600)


###########################################################################
###  Summarize coefficients
###########################################################################
### Create a matrix to hold coefficients
goeta_trend_models <- data.frame(model=rep(c("OLS","Sen"), each=2), coeff=rep(c("Int","Slope_annual"),2), estimate = NA, 'low_2.5_perc'=NA,  'high_97.5_perc'=NA)

### Insert Estimates
goeta_trend_models$estimate <- c(coef(trend_ols), coef(trend_sen))

### Insert 95% confidence interval
goeta_trend_models$low_2.5_perc <- c(confint(trend_ols)[,2], confint(trend_sen)[,2])
goeta_trend_models$high_97.5_perc <- c(confint(trend_ols)[,1], confint(trend_sen)[,1])

### Save this table
write.csv(goeta_trend_models, file.path(write_output_base_path, "goeta_trend_coefficients.csv"))

###########################################################################
###  Output Results
###########################################################################
### Dump full output to a text file
sink(file.path(write_output_base_path, "goeta_annual_min_trend_output.txt"))
	### Print the OLS trend results	
	print("Ordinary Least Squares Trend (Parametric)")
	summary(trend_ols)
	confint(trend_ols)

	### Print the non-parametric trend results
	print("Mann-Kendall Test and Thiel-Sen Slope (Nonparametric)")
	summary(mk_test_results)
	trend_sen
	confint.zyp(trend_sen)
sink()

###########################################################################
###  Print a Completion message
###########################################################################
print('===========================================================')
print('Succesfully Executed. Next execute the ?????.R file') 
print('===========================================================')

