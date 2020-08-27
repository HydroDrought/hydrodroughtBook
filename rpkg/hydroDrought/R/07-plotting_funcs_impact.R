
###########################################################################
## Theme for logistic regression
###########################################################################

theme_pub_logreg <- function(base_size = 10) {
  theme(
    axis.text.x =        element_text(size = base_size * 0.8, lineheight = 0.9, vjust = 1,colour = "black", margin(0.15, 0.15, 0.15, 0.15,"cm")),
    axis.text.y =        element_text(size = base_size * 0.8, lineheight = 0.9, hjust = 1,colour = "black", margin(0.15, 0.15, 0.15, 0.15,"cm")),
    axis.ticks =         element_line(colour = "black", size = 0.2),
    axis.ticks.length =  unit(0.15, "cm"),

	axis.title.x =       element_text(size = base_size),
    axis.title.y =       element_text(size = base_size),

	strip.text.x =       element_text(size = base_size * 0.8),
    strip.text.y =       element_text(size = base_size * 0.8, angle = -90),

	panel.grid.minor  = element_blank(),

	plot.title =         element_text(size = base_size * 1.2, face="bold")
  )
}

###########################################################################
## Link function
###########################################################################
logit2prop <- function(l){exp(l)/(1+exp(l))}


###########################################################################
## Vertically align a list of plots.
###########################################################################
### From here:  https://stackoverflow.com/questions/41569817/align-multiple-plots-in-ggplot2-when-some-have-legends-and-others-dont

VAlignPlots <- function(...,
                       globalTitle = "",
                       keepTitles = FALSE,
                       keepXAxisLegends = FALSE,
                       nb.columns = 1) {
  # Retrieve the list of plots to align
  plots.list <- list(...)

  # Remove the individual graph titles if requested
  if (!keepTitles) {
    plots.list <- lapply(plots.list, function(x) x <- x + ggtitle(""))
    plots.list[[1]] <- plots.list[[1]] + ggtitle(globalTitle)
  }

  # Remove the x axis labels on all graphs, except the last one, if requested
  if (!keepXAxisLegends) {
    plots.list[1:(length(plots.list)-1)] <-
      lapply(plots.list[1:(length(plots.list)-1)],
             function(x) x <- x + theme(axis.title.x = element_blank()))
  }

  # Builds the grobs list
  grobs.list <- lapply(plots.list, ggplotGrob)

  # Get the max width
  widths.list <- do.call(grid::unit.pmax, lapply(grobs.list, "[[", 'widths'))

  # Assign the max width to all grobs
  grobs.list <- lapply(grobs.list, function(x) {
    x[['widths']] = widths.list
    x})

  # Create the gtable and display it
  g <- grid.arrange(grobs = grobs.list, ncol = nb.columns)
  # An alternative is to use arrangeGrob that will create the table without
  # displaying it
  #g <- do.call(arrangeGrob, c(grobs.list, ncol = nb.columns))

  return(g)
}

###########################################################################
## Logistic plot with bar along the top and botton
###########################################################################
#' @export
logistbarplot <- function(log.fit,log.var) {
	### Load required packages
	#require(ggplot2)
	#require(grid)
	#require(gridExtra)

	### Extract data from model file, hits are in the first column
	data <- log.fit$model
	HIT <- log.fit$model[,1]

	var.names <- names(log.fit$coefficients)
	var.names <- var.names[var.names!="(Intercept)"]

	df.names <- colnames(data)
	var.id <- df.names %in% log.var
	data[,var.id==FALSE] <- 0

	# get bin numbers
	breaks <- "Sturges"
	hist_both <- hist(data[,var.id],plot=FALSE,breaks=breaks)
	hist0 <- hist(data[HIT==0,var.id],breaks=hist_both$breaks,plot=FALSE)
	hist1 <- hist(data[HIT==1,var.id],breaks=hist_both$breaks,plot=FALSE)
	max_count=max(c(hist0$counts,hist1$counts))

	# Find Min/Max
	min.x <- min(data[,var.id], na.rm=TRUE)
	max.x <- max(data[,var.id], na.rm=TRUE)

	bar.width <- mean(diff(hist_both$breaks))
	min.x.lim <- min(hist_both$breaks)-bar.width
	max.x.lim <- max(hist_both$breaks)+bar.width

	# Create evenly spaced data
	newdata2 <- matrix(0,100,dim(data)[2])
	newdata2[,var.id] <- seq(from = min.x.lim, to = max.x.lim, length.out = 100)
	colnames(newdata2) <- colnames(data)
	newdata2 <- data.frame(newdata2)

	newdata3 <- data.frame(X=newdata2[,var.id])
	newdata3 <- cbind(newdata3, predict(log.fit, newdata = newdata2, type = "link", se = TRUE))
	newdata3 <- within(newdata3, {
	    PredictedProb <- plogis(fit)
	    LL <- plogis(fit - (1.96 * se.fit))
	    UL <- plogis(fit + (1.96 * se.fit))
	})

	data.1subset <- data.frame(X=data[HIT==1,var.id], HIT=1)
	data.0subset <- data.frame(X=data[HIT==0,var.id], HIT=0)

	#xlim <- c(min(region1$SPI_6, na.rm=TRUE) - 0.1 * diff(range(region1$SPI_6, na.rm=TRUE)),
	#      max(region1$SPI_6, na.rm=TRUE) + 0.1 * diff(range(region1$SPI_6, na.rm=TRUE)))
	xlim <- c(min.x.lim,max.x.lim)

	### Create the logistic regression plot (main frame)
	p1 <- ggplot(newdata3, aes(X, PredictedProb))
	p1 <- p1 + geom_ribbon(aes(ymin = LL, ymax = UL), alpha = 0.2, fill = "grey20")
	p1 <- p1 + geom_vline(xintercept = 0)
	p1 <- p1 + geom_line(aes(x=X,y=PredictedProb), colour="red")
	if (length(var.names)==1){
		p1 <- p1 + ylab("Likelihood")
		}
	if (length(var.names)>1){
		p1 <- p1 + ylab("Partial Likelihood")
	}
	p1 <- p1 + geom_hline(yintercept=c(0,1))
	p1 <- p1 + coord_cartesian(xlim = xlim, ylim = c(0, 1))
	p1 <- p1 + theme_bw()
	p1 <- p1 + theme_pub_logreg()
	p1 <- p1 + scale_y_continuous(labels = scales::percent_format(accuracy = 5L), expand=c(0,0))
	p1 <- p1 + scale_x_continuous(expand=c(0,0))
	p1 <- p1 + theme(axis.text.x = element_blank(),
   		axis.title.x = element_blank(),
   		axis.ticks.x = element_blank(),
   		plot.margin = unit(c(-0.3, 0.2, -0.3, 0.5), "lines"),
		legend.position="none",
		plot.background = element_rect(fill = "transparent",colour = NA))

	# Horizontal marginal boxplot - to appear at the top of the chart
	p2 <- ggplot(data.1subset, aes(x = factor("Impact"), y = X))
	p2 <- p2 + geom_jitter(position = position_jitter(width = 0.05), colour="grey70" )
	p2 <- p2 + geom_hline(yintercept = 0)
	p2 <- p2 + geom_boxplot(outlier.colour = "black", alpha=0.5)
	p2 <- p2 + scale_y_continuous(limits = xlim, expand = c(0,0))
	p2 <- p2 + coord_flip()
	p2 <- p2 + theme_bw()
	p2 <- p2 + theme_pub_logreg()
	p2 <- p2 + ylab(log.var)
	p2 <- p2 + theme(
	   plot.margin = unit(c(0.2, 0.2, -0.3, 0.5), "lines"))
	p2 <- p2 + theme(axis.title.x=element_blank(),
		axis.title.y=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
		legend.position="none",
		plot.background = element_rect(fill = "transparent",colour = NA))

	# Horizontal marginal boxplot - to appear at the bottom of the chart
	p3 <- ggplot(data.0subset, aes(x = factor("No Impact"), y = X))
	p3 <- p3 + geom_jitter(position = position_jitter(width = 0.05), colour="grey70" )
	p3 <- p3 + geom_hline(yintercept = 0)
	p3 <- p3 + geom_boxplot(outlier.colour = "black", alpha=0.5)
	p3 <- p3 + scale_y_continuous(limits = xlim, expand = c(0,0))
	p3 <- p3 + coord_flip()
	p3 <- p3 + theme_bw()
	p3 <- p3 + theme_pub_logreg()
	p3 <- p3 + ylab(log.var)
	p3 <- p3 + theme(
	   plot.margin = unit(c(-0.25, 0.2, -0.8, 0.5), "lines"))
	p3 <- p3 + theme(axis.title.y=element_blank(),
		legend.position="none",
		plot.background = element_rect(fill = "transparent",colour = NA))


yup <- VAlignPlots(p2, p1, p3, keepXAxisLegends = FALSE)

heights=c(1, 4,1.1)
yup$heights <- unit(heights, units = "null")

grid.draw(grobTree(rectGrob(gp=gpar(fill="white", lwd=0)), yup))

#plot(yup)
}




###########################################################################
## Logistic plot with histograms to show the data
###########################################################################
#' @export
logisthistplot <- function(log.fit,log.var,breaks="Sturges") {

data <- log.fit$model
Impact <- log.fit$model[,1]

var.names <- names(log.fit$coefficients)
var.names <- var.names[var.names!="(Intercept)"]

df.names <- colnames(data)
var.id <- df.names %in% log.var
data[,var.id==FALSE] <- 0

# get bin numbers
hist_both <- hist(data[,var.id],plot=FALSE,breaks=breaks)
hist0 <- hist(data[Impact==0,var.id],breaks=hist_both$breaks,plot=FALSE)
hist1 <- hist(data[Impact==1,var.id],breaks=hist_both$breaks,plot=FALSE)
max_count=max(c(hist0$counts,hist1$counts))

# Find Min/Max
min.x <- min(data[,var.id], na.rm=TRUE)
max.x <- max(data[,var.id], na.rm=TRUE)

bar.width <- mean(diff(hist_both$breaks))
min.x.lim <- min(hist_both$breaks)-bar.width
max.x.lim <- max(hist_both$breaks)+bar.width

# Create evenly spaced data
newdata2 <- matrix(0,100,dim(data)[2])
newdata2[,var.id] <- seq(from = min.x.lim, to = max.x.lim, length.out = 100)
colnames(newdata2) <- colnames(data)
newdata2 <- data.frame(newdata2)

newdata3 <- data.frame(X=newdata2[,var.id])
newdata3 <- cbind(newdata3, predict(log.fit, newdata = newdata2, type = "link", se = TRUE))

newdata3 <- within(newdata3, {
    PredictedProb <- plogis(fit)
    LL <- plogis(fit - (1.96 * se.fit))
    UL <- plogis(fit + (1.96 * se.fit))
})


min_x <- min.x.lim
max_x <- max.x.lim

y_range <- c(0,1)
min_y <- min(y_range[1])
max_y <- max(y_range[2])
# need blank bar up to y_min, bars for 0's, blank bars up to 1's, bars for 1's up to y_max
hist_df=data.frame(mids=hist_both$mids,
counts_below=min_y,
counts0=hist0$counts/max_count*diff(y_range)/2,
counts_mid=diff(y_range)-hist_both$counts/max_count*diff(y_range)/2,
counts1=hist1$counts/max_count*diff(y_range)/2)

hist.m=gather(hist_df,"variable", "value", -mids) %>%
	mutate(variable = factor(variable, levels = c("counts_below", "counts0", "counts_mid", "counts1")))

p <- ggplot(newdata3, aes(x = X, y = PredictedProb))
#p <- p + geom_ribbon(aes(ymin = LL, ymax = UL), alpha = 0.2, fill = "grey20")
#p <- p + geom_line(colour = "red", size = 1)
#p <- p + theme_classic(10)
p <- p + theme_bw()
p <- p + theme_pub_logreg()

p <- p + geom_bar(data=hist.m,aes(mids,value,fill=variable),stat="identity", position="stack", width =bar.width)
p <- p + geom_vline(xintercept = 0)
p <- p + geom_ribbon(data=newdata3, aes(ymin = LL, ymax = UL), alpha = 0.2, fill = "grey20")
p <- p + geom_line( colour = "red") #, size = 1
#p <- p + coord_cartesian(ylim=y_range_orig)
p <- p + coord_cartesian(ylim=c(0,1), xlim=c(min.x.lim,max.x.lim))
p <- p + scale_fill_manual(values=c(NA,"#66c2a5",NA,"#fc8d62","red","blue"))
p <- p + scale_y_continuous(labels = scales::percent_format(accuracy = 5L), expand=c(0,0))
p <- p + guides(fill=FALSE)
if (length(var.names)==1){
p <- p + labs(y = "Impact Likelihood \n", x=paste("\n",log.var))
}
if (length(var.names)>1){
p <- p + labs(y = "Partial Impact Likelihood \n", x=paste("\n",log.var))
}
#return(newdata3)
(p)
}



