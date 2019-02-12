# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}



#' Calculation of learning rate. Doubling 
#'
#' @param costs The technology costs. Usually yearly means 
#' @param cumulative_capacity  Cumulative capacity. Need to be same length as costs
#' @param digits Rounding precision
#'
#' @return Learning rate
#' @export
#'
#' @examples 
calculate_learning_rate <- function(costs, cumulative_capacity, digits = 2){
  
  fit.lm.log <- lm(log(costs) ~ log(cumulative_capacity))
  
  # Extract learning rate from coefficient b1
  b1 <- coef(fit.lm.log)[2] 
  
  # b1 = delta/r; r = assuming constant returns-to-scale parameter = 1. delta_L is the so-called learning-by-doing elasticity, indicating the percentage change in cost following a one percentage increase in cumulative capacity.
  
  delta_L <- b1 
  learning_rate <- 1 - 2 ^ delta_L # Percentage decrease in wind power cost for each doubling of cumulative capacity.
  rsquared <- summary(fit.lm.log)$r.squared
  
  l <- list(
    learning_rate = round(learning_rate*100,digits),
    rsquared = round(rsquared*100,digits)
  )
  
  return(l)
  
}


make_interval_names <- function(intervals_list){
  
  n_intervals <- length(intervals_list)
  intervals_names <- character(n_intervals)
  
  for(i in 1:n_intervals){
    intervals_names[i] <- paste(paste(intervals_list[[i]][1],intervals_list[[i]][2],sep="-"))
  }
  
  return(intervals_names)
}


get_legend<-function(myggplot){
  tmp <- ggplot_gtable(ggplot_build(myggplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}


