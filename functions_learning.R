


# Calculation functions ---------------------------------------------------


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
  
  if(params$debug){
    print(costs)
    print(cumulative_capacity)
  }
  
  fit.lm.log <- lm(log(costs) ~ log(cumulative_capacity))
  
  # Extract learning rate from coefficient b1
  b1 <- coef(fit.lm.log)[2] 
  
  # b1 = delta/r; r = assuming constant returns-to-scale parameter = 1. delta_L is the so-called learning-by-doing elasticity, indicating the percentage change in cost following a one percentage increase in cumulative capacity.
  
  delta_L <- b1 
  learning_rate <- 1 - 2 ^ delta_L # Percentage decrease in wind power cost for each doubling of cumulative capacity.
  rsquared <- summary(fit.lm.log)$r.squared
  
  l <- list(
    learning_rate = as.double(round(learning_rate*100,digits)),
    rsquared = round(rsquared*100,digits),
    summary = summary(fit.lm.log)
  )
  
  return(l)
  
}




#' calculate_cummulative_sums
#' Calculates the deployment of capacity per year
#' @param projects_data A dataframe including the variable year and capacity
#'
#' @return
#' @export
#'
#' @examples
calculate_cummulative_sums <- function(projects_data){
  projects_data %>% 
    group_by(year, subset_name) %>% 
    summarise(x_global = sum(capacity, na.rm = TRUE)) %>% 
    ungroup()
}

calculate_delta <- function(projects, x_global){
  
  # x$x = amount deployed with cost reported in currency i, measured in non-monetary terms (e.g., MW installed) in year t
  x <- projects %>%
    group_by(year, local_currency) %>% 
    summarise(x = sum(capacity)) %>% ungroup() %>% 
    complete(year, local_currency, fill = list(x = 0))
  
  # delta$delta = the share of deployment with cost reported in currency i in year t
  delta <- x %>%
    left_join(x_global, by = "year") %>% 
    mutate(delta = x / x_global) %>% 
    rename(currency = local_currency)
  
  # Check if assumption is met
  check <- aggregate(delta$delta, list(year = delta$year), sum)
  
  for(i_check in length(check)){
    if(check[i_check, "x"]!=1){
      print(warning("Sum of deltas is not 1!"))
    }
  }
  
  delta <- delta %>% 
    arrange(year) %>% 
    filter(year %in% all_years) %>% 
    left_join(currency_to_currency_area_translation)
  
  delta$currency <- factor(delta$currency, levels = c(relevant_currencies, "Other"))
  
  return(delta)
  
}


convert_to_real_costs <- function(average_global_costs_df, deflator){
  
  average_global_costs_df %>% 
    inner_join(deflator, by = c("currency", "year")) %>% 
    mutate(real_costs = nominal_costs / defl) %>% 
    select(year, currency, real_costs)
  
}

get_exchange_rate_for_project <- function(project_date, project_currency) {
  
  # exchange_rates is global

  if(params$always_use_yearly_er){
    er <- exchange_rates_per_year %>% 
      filter(year == year(project_date), currency == project_currency) %>% 
      pull(rate)
    
    
  } else {
    # finds the closest date in exchange_rates$date vector and returns
    ## TODO: fix this function
    
    day_differences <- abs(exchange_rates_per_year$date - project_date)
    er_date <- which(abs(day_differences) == min(day_differences, na.rm = TRUE))[1] 
    er <- as.double(exchange_rates[er_date,currency])
  }
  return(er)
}



# Plot functions -----------------------------------------------------------


get_delta_plot <- function(delta_data, plot_type = "combined", legend = FALSE){
  
  delta_plot1 <- ggplot(data = delta_data, 
                        aes(x = year, y = x/1000, fill = currency_area)) +
    geom_bar(stat = "identity") +
    scale_x_continuous(breaks = seq(2004, 2017, 1), minor_breaks = NULL) +
    scale_fill_npg() +
    labs(x = "", 
         y = paste0("Added renewable capacity [GW]")) +
    guides(fill = guide_legend(title = "Currency area")) + 
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5 ))
  
  delta_plot2 <- ggplot(data = delta_data, 
                        aes(x = year, y = delta, fill = currency_area)) +
    geom_bar(position = "fill", stat="identity") +
    scale_x_continuous(breaks = seq(2004,2017,1), minor_breaks = NULL) +
    scale_fill_npg() +
    labs(x = "", 
         y = paste0("Global share of added renewable capacity")) +
    guides(fill=guide_legend(title="Currency area")) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1,vjust = 0.5), 
          legend.position = if_else(legend, "right", "none"))
  
  
  delta_plot_legend <- get_legend(delta_plot1)
  delta_plot1_no_legend <- delta_plot1 + theme(legend.position="none")

  
  if (plot_type == "absolute"){
    return(delta_plot1)
  } else if(plot_type == "relative"){
    return(delta_plot2)
  } else {
    combined_plot <- grid.arrange(delta_plot1_no_legend, delta_plot2, delta_plot_legend, 
                                  ncol=3, widths=c(2.2, 2.2, 0.9))
  }
  
}


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

get_legend<-function(myggplot){
  tmp <- ggplot_gtable(ggplot_build(myggplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}

# Print and display functions ---------------------------------------------

print_number_table <- function(table, caption, digits = params$default_digits, ...){
  if(params$use_kable){
    kable(table, caption = caption, digits = digits, ...)
  } else {
    print(table)
  }
}



# Helper functions --------------------------------------------------------


make_interval_names <- function(intervals_list){
  
  n_intervals <- length(intervals_list)
  intervals_names <- character(n_intervals)
  
  for(i in 1:n_intervals){
    intervals_names[i] <- paste(paste(intervals_list[[i]][1],intervals_list[[i]][2],sep="-"))
  }
  
  return(intervals_names)
}


# Functions to convert the format of CPI and bloomberg

convert_year_to_date <- function(year){
  
  # take the middle day
  deflator_date <- as.Date(paste("02.07.", year),format = "%d.%m.%Y")
  return(deflator_date)
  
}

convert_location_to_currency <- function(location){
  
  location_currency_translation <- list(
    "USA" = "USD",
    "EA19" = "EUR",
    "GBR" = "GBP",
    "JPN" = "JPY",
    "CHN" = "CNY",
    "IND" = "INR")
  
  currency <- NA
  
  if(location %in% names(location_currency_translation)){
    currency <- location_currency_translation[[location]]
  }
  return(currency)
  
} 


