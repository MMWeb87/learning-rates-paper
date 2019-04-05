

# Precondition: render_reports has been run with debug = yes in the params of the main file


# Comparisons -------------------------------------------------------------
 
# Here, I compare all the results for different base currencies. 


library(tidyverse)

source("init.R")
interval_test_name <- "2006-2016"

results_tibble <- tibble(
  "currency" = relevant_currencies)

coefficient <- results_tibble
coefficient_log <- results_tibble
regression_costs <- list()
regression_costs_unadjusted <- list()

regression_costs_norm <- results_tibble

exchange_rates <- list()

simple_learning_curves <- list()


for(currency in relevant_currencies){
 
  directory <- paste0("output/debug/", currency,"/")
  filename_prefix <- paste0(directory, interval_test_name, "_")
  
  coefficient[coefficient$currency == currency, "coefficient"] <- read_csv(paste0(filename_prefix, "adjusted_coefficient_lin.csv"))
  coefficient_log[coefficient$currency == currency, "coefficient"] <- read_csv(paste0(filename_prefix, "adjusted_coefficient_log.csv"))
  
  
  regression_costs[[currency]] <- read_csv(paste0(filename_prefix, "adjusted_regression_costs.csv")) %>% 
    add_column("currency" = currency)
  
  regression_costs_unadjusted[[currency]] <- read_csv(paste0(filename_prefix, "unadjusted_regression_costs.csv")) %>% 
    add_column("currency" = currency)
  
  
  regression_costs_norm[coefficient$currency == currency, "costs"] <- read_csv(paste0(filename_prefix, "adjusted_regression_costs_normalised.csv"))
  
  # local costs
  
  exchange_rates[[currency]] <- read_csv(paste0(directory, "exchange_rates_per_year.csv"))
  
  simple_learning_curves[[currency]] <- read_csv(paste0("output/debug/USD/", interval_test_name, "_simple_regression_costs_",currency,".csv"))
  

}


# Validity test -----------------------------------------------------------


# Results for the corrected learning rate should be the same in every base currency
# for adjusted

test_coefficient <- as.numeric(coefficient_log[1,"coefficient"])

difference <- coefficient_log %>% 
  mutate(diff = coefficient - !!test_coefficient, abs_diff = abs(coefficient - !!test_coefficient))

sum_of_differences <- summarise(difference, sum(abs_diff))


if(sum_of_differences != 0){
  warning("Unlike 0")
  difference
  sum_of_differences
}



# Plot different currencies -----------------------------------------------



all_costs <- left_join(
    reduce(regression_costs, bind_rows),
    reduce(regression_costs_unadjusted, bind_rows),
    by = c("capacity", "currency")) %>%
  left_join(reduce(simple_learning_curves, bind_rows), 
    by = c("capacity", "currency")) %>% 
  rename("adjusted" = costs.x, "unadjusted" = costs.y, "simple" = costs) %>% 
  gather(one_of("adjusted", "unadjusted", "simple"), key = "type", value = "costs")

# Also load the simple learning curevnes


ggplot(all_costs, mapping = aes(x = capacity, y = costs, col = currency, linetype = type, alpha = type)) +
  geom_line() +
  scale_alpha_manual(values = c("adjusted" = 1, "unadjusted" = 0.5, "simple" = 0.8)) +
  scale_y_log10() +
  scale_x_log10()



# Comparing average local costs -------------------------------------------

# They might contribute a difference
reduce(exchange_rates, bind_rows) %>% 
  filter(year == 2010, currency %in% c("CNY", "EUR"), lead_currency %in% c("CNY", "EUR")) %>% 
  mutate(inverse = 1 / rate) %>% 
  View()
#summarise(sum(C))

reduce(exchange_rates, bind_rows) %>% 
  filter(year == 2010, currency %in% c("CNY", "USD"), lead_currency %in% c("CNY", "USD")) %>% 
  mutate(inverse = 1 / rate) %>% 
  View()
  #summarise(sum(C))



