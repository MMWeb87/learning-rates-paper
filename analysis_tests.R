# tests

## Comparison of simple learning rates and unadjusted learning_rates

#average_global_costs (from the simple method) should be the same as the unadjusted learning rate in USD. Both consider the marketshare, but do not weight.


P_components <- average_local_costs %>% 
  full_join(w, by = c("currency", "year")) %>% 
  filter(between(year, 2006, 2016)) %>% 
  mutate(weighted_c = C * w)

methods_diff <- average_global_costs %>% 
  filter(currency == "USD") %>% 
  inner_join(P, by = c("year")) %>% 
  rename(simple = global_means, unadj = P) %>% 
  mutate(diff = (simple/unadj - 1))

# Let's focus on one entry, the first to find out whats wrong
methods_diff[1,]


## Compare methods
#Both should give the same result, i.e. the costs.

# Method 1
method_1_test <- projects_with_costs_in_currencies %>% 
  select(year, !!relevant_currencies) %>% 
  gather("currency", "value", !!relevant_currencies) %>% 
  group_by(currency, year) %>% 
  summarise(global_means = mean(value)) %>% 
  filter(currency == "USD", year == 2006)

# Method 2


method_1_test

filter(C, year == t)#

# In method 2, it only takes the two countries EUR + USD


# does it have to do, how I calculate the means?

projects_with_costs_in_currencies %>% 
  select(year, !!relevant_currencies) %>% 
  filter(year == 2006)

# idea: maybe exchange rates are not the same!!
# in one case i calculate costs with the exchange rate that is closes to the project
# in the other case, I calculate costs with a fixed exchange rate (or not at all!)

#gather("currency", "value", !!relevant_currencies) %>% 
#group_by(currency, year) 

#%>% 
#summarise(global_means = mean(value)) %>% 
#filter(currency == "USD", year == 2006)

# a further problem could be that there are Other deltas in the delta. But I only use the relevant countries for the simple learning rates-> no, but consider for IRENA




filter(average_global_costs_real, year == 2006, currency == "USD")
filter(P_real, year == 2006)

# Compare real


# So, the nominal difference between values is it big?

average_global_costs_real %>% 
  filter(currency == "USD") %>% 
  select(year, global_means) %>% 
  full_join(P, by = "year") %>% 
  mutate(diff = P / global_means)

# OK, so this is the error that I see above. The difference between the nominal values. I is smaller than 5%. However, I can not explain, where these 5% come from. I reduced the error by taking only 1 exchange rate per year (instead of a er for every project) and by taking the delta from the same number of projects.

# Try to find out, how much real and nominal values differ
P %>% 
  full_join(P_real, by = "year") %>% 
  mutate(diff = P.y/P.x) %>% 
  View()

# strange, diff is always the same = 1.021301. That's an error.
average_global_costs_real %>% 
  filter(currency == "USD") %>% 
  View()



# CHECK: why did I deflate by T_max? I need to take a deflator for every year!




# trace the error

# try to pin down the error




P_test <- tibble(
  "year" = all_years,
  "P" = numeric(length(all_years))
)


t <- 2006

for(i in relevant_currencies){
  
  # variables for currency i = current_currency and year = T_max
  delta_i_t <- filter(delta, year == t, currency == i) %>% pull(delta)
  w_i_t <- filter(w, year == t, currency == i) %>% pull(w)
  w_i_0 <- filter(w, year == T0, currency == i) %>% pull(w)
  C_i_t <- filter(C, year == t, currency == i) %>% pull(C)
  
  
  # weighted average global cost converted to lead currency l in year t
  print(paste(P_test[P_test$year == t, "P"], "+", delta_i_t ,"*", w_i_t  ,"*", C_i_t))
  
  
  P_test[P_test$year == t, "P"] <- P_test[P_test$year == t, "P"] + delta_i_t * w_i_t * C_i_t
  
}



# compared to the calculation to get to global_average (Method 1)
test_1 <- projects_with_costs_in_currencies %>% 
  gather("currency", "value", !!relevant_currencies) %>%
  filter(currency == "USD", year == "2006") 
test_1


filter(P_test, year == 2006)

# Let's have a look at the costs here
filter(C, year == "2006")
test_1
# OK; the one USD value is equal. what about the other?
# The mean value of the values is 8.45, which is different than 6.97
filter(C, year == "2006") # costs in local_currency, so need to compare

test_1

# mean of EUR projects in EUR:
filter(test_1, local_currency == "EUR") %>% summarise(mean(local_value))
# mean of EUR projects in USD:
filter(test_1, local_currency == "EUR") %>% summarise(mean(value)) %>% as.double()
# is this the same as below?
# namely as:
1.211782501 * 6.96963183269819
# yes it is. The only difference is the marketshare calculation.
# So, should I not devide by per capacity in the former learning rates?

# the calculation to get to P (Method2)
0.93260756868844 * 1.211782501 * 6.96963183269819 + 0.0673924313115604 * 1 * 7.1

# In method I simply take the mean of all projects in USD
test_2<- test_1 %>% 
  summarise(mean(value))

test_2


filter(average_global_costs, currency == "USD", year == "2006")
