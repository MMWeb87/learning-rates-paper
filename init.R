# paths

filenames <- list(
  bnef_data = "input/data_PV_all_nominal.csv",
  irena_data = "input/irena-capacity-generation.csv",
  exchange_rates = "input/USD_exchange_rates.csv",
  deflators = "input/OECD_CPI_yearly.csv",
  currency_to_currency_area_translation = "input/currency_to_currency_area_translation.csv",
  country_to_currency_translation = "input/country_to_currency_translation.csv",
  long_to_short_currencycode_translation = "input/currency_translation.csv",
  outliers = "input/outliers.csv"
)


# basic variables

# These currencies need to be in the currency_translation table
relevant_currencies <- c("CNY", "EUR", "GBP", "INR", "JPY", "USD")
relevant_deflators <- c("CHN", "EA19", "GBR", "IND", "JPN", "USA")

intervals <- list(
  c(2006,2011),
  c(2011,2016),
  c(2006,2016)
)


# Tables
table_order_currency <- c("USD", "EUR", "JPY", "CNY", "INR", "GBP")

# Plots
plot_order_currency <- relevant_currencies
plot_order_country <- c("China", "Euro Area", "United Kingdom", "India", "Japan", "United States", "ROW")
plot_order_linetype <- c("dotdash", "longdash", "dashed", "dotted", "twodash", "solid")

fig.width.baseline <- 7
output.size <- 1.4
out.width.default <- "100%"

combined_plot_interval <- "2006-2016"

