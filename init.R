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

if(exists("params")){
  if(params$run_test){
    filenames[["exchange_rates"]] <- "input/USD_exchange_rates_prop2_test.csv"
  }
}


# basic variables

# These currencies need to be in the currency_translation table
relevant_currencies <- c("CNY", "EUR", "GBP", "INR", "JPY", "USD")
relevant_deflators <- c("CHN", "EA19", "GBR", "IND", "JPN", "USA")

intervals <- list(
  c(2006,2016),
  c(2006,2011),
  c(2011,2016)
)

# Tables
table_order_currency <- c("USD", "EUR", "JPY", "CNY", "INR", "GBP")

# Plots
plot_order_currency <- relevant_currencies
plot_order_country <- c("China", "Euro Area", "United Kingdom", "India", "Japan", "United States", "ROW")
plot_order_linetype <- c("dotdash", "longdash", "dashed", "dotted", "twodash", "solid")

# mypal = pal_npg("nrc")(length(relevant_currencies)) # colour are from here 
plot_colours_currencies <- c(
  "CNY" = "#E64B35FF",
  "EUR" = "#4DBBD5FF",
  "GBP" = "#00A087FF",
  "INR" = "#8491B4FF",
  "JPY" = "#F39B7FFF",
  "USD" = "#3C5488FF"
)

fig.width.baseline <- 7
output.size <- 1.4
out.width.default <- "100%"

combined_plot_interval <- "2006-2016"


# Variable generation -----------------------------------------------------

T0 <- min(unlist(intervals))
T_max <- max(unlist(intervals))
all_years <- as.numeric(T0):as.numeric(T_max)
names(intervals) <- make_interval_names(intervals)

factor_order_intervals <- names(intervals)

