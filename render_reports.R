
filename <- paste("analysis_PV",Sys.Date(),sep = "_")
report_currency <- "USD"


# Final results  -------------------------------------------------------

# Use these results for the main paper. It's based on the IRENA capacity development and marketshares.
# The capacity in 2006 is not changed.

rmarkdown::render("analysis_learningrates_corrections.Rmd", output_format = "word_document", 
                  output_file = paste0(filename, "_IRENA_", report_currency, ".docx"), output_dir = "output2", 
                  params = list(
                    delta = "IRENA",
                    x_global_for_cumsum = "IRENA",
                    lead_currency = report_currency,
                    use_kable = TRUE, 
                    X_norm = FALSE))


# Supplementary results ---------------------------------------------------

# We use these results in the supplementary material. It compares learning 
# rates for BNEF and IRENA with the same capacity in 2006.

#BNEF marketshares
rmarkdown::render("analysis_learningrates_corrections.Rmd", output_format = "word_document", 
                  output_file = paste0(filename, "_BNEF_same_startcapacity_", report_currency, ".docx"), output_dir = "output", 
                  params = list(
                    delta = "BNEF",
                    x_global_for_cumsum = "BNEF with date",
                    X_norm_capacity = 5978.60, 
                    X_norm = TRUE,
                    use_kable = TRUE))

# Supplementary results ---------------------------------------------------

# We use these results in the supplementary material. It compares learning 
# rates for BNEF and IRENA with the original capacity in 2006.

#BNEF marketshares
rmarkdown::render("analysis_learningrates_corrections.Rmd", output_format = "word_document", 
                  output_file = paste0(filename, "_BNEF_", report_currency, ".docx"), output_dir = "output", 
                  params = list(
                    delta = "BNEF",
                    x_global_for_cumsum = "BNEF with date",
                    X_norm = FALSE,
                    use_kable = TRUE, 
                    default_digits = 1))



# Comparison of currencies ------------------------------------------------

# Results for the corrected learning rate should be the same in every base currency

report_currencies <- c("CNY","EUR","GBP","INR","JPY","USD")
filename <- paste("analysis_PV",Sys.Date(),sep = "_")

for(report_currency in report_currencies){
  
  rmarkdown::render("analysis_learningrates_corrections.Rmd", output_format = "word_document", 
                    output_file = paste0(filename, "_IRENA_", report_currency, ".docx"), output_dir = "output/currency_comparison4", 
                    params = list(
                      delta = "IRENA",
                      x_global_for_cumsum = "IRENA",
                      lead_currency = report_currency,
                      use_kable = TRUE, 
                      X_norm = FALSE))

  }


