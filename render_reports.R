report_currencies <- c("CNY","EUR","GBP","INR","JPY","USD")
filename <- paste("analysis_PV",Sys.Date(),sep = "_")

for(report_currency in report_currencies){
  
  # Use these results for the main paper. It's based on the IRENA capacity development and marketshares.
  # The capacity in 2006 is not changed.
  
  rmarkdown::render("analysis_learningrates_corrections.Rmd", output_format = "word_document", 
                    output_file = paste0(filename, "_IRENA_", report_currency, ".docx"), output_dir = "output/reports/", 
                    params = list(
                      lead_currency = report_currency,
                      use_kable = TRUE))

}


