report_currencies <- c("CNY","EUR","GBP","INR","JPY","USD")
filename <- paste("analysis_PV",Sys.Date(),sep = "_")

for(report_currency in report_currencies){
  
  # Use these results for the main paper. It's based on the IRENA capacity development and marketshares.
  # The capacity in 2006 is not changed.
  
  rmarkdown::render("analysis_learningrates_corrections.Rmd", output_format = "word_document", 
                    output_file = paste0(filename, "_", report_currency, "real.docx"), output_dir = "output/reports/", 
                    params = list(
                      deflate = TRUE,
                      lead_currency = report_currency,
                      use_kable = TRUE))
  
  rmarkdown::render("analysis_learningrates_corrections.Rmd", output_format = "word_document", 
                    output_file = paste0(filename, "_", report_currency, "nominal.docx"), output_dir = "output/reports/", 
                    params = list(
                      deflate = FALSE,
                      lead_currency = report_currency,
                      use_kable = TRUE))
}

# files
rmarkdown::render("analysis_learningrates_corrections.Rmd", output_format = "pdf_document", 
                  output_file = paste0(filename, ".pdf"), output_dir = "output/reports_files/", 
                  params = list(use_kable = TRUE))

rmarkdown::render("analysis_results.Rmd", output_format = "pdf_document", 
                  output_file = paste0("results.pdf"), output_dir = "output/reports_files/", 
                  params = list(use_kable = TRUE))
