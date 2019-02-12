
filename <- paste("analysis_PV",Sys.Date(),sep = "_")
report_currency <- "USD"

#IRENA marketshares
rmarkdown::render("analysis_learningrates_corrections.Rmd", output_format = "pdf_document", 
                  output_file = paste0(filename, "_IRENA_", report_currency, ".pdf"), output_dir = "output", 
                  params = list(
                    lead_currency = report_currency))


#BNEF marketshares
rmarkdown::render("analysis_learningrates_corrections.Rmd", output_format = "pdf_document", 
                    output_file = paste0(filename, "_BNEF_", report_currency, ".pdf"), output_dir = "output", 
                    params = list(
                      delta = "BNEF",
                      lead_currency = report_currency))
