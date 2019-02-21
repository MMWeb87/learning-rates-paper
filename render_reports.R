
filename <- paste("analysis_PV",Sys.Date(),sep = "_")
report_currency <- "USD"


#BNEF marketshares
rmarkdown::render("analysis_learningrates_corrections.Rmd", output_format = "pdf_document", 
                  output_file = paste0(filename, "_BNEF_", report_currency, ".pdf"), output_dir = "output", 
                  params = list(
                    delta = "BNEF",
                    use_kable = TRUE,
                    lead_currency = report_currency))


#IRENA marketshares
rmarkdown::render("analysis_learningrates_corrections.Rmd", output_format = "pdf_document", 
                  output_file = paste0(filename, "_IRENA_", report_currency, ".pdf"), output_dir = "output", 
                  params = list(
                    delta = "IRENA",
                    x_global_for_cumsum = "BNEF_relevant",
                    lead_currency = report_currency,
                    use_kable = TRUE))

