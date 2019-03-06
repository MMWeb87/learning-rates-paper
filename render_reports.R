
filename <- paste("analysis_PV",Sys.Date(),sep = "_")
report_currency <- "USD"


#BNEF marketshares
rmarkdown::render("analysis_learningrates_corrections.Rmd", output_format = "pdf_document", 
                  output_file = paste0(filename, "_BNEF_", report_currency, ".pdf"), output_dir = "output", 
                  params = list(
                    delta = "BNEF",
                    x_global_for_cumsum = "BNEF_relevant",
                    use_kable = TRUE,
                    lead_currency = report_currency))
# doc
rmarkdown::render("analysis_learningrates_corrections.Rmd", output_format = "word_document", 
                  output_file = paste0(filename, "_BNEF_", report_currency, ".docx"), output_dir = "output", 
                  params = list(
                    delta = "BNEF",
                    x_global_for_cumsum = "BNEF_relevant",
                    use_kable = TRUE,
                    lead_currency = report_currency))


#IRENA marketshares
rmarkdown::render("analysis_learningrates_corrections.Rmd", output_format = "pdf_document", 
                  output_file = paste0(filename, "_IRENA_", report_currency, ".pdf"), output_dir = "output", 
                  params = list(
                    delta = "IRENA",
                    x_global_for_cumsum = "IRENA",
                    lead_currency = report_currency,
                    use_kable = TRUE, 
                    X_norm = FALSE))

#IRENA marketshares
rmarkdown::render("analysis_learningrates_corrections.Rmd", output_format = "pdf_document", 
                  output_file = paste0(filename, "_IRENA2_", report_currency, ".pdf"), output_dir = "output", 
                  params = list(
                    delta = "IRENA",
                    x_global_for_cumsum = "IRENA",
                    lead_currency = report_currency,
                    use_kable = TRUE, 
                    X_norm = TRUE))

# Set to the same norm ----------------------------------------------------

#BNEF marketshares
rmarkdown::render("analysis_learningrates_corrections.Rmd", output_format = "word_document", 
                  output_file = paste0(filename, "_BNEF_same_startcapacity_", report_currency, ".docx"), output_dir = "output", 
                  params = list(
                    delta = "BNEF",
                    x_global_for_cumsum = "BNEF_relevant",
                    X_norm_capacity = 2750, 
                    X_norm = TRUE,
                    use_kable = TRUE))


#IRENA marketshares
rmarkdown::render("analysis_learningrates_corrections.Rmd", output_format = "word_document", 
                  output_file = paste0(filename, "_IRENA_same_startcapacity_", report_currency, ".docx"), output_dir = "output", 
                  params = list(
                    delta = "IRENA",
                    x_global_for_cumsum = "IRENA",
                    X_norm_capacity = 2750, 
                    X_norm = TRUE,
                    use_kable = TRUE))


