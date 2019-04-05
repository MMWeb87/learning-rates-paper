

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


