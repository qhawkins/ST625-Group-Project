file_path <- "C:/Users/quinn/Downloads/ST 625 - Group Project(New Data).csv"
data <- read.csv(file_path)

response_var <- "R.PA"

data$LgF <- ifelse(data$Lg == "NL", 1, 0)
print("LgF")
data$BatsRight <- ifelse(data$Bats == "Right", 1, 0)
print("Bats")
data$ThrowsRight <- ifelse(data$Throws == "Right", 1, 0)
print("Throws")

data$BirthplaceF <- as.factor(data$Birthplace)
print("BirthplaceF")

col_names <- names(data)
poly_degree <- 5
data_subset <- data[c("Height", "Weight", "LgF", "BatsRight", "ThrowsRight", "R.PA")]
poly_predictors <- c("Height", "Weight")

all_predictor_names <- setdiff(colnames(data_subset), response_var)
linear_predictors <- setdiff(all_predictor_names, poly_predictors)

poly_terms <- paste0("poly(", poly_predictors, ", ", poly_degree, ")")

all_terms <- c(poly_terms, linear_predictors)

formula_string <- paste(response_var, "~", paste(all_terms, collapse = " + "))

formula <- as.formula(formula_string)

model <- lm(formula, data = data)

print("Model Summary:")
print(summary(model))
model_summary <- summary(model)

coefficients_csv_path <- "model_coefficients.csv"
summary_csv_path <- "model_summary_output.csv"

coefficients_table <- model_summary$coefficients
write.csv(coefficients_table, file = coefficients_csv_path, row.names = TRUE)

summary_output_text <- capture.output(model_summary)
summary_df <- data.frame(SummaryOutput = summary_output_text)
write.csv(summary_df, file = summary_csv_path, row.names = FALSE, quote = TRUE)
