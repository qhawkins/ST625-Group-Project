file_path <- "C:/Users/quinn/Downloads/ST 625 - Group Project(New Data).csv"
data <- read.csv(file_path)

response_var <- "R.PA"

data$LgF <- ifelse(data$Lg == "NL", 1, 0)
print("LgF")
data$BatsRight <- ifelse(data$Bats == "Right", 1, 0)
print("Bats")
data$ThrowsRight <- ifelse(data$Throws == "Right", 1, 0)
print("Throws")
data$HasAward <- ifelse(data$Awards == "", 0, 1)
print("HasAward")
#data$TeamF <- as.factor(data$Team)
#print("TeamF")
data$BirthplaceF <- as.factor(data$Birthplace)
print("BirthplaceF")

col_names <- names(data)

cols_to_exclude_always <- c(response_var, "X", "Lg", "Bats", "Throws", "Awards", "Birthplace", "Pos", "Player", "TB", "Team", "GIDP", "AB")

potential_predictors <- setdiff(col_names, cols_to_exclude_always)

#print(data[potential_predictors])

print("Potential Predictors")
formula_string <- paste(response_var, "~",
  paste(potential_predictors, collapse = " + "))

model <- lm(as.formula(formula_string), data = data)

print("Model Summary:")
print(summary(model))
model_summary <- summary(model)

coefficients_csv_path <- "model_coefficients.csv"
summary_csv_path <- "model_summary_output.csv" # Will store the captured text summary

coefficients_table <- model_summary$coefficients
write.csv(coefficients_table, file = coefficients_csv_path, row.names = TRUE)

summary_output_text <- capture.output(model_summary)
summary_df <- data.frame(SummaryOutput = summary_output_text)
write.csv(summary_df, file = summary_csv_path, row.names = FALSE, quote = TRUE)