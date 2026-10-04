library(zoo)

df <- read.csv("household_power_consumption.csv", header = TRUE)

column_names <- names(df)[3:length(names(df))]

# Apply na.approx to each specified column
for (col in column_names) {
    df[[col]] <- na.approx(df[[col]])
  }

write.csv(df, "interpolated_project_data.csv", row.names = FALSE)
