library(zoo)
require(ggbiplot)
library(depmixS4)
library(stats)
library(pscl)

set.seed(7)

round_to_half <- function(x) {
  x <- x * 10
  remainder <- abs(x - floor(x))
  if (remainder < 0.50) {
    return(floor(x) / 10)
  } 
  else {
    return(ceiling(x) / 10)
  }
}

household_data <- read.csv("interpolated_std_scaled_project_data.csv", header = TRUE)

week_day = "5"

training_start <- as.Date("17/12/2006", format = "%d/%m/%Y")
training_end <- as.Date("22/11/2009", format = "%d/%m/%Y")

household_data$Date <- as.Date(household_data$Date,format="%d/%m/%Y")

training_data <- household_data[,c("Date","Time","Global_active_power","Global_reactive_power","Sub_metering_3")]
training_data <- training_data[training_data$Date >= training_start & training_data$Date <= training_end,]
training_data <- training_data[format(training_data$Date,format = "%u") == week_day,]

# Set time window
training_data <- training_data[training_data$Time >= "18:00:00" & training_data$Time <= "21:59:00",]

GAP <- training_data$Global_active_power
GRP <- training_data$Global_reactive_power
SM3 <- training_data$Sub_metering_3

# Round 'Global_active_power' to 4 decimal values
# Rounding does not effect number of unique values in GAP; no information lost
GAP <- round(GAP, 4)

# Round 'Global_reactive_power' & 'Sub_metering_3' data to 2 decimal values
# Rounding does not effect number of unique values in SM3 (37); no information lost
GRP <- round(GRP, 2)
SM3 <- round(SM3, 2)

# Apply the custom rounding function to 'Global_reactive_power' to reduce 
# number of discrete groups from 308 to 43
GRP <- sapply(GRP, round_to_half)

n_times <- replicate(153,nrow(training_data)/153)
n_states <- 13

# Train all response variables independently 
model <- depmix(response = list(GAP~1, GRP~1, SM3~1),
                data = training_data,
                nstates = n_states,
                ntimes = n_times,
                family = list(gaussian(), multinomial(), multinomial()))

print(fit(model))
