library(zoo)
require(ggbiplot)
library(depmixS4)
library(stats)
library(pscl)

set.seed(17)

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

# Extract response variable columns 
household_data <- read.csv("interpolated_std_scaled_project_data.csv", header = TRUE)
test_data <- household_data[,c("Date","Time","Global_active_power","Global_reactive_power","Sub_metering_3")]

# Define date and time windows to Friday 6PM - 10PM
test_data$Date <- as.Date(test_data$Date, format = "%d/%m/%Y")
test_start <- as.Date("29/11/2009", format = "%d/%m/%Y")
test_end <- as.Date("14/11/2010", format = "%d/%m/%Y")
week_day = "5"

test_data <- test_data[test_data$Date >= test_start & test_data$Date <= test_end,]
test_data <- test_data[format(test_data$Date,format = "%u") == week_day,]
test_data <- test_data[test_data$Time >= "18:00:00" & test_data$Time <= "21:59:00",]

# Divide test data into 10 subsets of 5 weeks per chunk
chunksize <-  nrow(test_data) / 10
nrow(test_data)
groups <-c(
  replicate(chunksize,1),
  replicate(chunksize,2),
  replicate(chunksize,3),
  replicate(chunksize,4),
  replicate(chunksize,5),
  replicate(chunksize,6),
  replicate(chunksize,7),
  replicate(chunksize,8),
  replicate(chunksize,9),
  replicate(chunksize,10)
)

# Assign 5 weeks of data to each chunk 
chunks <- split(test_data, groups)

for(chunk in chunks){
  testGAP <- chunk$Global_active_power
  testGRP <- chunk$Global_reactive_power
  testSM3 <- chunk$Sub_metering_3

  # Round 'Global_active_power' to 4 decimal values
  # Rounding does not effect number of unique values in GAP; no information lost
  testGAP <- round(testGAP, 4)

  # Round 'Global_reactive_power' & 'Sub_metering_3' data to 2 decimal values
  # Rounding does not effect number of unique values in SM3 (37); no information lost
  testGRP <- round(testGRP, 2)
  testSM3 <- round(testSM3, 2)
  
  # Apply the custom rounding function to 'Global_reactive_power' to reduce
  # number of discrete groups from 308 to 43
  testGRP <- sapply(testGRP, round_to_half)
  n_states <- 15

  # Train all response variables independently
  n_times <- replicate(5,nrow(chunk)/5)

  testModel <- depmix(response = list(testGAP~1, testGRP~1, testSM3~1),
                      data = chunk,
                      nstates = n_states,
                      ntimes = n_times,
                      family = list(gaussian(), multinomial(), multinomial()))
  print(fit(testModel))
}

# Rerun only chunks which failed convergence on initial run 
# rerun_chunks <- list(chunks[[7]], chunks[[10]]) 
# for (chunk in rerun_chunks){
#   testGAP <- chunk$Global_active_power
#   testGRP <- chunk$Global_reactive_power
#   testSM3 <- chunk$Sub_metering_3
#   
#   # Round 'Global_active_power' to 4 decimal values
#   # Rounding does not effect number of unique values in GAP; no information lost
#   testGAP <- round(testGAP, 4)
#   
#   # Round 'Global_reactive_power' & 'Sub_metering_3' data to 2 decimal values
#   # Rounding does not effect number of unique values in SM3 (37); no information lost
#   testGRP <- round(testGRP, 2)
#   testSM3 <- round(testSM3, 2)
#   
#   # Apply the custom rounding function to 'Global_reactive_power' to reduce
#   # number of discrete groups from 308 to 43
#   testGRP <- sapply(testGRP, round_to_half)
#   n_states <- 15
#   
#   # Train all response variables independently
#   n_times <- replicate(5,nrow(chunk)/5)
#   
#   testModel <- depmix(response = list(testGAP~1, testGRP~1, testSM3~1),
#                       data = chunk,
#                       nstates = n_states,
#                       ntimes = n_times,
#                       family = list(gaussian(), multinomial(), multinomial()))
#   print(fit(testModel))
# }