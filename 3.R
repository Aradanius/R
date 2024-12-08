getMean <- function(folder, type, id=001:332){

  pollutant_values <- numeric()

  for (monitor_id in id) {
    file_name <- sprintf("%03d.csv", monitor_id)
    file_path <- file.path(folder, file_name)
    data <- read.csv(file_path)
    pollutant_values <- c(pollutant_values, data[[type]])
  }
  mean_value <- mean(pollutant_values, na.rm = TRUE)
  return(mean_value)
}

mean_sulfate <- getMean("data", "sulfate", 1:10)
mean_nitrate <- getMean("data", "nitrate", 70:72)

print(paste("Среднее значение сульфата:", mean_sulfate))
print(paste("Среднее значение нитрата:", mean_nitrate))


getCompleteObservation <- function(folder, id = 1:332) {

  
  results <- data.frame(id = integer(), count = integer())
  
  for (monitor_id in id) {
    file_name <- sprintf("%03d.csv", monitor_id)
    file_path <- file.path(folder, file_name)
    
    data <- read.csv(file_path)
    complete_cases <- sum(complete.cases(data[, c("sulfate", "nitrate")]))
    results <- rbind(results, data.frame(id = monitor_id, count = complete_cases))
  }
  return(results)
}


result <- getCompleteObservation("data", 1)
print(result)
result2 <- getCompleteObservation("data", 30:25)
print(result2)


getCorrelation <- function(folder, limen = 0) {

  correlations <- numeric()
  files <- list.files(path = folder, pattern = "\\.csv$", full.names = FALSE)
  monitor_ids <- as.numeric(gsub("\\.csv", "", files))
  for (monitor_id in monitor_ids) {
    file_path <- file.path(folder, sprintf("%03d.csv", monitor_id))
    data <- read.csv(file_path)
    complete_cases <- sum(complete.cases(data[, c("sulfate", "nitrate")]))
    if (complete_cases >= limen) {
      correlation <- cor(data$sulfate, data$nitrate, use = "complete.obs")
      correlations <- c(correlations, correlation)
    }
  }
  
  return(correlations)
}


res <- getCorrelation("data", 150)
head(res)
summary(res)
