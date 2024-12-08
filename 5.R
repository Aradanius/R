res <- read.csv("results_of_care.csv", colClasses = "character")
res[, 11] <- as.numeric(res[, 11])
hist(res[, 11], 
     main = "Гистограмма 30-дневной смертности от сердечного приступа",
     xlab = "30-дневная смертность от сердечного приступа (%)",
     ylab = "Количество госпиталей")


getBestHospital <- function(state, criteria) {
  States <- unique(results_of_care$State)
  Crit <- c("heart attack", "heart failure", "pneumonia")
  if (!(state %in% States)) {
    stop("Некорректный штат")
  }
  if (!(criteria %in% Crit)) {
    stop("Некорректный показатель")
  }
  
  colIndex <- switch(criteria,
                     "heart attack" = 11,
                     "heart failure" = 17,
                     "pneumonia" = 23)
  data <- results_of_care[results_of_care$State == state & !is.na(results_of_care[, colIndex]), ]
  m <- as.numeric(data[, colIndex])
  index <- which.min(m)
  bestHospitals <- data[which(m == m[index]), ]
  bestHospitals <- bestHospitals[order(bestHospitals$Hospital.Name), ]
  bestHospitalName <- bestHospitals$Hospital.Name[1]
  return(bestHospitalName)
}

results_of_care <- read.csv("results_of_care.csv", colClasses = "character", na.strings = c("", "Not Available"))

getBestHospital("TX", "heart attack")
getBestHospital("TX", "heart failure")
getBestHospital("MD", "heart attack")


getHospitalRating <- function(state, criteria, n = "best") {
  States <- unique(results_of_care$State)
  Crit <- c("heart attack", "heart failure", "pneumonia")
  if (!(state %in% States)) {
    stop("Некорректный штат")
  }
  if (!(criteria %in% Crit)) {
    stop("Некорректный показатель")
  }
  
  colIndex <- switch(criteria,
                     "heart attack" = 11,
                     "heart failure" = 17,
                     "pneumonia" = 23)
  data <- results_of_care[results_of_care$State == state & !is.na(results_of_care[, colIndex]), ]
  if(nrow(data) == 0){
    return(NA)
  }
  m <- as.numeric(data[, colIndex])
  if (is.numeric(n)) {
    if (n > length(m)) {
      return(NA)
    }
    ord <- data[order(m, data$Hospital.Name), ]
    return(ord$Hospital.Name[n])
  } else if (n == "best") {
    ord <- data[order(m, data$Hospital.Name), ]
    return(ord$Hospital.Name[1])
  } else if (n == "worst") {
    ord <- data[order(-m, data$Hospital.Name), ]
    return(ord$Hospital.Name[1])
  } else {
    stop("Некорректное значение n")
  }
}

results_of_care <- read.csv("results_of_care.csv", colClasses = "character", na.strings = c("", "Not Available"))

getHospitalRating("TX", "heart failure", 4)
getHospitalRating("MD", "heart attack", "worst")
getHospitalRating("MN", "pneumonia", 5)



getRaiting <- function(criteria, n = "best") {
  Crit <- c("heart attack", "heart failure", "pneumonia")
  if (!(criteria %in% Crit)) {
    stop("Некорректный показатель")
  }
  colIndex <- switch(criteria,
                     "heart attack" = 11,
                     "heart failure" = 17,
                     "pneumonia" = 23)
  result <- data.frame(hospital = character(0), state = character(0))
  states <- unique(results_of_care$State)
  for (state in states) {
    datas <- results_of_care[results_of_care$State == state & !is.na(results_of_care[, colIndex]), ]
    
    if(nrow(datas) == 0){
      row <- data.frame(hospital = NA, state = state)
      result <- rbind(result, row)
      next
    }
    mortality <- as.numeric(datas[, colIndex])
    if (is.numeric(n)) {
      if (n > length(mortality)) {
        row <- data.frame(hospital = NA, state = state)
      } else {
        ord <- datas[order(mortality, datas$Hospital.Name), ]
        row <- data.frame(hospital = ord$Hospital.Name[n], state = state)
      }
    } else if (n == "best") {
      ord <- datas[order(mortality, datas$Hospital.Name), ]
      row <- data.frame(hospital = ord$Hospital.Name[1], state = state)
    } else if (n == "worst") {
      ord <- datas[order(-mortality, datas$Hospital.Name), ]
      row <- data.frame(hospital = ord$Hospital.Name[1], state = state)
    } else {
      stop("Некорректное значение n")
    }
    result <- rbind(result, row)
  }
  
  return(result)
}

results_of_care <- read.csv("results_of_care.csv", colClasses = "character", na.strings = c("", "Not Available"))
head(getRaiting("pneumonia", 15), 5)
tail(getRaiting("heart failure", "worst"), 54)

