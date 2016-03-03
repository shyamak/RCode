# Function to populate an array with Start times in POSIXlt format
# @param file csv file with data
# @param timeColNum column number for Start time in the csv file
# @return x the array with start times for the entries given in the file
populateArray <- function(file,timeColNum) {
  a <- nrow(file)
  x <- vector('list', a)
  for (i in 1:a) {
    x[[i]] <- strptime(file[i,timeColNum], "%H:%M")
  }
  return(x)
}

# Find the time slot that matches with the given Start time
# @param a firstArray
# @param b timeSlot dictionary array
# @return index array, it has the slot number 
findIndex <- function(a,b) {
  len <- length(a)
  x <- vector('list', len)
  for (i in 1:len) {
    diffArray <- c()
    for (j in 1:11) {
      diffArray <- c(diffArray, as.double((abs(difftime(b[j],a[[i]],units="mins")))))
    }
    x[[i]] <- which.min(diffArray)
  }
  return(x)
}

# Add a new column with the name "TimeSlot"
# @param file, the csv file being modified
# @param index, index array with the designated timeslots
# @return file, the updated file
addColumn <- function(file,index) {
  for (i in 1:nrow(file)) {
    file$TimeSlot[i] <- index[i]
  }
  return(file)
}

# dictionary with the time slots and converted into POSIXlt format array
timeSlots <- c("09:00","09:45","10:30","11:15","12:00","12:45","13:30","14:15","15:00","15:45","16:30")
timeSlotsArray <- strptime(timeSlots, "%H:%M")

firstFile <- read.csv(file = '~/Downloads/first.csv', header = TRUE, sep = ",")
secondFile <- read.csv(file = '~/Downloads/second.csv', header = TRUE, sep = ",")
firstArray <- populateArray(firstFile, 3)
secondArray <- populateArray(secondFile, 4)

indexArray <- findIndex(firstArray, timeSlotsArray)
indexArray2 <- findIndex(secondArray, timeSlotsArray)
firstFile <- addColumn(firstFile,indexArray)
secondFile <- addColumn(secondFile, indexArray2)

m <- merge(firstFile, secondFile, by=c("ID", "TimeSlot"))
drops <- c("StartTime", "EndTime", "EndDate", "Survey.Started.Date", "Trigger.Date", "Survey.Started.Time", "Trigger.Time")
n <- m[ , !(names(m) %in% drops)]