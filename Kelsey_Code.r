firstFile <- read.csv(file = '~/Downloads/first.csv', header = TRUE, sep = ",")
secondFile <- read.csv(file = '~/Downloads/second.csv', header = TRUE, sep = ",")
firstArray <- vector('list', nrow(firstFile))
for (i in 1:nrow(firstFile)) {
  firstArray[[i]] <- strptime(firstFile[i,3], "%H:%M")
}
timeSlots <- c("09:00","09:45","10:30","11:15","12:00","12:45","13:30","14:15","15:00","15:45","16:30")
timeSlotsArray <- strptime(timeSlots, "%H:%M")
indexArray <- vector('list', nrow(firstFile))
for (i in 1:length(firstArray)) {
  diffArray <- c()
  for (j in 1:11) {
    diffArray <- c(diffArray, as.double(abs(difftime(timeSlotsArray[j],firstArray[[i]],units="mins"))))
  }
  indexArray[[i]] <- which.min(diffArray)
}
for (i in 1:nrow(firstFile)){
  firstFile$TimeSlot[i] <- indexArray[i]
}
secondArray <- vector('list', nrow(secondFile))
for (i in 1:nrow(secondFile)) {
  secondArray[[i]] <- strptime(secondFile[i,4], "%H:%M")
}
indexArray2 <- vector('list', nrow(secondFile))
for (i in 1:length(secondArray)) {
  diffArray <- c()
  for (j in 1:11) {
    diffArray <- c(diffArray, as.double(abs(difftime(timeSlotsArray[j],secondArray[[i]],units="mins"))))
  }
  indexArray2[[i]] <- which.min(diffArray)
}
for (i in 1:nrow(secondFile)){
  secondFile$TimeSlot[i] <- indexArray2[i]
}
m <- merge(firstFile, secondFile, by=c("ID", "TimeSlot"))
# populateArray <- function(x, y, z){
#   x <- vector('list', nrow(y))
#   for (i in 1:nrow(y[z])) {
#     a <- strptime(y[i,z], "%H:%M")
#     x[[i]] <- a
#   }
#   return(x)
# }
# minArray2 <- vector('list', nrow(secondFile))
#secondArray <- populateArray(secondArray, secondFile, 4)
# for (i in 1:nrow(firstFile)){
#   firstFile$TimeSlot[i] <- minArray[i]
# }
# for (i in 1:length(secondArray)) {
#   diffArray <- vector('list',11)
#   for (j in 1:11) {
#     diffArray[[j]] <- abs(timeSlotsArray[j]-secondArray[[i]])
#   }
#   minArray2[[i]] <- min(diffArray)
# }
# for (i in 1:nrow(secondFile)){
#   secondFile$TimeSlot[i] <- minArray2[i]
# }
# m <- merge(firstFile, secondFile, by=c("ID", "TimeSlot"))