#!/usr/bin/env Rscript

# Author: Cristina Freitas Bazzano

# Description: Naive Bayes Model to Classify the final grade in Calculus I given the evidence
# of the course that the student was placed in, the grade in this course, his/her major 
# and the semester he/she is going to take Calculus I

# execute with:
# Rscript naiveBayesGrades.R
# The data must be in a file called FakeDataComplete.csv
library("binhf")

train_data <- read.csv(paste(getwd(), "FakeDataComplete.csv", sep = "/"), na.strings = "")
train_data$Math213 <- factor(train_data$Math213, levels=c("A ","A-","B+","B ","B-","C+","C ","C-","D+","D ", "D-","F "), labels = c("A","A-","B+","B","B-","C+","C","C-","D+","D", "D-","F"))
train_data$Math136 <- factor(train_data$Math136, levels = c("A","A-","B+","B","B-","C+","C","C-","D+","D", "D-","F"), labels = c("A","A-","B+","B","B-","C+","C","C-","D+","D", "D-","F"))
train_data$Math117 <- factor(train_data$Math117, levels = c("A","A-","B+","B","B-","C+","C","C-","D+","D", "D-","F"), labels = c("A","A-","B+","B","B-","C+","C","C-","D+","D", "D-","F"))

## Parse Grades
# remove not passed pre - courses
rows_remove <- vector()
for (i in seq_len(nrow(train_data))) {
  if ( any(train_data[i,c(1)] %in% c("D+","D", "D-","F") || train_data[i,c(2)] %in% c("D+","D", "D-","F") , na.rm = TRUE) ) {
    rows_remove <- c(rows_remove, i)
  }
}
train_data <- train_data[-rows_remove,]

# uncomment this to remove students placed directly
#rows_remove <- vector()

# obtain the course the student was placed in
course_placed <- vector()
course_grade <- vector()
for (i in seq_len(nrow(train_data))) {
  if (!is.na(train_data[i,1])) {
    course_placed <- c(course_placed, "CollegeAlg")
    course_grade <- c(course_grade, train_data[i, "Math117"])
  } else if (!is.na(train_data[i,2])) {
    course_placed <- c(course_placed, "Precal")
    course_grade <- c(course_grade, train_data[i, "Math136"])
  } else {
    course_placed <- c(course_placed, "CalcI")
    #course_grade <- c(course_grade, "P")
    course_grade <- c(course_grade, train_data[i, "Math213"])
    # uncomment this to remove students placed directly
    #rows_remove <- c(rows_remove, i)
  }
}
train_data$CoursePlaced <- factor(course_placed, levels=c("CollegeAlg","Precal","CalcI"))
train_data$Course_GPA <- factor(course_grade, labels=c("A","A-","B+","B","B-","C+","C","C-","D+","D", "D-","F"))

# uncomment this to remove students placed directly
#train_data <- train_data[-rows_remove,]

train_data <- train_data[,c(-1,-2)]
# clean environment
rm(list=c("course_placed","rows_remove", "course_grade"))

# Use 1/3 of the data to train the model and 2/3 to test
test_rows <- round(runif(2*nrow(train_data)/3, min=1,max=nrow(train_data)))
test_data <- train_data[test_rows,]
train_data <- train_data[-test_rows,]
cat("Traingin Data:\n")
print(head(train_data,10))

######################### MULTINOMINAL @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

# size of the training set
m <- length(train_data$Math213)

# Frequencies of each course placed in and course grade with each final grade
p_course_GPA_multi <- data.frame(table(finalGrade=train_data$Math213,grade=train_data$Course_GPA))
p_course_multi <- data.frame(table(finalGrade=train_data$Math213,course=train_data$CoursePlaced))
p_semester_multi <- data.frame(table(finalGrade=train_data$Math213,semester=train_data$Math213Semester))
p_major_multi <-data.frame(table(finalGrade=train_data$Math213,major=train_data$Major))

# frequency of each final grade
py_multi <- data.frame(table(finalGrade=train_data$Math213))

# parameter phy
oy_multi <- py_multi
oy_multi$Freq <- oy_multi$Freq/m

######### function compute the probability of x given y @@@@@@@@@@@@@@@@@@@@@@
pxy_multi <- function(x, y) {
  # obtain the number of students that passed with a grade equals y and smooth it
  n <- py_multi[py_multi$finalGrade == y,'Freq'] + 12
  
  # calculate all the phi(j|y) and smooth it
  op <- p_course_GPA_multi[(p_course_GPA_multi$grade == x$Course_GPA & p_course_GPA_multi$finalGrade == y),'Freq']+1
  op <- op/n
  oc <- p_course_multi[(p_course_multi$course == x$CoursePlaced & p_course_multi$finalGrade == y),'Freq']+1
  oc <- oc/n
  os <- p_semester_multi[(p_semester_multi$semester == x$Math213Semester & p_semester_multi$finalGrade == y),'Freq']+1
  os <- os/n
  om <- p_major_multi[(p_major_multi$major == x$Major & p_major_multi$finalGrade == y),'Freq']+1
  om <- om/n
  
  # return the produtory
  prod(op, oc, os, om)
  
}
####### /function @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@


cat("The final Grade in Calc I of the student is Probable: \n\n")

# compute each probability of x given each y
label_grades = c("A","A-","B+","B","B-","C+","C","C-","D+","D", "D-","F")
pxy_multi_p <- data.frame()
pxy_multi_aux <- data.frame(A=0, Aminus=0, Bplus=0, B=0, Bminus=0, Cplus=0, C=0, Cminus=0, Dplus=0, D=0, Dminus=0, F=0)
n_row_test_data <- nrow(test_data)

for (i in seq_len(n_row_test_data)) {
  for (j in (1:12)) {
    pxy_multi_aux[1,j] <- pxy_multi(test_data[i,], label_grades[j])
  }
  pxy_multi_p <- rbind(pxy_multi_p, pxy_multi_aux)
}

# compute the probability of y (final grade) given x (major, semester, course and course grade)
# evidence
d <- pxy_multi_p[1]*oy_multi$Freq[1]
for (i in (2:12)) {
  d <- d + pxy_multi_p[i]*oy_multi$Freq[i]
}

pyx_multi <- data.frame(A=rep(x=0, times=n_row_test_data),Aminus=rep(x=0, times=n_row_test_data),Bplus=rep(x=0, times=n_row_test_data),B=rep(x=0, times=n_row_test_data),Bminus=rep(x=0, times=n_row_test_data),Cplus=rep(x=0, times=n_row_test_data),C=rep(x=0, times=n_row_test_data),Cminus=rep(x=0, times=n_row_test_data),Dplus=rep(x=0, times=n_row_test_data),D=rep(x=0, times=n_row_test_data),Dminus=rep(x=0, times=n_row_test_data),F=rep(x=0, times=n_row_test_data))
for (i in (1:12)) {
  pyx_multi[i] <- pxy_multi_p[i]*oy_multi$Freq[i]/d
}

pyx_multi$FinalGrade <- factor(x=max.col(pyx_multi), levels=c(1,2,3,4,5,6,7,8,9,10,11,12), labels=c("A","A-","B+","B","B-","C+","C","C-","D+","D", "D-","F"))


# add the prediction to the test data and calculate the difference to the real output
test_data$FinalGrade  <- pyx_multi$FinalGrade
test_data$Diff <- as.numeric(test_data$Math213) - as.numeric(test_data$FinalGrade)
print((test_data))

# calculate how good is my prediction
cat("\nAverage Error: ", sum(abs(test_data$Diff))/sum(test_data$Diff != 0))

# use this in the prediction/ make unique rows
p_finalPrediction <-data.frame(table(finalGrade=test_data$CoursePlaced,diff=test_data$Diff))
m = length(test_data$Math213)

#print(p_finalPrediction)
predictionAccurance_exact <- sum(p_finalPrediction[p_finalPrediction$diff %in% c(0),"Freq"])/m
predictionAccurance_oneplaceDiff <- sum(p_finalPrediction[p_finalPrediction$diff %in% c(-1,0,1),"Freq"])/m
cat("\nHow good is the exactly prediction: ", predictionAccurance_exact)
cat("\nHow good is the prediction with one class admission error: ", predictionAccurance_oneplaceDiff)

