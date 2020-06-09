setwd("C:/Users/Olivia/Documents/Uni Basel/FS 20/Soziologie")
getwd()
StudentsPerformance <- read.csv("StudentsPerformance.csv")
StudentsPerformance <- read.csv("StudentsPerformance.csv", header=TRUE, sep=";", fill=TRUE, quote="")
View(StudentsPerformance)

library("tidyverse")
library("stats")

## 1. Aufgabe

# a.) Siehe Block

# b.) 

boxplot(StudentsPerformance$math_score, main = "1000 Students", ylab = "math score")
# Der Median liegt zwischen 60 und 80 Punkten, vermutlich bei ca. 67 Punkten. Es gibt Ausreisser nach unten. 50% der Schüler, 1. bis 3. Quartil, haben zwischen 55 und 75 Punkte erreicht.

# c.) 

mean(StudentsPerformance$math_score)
mean(StudentsPerformance$writing_score)
mean(StudentsPerformance$reading_score)

a <- sum(StudentsPerformance$math_score)
b <- sum(StudentsPerformance$writing_score)
c <- sum(StudentsPerformance$reading_score)

d <- a+b+c
e <- d/3

## 2. Aufgabe

# a.) 

Regression <- subset(StudentsPerformance, select = c(reading_score, writing_score))
modell <- lm(reading_score ~ writing_score, Regression)
summary(modell)

# Intercept von 6.75 und Regressionskoeffizient von 0.92. Wenn der Reading score 0 betragen würde, wäre der writing score bei 6.75. Wenn sich der reading score um 1% ändert, ändert sich der writing score um fast ein Prozent (0.92). Damit wird 91% der Varianz erklärt (R-Squared), es liegt also ein sehr starker Zusammenhang vor.

# b.) 

plot(Regression$reading_score, Regression$writing_score, main = "Title", xlab = "reading score", ylab = "writing score")
abline(modell, col = "red")

# Der Zusammenhang ist positiv und linear. 

# c.) 

## 3. Aufgabe

# a.) 

Prep <- StudentsPerformance %>% 
  filter(test_preparation_course == "completed") %>% 
  sample_n(300)

NoPrep <- StudentsPerformance %>% 
  filter(test_preparation_course == "none") %>% 
  sample_n(300)


