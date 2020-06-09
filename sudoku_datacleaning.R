library(tidyverse)



setwd("/users/students/19250281/project/")

# sudoku<-read.csv("3SudokuCombined.csv",stringsAsFactors = F)

sudoku<-read.csv("3SudokuCombined.csv", header=T, na.strings=c("","NA"),stringsAsFactors = F)

sudoku<-as_tibble(sudoku)


# Hypothesis
# 1. Does the type of Sudoku puzzle affect ability to complete it correctly? 
# 2. Does the type of Sudoku puzzle affect the time it takes to complete it?
# 1.Does Sudoku type affect ability to get the Sudoku correct? 
# 2.Does Sudoku experience affect ability to get the Sudoku correct?
# 3.Does Sudoku type affect the time it takes to complete the Sudoku?
# 4.Does Sudokuexperience affect the time it takes to complete the Sudoku?

# Data Cleaning (#before,type,correct,sec )###############################

#previous_exp(before1) and correct changed to factor and Time2(total time in sec o numeric)

sudoku$Before1<-as.factor(sudoku$Before1)
sudoku$Correct<-as.factor(sudoku$Correct)
sudoku$Time2<-suppressWarnings(as.numeric(sudoku$Time2))

table(sudoku$Type)

# 1. Cleaning Type

sudoku$Type<-ifelse(sudoku$Type=="Letter","Letters",sudoku$Type)
sudoku$Type<-ifelse(sudoku$Type=="Number","Numbers",sudoku$Type)
sudoku$Type<-ifelse(sudoku$Type=="Symbol","Symbols",sudoku$Type)

sudoku$Type<-factor(sudoku$Type)

table(sudoku$Type)

#2 Clening before2


table(sudoku$Before2)


sudoku$Before2<-ifelse(sudoku$Before2=="Inside","Within_3",sudoku$Before2)
sudoku$Before2<-ifelse(sudoku$Before2=="Outside","Out_3",sudoku$Before2)

# factor(sudoku$Before2)


## Filling missing values of before1 using before2 details

sudoku%>%
  mutate(Before1=replace(Before1,
                         is.na(Before1)&!is.na(Before2)&Before2=='No',
                         'No')
  )->sudoku     

sudoku%>%
  mutate(Before1=replace(Before1,
                         is.na(Before1)&!is.na(Before2)&Before2!='No',
                         'Yes')
  )->sudoku 


table(sudoku$Before1)
table(sudoku$Before2)


#### Checking if missing null values in Time2 can be filled using Time1:Min:Sec

sudoku%>%filter(is.na(Time2),!is.na(Time1),!is.na(Mins),!is.na(Seconds))

# As it returns zero rows so Time1,Mins and Seconds are worthless columns so dropping it.

sudoku%>%select(-Time1,-Mins,-Seconds)->sudoku

glimpse(sudoku)


#checking if there is any column filled with value "."

lapply(sudoku,function(x)which((x==".")==T))

sudoku[c(1302,1304,1499,1533,1548),]

glimpse(sudoku)