setwd("C:/Users/Zouzi Qi/Desktop/Capstone Team 1")

rm(list=ls())
#install.packages("readxl")
library(readxl)
library(tidyverse)
library(randomForest)
library(tidyr)
#library(ranger)
cate_gpa <- read.csv("dei_survey_Final_v1.csv",na.strings = c(" ","","I donât know","0"))   #这边会有一个G_outcome =0.5的问题！！！！
#check NA in Each Column
sapply(cate_gpa, function(y) sum(((is.na(y)))))
#Delete row when cate_gpa$gpa is NA
cate_gpa_clear <- cate_gpa[!is.na(cate_gpa$gpa),]
#check NA in gpa column
sapply(cate_gpa_clear, function(y) sum(((is.na(y)))))

#fill the column when NA will assigned "No_Response"




#check data types 
str(cate_gpa_clear)


# Each column to fill: Get Median Number 
#grade
median_grade <- median(cate_gpa_clear$ï..grade,na.rm = T)
cate_gpa_clear$ï..grade[is.na(cate_gpa_clear$ï..grade)] <- median_grade 
#gender
median_gender <- median(cate_gpa_clear$gender,na.rm = T)
cate_gpa_clear$gender[is.na(cate_gpa_clear$gender)] <- median_gender 


#mode 众数 build a function 
v <- 0
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

# categorical variable
#transgender
result_transgender <- getmode(cate_gpa_clear$transgender)
print(result_transgender) # No 
cate_gpa_clear$transgender[is.na(cate_gpa_clear$transgender)] <- result_transgender

#sexual_orientation
result_sexual_orientation <- getmode(cate_gpa_clear$sexual_orientation)
print(result_sexual_orientation) # NA   #是否改为 No_Response
cate_gpa_clear$sexual_orientation[is.na(cate_gpa_clear$sexual_orientation)] <- getmode(cate_gpa_clear$sexual_orientation)

#religion
result_religion <- getmode(cate_gpa_clear$religion)
print(result_religion) # Christian 
cate_gpa_clear$religion[is.na(cate_gpa_clear$religion)] <- result_religion

#ethnicity
result_ethnicity <- getmode(cate_gpa_clear$ethnicity)
print(result_ethnicity) # White 
cate_gpa_clear$ethnicity[is.na(cate_gpa_clear$ethnicity)] <- result_ethnicity

#physical_disability 
result_physical_disability  <- getmode(cate_gpa_clear$physical_disability )
print(result_physical_disability) # NA
cate_gpa_clear$physical_disabilit[is.na(cate_gpa_clear$physical_disabilit)] <- result_physical_disability

#learning_disability 
result_learning_disability   <- getmode(cate_gpa_clear$learning_disability  )
print(result_learning_disability) # NA
cate_gpa_clear$learning_disability [is.na(cate_gpa_clear$learning_disability )] <- result_learning_disability

#parent_guardian 
result_parent_guardian    <- getmode(cate_gpa_clear$parent_guardian   )
print(result_parent_guardian) # Parents
cate_gpa_clear$parent_guardian  [is.na(cate_gpa_clear$parent_guardian  )] <- result_parent_guardian

#guardian_degree 
result_guardian_degree     <- getmode(cate_gpa_clear$guardian_degree    )
print(result_guardian_degree) # NA
cate_gpa_clear$guardian_degree   [is.na(cate_gpa_clear$guardian_degree   )] <- result_guardian_degree

#language_at_home_number 
result_language_at_home_number     <- getmode(cate_gpa_clear$language_at_home_number    )
print(result_language_at_home_number) # 1
cate_gpa_clear$language_at_home_number   [is.na(cate_gpa_clear$language_at_home_number   )] <- result_language_at_home_number

#book_in_home_number 
result_book_in_home_number     <- getmode(cate_gpa_clear$book_in_home_number    )
print(result_book_in_home_number ) # 1
cate_gpa_clear$book_in_home_number   [is.na(cate_gpa_clear$book_in_home_number   )] <- result_book_in_home_number 

#book_read_number 
result_book_read_number     <- getmode(cate_gpa_clear$book_read_number    )
print(result_book_read_number ) # NA
cate_gpa_clear$book_read_number   [is.na(cate_gpa_clear$book_read_number   )] <- result_book_read_number 

#computer_at_home_number 
result_computer_at_home_number      <- getmode(cate_gpa_clear$computer_at_home_number     )
print(result_computer_at_home_number  ) # Yes, three or more
cate_gpa_clear$computer_at_home_number    [is.na(cate_gpa_clear$computer_at_home_number    )] <- result_computer_at_home_number  



##
# dei missing value fill

#dei_01
cate_gpa_clear$dei_01[is.na(cate_gpa_clear$dei_01)] <-median(cate_gpa_clear$dei_01,na.rm = T) # 4 
#dei_02
cate_gpa_clear$dei_02[is.na(cate_gpa_clear$dei_02)] <-median(cate_gpa_clear$dei_02,na.rm = T) # 5 
#dei_03
cate_gpa_clear$dei_03[is.na(cate_gpa_clear$dei_03)] <-median(cate_gpa_clear$dei_03,na.rm = T) # 4 
#dei_04
cate_gpa_clear$dei_04[is.na(cate_gpa_clear$dei_04)] <-median(cate_gpa_clear$dei_04,na.rm = T) # 5
#dei_fair_to_boys    #NA 1611 较多

#G_School Climate for Diversity, Equity, and Inclusion	#4.73 （01、02、03、04  all filled so just sum/4
cate_gpa_clear$G_School.Climate.for.Diversity..Equity..and.Inclusion... <- (cate_gpa_clear$dei_01+cate_gpa_clear$dei_02+cate_gpa_clear$dei_03+cate_gpa_clear$dei_04)/4 
#cate_gpa_clear$G_School.Climate.for.Diversity..Equity..and.Inclusion...[is.na(cate_gpa_clear$G_School.Climate.for.Diversity..Equity..and.Inclusion...)] <-median(cate_gpa_clear$G_School.Climate.for.Diversity..Equity..and.Inclusion...,na.rm = T) # 4.73

#dei_05
cate_gpa_clear$dei_05[is.na(cate_gpa_clear$dei_05)] <-median(cate_gpa_clear$dei_05,na.rm = T) # 4
#dei_06
cate_gpa_clear$dei_06[is.na(cate_gpa_clear$dei_06)] <-median(cate_gpa_clear$dei_06,na.rm = T) # 4
#dei_07
cate_gpa_clear$dei_07[is.na(cate_gpa_clear$dei_07)] <-median(cate_gpa_clear$dei_07,na.rm = T) # 5
#dei_10
cate_gpa_clear$dei_10[is.na(cate_gpa_clear$dei_10)] <-median(cate_gpa_clear$dei_10,na.rm = T) # 4


#G_School Climate Overall	
cate_gpa_clear$G_School.Climate.Overall...<- (cate_gpa_clear$dei_05+cate_gpa_clear$dei_06+cate_gpa_clear$dei_07+cate_gpa_clear$dei_10)/4 
#cate_gpa_clear$G_School.Climate.Overall...[is.na(cate_gpa_clear$G_School.Climate.Overall...)] <-median(cate_gpa_clear$G_School.Climate.Overall...,na.rm = T) # 4.73



#dei_12
cate_gpa_clear$dei_12[is.na(cate_gpa_clear$dei_12)] <-median(cate_gpa_clear$dei_12,na.rm = T) # 3 
#dei_13
cate_gpa_clear$dei_13[is.na(cate_gpa_clear$dei_13)] <-median(cate_gpa_clear$dei_13,na.rm = T) # 3
#dei_16
cate_gpa_clear$dei_16[is.na(cate_gpa_clear$dei_16)] <-median(cate_gpa_clear$dei_16,na.rm = T) # 3
#dei_17
cate_gpa_clear$dei_17[is.na(cate_gpa_clear$dei_17)] <-median(cate_gpa_clear$dei_17,na.rm = T) # 3
#dei_18
cate_gpa_clear$dei_18[is.na(cate_gpa_clear$dei_18)] <-median(cate_gpa_clear$dei_18,na.rm = T) # 4
#dei_21
cate_gpa_clear$dei_21[is.na(cate_gpa_clear$dei_21)] <-median(cate_gpa_clear$dei_21,na.rm = T) # 3
#dei_75
cate_gpa_clear$dei_75[is.na(cate_gpa_clear$dei_75)] <-median(cate_gpa_clear$dei_75,na.rm = T) # 3

print(median(cate_gpa_clear$dei_75,na.rm = T))
#G_Classroom Teaching Supporting Diversity, Equity, and Inclusion	
cate_gpa_clear$G_Classroom.Teaching.Supporting.Diversity..Equity..and.Inclusion...... <- (cate_gpa_clear$dei_12+cate_gpa_clear$dei_13+cate_gpa_clear$dei_16+cate_gpa_clear$dei_17+cate_gpa_clear$dei_18+cate_gpa_clear$dei_21+cate_gpa_clear$dei_75)/7 



#dei_24
cate_gpa_clear$dei_24[is.na(cate_gpa_clear$dei_24)] <-median(cate_gpa_clear$dei_24,na.rm = T) # 4
#dei_25   
cate_gpa_clear$dei_25[is.na(cate_gpa_clear$dei_25)] <-median(cate_gpa_clear$dei_25,na.rm = T) # 4
#dei_26
cate_gpa_clear$dei_26[is.na(cate_gpa_clear$dei_26)] <-median(cate_gpa_clear$dei_26,na.rm = T) # 4

# G_Co-curricular Activities Supporting Diversity, Equity, and Inclusion	
cate_gpa_clear$G_Co.curricular.Activities.Supporting.Diversity..Equity..and.Inclusion.. <- (cate_gpa_clear$dei_24+cate_gpa_clear$dei_25+cate_gpa_clear$dei_26)/3



#dei_27
cate_gpa_clear$dei_27[is.na(cate_gpa_clear$dei_27)] <-median(cate_gpa_clear$dei_27,na.rm = T) # 4
#dei_28
cate_gpa_clear$dei_28[is.na(cate_gpa_clear$dei_28)] <-median(cate_gpa_clear$dei_28,na.rm = T) # 4
#dei_29
cate_gpa_clear$dei_29[is.na(cate_gpa_clear$dei_29)] <-median(cate_gpa_clear$dei_29,na.rm = T) # 4
# G_School Commitment to Diversity, Equity, and Inclusion
cate_gpa_clear$G_School.Commitment.to.Diversity..Equity..and.Inclusion <-(cate_gpa_clear$dei_27+cate_gpa_clear$dei_28+cate_gpa_clear$dei_29)/3




#dei_30
cate_gpa_clear$dei_30[is.na(cate_gpa_clear$dei_30)] <-median(cate_gpa_clear$dei_30,na.rm = T) # 2 
#dei_31
cate_gpa_clear$dei_31[is.na(cate_gpa_clear$dei_31)] <-median(cate_gpa_clear$dei_31,na.rm = T) # 2
#dei_32
cate_gpa_clear$dei_32[is.na(cate_gpa_clear$dei_32)] <-median(cate_gpa_clear$dei_32,na.rm = T) # 2
#dei_35
cate_gpa_clear$dei_35[is.na(cate_gpa_clear$dei_35)] <-median(cate_gpa_clear$dei_35,na.rm = T) # 2
#dei_36
cate_gpa_clear$dei_36[is.na(cate_gpa_clear$dei_36)] <-median(cate_gpa_clear$dei_36,na.rm = T) # 1
#dei_37
cate_gpa_clear$dei_37[is.na(cate_gpa_clear$dei_37)] <-median(cate_gpa_clear$dei_37,na.rm = T) # 1
# G_Everyday Discrimination by Students	
cate_gpa_clear$G_Everyday.Discrimination.by.Students..... <-(cate_gpa_clear$dei_30+cate_gpa_clear$dei_31+cate_gpa_clear$dei_32+cate_gpa_clear$dei_35+cate_gpa_clear$dei_36+cate_gpa_clear$dei_37)/6


###

#dei_48  ########youwenti 
cate_gpa_clear$dei_48[is.na(cate_gpa_clear$dei_48)] <-getmode(cate_gpa_clear$dei_48[!is.na(cate_gpa_clear$dei_48)] )
#dei_49
cate_gpa_clear$dei_49[is.na(cate_gpa_clear$dei_49)] <-median(cate_gpa_clear$dei_49,na.rm = T) # 1
#dei_50
cate_gpa_clear$dei_50[is.na(cate_gpa_clear$dei_50)] <-median(cate_gpa_clear$dei_50,na.rm = T) # 1
#dei_53
cate_gpa_clear$dei_53[is.na(cate_gpa_clear$dei_53)] <-median(cate_gpa_clear$dei_53,na.rm = T) # 1
#dei_54
cate_gpa_clear$dei_54[is.na(cate_gpa_clear$dei_54)] <-median(cate_gpa_clear$dei_54,na.rm = T) # 1
#dei_55
cate_gpa_clear$dei_55[is.na(cate_gpa_clear$dei_55)] <-median(cate_gpa_clear$dei_55,na.rm = T) # 1
#G_Everyday Discrimination by Teachers	
cate_gpa_clear$G_Everyday.Discrimination.by.Teachers..... <-(cate_gpa_clear$dei_48+cate_gpa_clear$dei_49+cate_gpa_clear$dei_50+cate_gpa_clear$dei_53+cate_gpa_clear$dei_54+cate_gpa_clear$dei_55)/6





#dei_72
cate_gpa_clear$dei_72[is.na(cate_gpa_clear$dei_72)] <-median(cate_gpa_clear$dei_72,na.rm = T) # 4
#dei_73
cate_gpa_clear$dei_73[is.na(cate_gpa_clear$dei_73)] <-median(cate_gpa_clear$dei_73,na.rm = T) # 4
#dei_74
cate_gpa_clear$dei_74[is.na(cate_gpa_clear$dei_74)] <-median(cate_gpa_clear$dei_74,na.rm = T) # 4
#dei_76
cate_gpa_clear$dei_76[is.na(cate_gpa_clear$dei_76)] <-median(cate_gpa_clear$dei_76,na.rm = T) # 4
#G_Outcomes		
cate_gpa_clear$G_Outcomes..... <- (cate_gpa_clear$dei_72+cate_gpa_clear$dei_73+cate_gpa_clear$dei_74+cate_gpa_clear$dei_76)/4




#dei_80
cate_gpa_clear$dei_80[is.na(cate_gpa_clear$dei_80)] <-median(cate_gpa_clear$dei_80,na.rm = T) # 4
#dei_81
cate_gpa_clear$dei_81[is.na(cate_gpa_clear$dei_81)] <-median(cate_gpa_clear$dei_81,na.rm = T) # 4
#dei_82
cate_gpa_clear$dei_82[is.na(cate_gpa_clear$dei_82)] <-median(cate_gpa_clear$dei_82,na.rm = T) # 4
#dei_83
cate_gpa_clear$dei_83[is.na(cate_gpa_clear$dei_83)] <-median(cate_gpa_clear$dei_83,na.rm = T) # 4
#dei_84
cate_gpa_clear$dei_84[is.na(cate_gpa_clear$dei_84)] <-median(cate_gpa_clear$dei_84,na.rm = T) # 4
# G_Meaningful Interactions Across Difference		
cate_gpa_clear$G_Meaningful.Interactions.Across.Difference.... <- (cate_gpa_clear$dei_80+cate_gpa_clear$dei_81+cate_gpa_clear$dei_82+cate_gpa_clear$dei_83+cate_gpa_clear$dei_84)/5


# 目前NA 的有physical_disabilit book_read_number guardian_degree learning_disability physical_disability sexual_orientation 
# All NA transfer
cate_gpa_clear[is.na(cate_gpa_clear)] = "No_Respnonse/Missing"




# output 
write.csv(cate_gpa_clear,file = "cate_gpa_clear_test.csv")





# read clean csv

dei_cate_clear <- read.csv("cate_gpa_clear_test.csv",stringsAsFactors = TRUE)
dei_cate_clear <- dei_cate_clear[,-1]
sapply(dei_cate_clear, function(y) sum(((is.na(y)))))
str(dei_cate_clear)
dim(dei_cate_clear)


# change variable to factors

dei_cate_clear$gpa <- as.factor(dei_cate_clear$gpa )
dei_cate_clear$district <- as.factor(dei_cate_clear$district)
dei_cate_clear$ï..grade <- as.factor(dei_cate_clear$ï..grade)
str(dei_cate_clear)

# split for train and test
set.seed(2016)
trainIndex <- createDataPartition(dei_cate_clear$gpa,  
                                  p = .8,   
                                  list = FALSE,
                                  times = 1)
dei_cateTraining <- dei_cate_clear[trainIndex[, 1], ]  #training 
dei_cateTesting <- dei_cate_clear[-trainIndex[, 1], ]  #testing

class(dei_cateTraining)
str(dei_cateTraining)
#random forest for cate_gpa
mtry <- c(9,15,69 )


# for all
model_all <- randomForest(gpa~., data=dei_cateTraining,  mtry=sqrt(69), importance = TRUE)
varImpPlot(model_all)
yhat<-predict(model_all, dei_cateTesting, type='response')
print(yhat)
yhat <- as.numeric(yhat)
dei_cateTesting$gpa <- as.numeric(dei_cateTesting$gpa)
a <- cbind(yhat,dei_cateTesting$gpa)
write.csv(a,"a.csv")
mean((yhat-dei_cateTesting$gpa)^2) 





#only cate and gpa
only_cate_gpa <- dei_cate_clear[,1:16]
set.seed(2016)
trainIndex <- createDataPartition(only_cate_gpa$gpa,  
                                  p = .8,   
                                  list = FALSE,
                                  times = 1)
only_cateTraining <- only_cate_gpa[trainIndex[, 1], ]  #training 
only_cateTesting <- only_cate_gpa[-trainIndex[, 1], ]  #testing
model_cate <- randomForest(gpa~., data=only_cateTraining,  mtry=sqrt(15), importance = TRUE)
varImpPlot(model_cate)
yhat<-predict(model_cate, only_cateTesting, type='response')
print(yhat)
yhat <- as.numeric(yhat)
only_cateTesting$gpa <- as.numeric(only_cateTesting$gpa)
mean((yhat-only_cateTesting$gpa)^2) 
b <- cbind(yhat,only_cateTesting$gpa)

write.csv(b,"b.csv")
mean((yhat-dei_cateTesting$gpa)^2) 



# only G and gpa

only_G_gpa <- dei_cate_clear[,c(16,22,27,36,40,44,51,58,63,69)]
set.seed(2016)
trainIndex <- createDataPartition(only_G_gpa$gpa,  
                                  p = .8,   
                                  list = FALSE,
                                  times = 1)
only_GTraining <- only_G_gpa[trainIndex[, 1], ]  #training 
only_GTesting <- only_G_gpa[-trainIndex[, 1], ]  #testing
model_G <- randomForest(gpa~., data=only_GTraining,  mtry=sqrt(9), importance = TRUE)
varImpPlot(model_G)
yhat<-predict(model_G, only_GTesting, type='response')
print(yhat)
yhat <- as.numeric(yhat)
only_GTesting$gpa <- as.numeric(only_GTesting$gpa)
mean((yhat-only_GTesting$gpa)^2) 
c <- cbind(yhat,only_GTesting$gpa)

write.csv(c,"c.csv")









