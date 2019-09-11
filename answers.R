# Names:
# Sneha Choudhary
# Suchetha
# Vidhi Gupta
# Siddharth Suresh


# Libraries used 
install.packages('datetime')
install.packages("stringr")
install.packages("caTools")
library('stringr')
library('caTools')
library('datetime')
library('MASS')
library('olsrr')

# sanitized dataset for the answers: The dataset was cleaned and processed and saved as answers.csv. This file was imported for regression analysis.

data_model1 <- read.csv('answers.csv')


#------ Model 1 -------

# Regressing Answer Score on Question Score, Comment count, time taken for answer to be posted, Question length, Answer Length and a few question tags {Machine Learning, Regression, Neural Networks, Correlation, Standard Deviation, Bayesian}
mod_lm1 <-  lm(data_model1$AnswerScore ~ data_model1$QuestionScore+data_model1$CommentCount+data_model1$timeforAnswer+data_model1$QuestionLength+data_model1$AnswerLength+data_model1$HasMLTag+data_model1$HasRegressionTag+data_model1$HasNNTag+data_model1$HasCorrelationTag+data_model1$HasSDTag+data_model1$HasBayesianTag)
summary(mod_lm1)
# Apart from the coefficients for tags - Correlation, Standard Deviation and Bayesian, all other regressors are significant at the 95% level

extern_s_resids_1 <- studres(mod_lm1) # externally studentized residuals
qqstuff_1 <- qqnorm(extern_s_resids_1) # cannot be considered a normally distributed plot as the Q-Q plot contains fat tails and skewness
qqline(extern_s_resids_1) 
plot(fitted.values(mod_lm1), extern_s_resids_1) # The model exhibits heteroskedasticity
AIC(mod_lm1) # AIC value = 988,660.7

#------ Model 2 -------

# Adding the time related regressors in model 1 like month/date/time of the question posted, month/date/time of the answer posted
# Added categorical variables for month (holiday month & non-holiday month), date (first half & second half oa a month) and time(Day time - 5:00 am to 11:00 pm & Night time - 11:00 pm to 5:00 am)
mod_lm2 <-  lm(data_model1$AnswerScore ~ data_model1$QuestionMonth+data_model1$QuestionDate+data_model1$QuestionMonth_cat+data_model1$QuestionDate_cat+data_model1$QuestionTime_cat+data_model1$AnswerMonth+data_model1$AnswerDate+data_model1$AnswerTime_cat+data_model1$QuestionScore+data_model1$CommentCount+data_model1$timeforAnswer+data_model1$QuestionLength+data_model1$AnswerLength+data_model1$HasMLTag+data_model1$HasRegressionTag+data_model1$HasNNTag+data_model1$HasCorrelationTag+data_model1$HasSDTag+data_model1$HasBayesianTag)
summary(mod_lm2)

extern_s_resids_2 <- studres(mod_lm2) # externally studentized residuals
qqstuff_2 <- qqnorm(extern_s_resids_2) # cannot be considered a normally distributed plot as the Q-Q plot contains fat tails and skewness
qqline(extern_s_resids_2)
plot(fitted.values(mod_lm2), extern_s_resids_2) # The model exhibits heteroskedasticity
AIC(mod_lm2) # AIC value = 988,543.2

#----- Model 3 ------

# Translating y to (y+1-min(y)). This is done in order to facilitate transformations of the response variable (which contains negative values)
min_score <-  min(data_model1$AnswerScore) # calculating the minimum value of y (the most negative value)

data_model1$Trans_score <-  data_model1$AnswerScore + 1 - min_score # Using this translation, the minimum value of y will get translated to 1

# We use the translated y to regress over all regressors from model 2
mod_lm3 <-  lm(data_model1$Trans_score ~ data_model1$QuestionMonth+data_model1$QuestionDate+ data_model1$QuestionMonth_cat+data_model1$QuestionDate_cat+data_model1$QuestionTime_cat+data_model1$AnswerMonth+data_model1$timeforAnswer+data_model1$AnswerDate+data_model1$AnswerTime_cat+data_model1$QuestionScore+data_model1$CommentCount+data_model1$QuestionLength+data_model1$AnswerLength+data_model1$HasMLTag+data_model1$HasRegressionTag+data_model1$HasNNTag+data_model1$HasCorrelationTag+data_model1$HasSDTag+data_model1$HasBayesianTag)
summary(mod_lm3)

extern_s_resids_3 <- studres(mod_lm3) # externally studentized residuals
qqstuff_3 <- qqnorm(extern_s_resids_3) # cannot be considered a normally distributed plot as the Q-Q plot contains fat tails and skewness
qqline(extern_s_resids_3)
plot(fitted.values(mod_lm3), extern_s_resids_3) # The model exhibits heteroskedasticity
AIC(mod_lm3) # AIC value = 988,543.2

#----------- Model 4 ----------

# considering only significant regressors and discarding other regressors
mod_lm4 <- lm(data_model1$Trans_score ~ data_model1$QuestionDate+data_model1$timeforAnswer+data_model1$QuestionScore +data_model1$CommentCount +data_model1$QuestionLength +data_model1$AnswerLength+data_model1$HasMLTag+data_model1$HasRegressionTag+data_model1$HasNNTag)
summary(mod_lm4)

extern_s_resids_4 <- studres(mod_lm4) # externally studentized residuals
qqstuff_4 <- qqnorm(extern_s_resids_4) # cannot be considered a normally distributed plot as the Q-Q plot contains fat tails and skewness which indicates that the model is not a normal distributed model
qqline(extern_s_resids_4)
plot(fitted.values(mod_lm4), extern_s_resids_4) # The model exhibits heteroskedasticity
AIC(mod_lm4) # AIC value = 988,652.9

#------------ Model 5 ----------

#Box Cox transformation on the translated y variable
boxcox_mlr <- boxcox(mod_lm4, data = data_model1)
lambda <- boxcox_mlr$x[boxcox_mlr$y == max(boxcox_mlr$y)]
data_model1$Trans_score2 <- (data_model1$Trans_score^lambda - 1)/lambda

mod_lm5 <- lm(data_model1$Trans_score2 ~ data_model1$QuestionDate+data_model1$timeforAnswer+data_model1$QuestionScore +data_model1$CommentCount +data_model1$QuestionLength +data_model1$AnswerLength+data_model1$HasMLTag+data_model1$HasRegressionTag+data_model1$HasNNTag)
summary(mod_lm5)

extern_s_resids_5 <- studres(mod_lm5)
qqstuff_5 <- qqnorm(extern_s_resids_5) # after transforming y using box-cox, the Q-Q plot seems to improve
qqline(extern_s_resids_5)
plot(fitted.values(mod_lm5), extern_s_resids_5) # Model stills exhibits heteroskedasticity, however the plot seems to have some improvement
AIC(mod_lm5) # AIC value = -698198.4

#------------ Model 6 ------------

#Transforming regressors using log transformations
data_model1$Log_time_answer <- log(data_model1$timeforAnswer)
data_model1$Log_question_length <- log(data_model1$QuestionLength)
data_model1$Log_answer_length <- log(data_model1$AnswerLength)

mod_lm6 <- lm(data_model1$Trans_score2 ~ data_model1$QuestionDate + data_model1$Log_time_answer+data_model1$QuestionScore+data_model1$CommentCount+data_model1$Log_question_length +data_model1$Log_answer_length+data_model1$HasMLTag+data_model1$HasNNTag)
summary(mod_lm6)

extern_s_resids_6 <- studres(mod_lm6)
qqstuff_6 <- qqnorm(extern_s_resids_6) # after transforming y using box-cox, the Q-Q plot seems to improve
qqline(extern_s_resids_6)
plot(fitted.values(mod_lm6), extern_s_resids_6) # Model stills exhibits heteroskedasticity, however the plot seems to have some improvement
AIC(mod_lm6) # AIC value = -698730.5

#-----------  Model 7 -------------

# Removing question length from the regressors since it does not seem practical to include it in a regression where the response variable is the answer score
mod_lm7 <- lm(data_model1$Trans_score2 ~ data_model1$Log_time_answer+data_model1$QuestionScore+data_model1$CommentCount+data_model1$Log_answer_length+data_model1$HasMLTag +data_model1$HasNNTag)
summary(mod_lm7)

extern_s_resids_7 <- studres(mod_lm7)
qqstuff_7 <- qqnorm(extern_s_resids_7) # the Q-Q plot seems to be fine for this dataset
qqline(extern_s_resids_7)
plot(fitted.values(mod_lm7), extern_s_resids_7) # Model is quite close to exhibit homoskedasticity
AIC(mod_lm7) # AIC value  -697637.5

#------- Prediction of Final Model -----------
set.seed(101)            #This is used to create same samples everytime

split1=sample.split(data_model1$Trans_score2,SplitRatio=2/3)

train=subset(data_model1,split1==TRUE)

test=subset(data_model1,split1==FALSE)
model_train <- lm(train$Trans_score2 ~ train$Log_time_answer+train$QuestionScore+train$CommentCount +train$Log_answer_length+train$HasMLTag+train$HasRandomTag +train$HasNNTag)

prediction <- predict(model_train, test, interval = "prediction")

actuals_preds <- data.frame(cbind(actuals=test$Trans_score2, predicteds=prediction)) 

correlation_accuracy <- cor(actuals_preds)

min_max_accuracy <- mean(apply(actuals_preds, 1, min) / apply(actuals_preds, 1, max)) 

cooks_distances_612 <- cooks.distance(mod_lm7)
plot(mod_lm7, which=5)
#This plot shows that there are no influential points in the dataset, since there are no points lying beyond a threshold of 1.

