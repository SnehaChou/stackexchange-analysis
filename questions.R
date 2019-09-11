# Names:
# Sneha Choudhary
# Suchetha
# Vidhi Gupta
# Siddharth Suresh

# Context ----

## This dataset has been queried from stackexchange.com and contains non-deleted posts on their portal

## Initial Data Analysis:
### 99% of the posts were either of the type question or answer
### Extracted the questions from the dataset into a separate csv
### Drop irrelevant columns
### Top 3 domains: Regression, Machine Learning, Bayesian

## Question
### Determine the probability that the question owner gets a satisfactory answer

# Required Packages ----

install.packages('MASS')
library('MASS')

install.packages('car')
library('car')

install.packages('stringr')
library('stringr')

install.packages('bestglm')
library('bestglm')

install.packages('caret')
library('caret')

# UDFs ----

parseOutHTML <- function(s) {
  return(gsub("<.*?>", "", s))
}

# Load dataset ----
questions <- read.csv('questions-dataset.csv', stringsAsFactors = FALSE)

## Preprocess data to extract rows with no answers

### Remove missing values from AnswerCount column
questions$AnswerCount[is.na(questions$AnswerCount)] <- 0

### Extract questions that have atleast once answer
### Reason: A question does not have an answer cannot have a satisfactory answer
answeredQues <- questions[ which(questions$AnswerCount != 0), ]

### Make sure there are no na values for mean-ans-time 
#### any(is.na(answeredQues$MeanAnswerTime_Days))

# Version 1 ----

## Data Cleaning ----

### Add HasAcceptedAnswer column to indicate weather the question owner found the right answer or not
answeredQues$HasAcceptedAnswer <- as.numeric(!is.na(answeredQues$AcceptedAnswerId))

### Add PostLength column to indicate the length of the question
#### Assumption: A lengthy question is more detailed
#### remove HTML tags from the body, then count the number of words to populate the PostLength
answeredQues$PostLength_Words <- as.integer(lapply(answeredQues$Body, function(x) sapply(strsplit(parseOutHTML(x), " "), length)))

### Add IsFavourite column to indicate weather a question was ever marked favourite or not
#### FavoriteCount = 0 means that the question was marked favorite at least once and then removed from favorites
#### FavoriteCount = NA means that the question was never marked favorite
answeredQues$IsFavourite <- as.numeric(!is.na(answeredQues$FavoriteCount))

### Add dummy variable to indicate if the question belongs to machine-learning tag
answeredQues$HasMLTag <- as.integer(grepl(pattern = "machine-learning", answeredQues$Tags, ignore.case = T))

## Add dummy variable to indicate if the question belongs to regression tag
answeredQues$HasRegressionTag <- as.integer(grepl(pattern = "regression", answeredQues$Tags, ignore.case = T))

## Add dummy variable to indicate if the question belongs to bayesian tag
answeredQues$HasBayesianTag <- as.integer(grepl(pattern = "bayesian", answeredQues$Tags, ignore.case = T))

## Model Selection ----

### Create subset dataframe that only contains the response variable as `y`[bestglm requires response variable to be y] and the regressors for the full model
ansQuesSubsetDFv1 <- within(answeredQues, {
  id <- NULL
  AcceptedAnswerId <- NULL
  CreationDate <- NULL
  Body <- NULL
  LastActivityDate <- NULL
  Tags <- NULL
  FavoriteCount <- NULL
  MeanAnswerTime_Mins <- NULL
  y <- HasAcceptedAnswer   
  HasAcceptedAnswer <- NULL
})

### Perform Model Selection using AIC as criteria
aicModelSelectionsv1 <- bestglm(Xy = ansQuesSubsetDFv1,
                              family = binomial(link = "logit"), # binomial family for logit
                              IC = "AIC", # AIC chosen to select models
                              method = "exhaustive")

### Determining BestModels
aicModelSelectionsv1$BestModels

#### Best Model
##### HasAcceptedAnswer ~ Score + AnswerCount + CommentCount + PostLength_Words + IsFavourite + HasMLTag + HasRegressionTag + HasBayesianTag [AIC - 131744.4]

## Model Building ----

### Create Models
nullModel <- glm(HasAcceptedAnswer ~ 1, data = answeredQues, family = binomial(link = "logit"))
modelv1 <- glm(HasAcceptedAnswer ~ Score + AnswerCount + CommentCount + PostLength_Words + IsFavourite + HasMLTag + HasRegressionTag + HasBayesianTag, data = answeredQues, family = binomial(link = "logit"))

summary(modelv1)
### At 95% significance, based on the wald test's p-values all regressors are signficant
### Negative betas => AnswerCount, CommentCount, HasMLTag, HasRegressionTag
### Positive betas => Score, PostLength_Words, IsFavorite, HasBayesianTag
### Residual Deviance = 131720

### odds ratio
exp(cbind(OR = coef(modelv1), confint(modelv1)))
### The odds of getting an accepted ans increases with the increase in Score, PostLength_Words(length of ques), IsFavorite(weather ques is fav. or not)
### The odds of getting an accepted ans decreases with the increase in AnswerCount, CommentCount
### The odds of getting an accepted ans decreases by around 0.80-0.88 if the question has ML Tag
### The odds of getting an accepted ans decreases by around 0.88-0.94 if the question has Regression Tag
### The odds of getting an accepted ans increases by around 1.18-1.35 if the question has Bayesian Tag

anovaModelv1 <- anova(nullModel, modelv1)
### Difference in deviance - 1966.8
pvalModelv1 <- 1 - pchisq(anovaModelv1$Deviance[2], anovaModelv1$Df[2]) ## p-val ~ 0

### Multicollinearity
vif(modelv1) ## no multicollinearity

## Plots ----

plot(answeredQues$ViewCount, answeredQues$HasAcceptedAnswer, pch = 16, xlab = "Number of views", ylab = "Has Accepted Answer?")
lines(answeredQues$ViewCount, predict(modelv1, type = "response"), type = "l", col = "blue")

plot(answeredQues$Score, answeredQues$HasAcceptedAnswer, pch = 16, xlab = "Score", ylab = "Has Accepted Answer?")
lines(answeredQues$Score, predict(modelv1, type = "response"), type = "l", col = "blue")

plot(answeredQues$AnswerCount, answeredQues$HasAcceptedAnswer, pch = 16, xlab = "Number of Answers", ylab = "Has Accepted Answer?")
lines(answeredQues$AnswerCount, predict(modelv1, type = "response"), type = "l", col = "blue")

plot(answeredQues$HasMLTag, answeredQues$HasAcceptedAnswer, pch = 16, xlab = "ML Tag", ylab = "Has Accepted Answer?")
lines(answeredQues$HasMLTag, predict(modelv1, type = "response"), type = "l", col = "blue")

plot(answeredQues$HasRegressionTag, answeredQues$HasAcceptedAnswer, pch = 16, xlab = "Regression Tag", ylab = "Has Accepted Answer?")
lines(answeredQues$HasRegressionTag, predict(modelv1, type = "response"), type = "l", col = "blue")

plot(answeredQues$HasBayesianTag, answeredQues$HasAcceptedAnswer, pch = 16, xlab = "Bayesian Tag", ylab = "Has Accepted Answer?")
lines(answeredQues$HasBayesianTag, predict(modelv1, type = "response"), type = "l", col = "blue")

## Predict ----

confusionMatrix(table(predict(modelv1, type="response") >= 0.5,answeredQues$HasAcceptedAnswer == 1))

# Version 2 ----

### Trying to identify if the average time taken to answer a question impacts weather the question gets an acceptable answer

### Add MeanAnswerTime_Days column to indicate the number of days it took (on an avg) to get an answer to a question
answeredQues$MeanAnswerTime_Days <- as.integer(answeredQues$MeanAnswerTime_Mins/(24*60))

## Model Selection ----

### Create subset dataframe that only contains the response variable as `y`[bestglm requires response variable to be y] and the regressors for the full model
ansQuesSubsetDFv2 <- within(answeredQues, {
  id <- NULL
  AcceptedAnswerId <- NULL
  CreationDate <- NULL
  Body <- NULL
  LastActivityDate <- NULL
  Tags <- NULL
  FavoriteCount <- NULL
  MeanAnswerTime_Mins <- NULL
  y <- HasAcceptedAnswer   
  HasAcceptedAnswer <- NULL
})

### Perform Model Selection using AIC as criteria
aicModelSelectionsv2 <- bestglm(Xy = ansQuesSubsetDFv2,
                                family = binomial(link = "logit"), # binomial family for logit
                                IC = "AIC", # AIC chosen to select models
                                method = "exhaustive")

### Determining BestModels
aicModelSelectionsv2$BestModels

#### Best Model
##### HasAcceptedAnswer ~ Score + ViewCount + AnswerCount + CommentCount + PostLength_Words + IsFavourite + HasMLTag + HasRegressionTag + HasBayesianTag + MeanAnswerTime_Days [AIC - 129504.1]

## Model Building ----

### Create Models
modelv2 <- glm(HasAcceptedAnswer ~ Score + ViewCount + AnswerCount + CommentCount + PostLength_Words + IsFavourite + HasMLTag + HasRegressionTag + HasBayesianTag + MeanAnswerTime_Days, data = answeredQues, family = binomial(link = "logit"))

summary(modelv2)
### At 95% significance, based on the wald test's p-values ViewCount is not a significant regressor
### Negative betas => AnswerCount, CommentCount, HasMLTag, HasRegressionTag, MeanAnswerTime_Days
### Positive betas => Score, PostLength_Words, IsFavorite, HasBayesianTag
### Residual Deviance - 129475

modelv2_1 <- glm(HasAcceptedAnswer ~ Score + AnswerCount + CommentCount + PostLength_Words + IsFavourite + HasMLTag + HasRegressionTag + HasBayesianTag + MeanAnswerTime_Days, data = answeredQues, family = binomial(link = "logit"))

summary(modelv2_1)
### At 95% significance, based on the wald test's p-values all regressors are significant
### Residual Deviance - 129488
### AIC - 129505.7

### Partial F-test
anovaModelv2_1_vs_v2 <- anova(modelv2_1, modelv2)
pvalModelv2_1_vs_v2 <- 1 - pchisq(anovaModelv2_1_vs_v2$Deviance[2], anovaModelv2_1_vs_v2$Df[2]) ## p-val ~ 0.056
#### At 95% significance we cannot reject the hypothesis that ViewCount is not a significant regressor. This implies that ViewCount may or may not be a significant regressor. For now we choose to include ViewCount in our model

### odds ratio
exp(cbind(OR = coef(modelv2), confint(modelv2)))
### The odds of getting an accepted ans increases with the increase in Score, ViewCount, PostLength_Words(length of ques), IsFavorite(weather ques is fav. or not)
### The odds of getting an accepted ans decreases with the increase in AnswerCount, CommentCount
### The odds of getting an accepted ans decreases by around 0.9999 if the time taken to get an answer increases by one day [MeanAnswerTime_Days]
### The odds of getting an accepted ans decreases by around 0.81-0.88 if the question has ML Tag
### The odds of getting an accepted ans decreases by around 0.87-0.93 if the question has Regression Tag
### The odds of getting an accepted ans increases by around 1.16-1.33 if the question has Bayesian Tag

anovaModelv2 <- anova(nullModel, modelv2)
### Difference in deviance - 4212.7
pvalModelv2 <- 1 - pchisq(anovaModelv2$Deviance[2], anovaModelv2$Df[2]) ## p-val ~ 0

### Multicollinearity
vif(modelv2) ## no multicollinearity

## Plots ----

plot(answeredQues$Score, answeredQues$HasAcceptedAnswer, pch = 16, xlab = "Score", ylab = "Has Accepted Answer?")
lines(answeredQues$Score, predict(modelv2, type = "response"), type = "l", col = "blue")

plot(answeredQues$AnswerCount, answeredQues$HasAcceptedAnswer, pch = 16, xlab = "Number of Answers", ylab = "Has Accepted Answer?")
lines(answeredQues$AnswerCount, predict(modelv2, type = "response"), type = "l", col = "blue")

plot(answeredQues$HasMLTag, answeredQues$HasAcceptedAnswer, pch = 16, xlab = "ML Tag", ylab = "Has Accepted Answer?")
lines(answeredQues$HasMLTag, predict(modelv2, type = "response"), type = "l", col = "blue")

plot(answeredQues$HasRegressionTag, answeredQues$HasAcceptedAnswer, pch = 16, xlab = "Regression Tag", ylab = "Has Accepted Answer?")
lines(answeredQues$HasRegressionTag, predict(modelv2, type = "response"), type = "l", col = "blue")

plot(answeredQues$HasBayesianTag, answeredQues$HasAcceptedAnswer, pch = 16, xlab = "Bayesian Tag", ylab = "Has Accepted Answer?")
lines(answeredQues$HasBayesianTag, predict(modelv2, type = "response"), type = "l", col = "blue")

## Predict ----

confusionMatrix(table(predict(modelv2_1, type="response") >= 0.5,answeredQues$HasAcceptedAnswer == 1))

# Version 3 ----

### Trying to identify if the post being inactive for a long time has any relation with the question having an accepted answer

### Add InactiveSince_Days column to indicate the number of days since which the post is inactive
### Reference date: 07/30/2019
answeredQues$InactiveSince_Days <- as.integer(as.Date("2019-07-30") - as.Date(as.character(answeredQues$LastActivityDate), format="%m/%d/%y"))

## Model Selection ----

### Create subset dataframe that only contains the response variable as `y`[bestglm requires response variable to be y] and the regressors for the full model
ansQuesSubsetDFv3 <- within(answeredQues, {
  id <- NULL
  AcceptedAnswerId <- NULL
  CreationDate <- NULL
  Body <- NULL
  LastActivityDate <- NULL
  Tags <- NULL
  FavoriteCount <- NULL
  MeanAnswerTime_Mins <- NULL
  y <- HasAcceptedAnswer   
  HasAcceptedAnswer <- NULL
})

### Perform Model Selection using AIC as criteria
aicModelSelectionsv3 <- bestglm(Xy = ansQuesSubsetDFv3,
                                family = binomial(link = "logit"), # binomial family for logit
                                IC = "AIC", # AIC chosen to select models
                                method = "exhaustive")

### Determining BestModels
aicModelSelectionsv3$BestModels

#### Best Model
##### HasAcceptedAnswer ~ Score + ViewCount + AnswerCount + CommentCount + PostLength_Words + IsFavourite + HasMLTag + HasRegressionTag + HasBayesianTag + MeanAnswerTime_Days + InactiveSince_Days [AIC - 129215.1]

## Model Building ----

modelv3 <- glm(HasAcceptedAnswer ~ Score + ViewCount + AnswerCount + CommentCount + PostLength_Words + IsFavourite + HasMLTag + HasRegressionTag + HasBayesianTag + MeanAnswerTime_Days + InactiveSince_Days, data = answeredQues, family = binomial(link = "logit"))

summary(modelv3)
### At 95% significance, based on the wald test's p-values ViewCount is not a significant regressor but we choose to keep it in the model
### Negative betas => AnswerCount, CommentCount, HasMLTag, HasRegressionTag, MeanAnswerTime_Days
### Positive betas => Score, PostLength_Words, IsFavorite, HasBayesianTag, InactiveSince_Days
### Residual Deviance - 129193

### odds ratio
exp(cbind(OR = coef(modelv3), confint(modelv3)))
### The odds of getting an accepted ans increases with the increase in Score, PostLength_Words(length of ques), IsFavorite(weather ques is fav. or not)
### The odds of getting an accepted ans decreases with the increase in AnswerCount, CommentCount
### The odds of getting an accepted ans increases by around 1 if the number of days since which the post is inactive increases by one day (InactiveSince_Days)
### The odds of getting an accepted ans decreases by around 0.9999 if the time taken to get an answer increases by one day (MeanAnswerTime_Days)
### The odds of getting an accepted ans decreases by around 0.83-0.91 if the question has ML Tag
### The odds of getting an accepted ans decreases by around 0.87-0.93 if the question has Regression Tag
### The odds of getting an accepted ans increases by around 1.18-1.35 if the question has Bayesian Tag

anovaModelv3 <- anova(nullModel, modelv3)
### Difference in deviance - 4494.1
pvalModelv3 <- 1 - pchisq(anovaModelv3$Deviance[2], anovaModelv3$Df[2]) ## p-val ~ 0

### Multicollinearity
vif(modelv3) ## no multicollinearity

## Plots ----

plot(answeredQues$Score, answeredQues$HasAcceptedAnswer, pch = 16, xlab = "Score", ylab = "Has Accepted Answer?")
lines(answeredQues$Score, predict(modelv3, type = "response"), type = "l", col = "blue")

plot(answeredQues$AnswerCount, answeredQues$HasAcceptedAnswer, pch = 16, xlab = "Number of Answers", ylab = "Has Accepted Answer?")
lines(answeredQues$AnswerCount, predict(modelv3, type = "response"), type = "l", col = "blue")

plot(answeredQues$HasMLTag, answeredQues$HasAcceptedAnswer, pch = 16, xlab = "ML Tag", ylab = "Has Accepted Answer?")
lines(answeredQues$HasMLTag, predict(modelv3, type = "response"), type = "l", col = "blue")

plot(answeredQues$HasRegressionTag, answeredQues$HasAcceptedAnswer, pch = 16, xlab = "Regression Tag", ylab = "Has Accepted Answer?")
lines(answeredQues$HasRegressionTag, predict(modelv3, type = "response"), type = "l", col = "blue")

plot(answeredQues$HasBayesianTag, answeredQues$HasAcceptedAnswer, pch = 16, xlab = "Bayesian Tag", ylab = "Has Accepted Answer?")
lines(answeredQues$HasBayesianTag, predict(modelv3, type = "response"), type = "l", col = "blue")

## Predict ----

confusionMatrix(table(predict(modelv3, type="response") >= 0.5,answeredQues$HasAcceptedAnswer == 1))

# Version 4 ----

### Trying to identify if adding more tags as regressors impacts the probability of getting an accepted answer

## Add dummy variable to indicate if the question belongs to neural network tag
answeredQues$HasNNTag <- as.integer(grepl(pattern = "neural-network", answeredQues$Tags, ignore.case = T))

## Add dummy variable to indicate if the question belongs to random tag
answeredQues$HasRandomTag <- as.integer(grepl(pattern = "random", answeredQues$Tags, ignore.case = T))

## Model Selection ----

### Create subset dataframe that only contains the response variable as `y`[bestglm requires response variable to be y] and the regressors for the full model
ansQuesSubsetDFv4 <- within(answeredQues, {
  id <- NULL
  AcceptedAnswerId <- NULL
  CreationDate <- NULL
  Body <- NULL
  LastActivityDate <- NULL
  Tags <- NULL
  FavoriteCount <- NULL
  MeanAnswerTime_Mins <- NULL
  y <- HasAcceptedAnswer   
  HasAcceptedAnswer <- NULL
})

### Perform Model Selection using AIC as criteria
aicModelSelectionsv4 <- bestglm(Xy = ansQuesSubsetDFv4,
                                family = binomial(link = "logit"), # binomial family for logit
                                IC = "AIC", # AIC chosen to select models
                                method = "exhaustive")

### Determining BestModels
aicModelSelectionsv4$BestModels

#### Best Model
##### HasAcceptedAnswer ~ Score + ViewCount + AnswerCount + CommentCount + PostLength_Words + IsFavourite + HasMLTag + HasRegressionTag + HasBayesianTag + MeanAnswerTime_Days + InactiveSince_Days + HasNNTag + HasRandomTag [AIC - 129201.8]

## Model Building ----

modelv4 <- glm(HasAcceptedAnswer ~ Score + ViewCount + AnswerCount + CommentCount + PostLength_Words + IsFavourite + HasMLTag + HasRegressionTag + HasBayesianTag + MeanAnswerTime_Days + InactiveSince_Days + HasNNTag + HasRandomTag, data = answeredQues, family = binomial(link = "logit"))

summary(modelv4)
### At 95% significance, based on the wald test's p-values ViewCount is not a significant regressor
### Negative betas => AnswerCount, CommentCount, HasMLTag, HasRegressionTag, MeanAnswerTime_Days
### Positive betas => Score, ViewCount, PostLength_Words, IsFavorite, HasBayesianTag, InactiveSince_Days, HasNNTag, HasRandomTag
### Residual Deviance - 129176

### odds ratio
exp(cbind(OR = coef(modelv4), confint(modelv4)))
### The odds of getting an accepted ans increases with the increase in Score, PostLength_Words(length of ques), IsFavorite(weather ques is fav. or not)
### The odds of getting an accepted ans decreases with the increase in AnswerCount, CommentCount
### The odds of getting an accepted ans increases by around 1 if the number of days since which the post is inactive increases by one day (InactiveSince_Days)
### The odds of getting an accepted ans decreases by around 0.9999 if the time taken to get an answer increases by one day (MeanAnswerTime_Days)
### The odds of getting an accepted ans decreases by around 0.82-0.90 if the question has ML Tag
### The odds of getting an accepted ans decreases by around 0.88-0.94 if the question has Regression Tag
### The odds of getting an accepted ans increases by around 1.19-1.36 if the question has Bayesian Tag
### The odds of getting an accepted ans increases by around 1.05-1.21 if the question has Random Tag
### The odds of getting an accepted ans increases by around 1.01-1.16 if the question has Neural Network Tag


anovaModelv4 <- anova(nullModel, modelv4)
### Difference in deviance - 4511.4
pvalModelv4 <- 1 - pchisq(anovaModelv4$Deviance[2], anovaModelv4$Df[2]) ## p-val ~ 0

### Multicollinearity
vif(modelv4) ## no multicollinearity

## Plots ----

plot(answeredQues$Score, answeredQues$HasAcceptedAnswer, pch = 16, xlab = "Score", ylab = "Has Accepted Answer?")
lines(answeredQues$Score, predict(modelv4, type = "response"), type = "l", col = "blue")

plot(answeredQues$AnswerCount, answeredQues$HasAcceptedAnswer, pch = 16, xlab = "Number of Answers", ylab = "Has Accepted Answer?")
lines(answeredQues$AnswerCount, predict(modelv4, type = "response"), type = "l", col = "blue")

plot(answeredQues$HasMLTag, answeredQues$HasAcceptedAnswer, pch = 16, xlab = "ML Tag", ylab = "Has Accepted Answer?")
lines(answeredQues$HasMLTag, predict(modelv4, type = "response"), type = "l", col = "blue")

plot(answeredQues$HasRegressionTag, answeredQues$HasAcceptedAnswer, pch = 16, xlab = "Regression Tag", ylab = "Has Accepted Answer?")
lines(answeredQues$HasRegressionTag, predict(modelv4, type = "response"), type = "l", col = "blue")

plot(answeredQues$HasBayesianTag, answeredQues$HasAcceptedAnswer, pch = 16, xlab = "Bayesian Tag", ylab = "Has Accepted Answer?")
lines(answeredQues$HasBayesianTag, predict(modelv4, type = "response"), type = "l", col = "blue")

## Predict ----

confusionMatrix(table(predict(modelv4, type="response") >= 0.5,answeredQues$HasAcceptedAnswer == 1))
