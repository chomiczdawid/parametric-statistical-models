#### Used libraries
library("car")
library("ggplot2")
library("pscl")
library("pROC")
library("gridExtra")

## Data loading and initial analysis
setwd("D:/Dev/GitHub/parametric-statistical-models")
df <-  read.table("data/households.csv", header = TRUE, sep = ";",dec=",")
df$gender <- as.factor(df$gender)
df$satisfaction <- as.factor(df$satisfaction)
summary(df)

### Relationship between material satisfaction and potential predictors
par(mfrow = c(3,2))
cdplot(df$income, df$satisfaction, xlab = "income", ylab = "satisfaction")
cdplot(df$expenses, df$satisfaction, xlab = "expenses", ylab = "satisfaction")
cdplot(df$expenses, df$gender, xlab = "expenses", ylab = "gender")
cdplot(df$income, df$gender, xlab = "income", ylab = "gender")
cdplot(df$cars, df$gender, xlab = "cars", ylab = "gender")
cdplot(df$cars, df$satisfaction, xlab = "cars", ylab = "satisfaction")

### Dividing dataset into training and test
set.seed(1257)
n <- nrow(df)
rand_num <- sample(c(1:n), round(0.7*n), replace = FALSE)
df_train <- df[rand_num,]
df_test <- df[-rand_num,]
table(df$satisfaction)/nrow(df)
table(df_train$satisfaction)/nrow(df_train)
table(df_test$satisfaction)/nrow(df_test)

### Checking the correlation of explanatory variables
cor(df_train[,c(1,2,4)])

## Estimation of one-factor binomial logit models
logit1 <- glm(satisfaction ~ income, data = df_train, family = binomial)
logit2 <- glm(satisfaction ~ expenses, data = df_train, family = binomial)
logit3 <- glm(satisfaction ~ cars, data = df_train, family = binomial)
logit4 <- glm(satisfaction ~ gender, data = df_train, family = binomial)
summary(logit1)$coefficients
summary(logit2)$coefficients
summary(logit3)$coefficients
summary(logit4)$coefficients

### Evaluation of logit models 1-4
binomial_model_eval <- function(model) {
  AIC <- c(model$aic)
  McFadden<-pR2(model)[4]
  Cragg_Uhler<-pR2(model)[6]
  score <- data.frame(AIC, McFadden, Cragg_Uhler)
  return(score)
}
eval_res_logit <- rbind(
  model_1=binomial_model_eval(logit1), 
  model_2=binomial_model_eval(logit2), 
  model_3=binomial_model_eval(logit3), 
  model_4=binomial_model_eval(logit4))
eval_res_logit
logit0 <- glm(satisfaction ~ income + cars, data = df_train, family = binomial)
summary(logit0)$coefficients

## Model selection and interpretation
logit1$coefficients
cat("\nexp(bi)\n")
exp(logit1$coefficients)
cat("\nexp(100*bi)\n")
exp(logit1$coefficients[2]*100)

### Estimation of the probit binomial model
probit1 <- glm(satisfaction ~ income, data = df_train, family = binomial(link=probit))
summary(probit1)$coefficients

### Comparative evaluation of logit1 and probit1 models
eval_res__logit_probit <- rbind(
  model_logit_1=binomial_model_eval(logit1), 
  model_probit_1=binomial_model_eval(probit1))
eval_res__logit_probit
eval_res__logit_probit

### Comparison of the prediction quality of logit1`and probit1 models
p <- table(df_train$satisfaction)[2]/nrow(df_train)
tab_traf <- data.frame(observed=logit1$y, predicted=ifelse(logit1$fitted.values>p, 1, 0))
table(tab_traf)
tab_traf <- data.frame(observed=probit1$y, predicted=ifelse(probit1$fitted.values>p, 1, 0))
table(tab_traf)
tab_traf <- data.frame(observed=df_test$satisfaction, predicted=ifelse(predict(logit1, df_test, type = "response")>p, 1, 0))
table(tab_traf)
tab_traf <- data.frame(observed=df_test$satisfaction, predicted=ifelse(predict(probit1, df_test, type = "response")>p, 1, 0))
table(tab_traf)

### Prediction quality measures
pred_measure <- function(model, data, Y, p = 0.5) {
  tab <- table(observed = Y, predicted = ifelse(predict(model, data, type = "response") > p, 1, 0))
  ACC <- (tab[1,1]+tab[2,2])/sum(tab)
  ER <- (tab[1,2]+tab[2,1])/sum(tab)
  SENS <- (tab[2,2]/(tab[2,1]+tab[2,2]))
  SPEC <- (tab[1,1]/(tab[1,1]+tab[1,2]))
  PPV <- (tab[2,2]/(tab[1,2]+tab[2,2]))
  NPV <- (tab[1,1]/(tab[1,1]+tab[2,1]))
  measures <- data.frame(ACC, ER, SENS, SPEC, PPV, NPV)
  return(measures)
}
pred_res <- rbind(
  model_logit = pred_measure(model = logit1, data = df_train,  Y = df_train$satisfaction, p), 
  model_probit = pred_measure(model = probit1, data = df_train, Y = df_train$satisfaction,  p))
pred_res
pred_res <- rbind(
  model_logit = pred_measure(model = logit1, data = df_test,  Y = df_test$satisfaction, p), 
  model_probit = pred_measure(model = probit1, data = df_test, Y = df_test$satisfaction,  p))
pred_res

### Receiver Operating Characteristic curve
par(mfrow = c(1,2))
rocobj1 <- roc(logit1$y, logit1$fitted.values)
rocobj1_t <- roc(df_test$satisfaction, predict(logit1, df_test, type = "response"))
plot(rocobj1, main = "ROC for logit model", col="red")
lines(rocobj1_t, col="blue")
legend(x = "bottomright",legend=c("test set ROC", "train set ROC"),col=c("blue","red"),lty=1)

rocobj2 <- roc(probit1$y, probit1$fitted.values)
rocobj2_t <- roc(df_test$satisfaction, predict(probit1, df_test, type = "response"))
plot(rocobj2, main = "ROC for probit model", col="red")
lines(rocobj2_t, col="blue")
legend(x = "bottomright",legend=c("test set ROC", "train set ROC"),col=c("blue","red"),lty=1)

### ROC curve area
area_AUC_logit<-as.numeric(auc(logit1$y, logit1$fitted.values))
area_AUC_probit<-as.numeric(auc(probit1$y, probit1$fitted.values))
area_AUC <- rbind(area_AUC_logit, area_AUC_probit)
area_AUC
area_AUC_logit<-as.numeric(auc(df_test$satisfaction, predict(logit1, df_test, type = "response")))
area_AUC_probit<-as.numeric(auc(df_test$satisfaction, predict(probit1, df_test, type = "response")))
area_AUC <- rbind(area_AUC_logit, area_AUC_probit)
area_AUC
