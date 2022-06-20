#### Used libraries
library("car")
library("ggplot2")
library("pscl")
library("lmtest")

### Data loading and initial analysis
df1 <-  read.table("../data/sbp.csv", header = TRUE, sep = ";",dec=",")
df1$gender <- as.factor(df1$gender)
summary(df1)

### Selection of the GLM model type for the variable Y = sbp
#### Identity link function
m4 <- glm(sbp ~ age + gender, data = df1, subset = 1:69, family = gaussian)
summary(m4)

lrtest(m4)
waldtest(m4)

m3 <- lm(sbp ~ age + gender, data = df1, subset = 1:69)
summary(m3)

#### Logarithmic link function
m5 <- glm(sbp ~ age + gender, data = df1, subset = 1:69, family = gaussian(link = "log"))
summary(m5) 

##### Significance tests of variables
lrtest(m5)
waldtest(m5)

#### Inverse link function
m6 <- glm(sbp ~ age + gender, data = df1, subset = 1:69, family = gaussian(link = "inverse"))
summary(m6) 

##### Significance tests of variables
lrtest(m6)
waldtest(m6)

### Diagnostic charts for models 4-6
plot(m4, which = 1)
plot(m5, which = 1)
plot(m6, which = 1)
outlierTest(m4, n.max = Inf)
outlierTest(m5, n.max = Inf)
outlierTest(m6, n.max = Inf)

#### Identifying influential observations in charts
plot(m4, which = 4)
plot(m5, which = 4)
plot(m6, which = 4)
plot(m4, which = 5)
plot(m5, which = 5)
plot(m6, which = 5)

### Comparison of models 4-6
glm_model_eval <- function(model) {
  residual_std_error <- (model$deviance/model$df.residual)^0.5
  AIC <- c(model$aic)
  McFadden <- pR2(model)[4]
  Cragg_Uhler <- pR2(model)[6]
  evaluation <- data.frame(residual_std_error, AIC, McFadden, Cragg_Uhler)
  return(evaluation)
}
models_eval <- rbind(model_4=glm_model_eval(m4), model_5=glm_model_eval(m5), model_6=glm_model_eval(m6))
models_eval
m5$coefficients
exp(m5$coefficients)