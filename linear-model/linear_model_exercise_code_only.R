### Only estimations from linear_model_exercise

library("car")
library("ggplot2")
library("lmtest")

### Data loading and initial analysis

df <-  read.table("sbp.csv", header = TRUE, sep = ";",dec=",")

df$gender <- as.factor(df$gender)
summary(df)


## Linear model with age variable

### Estimation of Model 1

m1 <- lm(sbp ~ age, data = df)
summary(m1)

ggplot() +
  ggtitle("Linear model sbp = b0 + b1 * age") +
  geom_point(aes(df$age, df$sbp)) +
  geom_line(aes(m1$model$age, m1$fitted.values), color = "green3") +
  xlab("age")+
  ylab("systolic blood pressure") +
  theme_classic()

### Verification of the Model 1 assumptions

plot(m1, which = 1:3)

#### Statistical tests


##### Shapiro-Wilk normality test for residuals


shapiro.test(m1$residuals)

##### Breusch-Pagan test for heteroskedasticity

bptest(m1)

##### Durbin-Watson test for autocorrelation 


dwtest(m1, order.by = ~age, data = df)

##### Harvey-Collier test for linearity

harvtest(m1, order.by = ~age, data = df)

##### Detecting influential observations

plot(m1, which = 4:5)

### Estimation of Model 2

m2 <- lm(sbp ~ age, data = df, subset = 1:69)
summary(m2)

ggplot() +
  ggtitle("Linear models comparison", 
          subtitle= "          green line - model for the entire set, 
          orange line - model after discarding outliers") +
  geom_point(aes(df$age, df$sbp)) +
  geom_line(aes(m1$model$age, m1$fitted.values), color = "green3") +
  geom_line(aes(m2$model$age, m2$fitted.values), color = "darkorange") +
  xlab("age")+
  ylab("systolic blood pressure") +
  theme_classic()

### Verification of the Model 2 assumptions

plot(m2)

#### Statistical tests


##### Shapiro-Wilk normality test for residuals

shapiro.test(m2$residuals)

##### Breusch-Pagan test for heteroskedasticity

bptest(m2)

##### Durbin-Watson test for autocorrelation 

dwtest(m2, order.by = ~age, data = df[1:69,])

##### Harvey-Collier test for linearity

harvtest(m2, order.by = ~age, data = df[1:69,])

##### Bonferroni Outlier Test

outlierTest(m1, n.max = Inf)
outlierTest(m2, n.max = Inf)

## Linear model with age and gender variables

### Estimation of Model 3

m3 <- lm(sbp ~ age + gender, data = df, subset = 1:69)
summary(m3)

ggplot() +
  ggtitle("Linear model sbp = b0 + b1 * age + b2 * gender", 
          subtitle= "          blue - male
          red - female") +
  geom_point(aes(m3$model[m3$model$gender=="male",]$age, m3$model[m3$model$gender=="male",]$sbp), color = "blue") +
  geom_point(aes(m3$model[m3$model$gender=="female",]$age, m3$model[m3$model$gender=="female",]$sbp), color = "red") +
  geom_line(aes(m3$model[m3$model$gender=="male",]$age, predict(m3, data.frame(age = m3$model[m3$model$gender=="male",]$age, gender ="male"))), color = "blue") +
  geom_line(aes(m3$model[m3$model$gender=="female",]$age, predict(m3, data.frame(age = m3$model[m3$model$gender=="female",]$age, gender ="female"))), color = "red") +
  xlab("age")+
  ylab("systolic blood pressure") +
  theme_classic()

### Verification of the Model 3 assumptions

vif(m3)

plot(m3)

#### Statistical tests


##### Shapiro-Wilk normality test for residuals

shapiro.test(m3$residuals)

##### Breusch-Pagan test for heteroskedasticity

bptest(m3)

##### Durbin-Watson test for autocorrelation 

dwtest(m3, order.by = ~age, data = df[1:69,])

##### Harvey-Collier test for linearity

harvtest(m3, order.by = ~age, data = df[1:69,])