glm.RR <- function(GLM.RESULT, digits = 2) {
  
  if (GLM.RESULT$family$family == "binomial") {
    LABEL <- "OR"
  } else if (GLM.RESULT$family$family == "poisson") {
    LABEL <- "RR"
  } else {
    stop("Not logistic or Poisson model")
  }
  
  COEF      <- stats::coef(GLM.RESULT)
  CONFINT   <- stats::confint(GLM.RESULT)
  TABLE     <- cbind(coef=COEF, CONFINT)
  TABLE.EXP <- round(exp(TABLE), digits)
  
  colnames(TABLE.EXP)[1] <- LABEL
  
  TABLE.EXP
}


## Create a dataset manually
nonmel <- read.table(header = TRUE,
                     text = "
                     cases city u1 u2 u3 u4 u5 u6 u7      n
                     1      1    0  1  0  0  0  0  0  0 172675
                     2     16    0  0  1  0  0  0  0  0 123065
                     3     30    0  0  0  1  0  0  0  0  96216
                     4     71    0  0  0  0  1  0  0  0  92051
                     5    102    0  0  0  0  0  1  0  0  72159
                     6    130    0  0  0  0  0  0  1  0  54722
                     7    133    0  0  0  0  0  0  0  1  32185
                     8     40    0  0  0  0  0  0  0  0   8328
                     9      4    1  1  0  0  0  0  0  0 181343
                     10    38    1  0  1  0  0  0  0  0 146207
                     11   119    1  0  0  1  0  0  0  0 121374
                     12   221    1  0  0  0  1  0  0  0 111353
                     13   259    1  0  0  0  0  1  0  0  83004
                     14   310    1  0  0  0  0  0  1  0  55932
                     15   226    1  0  0  0  0  0  0  1  29007
                     16    65    1  0  0  0  0  0  0  0   7583
                     ")

## Create age.range variable and city variable
nonmel <- within(nonmel, {
    age.range <- rep(c("15_24","25_34","35_44","45_54","55_64","65_74","75_84","85+"), 2)
    age.range <- factor(age.range)
    age.range <- relevel(age.range, ref = "85+")

    city <- factor(city, 0:1, c("Minneapolis", "Dallas"))
})

## rop unnecessary columns
nonmel <- nonmel[c("cases","n","city","age.range")]

## Check data
nonmel

## Including offset(log(n)) in the right hand side
model.1 <- glm(cases ~ city + age.range + offset(log(n)), family = poisson(link = "log"), data = nonmel)
## Using the offset option
model.1 <- glm(cases ~ city + age.range, offset = log(n), family = poisson(link = "log"), data = nonmel)

## Results from regular Poisson
summary(model.1)

glm.RR(model.1, 3)

## Predict case per person (n = 1) for oldest people in the Minneapolis
exp(predict(model.1, newdata = data.frame(city = "Minneapolis", age.range = "85+", n = 1)))

## Create dataset to predict for
newdat1 <- nonmel[c("city","age.range")]
newdat2 <- newdat1

## Predicted number of cases per person
newdat1$n <- 1
nonmel$pred.cases.per.one <- exp(predict(model.1, newdat1))

## Predicted number of cases per one thousand persons
newdat2$n <- 1000
nonmel$pred.cases.per.thousand <- exp(predict(model.1, newdat2))

## Predicted number of cases per actual population
nonmel$pred.cases <- exp(predict(model.1))

## Show prediction results
nonmel

## quasi-Poisson to allow the scale parameter to change from 1. Show the dispersion parameter.
model.1q <- glm(cases ~ city + age.range, offset = log(n), family = quasipoisson(link = "log"), data = nonmel)
summary(model.1q)

#The quasi-Poisson method can be used to estimate the dispersion parameter, i.e., degree of overdispersion. It is 1.16, and almost equal to 1, which is the value used in regular Poisson regression. Thus, the Poisson model is appears ok. If it is large, the standard error estimation from the quasi-Poisson model should be used. Here because of the near one dispersion parameter, the SE estimates are almost identical in both methods.

#link function: \( g(\mu) = ln(\mu) \)
#variance function: \( V(\mu) = \theta\mu \) where \( \theta \) is estimated unlike Poisson regression where it is fixed at 1.

## Load sandwich package for robust estimator
library(sandwich)
## Load lmtest package for coeftest
library(lmtest)
## Poisson model with SE estimated via robust variance estimator
coeftest(model.1, vcov = sandwich)

