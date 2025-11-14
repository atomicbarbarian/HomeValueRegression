getwd()

options(scipen=50,digits=6)
library(tidyverse)
library(psych) #for descriptive statistics
library(car) #for levene test
library(mosaic) #for histograms

#read dataset file
housingmarket <- read.csv("boston.csv", header=T)
head(housingmarket)
summary(housingmarket)

#find summary statistics
describe(housingmarket,IQR=TRUE, omit=TRUE, skew=TRUE)

#change CHAS to categorical with levels
housingmarket$CHAS <-as.factor(housingmarket$CHAS)
levels(housingmarket$CHAS)
housingmarket <- housingmarket %>%
  mutate(CHAS = factor(CHAS, levels=c('0', '1')))
levels(housingmarket$CHAS)

#comparing statistical summary between two CHAS groups
housingmarket %>%
  group_by(CHAS) %>%
  summarise(n = n(), Average = mean(MEDV), Median = median(MEDV),
            SD = sd(MEDV),  IQR = IQR(MEDV),Min =min(MEDV), Max=max(MEDV)) %>%
  knitr::kable(digits = 3)

#examining the two distributions of MEDV for by the water and not
boxplot(MEDV ~ CHAS,data = housingmarket,col="yellow" )

#two sample independent t-test for CHAS groups
t.test(MEDV ~ CHAS, var.equal=TRUE, alternative = "less", data=housingmarket)

#shapiro test for normality
tapply(housingmarket$MEDV,housingmarket$CHAS,shapiro.test)

#Checking equal variance assumption with Levene test
leveneTest(MEDV ~ CHAS, data=housingmarket)

lowerCor(select(housingmarket, CRIM:MEDV))

#Get matrix plots
library(GGally)
ggpairs(housingmarket, columns = c(1, 2, 3, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14),
        aes(size = I(0.2)))

#histograms to see skew for potential log transformation
histogram(housingmarket$MEDV)
histogram(log(housingmarket$MEDV))

#adding new log variables to explore their potential for model
housingmarketnew <- transform(housingmarket,logINDUS = log(INDUS), 
                              logDIS = log(DIS), logLSTAT = log(LSTAT),
                              logMEDV = log(MEDV))

#removing ZN, RAD, TAX, and B because in plots against MEDV, data was very scattered
ggpairs(housingmarketnew, columns = c(1, 3, 5, 6, 7, 8, 10, 11, 13, 14, 15, 16,
                                      17, 18), aes(size = I(0.2)))

# M1: MEDV estimation using all variables
m1 <- lm(MEDV ~ CRIM + ZN + INDUS + NOX + RM + AGE + DIS + RAD + TAX + PTRATIO + B + LSTAT +
           logINDUS + logDIS + logLSTAT, data = housingmarketnew)
summary(m1)

# M2: logMEDV estimation
#not used in final project, logMEDV determined to be unnecessary
m2 <- lm(logMEDV ~ CRIM + INDUS + NOX + RM + AGE + DIS + PTRATIO + LSTAT +
           logINDUS + logDIS + logLSTAT, data = housingmarketnew)
summary(m2)

# M3: model created by stepout
m3 <- lm(MEDV ~ CRIM + INDUS + NOX + RM + RAD + TAX + PTRATIO + B + LSTAT +
           logINDUS + logDIS + logLSTAT, data = housingmarketnew)
summary(m3)

# M4: parallel line model using M3 modified by CHAS groups
m4 <- lm(MEDV ~ CRIM + INDUS + NOX + RM + RAD + TAX + PTRATIO + B + LSTAT +
           logINDUS + logDIS + logLSTAT + CHAS,
         data = housingmarketnew)
summary(m4)
#for parameter and parameter C.I. display
stepout1 <- step(object = m1, trace = FALSE)
stepout1

#F test to test equality of intercepts in parallel lines model
anova(m3, m4)

#Checking model assumptions

#qq plot of residuals 
ggplot(m4, mapping = aes(sample = m4$residuals)) + 
  stat_qq() +
  stat_qq_line(color = "red") +
  geom_qq(color = "blue") +
  ggtitle("Median Home Value Prediction ") +
  ylab("Residuals") +
  xlab("N(0,1) quantiles") +
  theme_bw()

#Plot of residuals vs fitted values
ggplot(m4, aes(x = .fitted, y = .resid)) + 
  geom_point(color = "dark blue") +
  geom_hline(yintercept = 0, color = 'red') +
  xlab("Fitted Values") +
  ylab("Residuals") +
  ggtitle("Median Home Value Prediction" )+
  theme_bw()

