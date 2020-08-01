#Question 1

# Walmart

Ques_1 <- read.csv("Question1.csv")

#part a

#Walmart
head(Ques_1)
#### Correlation Coefficient and Scatterplot ####
help(plot)
plot(Ques_1$MKTRET, Ques_1$WALMARTRET, main = "Stock Returns versus Market Returns", xlab = "Market Return", ylab = "Stock Return")
abline(lm(Ques_1$WALMARTRET ~ Ques_1$MKTRET))

cor(Ques_1$MKTRET, Ques_1$WALMARTRET)

#### Regression Model ####

Walmart_R <- Ques_1$WALMARTRET
Market_Rm <- Ques_1$MKTRET

lm.CAPM.Walmart <- lm(Walmart_R ~ Market_Rm)

anova(lm.CAPM.Walmart)

summary(lm.CAPM.Walmart)

#### The estimate for the beta coefficient for Walmart is 0.73167 and its standard error is 0.19121
#### Since, beta is less than 1, so the stock is conservative

#Dell
#### Correlation Coefficient and Scatterplot ####

plot(Ques_1$MKTRET, Ques_1$DELLRET, main = "Stock Returns versus Market Returns", xlab = "Market Return", ylab = "Stock Return")
abline(lm(Ques_1$DELLRET ~ Ques_1$MKTRET))

cor(Ques_1$MKTRET, Ques_1$DELLRET)

#### Regression Model ####

Dell_R <- Ques_1$DELLRET
Market_Rm <- Ques_1$MKTRET

lm.CAPM.Dell <- lm(Dell_R ~ Market_Rm)

anova(lm.CAPM.Dell)

summary(lm.CAPM.Dell)

#### The estimate for the beta coefficient for Dell is 1.66791 and its standard error is 0.32605
#### Since, beta is more than 1, so the stock is aggresive


#Sabre
#### Correlation Coefficient and Scatterplot ####

plot(Ques_1$MKTRET, Ques_1$SABRERET, main = "Stock Returns versus Market Returns", xlab = "Market Return", ylab = "Stock Return_Sabre")
abline(lm(Ques_1$SABRERET ~ Ques_1$MKTRET))

cor(Ques_1$MKTRET, Ques_1$SABRERET)

#### Regression Model ####

Sabre_R <- Ques_1$SABRERET
Market_Rm <- Ques_1$MKTRET

lm.CAPM.Sabre <- lm(Sabre_R ~ Market_Rm)

anova(lm.CAPM.Sabre)

summary(lm.CAPM.Sabre)

#### The estimate for the beta coefficient for Sabre is 1.470630 and its standard error is 0.245524
#### Since, beta is more than 1, so the stock is aggresive


#part b

#Walmart
### we use a two-tailed test.
### H0: rho = 0
### H1: rho =/= 0

cor.test(Ques_1$MKTRET, Ques_1$WALMARTRET, alternative = "two.sided", method = "pearson")

#p-value = 0.0003207, which is less than 0.05
#linear relationship exists with a factor of 0.4489551

#Dell
### we use a two-tailed test.
### H0: rho = 0
### H1: rho =/= 0

cor.test(Ques_1$MKTRET, Ques_1$DELLRET, alternative = "two.sided", method = "pearson")

#p-value = 3.691e-06, which is less than 0.05
#linear relationship exists with a factor of 0.5575947

#Sabre
### we use a two-tailed test.
### H0: rho = 0
### H1: rho =/= 0

cor.test(Ques_1$MKTRET, Ques_1$SABRERET, alternative = "two.sided", method = "pearson")

#p-value = 1.417e-07, which is less than 0.05
#linear relationship exists with a factor of 0.6182008


#part c

#Dell
#### we formulate the hypotheses as 
#  H0: beta <= 1
#  H1: beta > 1

#### First, we need to find the t_critical value. We will use qt function in R.
#### Note the degrees of freedom (df) is n - k - 1 where k = number of predictors, which in this case is 1.

t_critical <- qt(0.05, 60-1-1, lower.tail = FALSE)
t_critical

#### Next, we need to find the t_test_statistic value.
#### t = (b - beta)/std. error

t_test_stat <- (1.66791 - 1)/0.32605
t_test_stat

#### Decision.
#### Since the t_test_stat is more than the t_critical, for a right-tailed test we reject H0: beta <= 1.
#### We infer at 5% significance level that beta > 1 and is statistically significant.
#### Return on Dell stock is more risky than the returns on the market.

#part d

#Walmart
#### we formulate the hypotheses as 
#  H0: beta >= 1
#  H1: beta < 1

#### First, we need to find the t_critical value. We will use qt function in R.
#### Note the degrees of freedom (df) is n - k - 1 where k = number of predictors, which in this case is 1.

t_critical <- qt(0.05, 60-1-1, lower.tail = TRUE)
t_critical

#### Next, we need to find the t_test_statistic value.
#### t = (b - beta)/std. error

t_test_stat <- (0.73167 - 1)/0.19121
t_test_stat

#### Decision.
#### Since the t_test_stat is more than the t_critical, for a left-tailed test we fail to reject H0: beta >= 1.
#### We infer at 5% significance level that beta >= 1 and is not statistically significant.



#---------------------------------------------------------------------------------------------------------


#Question 2 - left

#Question 3 - left

#Question 4 - left


#Question 5

Ques_5 <- read.csv("Question5.csv")
head(Ques_5)

plot(Ques_5$POLICE, Ques_5$EXPEND, main = "Question 5 ", xlab = "police", ylab = "expenditure")
abline(lm(Ques_5$EXPEND ~ Ques_5$POLICE), col = "red")
lines(lowess(Ques_5$POLICE, Ques_5$EXPEND), col="blue")

#Linear Regression Model

expen_5 <- Ques_5$EXPEND
police_5 <- Ques_5$POLICE

model_5 <- lm(expen_5 ~ police_5)
summary(model_5)

#The adjusted R square is 95.62% which is very good
#the intercept is not significant, but the number of police is

#For 10,000 police officers, the cost predicted would be 18163800

#95% prediction

pred <- predict(model_5, data.frame(police_5 = 10000), interval = "prediction", level = 0.95)
pred


#fit      lwr      upr
#17423545 813604.6 34033484

#-----------------------------------------------------------------------------------------------------------------



#Question 6

Ques_6 <- read.csv("Question6.csv")
head(Ques_6)

#part a

plot(Ques_6$Crew.Size, Ques_6$Output, main = "Question 6", xlab = "crew size", ylab = "output")
abline(lm(Ques_6$Output ~ Ques_6$Crew.Size), col = "Red")
lines(lowess(Ques_6$Crew.Size, Ques_6$Outpu), col = "Blue")

#Based on the scatterplot, the crew size of 7 seems optimal with an average output of 17.5


#part b

#Linear regression model

output_6 <- Ques_6$Output
crew_size_6 <- Ques_6$Crew.Size

model_6_linear <- lm(output_6 ~ crew_size_6)
summary(model_6_linear)

#Quadratic model

model_6_quad <- lm(output_6 ~ crew_size_6 + I(crew_size_6^2))
summary(model_6_quad)


##The quadratic model has an adjusted R square value of 63.07% which is way better than the linear model, 
#which has a negative R-sqr value of 2.84%

##For the linear model, the independent variable, crew-size is not significant whereas for the quadratic model, 
#both the crew size and its square variables are highly significant

##The quadratic model is a better fit


#part c

#For a crew size of 5, they would be expected to finish 4.59596 * 5 - -0.37374 * 25 = 22.9798 - 9.3435 =~ 13.6 jobs in a week


#part d

#cubic regression model
model_6_cube <- lm(output_6 ~ crew_size_6 + I(crew_size_6^2) + I(crew_size_6^3))
summary(model_6_cube)

#No, it does not improve the model as compared to the quadratic model.
#The adjusted R-sqr value decreases to 61.7% and the variables also become insignificant


#-----------------------------------------------------------------------------------------------------------------


#Question 7

ques_7 <- read.csv("Question7.csv")
head(ques_7)

#part a

happiness <- ques_7$Happiness
age <- ques_7$Age
income <- ques_7$Family.Income

plot(ques_7)

#Linear model

model_7_linear <- lm(happiness ~ age + income, data = ques_7)
summary(model_7_linear)


#Linear-log model


model_7_lin_log_trial_1 <- lm(happiness ~ log(income) + age, data = ques_7)
summary(model_7_lin_log_trial_1)


#Log-Linear model

model_7_log_lin <- lm(log(happiness) ~ age + income, data = ques_7)
summary(model_7_log_lin)

#log-Log model

model_7_log_log <- lm(log(happiness) ~ income + log(income), data = ques_7)
summary(model_7_log_log)  

#Summary
#The adjusted R-sqr values for the models along with significant variables are as follows:
#Linear model: Adjusted R-sqr = 48.63%, both age and income as well as intercept highly significant
#Lin-log model: Adjusted R-sqr = 51.91%, all the variables and intercept are still significant
#Log-lin model: Adjusted R-sqr = 46.96%, both age and income as well as intercept highly significant and s.d. has also decresed
#Log-log model: Adjusted R-sqr = 44.59%, all the variables and intercept are still significant

#The lin-log model fits the data best in this case
  
 
#part b

#age = 40, happiness = 40*2.212e-01 + 80000*1.404e-04 + 4.919e+01 = 69.27
#age = 30, happiness = 30*2.212e-01 + 80000*1.404e-04 + 4.919e+01 = 67.058
#age = 50, happiness = 30*2.212e-01 + 80000*1.404e-04 + 4.919e+01 = 71.482


#part c
#income = 20000, happiness = 60*2.212e-01 + 20000*1.404e-04 + 4.919e+01 = 65.27
#income = 50000, happiness = 60*2.212e-01 + 50000*1.404e-04 + 4.919e+01 = 69.482
#income = 100,000, happiness = 60*2.212e-01 + 100000*1.404e-04 + 4.919e+01 = 76.502


#-----------------------------------------------------------------------------------------------------------------


#Question 8

ques_8 <- read.csv("Question8.csv")
head(ques_8)

#H1:mu_flexi - mu_fixed = 0 
#H0:mu_flexi - mu_fixed <> 0


ques_8_stcked <- stack(ques_8)
ques_8_stcked

names(ques_8_stcked) <- c("duration","work-hours")

duration <- ques_8_stcked$duration
work_hrs <- ques_8_stcked$`work-hours`

model_ex8 <- lm(duration ~ work_hrs)
resids_ex8 <- residuals(model_ex8)

nortest::ad.test(resids_ex8)

hist(resids_ex8)

qqnorm(resids_ex8)
qqline(resids_ex8)

car::leveneTest(duration ~ work_hrs)

wilcox.test(duration ~ work_hrs, alt = "two.sided", paired = FALSE, conf.level = 0.95)

#cannot reject null hypothesis
#means are not  different

#-----------------------------------------------------------------------------------------------------------------

#Question 12 

require(ggplot2)
ques_12 <- read.csv("Question12.csv")

plot(ques_12$TIME, ques_12$SALES, main = "Time series", xlab = "Quarter", ylab = "sales")

#part a


ques_12_lin <- lm(ques_12$SALES ~ ques_12$TIME)
summary(ques_12_lin)

plot(ques_12_lin)

#Pattern: The fitted values and residuals are not linear


# part b

ggplot(data = ques_12, aes(x = ques_12$TIME, y = ques_12$SALES))+ geom_line(color = "blue", size = 1)

#yes, there are seasonal patterns, there is a drop in sales every quarter


# part c

quarter1_dummy <- ifelse(ques_12$QUARTER == 1, 1, 0)
quarter2_dummy <- ifelse(ques_12$QUARTER == 2, 1, 0)
quarter3_dummy <- ifelse(ques_12$QUARTER == 3, 1, 0)
quarter4_dummy <- ifelse(ques_12$QUARTER == 4, 1, 0)


#part d

#H0: All the dummy variables are zero
#H1: One of the dummy variabes is non-zero

### Full model

model_12_full <- lm(ques_12$SALES ~ quarter2_dummy + quarter3_dummy + quarter4_dummy + ques_12$TIME)
summary(model_12_full)

### Reduced model
model_12_reduced <- lm(ques_12$SALES ~ ques_12$TIME)
summary(model_12_reduced)

anova(model_12_reduced, model_12_full)

#We reject the null hypothesis at 5% level of significance
#and conclude that atleast one of the dummy variables is not zero
#and hence, the full model is superior



#----------------------------------------------------------------------------------------------------


#Question 14


ques_14 <- read.csv("Question14.csv")
head(ques_14)

## Create dummy variables
fs1 <- ifelse(ques_14$SATIS == 1, 1, 0)   
fs2 <- ifelse(ques_14$SATIS == 2, 1, 0)
fs3 <- ifelse(ques_14$SATIS == 3, 1, 0)
fs4 <- ifelse(ques_14$SATIS == 4, 1, 0)

seninv <- 1/(ques_14$SENIOR)
complx <- ques_14$COMPLX
absent <- ques_14$ABSENT

#part a
#with all the variables

model_full_14 <- lm(absent ~ fs1+fs2+fs3+fs4+complx+seninv, data = ques_14)

summary(model_full_14)


#part b
#after removing indicator variable

model_reduced_14 <- lm(absent ~ complx + seninv, data = ques_14)

summary(model_reduced_14)


#part c

#partial f-test

#H0: beta(fs1) = beta(fs2) = beta(fs3) = beta(fs4) = 0
#H1: atleast one of them is not zero

anova(model_reduced_14, model_full_14)

#We reject the null hypothesis at 5% level of significance
#and conclude that atleast one of fs1, fs2, fs3, fs4 is not zero
#and hence, the full model is superior


#part d

model_full_14 <- lm(absent ~ fs1+fs2+fs3+fs4+complx+seninv, data = ques_14)

summary(model_full_14)

#avg absenteism for very dissatisfied employees = 1.630604 - 0.014070*60 + 1.102841*(1/30) + 0.904105 = 1.72727

#part e

#avg absenteism for very satisfied employees = 1.630604 - 0.014070*60 + 1.102841*(1/30) + 0.069527 = 0.8926924

#part f

#avg absenteism for very dissatisfied employees = 1.630604 - 0.014070*10 + 1.102841*(1/3) + 0.904105 = 2.761623


#part g

#avg absenteism for very satisfied employees = 1.630604 - 0.014070*10 + 1.102841*(1/3)  + 0.069527 = 1.9270


#part h

#The company can identify those employees and get initiatives to lower their absent rates by targetting the employees according to
#their dissatisfaction level
#For the other factors, like job complexity and seniority, the company can introduce flexible working hours maybe to reduce their absent rates

#in other terms, people who have low job complexity and new to the system with seniority level low are to be targeted first


#-----------------------------------------------------------------------------------------------------------------


#Question 15

ques_15=read.csv("Question15.csv")
head(ques_15)

salary_15=ques_15$SALARY
educat_15=ques_15$EDUCAT
exper_15=ques_15$EXPER
months_15=ques_15$MONTHS
gender_dummy = ifelse(ques_15$GENDER == "MALE", 1, 0)


#part a

model_15 <- lm(salary_15 ~ educat_15 + gender_dummy)
summary(model_15)


#part b

#The differential intercept coefficient is for Coefficients attached to the dummy variable,
#since education is not the dummy variable, so it has a slope co-efficient of 80.70

#part c
require(ggplot2)
ggplot(ques_15,aes(y=salary_15,x=educat_15,color=factor(gender_dummy)))+geom_point()+stat_smooth(method="lm",se=FALSE)

#they are concurrent regression

#part d
model_int_15 = lm(salary_15 ~ educat_15 + gender_dummy + educat_15*gender_dummy)

summary(model_int_15)


#part e

#both interaction & dummy variables are not significant

#part f

#model with only EDUCATION and the dummy variable have both the variables as significant
#but model with interaction does not have any variable as significant and the sign of the slope changes as well


#------------------------------------------------------------------------------------------------------------


#Question 16

#Part a

# Instantaneous growth rates are the slope coefficients for the independent variables

#hence,
#Real GNP (1954-1987) - 3.02% per year
#Labor force participation rate(1973-1987) - 5.3% per year
#S&P 500 index (1954-1987) - 4.56% per year
#S&P 500 index (1965-1987 quarterly data) - 1.14% per quarter


#Part b

#tale slope equal to ln(1+r), 
#so, r is antilog(instantaneous) - 1

real_gdp_compound <- (exp(0.0302) - 1)*100
labor_force_compound <- (exp(0.053) -1)*100
SnP_yearly_compound <- (exp(0.0456)-1)*100
SnP_quarterly_compound <- (exp(0.0114)-1)*100

real_gdp_compound
labor_force_compound
SnP_yearly_compound
SnP_yearly_compound


#------------------------------------------------------------------------------------------------------------


#Question 17

ques_17 <- read.csv("Question17.csv")

cost_17 <- ques_17$COST
temp_17 <- ques_17$TEMP
insul_17 <- ques_17$INSUL
age_17 <- ques_17$AGE

#create dummy variable
dummy_garage_17 <- ifelse(ques_17$GARAGE == "Yes", 1, 0)

### First run a regression model with all variables for a quick check 
### for multicollinearity
model_sol_17 <- lm(cost_17 ~ temp_17 + insul_17 + age_17 + dummy_garage_17)

car::vif(model_sol_17)  ### No evidence of multicollinearity 

### Store all vars + Dummy in a separate dataframe. 
df_17 <- data.frame(cost_17, temp_17, insul_17, age_17, dummy_garage_17)

### Run Stepwise Regression
model_17 <- lm(cost_17 ~ ., data = df_17)
summary(model_17)

install.packages("olsrr")
library(olsrr)

k <- olsrr::ols_step_both_p(model_17, prem = 0.10, pent = 0.10, details = TRUE)
plot(k)


#Cost of the house = 393.6657 -3.963*temp  + 77.432*dummy_garage - 11.334*insulation

















