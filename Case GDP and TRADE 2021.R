####################################################################
####################################################################
###                   Case Econometrics                          ###
### Bachelor of Science in Economics and Business Engineering    ###
###         Academic year 2021-21 Ghent University                ###
####################################################################
####################################################################


####################################################################
## Install required packages: this only needs to be done the first time
## before you use these packages
####################################################################
if (!require(pastecs)) {
  install.packages("pastecs")
}
if (!require(psych)) {
  install.packages("psych")
}
if (!require(moments)) {
  install.packages("moments")
}
if (!require(lmtest)) {
  install.packages("lmtest")
}
if (!require(sandwich)) {
  install.packages("sandwich")
}
if (!require(AER)) {
  install.packages("AER")
}
if (!require(stargazer)) {
  install.packages("stargazer")
}
if (!require(nlme)) {
  install.packages("nlme")
}
if (!require(orcutt)) {
  install.packages("orcutt")
}
if (!require(openxlsx)) {
  install.packages("openxlsx")
}

####################################################################
## To install the EconometricsUGent-package, execute the following steps:
## 1. Copy-paste the EconometricsUGent_1.0.tar.gz file to your desktop
## 2. Change the part "C:\\users\\ymeersch\\Desktop" in the code below
##    so that it corresponds with your pc directory to the desktop
####################################################################

## Rstudio installed on your pc
## For Windows
install.packages("EconometricsUGent_1.0.tar.gz",
                 source = TRUE,
                 repos = NULL)

## For Mac
## install.packages("/users/ymeersch/Desktop/EconometricsUGent_1.0.tar.gz", type="source", repos = NULL)

## RStudio through Athena
## For Windows
## install.packages("H://Desktop//EconometricsUGent_1.0.tar.gz", type="source", repos = NULL, lib = output)

## For Mac
## install.packages("H:/Desktop/EconometricsUGent_1.0.tar.gz", type="source",repos = NULL, lib = output)


####################################################################
## Load required packages: this needs to be done every time you
## close an R-session
####################################################################
library(pastecs)  ## Descriptive Statistics
library(psych)    ## Correlation plot
library(moments)  ## Testing for Normality check up
library(lmtest)   ## Tests for heteroscedasticity
library(sandwich) ## Newey-West HAC estimators
library(AER)      ## 2SLS
library(stargazer)## Stargazer
library(orcutt)   ## Cochrane-Orcutt EGLS
library(nlme)     ## Linear models with autocorrelated error terms
library(openxlsx) ## To read an excel xlsx data file
library(EconometricsUGent)  ## Additional functions
library(MASS)     ##implement studres


####################################################################
## Import data set
####################################################################
data = read.xlsx("Trade.xlsx", colNames = TRUE)

####################################################################
## Determine number of observations and number of variables
####################################################################
dim(data)             ## Number of variables and sample size
n = length(data[, 1])  ## Sample size

####################################################################
## Define variables
####################################################################
gdp   = log(data$`GDP.per.worker.(in.US.dollars)`)  # ln GDP per worker
area  = log(data$`Area.(in.sq.miles)`)              # ln Area
pop   = log(data$`Workers.(in.thousands)`)          # ln Population
trade = data$Trade                                  # Trade share
landlocked = factor(data$Landlocked.dummy)          # Landlocked dummy
neighbors = data$Number.of.neighboring.countries    # Number of neighboring countries

####################################################################
## Data Descriptives
####################################################################

#get basic information
describe(data)
#country code and name of country not relevant + scaling of variables + removal of factor column
df = data.frame(gdp, area, pop, trade, neighbors)
describe(df)
#covariance
cov(df)
#plot pairs
pairs(df)

#regression
#hypothesis
## H0: B1 = B1*, H1: B1 != B1*
## H0: trade has no positive impact on GDP

linreg = lm(gdp ~ trade + pop + area)
stargazer(linreg,
          type = "text",
          digits = 4,
          style = "all") # Make table with results
vcov(linreg)                                         # Show variance-covariance matrix
plot(trade, gdp)

#plot residuals
par(mfrow = c(2, 2))
plot(linreg)
residualPlots(linreg)

#multicollinearity
vif(linreg)
cor(df)

# Goldfeld-Quandt Test
GQ = gqtest(linreg,
            fraction = 30,
            order.by = ~ trade + pop + area)
GQ
#white test / bp test
bptest(linreg,
       varformula = ~ trade + pop + area,
       studentize = F)


## White general heteroskedasticity test on linear model
res2 = (linreg$residuals) ^ 2
#Test on both specification error and hetero
White_Both = lm(res2 ~ trade + pop + area + I(pop ^ 2) + I(area ^ 2) + I(trade *
                                                                           pop) + I(trade * area) + I(pop * area))
#test only on hetero
White_Only = lm(res2 ~ trade + pop + area + I(pop ^ 2) + I(area ^ 2))

stargazer(
  White_Both,
  type = "text",
  style = "all",
  dep.var.labels = "squared(res)"
)
stargazer(
  White_Only,
  type = "text",
  style = "all",
  dep.var.labels = "squared(res)"
)


## White heteroskedasticity consistent standard errors linear model
White_se = sqrt(diag(vcovHC(linreg)))
stargazer(linreg,
          type = "text",
          style = "all",
          se = list(White_se))


#model selection
  df2 = data.frame(gdp, trade, area, pop)
  df2_sorted = df2[order(df2$trade),]
  df2_sorted
  model_no_order = lm(gdp ~ trade + area + pop)
  model_order = lm(df2_sorted$gdp ~ df2_sorted$trade + df2_sorted$area +
                     df2_sorted$pop)
  model_order_func = lm(df2_sorted$gdp ~ df2_sorted$trade + df2_sorted$area +
                          df2_sorted$pop + df2_sorted$pop/df2_sorted$area + I(df2_sorted$trade ^2) )
  stargazer(model_no_order, model_order,model_order_func, type ="text")
  
  #testing order on data
  
  df1 = data.frame(trade,gdp)
  model_basic_no_order = lm(gdp~trade)
  model_sorted = df1[order(df1$trade),]
  model_basic_order = lm(model_sorted$gdp~model_sorted$trade)
  
  summary(model_basic_no_order)
  summary(model_basic_order)
  
  stargazer(model_basic_no_order,model_basic_order, type ="text")
  
  par(mfrow=c(2,2))
  
  plot(model_basic_no_order$fitted.values, studres(model_basic_no_order), xlab = "OUTLIER: NO ORDER")
  plot(hatvalues(model_basic_no_order), xlab ="LEVERAGE: NO ORDER")
  plot(model_basic_order$fitted.values, studres(model_basic_order), xlab ="OUTLIER: ORDER")
  plot(hatvalues(model_basic_order), xlab ="LEVERAGE: ORDER")
  
     #conclusion: although summary of 2 models is the same, leverage plots are different

  residuals_model_no_order = model_no_order$residuals
  residuals_model_order = model_order$residuals
  residuals_model_order_func = model_order_func$residuals
  
  
  par(mfrow=c(1,3))
  plot(residuals_model_no_order, type = "l", ylim = c(-2.5, 2.5), xlab ="RESIDUALS: NO ORDER")
  plot(residuals_model_order, type = "l", ylim = c(-2.5, 2.5), xlab ="RESIDUALS: ORDER")
  plot(residuals_model_order_func, type = "l", ylim = c(-2.5, 2.5), xlab ="RESIDUALS: ORDER FUNC")
  
  
  par(mfrow=c(2,2))
  plot(model_no_order$fitted.values, studres(model_no_order), xlab = "OUTLIER: NO ORDER")
  plot(hatvalues(model_no_order), xlab ="LEVERAGE: NO ORDER")
  plot(model_order$fitted.values, studres(model_order), xlab ="OUTLIER: ORDER")
  plot(hatvalues(model_order), xlab ="LEVERAGE: ORDER")
  

# Durbin Watson D test
  DW_no_order = dwtest(model_no_order)
  DW_order = dwtest(model_order)
  DW_order_func = dwtest(model_order_func)
  
  DW_summary_no_order = c(DW_no_order$statistic, DW_no_order$p.value)
  DW_summary_order = c(DW_order$statistic, DW_order$p.value)
  DW_summary_order_func = c(DW_order_func$statistic, DW_order_func$p.value)
  
  
  names(DW_summary_no_order) = c("Test-statistic", "P-value")
  names(DW_summary_order) = c("Test-statistic", "P-value")
  names(DW_summary_order_func) = c("Test-statistic", "P-value")
  

  stargazer(DW_summary_no_order, type = "text")              #there is auto correlation
  stargazer(DW_summary_order, type = "text")                 #there is less auto correlation
  stargazer(DW_summary_order_func, type = "text")            #there is less auto correlation. effect of interaction term, higher powers is minimal

#ramsey test
  resettest(model_no_order, power = 2:3, type = "fitted")  #P is high => NOT reject null hypthesis. Conclusion: no specification error
  resettest(model_order, power = 2:3, type = "fitted")     #same result: sorting no result
  resettest(model_order_func, power = 2:3, type = "fitted")     #same conclusion: P is very high
  
  
# LM test
  lagrange_test_no_order = lm(residuals_model_no_order ~ trade + I(trade ^ 2) + I(trade ^ 3))
  lagrange_test_order = lm(residuals_model_order ~ df2_sorted$trade + I(df2_sorted$trade ^ 2) + I(df2_sorted$trade ^3) + df2_sorted$area + I(df2_sorted$area^2) + I(df2_sorted$area^3) + df2_sorted$pop + I(df2_sorted$pop^2) + I(df2_sorted$pop^3))
  
  stargazer(lagrange_test_no_order,lagrange_test_order, type = "text", style = "all")
  
  number_of_observations = 150
  
  LM_test_no_order = number_of_observations * summary(lagrange_test_no_order)$r.squared
  LM_test_order = number_of_observations * summary(lagrange_test_order)$r.squared
  
  LM_summary_no_order  = c(LM_test_no_order, pchisq(LM_test_no_order, df = 3, lower.tail = FALSE))        #unsure about df=3    Same as df from F stat?
  LM_summary_order = c(LM_test_order, pchisq(LM_test_order, df = 9, lower.tail = FALSE))                  #unsure about df=9
  
  names(LM_summary_no_order ) = c("Test-statistic", "P-value")
  names(LM_summary_order ) = c("Test-statistic", "P-value")
  
  stargazer(LM_summary_no_order , type = "text")   #H0: higher power is irrelevant => p= 0.334: no specification
  stargazer(LM_summary_order , type = "text")     

#chi squared
  model_no_order_test = lm(gdp[1:110] ~ trade[1:110])                # OLS on first 110 obs
  model_order_test = lm(df2_sorted$gdp[1:110] ~ df2_sorted$trade[1:110]+df2_sorted$area[1:110]+ df2_sorted$pop[1:110])          # OLS on first 110 obs
  
  beta_no_order = model_no_order_test$coefficients
  beta_order = model_order_test$coefficients
  
  residual_no_order_train = gdp[111:150] - beta_no_order[1] - beta_no_order[2] * trade[111:150] # error terms over holdout sample (last 49 obs)
  residual_order_train = df2_sorted$gdp[111:150] - beta_order[1] - beta_order[2] * df2_sorted$trade[111:150] - beta_order[3] * df2_sorted$area[111:150] - beta_order[4] * df2_sorted$pop[111:150] # error terms over holdout sample (last 49 obs)
  
  chi2_no_order = sum(residual_no_order_train ^ 2) / (sigma(model_no_order_test) ^ 2)
  chi2_order = sum(residual_order_train ^ 2) / (sigma(model_order_test) ^ 2)
  
  chi2_summary_no_order = c(chi2_no_order, pchisq(chi2_no_order, df = 49, lower.tail = FALSE))   #unsure about df=49
  chi2_summary_order = c(chi2_order, pchisq(chi2_order, df = 49, lower.tail = FALSE))   #unsure about df=49
  
  names(chi2_summary_no_order) = c("Test-statistic", "P-value")
  names(chi2_summary_order) = c("Test-statistic", "P-value")
  
  stargazer(chi2_summary_no_order, type = "text")              #P = 0.138, correct model
  stargazer(chi2_summary_order, type = "text")                 #P = 0.179, correct model             
  