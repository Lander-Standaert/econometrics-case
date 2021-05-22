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
if (!require(olsrr)) {
  install.packages("olsrr")
}
if (!require(olsrr)) {
  install.packages("rtf")
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
library(olsrr)     ##implement ols tests

####################################################################
## Set output files 
####################################################################
fileMap = "D:/R output"
####################################################################
## Import data set
####################################################################
data = read.xlsx("Trade.xlsx", colNames = TRUE)


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
## Define model
####################################################################
model = lm(gdp ~ trade + pop + area)
plot(model$fitted.values, model$residuals)
fitted_values = model$fitted.values

stargazerRegression(model, fileDirectory = fileMap, fileName = "Model")

####################################################################
## Test normal distribution of error terms
####################################################################
jarque.test(model$residuals)

stargazerTest(JB, fileDirectory = fileMap, fileName = "Jarque Bera Test")

####################################################################
## Test multicollinearity
####################################################################

# correlation matrix
  df = data.frame(gdp, area, pop, trade, neighbors)
  cor(df)
  
  stargazerTable(cor(df), fileDirectory = fileMap, fileName = "Correlation matrix")
# VIF
  vif(model)
  
  stargazerTable(vif(model), fileDirectory = fileMap, fileName = "VIF")
  
####################################################################
## Test heteroskedasticity
####################################################################

  
# Goldfeld-Quandt Test
  GQ_gdp=gqtest(model, fraction = 30, order.by = gdp)
  GQ_pop=gqtest(model, fraction = 30, order.by = pop)
  GQ_area=gqtest(model, fraction = 30, order.by = area)

  GQ_gdp;GQ_area;GQ_pop
  
  stargazerTest(GQ_gdp, fileDirectory = fileMap, fileName = "GQ")
  stargazerTest(GQ_area, fileDirectory = fileMap, fileName = "GQ")
  stargazerTest(GQ_pop, fileDirectory = fileMap, fileName = "GQ")

# White Test
  res_squared=(model$residuals)^2
  
  #pure hetero: no crossproduct
  White = lm(res_squared ~ trade + pop + area + I(trade^2)+ I(pop^2) + I(area^2))
  stargazer(White,type="text",style="all",dep.var.labels = "squared(res)")
  Value1 = summary(White)$r.squared * 150
  pchisq(Value1, df = 6)
  
  stargazerRegression(White,fileDirectory = fileMap, fileName = "White_no_crossproduct")
  
  #hetero and specification: crossproduct
  White1 = lm(res_squared ~ trade + pop + area + I(trade^2)+ I(pop^2) + I(area^2) + I(trade*pop) + I(trade*area) + I(area*pop) + I(pop/area))
  stargazer(White1,type="text",style="all",dep.var.labels = "squared(res)")
  Value2 = summary(White1)$r.squared * 150
  pchisq(Value2, df = 10)
  
  stargazerRegression(White1,fileDirectory = fileMap, fileName = "White_crossproduct")
  
####################################################################
## Test autocorrelation
####################################################################
  df = data.frame(gdp, trade, area, pop, neighbors, landlocked)
  df_ordered_trade =  df[order(df$trade),]
  df_ordered_area =  df[order(df$area),]
  df_ordered_pop =  df[order(df$pop),]
  
  model_trade = lm(df_ordered_trade$gdp ~ df_ordered_trade$trade + df_ordered_trade$pop + df_ordered_trade$area)
  model_area =  lm(df_ordered_area$gdp ~ df_ordered_area$trade + df_ordered_area$pop + df_ordered_area$area)
  model_pop = lm(df_ordered_pop$gdp ~ df_ordered_pop$trade + df_ordered_pop$pop + df_ordered_pop$area)
  
# Graphical method
  res_trade=(model_trade$residuals)
  res_area=(model_area$residuals)
  res_pop= (model_pop$residuals)
  
  par(mar = c(2,2,0,2)) 
  par(mfrow = c(3,1))
  
  plot(2*res_trade, type = "l", ylim = c(-5,5))
  plot(2*res_area, type = "l", ylim = c(-5,5))
  plot(2*res_pop, type = "l", ylim = c(-5,5))
  
  
# Runs test
  Nruns = runs(model_trade)
  R = Nruns[1]
  N1 = Nruns[2]
  N2 = Nruns[3]
  N=N1+N2
  E_R = 2*N1*N2/N+1
  s_R = sqrt(2*N1*N2*(2*N1*N2-N)/(N^2)/(N-1))
  results_R = c(R,E_R,E_R-1.96*s_R,E_R+1.96*s_R)
  names(results_R)=c("Observed Runs","Expected Runs","95% Lower bound","95% Upper bound")
  stargazer(results_R,type="text")  
  
  stargazerTable(results_R,fileDirectory = fileMap, fileName = "Runs")
  
# DW test
  DW_trade = dwtest(model_trade)   #error terms normally distributed?
  DW_pop = dwtest(model_pop)
  DW_area = dwtest(model_area)
  
  DW_summary_trade =c(DW_trade$statistic,DW_trade$p.value)
  DW_summary_pop =c(DW_pop$statistic,DW_pop$p.value)
  DW_summary_area =c(DW_area$statistic,DW_area$p.value)
  
  Labl = c("Test-statistic","P-value")
  names(DW_summary_trade)= Labl; names(DW_summary_pop) = Labl; names(DW_summary_area) = Labl
  stargazer(DW_summary_trade, DW_summary_area, DW_summary_pop,type="text", title = c("DW trade","DW pop","DW area"))

  stargazerTable(DW_summary_trade,fileDirectory = fileMap, fileName = "DW_TRADE")
  stargazerTable(DW_summary_area,fileDirectory = fileMap, fileName = "DW_AREA")
  stargazerTable(DW_summary_pop,fileDirectory = fileMap, fileName = "DW_POP")
  
# BG test
  buffer = data.frame()
  for (i in c(1:8)) {
    BG = bgtest(model_area,order=i)   #we are not working with time series data, so how does choosing P affect our results?
    BG_summary=c(BG$statistic,BG$p.value)
    buffer = rbind(buffer,BG_summary)
  }
  names(buffer) = c("Test-statistic","P-value")
  buffer
  
  stargazerTable(buffer,fileDirectory = fileMap, fileName = "BG test")
  
####################################################################
## Test Specification Error
####################################################################

#total model
  
  # Durbin Watson D test  
  DW = dwtest(model_trade)
  DW_summary=c(DW$statistic,DW$p.value)
  names(DW_summary)=c("Test-statistic","P-value")
  stargazer(DW_summary,type="text", title = "DW test")
  
  #reset test
  resettest(model_trade, power = 2:3, type = "fitted")
  resettest(model_pop, power = 2:3, type = "fitted")
  resettest(model_area, power = 2:3, type = "fitted")
  
  stargazerTest(resettest(model_trade, power = 2:3, type = "fitted"),fileDirectory = fileMap, fileName = "reset_trade")
  stargazerTest(resettest(model_pop, power = 2:3, type = "fitted"),fileDirectory = fileMap, fileName = "reset_pop")
  stargazerTest(resettest(model_area, power = 2:3, type = "fitted"),fileDirectory = fileMap, fileName = "reset_area")
  

  # Ramsey test special voor lies <3
  new_regressors = length(Ramsey_reg$coefficients) - length(model$coefficients)
  new_coefficients  = length(Ramsey_reg$coefficients)

  Ramsey_reg=lm(gdp~trade+area+pop+I(fitted_values^2)+I(fitted_values^3))
  stargazerRegression(Ramsey_reg, fileDirectory = fileMap, fileName = "ramsey_regression")
  Ramsey_test = ((summary(Ramsey_reg)$r.squared-summary(model)$r.squared)/new_regressors)/((1-summary(Ramsey_reg)$r.squared)/(150-new_coefficients))
  Ramsey_summary=c(Ramsey_test,pf(Ramsey_test, new_regressors, 150-new_coefficients, lower.tail = FALSE))
  names(Ramsey_summary)=c("Test-statistic","P-value")
  stargazer(Ramsey_summary,type="text")
  
  
  # Forecast chi2 test
  buffer_chi = data.frame()
 
  for (n in c(100:125)) {
    interval = c(1:n)
    holdout = c((n+1):150)
    
    gdp1 = df_ordered_trade$gdp[interval]
    trade1 = df_ordered_trade$trade[interval]
    pop1 = df_ordered_trade$pop[interval]
    area1 = df_ordered_trade$area[interval]
  
    gdp2 = df_ordered_trade$gdp[holdout]
    trade2 = df_ordered_trade$trade[holdout]
    pop2 = df_ordered_trade$pop[holdout]
    area2 = df_ordered_trade$area[holdout]

    linear_model_reduced=lm(gdp1 ~ trade1 + pop1  + area1) # OLS on first X obs
    
    beta = linear_model_reduced$coefficients
    
    res_holdout = gdp2 - beta[1] - beta[2]*trade2 - beta[3]*pop2 - beta[4]*area2  #holdout of X
   
    chi2 = sum(res_holdout^2)/(sigma(linear_model_reduced)^2)
    chi2_summary=c(chi2,pchisq(chi2,df=(150-(n+1)),lower.tail=FALSE))
  
    
    buffer_chi = rbind(buffer_chi,chi2_summary)
  }

  buffer_chi
  names(buffer_chi)=c("Test-statistic","P-value")
  plot(buffer_chi$`P-value`)  
  abline(h=0.05) 
  
  stargazerTable(buffer_chi, fileDirectory = fileMap, fileName = "Chi squared")
  
  # LM test
  number_of_observations = dim(df_ordered_trade)[1]
  degrees_of_freedom = length(lagrange_test$coefficients)-1
  
  lagrange_test = lm(model_trade$residuals ~ trade + I(trade ^ 2) + I(trade ^3) + area + I(area^2) + I(area^3) + pop + I(pop^2) + I(pop^3), data = df_ordered_trade)
  stargazerRegression(lagrange_test, fileDirectory = fileMap, fileName = "lagrange")
  LM_test = number_of_observations * summary(lagrange_test)$r.squared
  LM_summary= c(LM_test, pchisq(LM_test, df = degrees_of_freedom, lower.tail = FALSE))
  names(LM_summary) = c("Test-statistic", "P-value")
  stargazer(LM_summary , type = "text", title = "LM test")
  
  stargazerTable(LM_summary,fileDirectory = fileMap, fileName = "LM")     
  
  0.6*0.01
  85*0.006
############################################
## Endogeneity
############################################
  
  # OLS estimation
  reg_OLS = lm(gdp~trade + pop + area) #(Y1 ~ Y2 + X1 + X2)
  
  # 2SLS estimation gdp, trade, area, pop, neighbors, landlocked
  reg_IV=ivreg(gdp ~ trade + pop + area | area + pop + neighbors + landlocked) #(Y1 ~ Y2 + X1 + X2 | X1 + X2 + X3 + X4)

  # First stage OLS estimation
  reg_1stage=lm(trade ~ pop + area + neighbors + landlocked) #(Y2 ~ "X1 + X2" + X3 + X4 )
  summary(reg_1stage)
  
  # Hausman test
  reg_Haus=lm(gdp ~ trade + pop + area + reg_1stage$residuals) #(Y1 ~ Y2 + X1 + X2 + Res)
  summary(reg_Haus)
  stargazer(reg_OLS,reg_IV,reg_Haus,style = "all", type = "text", dep.var.labels = c("2SLS"), table.placement = "H")
  