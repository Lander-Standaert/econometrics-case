####################################################################
####################################################################
###                   Case Econometrics                          ###
### Bachelor of Science in Economics and Business Engineering    ###
###         Academic year 2021-21 Ghent University                ###
####################################################################
####################################################################

####################################################################
### In this case we want to estimate the impact of trade         ###
###                 on GDP per worker                             ###   
####################################################################

rm(list = ls())   # Clear workspace 

####################################################################
## Set input file directory: change the path to your own pc directory
## !! Note: EconometricsUGent package and dataset are saved in this 
## input file directory
####################################################################

## Rstudio installed on your pc
## For Windows
setwd("C:\\users\\jules\\Desktop\\School\\Econometrie\\Case\\R-files")

####################################################################
## Set output file directory: change the path to your own pc directory
## The output tables are created in this directory
####################################################################

## Rstudio installed on your pc
## For Windows
output="C:\\users\\jules\\Desktop\\School\\Econometrie\\Case\\Output"

####################################################################
## Install required packages: this only needs to be done the first time
## before you use these packages
####################################################################
if(!require(pastecs)){install.packages("pastecs")}
if(!require(psych)){install.packages("psych")}
if(!require(moments)){install.packages("moments")}
if(!require(lmtest)){install.packages("lmtest")}
if(!require(sandwich)){install.packages("sandwich")}
if(!require(AER)){install.packages("AER")}
if(!require(stargazer)){install.packages("stargazer")}
if(!require(nlme)){install.packages("nlme")}
if(!require(orcutt)){install.packages("orcutt")}
if(!require(openxlsx)){install.packages("openxlsx")}

####################################################################
## To install the EconometricsUGent-package, execute the following steps:
## 1. Copy-paste the EconometricsUGent_1.0.tar.gz file to your desktop
## 2. Change the part "C:\\users\\ymeersch\\Desktop" in the code below
##    so that it corresponds with your pc directory to the desktop
####################################################################

## Rstudio installed on your pc
## For Windows
##install.packages("C:\\users\\jules\\Desktop\\School\\Econometrie\\Case\\R-files\\EconometricsUGent_1.0.tar.gz", source = TRUE, repos = NULL)

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

####################################################################
## Import data set
####################################################################
data = read.xlsx("Trade.xlsx", colNames = TRUE)

####################################################################
## Determine number of observations and number of variables
####################################################################
dim(data)             ## Number of variables and sample size
n = length(data[,1])  ## Sample size

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

####################################################################
## Model OLS
####################################################################
model = lm(gdp~trade + pop + area)
stargazer(model,type="text",digits = 4,style="all")
stargazerRegression(model, fileDirectory = output, fileName = "OLS")
summary(model)

vcov(model)                                         # Show variance-covariance matrix
plot(trade, gdp)

#plot residuals
par(mfrow = c(2, 2))
plot(model)
residualPlots(model)

####################################################################
## Multicollinearity
####################################################################
vif(model)
cor(df)

# Goldfeld-Quandt Test
GQ = gqtest(model,
            fraction = 30,
            order.by = ~ trade + pop + area)
stargazerTest(GQ, fileDirectory = output, fileName = "Goldfeld-Quandt_Test")
GQ

#white test / BP test
BP = bptest(model,varformula = ~ trade + pop + area, studentize = F)
stargazerTest(BP, fileDirectory = output, fileName = "BP_Test")
BP

###################################################################
## Heteroskedasticity
###################################################################
res2 = (model$residuals)^2
## Both heterosked. and autocorrelation
White_Both = lm(res2~trade+pop+area+I(trade^2)+I(pop^2)+I(area^2)+I(trade*pop)+I(trade*area)+I(pop*area))
stargazer(White_Both,type="text",style="all",dep.var.labels = "squared(res)")
stargazerRegression(White_Both, fileDirectory = output, fileName = "White's General Heteroskedasticity Test (Heterosked+Autocorr)")
## Only heterosked.
White_Only = lm(res2~trade+pop+area+I(trade^2)+I(pop^2)+I(area^2))
stargazer(White_Only,type="text",style="all",dep.var.labels = "squared(res)")
stargazerRegression(White_Only, fileDirectory = output, fileName = "White's General Heteroskedasticity Test (only Heterosked)")

###################################################################
## Ordering data
###################################################################
df2 = data.frame(gdp, trade, area, pop)
df2_sorted = df2[order(df2$trade),]
df2_sorted
model_no_order = lm(gdp ~ trade + area + pop)
model_order = lm(df2_sorted$gdp ~ df2_sorted$trade + df2_sorted$area + df2_sorted$pop)
model_order_func = lm(df2_sorted$gdp ~ df2_sorted$trade + df2_sorted$area + df2_sorted$pop + df2_sorted$pop/df2_sorted$area + I(df2_sorted$trade ^2) )
stargazer(model_no_order, model_order,model_order_func, type ="text")

#Testing order on data
# df1 = data.frame(trade,gdp)
# model_basic_no_order = lm(gdp~trade)
# model_sorted = df1[order(df1$trade),]
# model_basic_order = lm(model_sorted$gdp~model_sorted$trade)
# 
# summary(model_basic_no_order)
# summary(model_basic_order)
# 
# stargazer(model_basic_no_order,model_basic_order, type ="text")
# 
# par(mfrow=c(2,2))
# 
# plot(model_basic_no_order$fitted.values, studres(model_basic_no_order), xlab = "OUTLIER: NO ORDER")
# plot(hatvalues(model_basic_no_order), xlab ="LEVERAGE: NO ORDER")
# plot(model_basic_order$fitted.values, studres(model_basic_order), xlab ="OUTLIER: ORDER")
# plot(hatvalues(model_basic_order), xlab ="LEVERAGE: ORDER")

#conclusion: although summary of 2 models is the same, leverage plots are different

# residuals_model_no_order = model_no_order$residuals
# residuals_model_order = model_order$residuals
# residuals_model_order_func = model_order_func$residuals
# 
# par(mfrow=c(1,3))
# plot(residuals_model_no_order, type = "l", ylim = c(-2.5, 2.5), xlab ="RESIDUALS: NO ORDER")
# plot(residuals_model_order, type = "l", ylim = c(-2.5, 2.5), xlab ="RESIDUALS: ORDER")
# plot(residuals_model_order_func, type = "l", ylim = c(-2.5, 2.5), xlab ="RESIDUALS: ORDER FUNC")
# 
# par(mfrow=c(2,2))
# plot(model_no_order$fitted.values, studres(model_no_order), xlab = "OUTLIER: NO ORDER")
# plot(hatvalues(model_no_order), xlab ="LEVERAGE: NO ORDER")
# plot(model_order$fitted.values, studres(model_order), xlab ="OUTLIER: ORDER")
# plot(hatvalues(model_order), xlab ="LEVERAGE: ORDER")

###################################################################
## Autocorrelation
###################################################################

## Graphical method
res=(model$residuals)
par(mar = c(2,2,0,2)) 
plot(res, type = "l", ylim = c(-4,4))
abline(h = seq(-4, 4, 1), col = "darkgrey", lty = 2)

## Runs test
Nruns = runs(model_order)
R = Nruns[1]
N1 = Nruns[2]
N2 = Nruns[3]
N=N1+N2
E_R = 2*N1*N2/N+1
s_R = sqrt(2*N1*N2*(2*N1*N2-N)/(N^2)/(N-1))
results_R = c(R,E_R,E_R-1.96*s_R,E_R+1.96*s_R)
names(results_R)=c("Observed Runs","Expected Runs","95% Lower bound","95% Upper bound")
stargazer(results_R,type="text")

## Durbin Watson D test 

DW_no_order = dwtest(model_no_order)
DW_order = dwtest(model_order)
DW_order_func = dwtest(model_order_func)
# Save to word doc
stargazerTest(DW_no_order, fileDirectory = output, fileName = "DW_No_Order")
stargazerTest(DW_order, fileDirectory = output, fileName = "DW_Order")
stargazerTest(DW_order_func, fileDirectory = output, fileName = "DW_OrderFunc")
# print to screen
DW_summary_no_order=c(DW_no_order$statistic,DW_no_order$p.value)
names(DW_summary_no_order)=c("Test-statistic","P-value")
stargazer(DW_summary_no_order,type="text")

DW_summary_order=c(DW_order$statistic,DW_order$p.value)
names(DW_summary_order)=c("Test-statistic","P-value")
stargazer(DW_summary_order,type="text")

DW_summary_order_func=c(DW_order_func$statistic,DW_order_func$p.value)
names(DW_summary_order_func)=c("Test-statistic","P-value")
stargazer(DW_summary_order_func,type="text")

## Breusch-Godfrey LM test 

BG1_no_order = bgtest(model_no_order,order=1)
BG2_no_order = bgtest(model_no_order,order=2)
BG3_no_order = bgtest(model_no_order,order=3)

BG1_order = bgtest(model_order, order = 1)
BG2_order = bgtest(model_order, order = 2)
BG3_order = bgtest(model_order, order = 3)

BG1_order_func = bgtest(model_order_func, order = 1)
BG2_order_func = bgtest(model_order_func, order = 2)
BG3_order_func = bgtest(model_order_func, order = 3)

# Save to word doc
stargazerTest(BG1_no_order,fileName = "Breusch_Godfrey_Test",fileDirectory = output)
stargazerTest(BG2_no_order,fileName = "Breusch_Godfrey_Test",fileDirectory = output)
stargazerTest(BG3_no_order,fileName = "Breusch_Godfrey_Test",fileDirectory = output)

stargazerTest(BG1_order,fileName = "Breusch_Godfrey_Test",fileDirectory = output)
stargazerTest(BG2_order,fileName = "Breusch_Godfrey_Test",fileDirectory = output)
stargazerTest(BG3_order,fileName = "Breusch_Godfrey_Test",fileDirectory = output)

stargazerTest(BG1_order_func,fileName = "Breusch_Godfrey_Test",fileDirectory = output)
stargazerTest(BG2_order_func,fileName = "Breusch_Godfrey_Test",fileDirectory = output)
stargazerTest(BG3_order_func,fileName = "Breusch_Godfrey_Test",fileDirectory = output)

# print to screen (enkel BG1_order)
BG1_order_summary=c(BG1_order$statistic,BG1_order$p.value)
names(BG1_order_summary)=c("Test-statistic","P-value")
stargazer(BG1_order_summary,type="text")

## EGLS: Cochrane-orcutt iterative method
CO_no_order = cochrane.orcutt(model_no_order)
stargazerRegression(CO_no_order,fileName = "Cochrane_Orcutt_EGLS_No_Order",fileDirectory = output)

CO_order = cochrane.orcutt(model_order)
stargazerRegression(CO_order,fileName = "Cochrane_Orcutt_EGLS_Order",fileDirectory = output)

CO_order_func = cochrane.orcutt(model_order_func)
stargazerRegression(CO_order_func,fileName = "Cochrane_Orcutt_EGLS_Order_Func",fileDirectory = output)

## Newey-West (HAC) consistent standard errors 
HACSE_no_order = robust(model_no_order,type = "HAC",fileName = "HACSE_no_order",fileDirectory = output) # saves results to word file
HACSE_order = robust(model_order,type = "HAC",fileName = "HACSE_order",fileDirectory = output) # saves results to word file
HACSE_order_func = robust(model_order_func,type = "HAC",fileName = "HACSE_order_func",fileDirectory = output) # saves results to word file

###################################################################
## Specification errors
###################################################################
Yfit = model$fitted.values

## Linear model, unordered data

# Plot error terms
par(mar = c(2,2,0,2)) 
plot(res, type = "l", ylim = c(-150,150))

# Durbin Watson D test  
DW = dwtest(model)
DW_summary=c(DW$statistic,DW$p.value)
names(DW_summary)=c("Test-statistic","P-value")
stargazer(DW_summary,type="text")

# Overfit
inv_trade=1/trade
inv_pop = 1/pop
inv_area = 1/area
Overfit_reg=lm(gdp~trade+pop+area+inv_trade+inv_pop+inv_area)
stargazer(Overfit_reg,type="text",style="all")

# Ramsey RESET test
Ramsey_reg=lm(gdp~trade+pop+area+I(Yfit^2)+I(Yfit^3))
stargazer(Ramsey_reg,type="text",style="all")
Ramsey_test = ((summary(Ramsey_reg)$r.squared-summary(model)$r.squared)/2)/((1-summary(Ramsey_reg)$r.squared)/(64-4))
Ramsey_summary=c(Ramsey_test,pf(Ramsey_test, 2, 60, lower.tail = FALSE))
names(Ramsey_summary)=c("Test-statistic","P-value")
stargazer(Ramsey_summary,type="text")

# LM test
LM_reg=lm(res~trade+I(trade^2)+I(trade^3))
stargazer(LM_reg,type="text",style="all")
LM_test = 64*summary(LM_reg)$r.squared
LM_summary=c(LM_test,pchisq(LM_test,df=2,lower.tail=FALSE))
names(LM_summary)=c("Test-statistic","P-value")
stargazer(LM_summary,type="text")

# Forecast chi2 test
linear_model_reduced=lm(CM[1:40]~PGNP[1:40]) # OLS on first 40 obs
beta = linear_model_reduced$coefficients
res_holdout = CM[41:64] - beta[1] - beta[2]*PGNP[41:64] # error terms over holdout sample (last 24 obs)
chi2 = sum(res_holdout^2)/(sigma(linear_model_reduced)^2)
chi2_summary=c(chi2,pchisq(chi2,df=24,lower.tail=FALSE))
names(chi2_summary)=c("Test-statistic","P-value")
stargazer(chi2_summary,type="text")

## Linear model, ordered data

# Order data
sample=sample[order(PGNP),]
CM=sample[,1]
FLFP=sample[,2]
PGNP=sample[,3]
TFR=sample[,4]

# OLS estimation
linear_model_ordered=lm(CM~PGNP)
res=linear_model_ordered$residuals
Yfit=linear_model_ordered$fitted.values
stargazer(linear_model_ordered,type="text",style="all")

# Plot error terms
par(mar = c(2,2,0,2)) 
plot(res, type = "l", ylim = c(-150,150))

# Durbin Watson D test  
DW = dwtest(linear_model_ordered)
DW_summary=c(DW$statistic,DW$p.value)
names(DW_summary)=c("Test-statistic","P-value")
stargazer(DW_summary,type="text")

# Forecast chi2 test
linear_model_reduced=lm(CM[1:40]~PGNP[1:40]) # OLS on first 40 obs
beta = linear_model_reduced$coefficients
res_holdout = CM[41:64] - beta[1] - beta[2]*PGNP[41:64] # error terms over holdout sample (last 24 obs)
chi2 = sum(res_holdout^2)/(sigma(linear_model_reduced)^2)
chi2_summary=c(chi2,pchisq(chi2,df=24,lower.tail=FALSE))
names(chi2_summary)=c("Test-statistic","P-value")
stargazer(chi2_summary,type="text")

## Non-linear model, ordered data

# OLS estimation
inv_PGNP=1/PGNP
nonlinear_model_ordered=lm(CM~inv_PGNP)
res=nonlinear_model_ordered$residuals
Yfit=nonlinear_model_ordered$fitted.values
stargazer(nonlinear_model_ordered,type="text",style="all")

# Plot error terms
par(mar = c(2,2,0,2)) 
plot(res_ordered, type = "l", ylim = c(-150,150))

# Durbin Watson D test  
DW = dwtest(nonlinear_model_ordered)
DW_summary=c(DW$statistic,DW$p.value)
names(DW_summary)=c("Test-statistic","P-value")
stargazer(DW_summary,type="text")

# Ramsey test
Ramsey_reg=lm(CM~inv_PGNP+I(Yfit^2)+I(Yfit^3))
stargazer(Ramsey_reg,type="text",style="all")
Ramsey_test = ((summary(Ramsey_reg)$r.squared-summary(nonlinear_model_ordered)$r.squared)/2)/((1-summary(Ramsey_reg)$r.squared)/(64-4))
Ramsey_summary=c(Ramsey_test,pf(Ramsey_test, 2, 60, lower.tail = FALSE))
names(Ramsey_summary)=c("Test-statistic","P-value")
stargazer(Ramsey_summary,type="text")

# LM test
LM_reg=lm(res~inv_PGNP+I(inv_PGNP^2)+I(inv_PGNP^3))
stargazer(LM_reg,type="text",style="all")
LM_test = 64*summary(LM_reg)$r.squared
LM_summary=c(LM_test,pchisq(LM_test,df=2,lower.tail=FALSE))
names(LM_summary)=c("Test-statistic","P-value")
stargazer(LM_summary,type="text")

# Forecast chi2 test
nonlinear_model_reduced=lm(CM[1:40]~inv_PGNP[1:40]) # OLS on first 40 obs
beta = nonlinear_model_reduced$coefficients
res_holdout = CM[41:64] - beta[1] - beta[2]*inv_PGNP[41:64] # error terms over holdout sample (last 24 obs)
chi2 = sum(res_holdout^2)/(sigma(nonlinear_model_reduced)^2)
chi2_summary=c(chi2,pchisq(chi2,df=24,lower.tail=FALSE))
names(chi2_summary)=c("Test-statistic","P-value")
stargazer(chi2_summary,type="text")

###################################################################
## Endogeneity
###################################################################

############################################
## 2SLS estimation
############################################
reg_IV=ivreg(gdp~trade|pop+area+landlocked+neighbors)
stargazer(reg_IV,type="text",style="all")
stargazerRegression(reg_IV, fileDirectory = output, fileName = "2SLS")

############################################
## First stage OLS estimation
############################################
reg_1stage=lm(trade~pop+area+landlocked+neighbors)
stargazer(reg_1stage,type="text",style="all")
stargazerRegression(reg_1stage, fileDirectory = output, fileName = "FirstStageOLS")

############################################
## Hausman test
############################################
reg_Haus=lm(gdp~trade+reg_1stage$residuals)
stargazer(reg_Haus,type="text",style="all")
stargazerRegression(reg_Haus, fileDirectory = output, fileName = "HausmanTest")
