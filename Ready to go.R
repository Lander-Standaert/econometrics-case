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
## setwd("C:\\users\\ymeersch\\Desktop")

## For Mac
 setwd("/Users/sanderdecoster/Desktop/Econometrieproject/input")

## RStudio through Athena, path corresponds to the desktop or other folder on your H-drive
## For Windows
## setwd("\\Desktop")

## For Mac
## setwd("/Desktop")

####################################################################
## Set output file directory: change the path to your own pc directory
## The output tables are created in this directory
####################################################################

## Rstudio installed on your pc
## For Windows
## output="C:\\users\\ymeersch\\Desktop"


## For Mac
 output="/Users/sanderdecoster/Desktop/Econometrieproject/output"

## RStudio through Athena, path corresponds to the desktop or other folder on your H-drive 
## For Windows
## output = "\\Desktop"

## For Mac
## output = "/Desktop"

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
## install.packages("C:\\users\\ymeersch\\Desktop\\EconometricsUGent_1.0.tar.gz", source = TRUE, repos = NULL)

## For Mac
install.packages("/Users/sanderdecoster/Desktop/Econometrieproject/input/EconometricsUGent_1.0.tar.gz", type="source", repos = NULL)

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

reg1 = lm(gdp~trade)
stargazer(reg1, type ="text", style = "all")

plot(pop, trade)
plot(area, trade)
cor(pop,trade)
cor(area, trade)
stat.desc(trade, basic = TRUE, desc = TRUE, norm = FALSE, p =0.95)
stat.desc(gdp,basic = TRUE, desc = TRUE, norm = FALSE, p =0.95)

####################################################################
## Data Descriptives
####################################################################



#get basic information
describe(data)
#country code and name of country not relevant + scaling of variables + removal of factor column
df = data.frame(gdp,area,pop,trade,neighbors)
describe(df)
#covariance
cov(df)
#plot pairs
pairs(df)

#regression
#hypothesis
## H0: B1 = B1*, H1: B1 != B1*
## H0: trade has no positive impact on GDP

linreg = lm(gdp~trade+pop+area)
stargazer(linreg,type="text",digits = 4,style="all") # Make table with results
vcov(linreg)                                         # Show variance-covariance matrix
plot(trade,gdp)

plot(linreg)
residualPlots(linreg)



vif(linreg)  
summary(linreg)

############################################
## Linear model
############################################
df[order(df$gdp),]
linreg = lm(gdp~trade+pop+area)
stargazer(linreg,type="text",style="all")

############################################
##
## Autocorrelation testing
##
############################################
## Sort data by trade
############################################
ordered_data <- data[order(trade),]
tradeNew <- ordered_data[,6]
gdpNew <- log(ordered_data[,3])
areaNew <- log(ordered_data[,4])
popNew <- log(ordered_data[,5])
linreg2 = lm(gdpNew~tradeNew+popNew+areaNew)
############################################
## Graphical method
############################################
res=(linreg2$residuals)
par(mar = c(4,4,2,4)) 
plot(res, type = "l", ylim = c(-4,4))
abline(h = seq(-4, 4, 1), col = "darkgrey", lty = 2)

############################################
## Runs test
############################################
Nruns = runs(linreg2)
R = Nruns[1]
N1 = Nruns[2]
N1
N2 = Nruns[3]
N2
N=N1+N2
E_R = 2*N1*N2/N+1
s_R = sqrt(2*N1*N2*(2*N1*N2-N)/(N^2)/(N-1))
results_R = c(R,E_R,E_R-1.96*s_R,E_R+1.96*s_R)
names(results_R)=c("Observed Runs","Expected Runs","95% Lower bound","95% Upper bound")
stargazer(results_R,type="text")

############################################
## Durbin Watson D test  
############################################
DW = dwtest(linreg2)
DW
# Save to word doc
stargazerTest(DW,fileName = "Durbin_Watson_Test",fileDirectory = output)

# print to screen
DW_summary=c(DW$statistic,DW$p.value)
names(DW_summary)=c("Test-statistic","P-value")
stargazer(DW_summary,type="text")

############################################
## Breusch-Godfrey LM test   
############################################
BG = bgtest(linreg2,order=6)   ##do this for order 1,2,3

# Save to word doc
stargazerTest(BG,fileName = "Breusch_Godfrey_Test",fileDirectory = output)

# print to screen
BG_summary=c(BG$statistic,BG$p.value)
names(BG_summary)=c("Test-statistic","P-value")
stargazer(BG_summary,type="text")

####################################################################
## EGLS: Cochrane-orcutt iterative method
####################################################################
CO=cochrane.orcutt(linreg2)
stargazerRegression(CO,fileName = "Cochrane_Orcutt_EGLS",fileDirectory = output)

###################################################################
## Newey-West (HAC) consistent standard errors 
###################################################################
HACSE = robust(linear_model,type = "HAC",fileName = "HAC",fileDirectory = output) # saves results to word file


