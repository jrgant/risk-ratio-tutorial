#title: "RR paper code"
#author: "Rachel Yorlets"
#date: "4/7/2022"
#goal: The purpose of this code it to supplement a tutorial-style manuscript on calculating RRs and PRs using a modified Poisson model in R

#This is a test for git

#Set Brown Dropbox as working directory
setwd('C:/Users/ryorlets/Dropbox (Brown)/Brown SPH - Manuscripts/Calculations in R for RR and PR/Code')

#Load NHEFS data 
d <- read.dta('C:/Users/ryorlets/Dropbox (Brown)/Brown SPH - Manuscripts/Calculations in R for RR and PR/Data/nhefs.dta')

#Consider a binary outcome of death by 1992 and a continuous exposure of weight gain between 1971 and 1982
pacman::p_load(foreign, sandwich, writexl)

model <- glm(death ~ wt82_71, data = d, family = poisson(link = log), start = c(0, 0), maxit = 10000)
summary(model)

#HC0 is the robust sandwich estimator, which is what Zou tells us to use
cov <- vcovHC(model, type = 'HC0')
se <- sqrt(diag(cov))
output <- cbind(Estimate = exp(coef(model)), 'Robust SE' = se, Lower = exp(coef(model) - 1.96 * se), Upper = exp(coef(model) + 1.96 * se))

View(output)

#Export model output to Excel for final formatting
#First, must coerce output into a data frame
output <- as.data.frame(output)
write_xlsx(output, 'Modified Poisson model.xlsx')

