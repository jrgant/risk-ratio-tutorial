#title: "RR paper code"
#author: "Rachel Yorlets"
#date: "4/7/2022"
#goal: The purpose of this code it to supplement a tutorial-style manuscript on calculating RRs and PRs using a modified Poisson model in R

#This is a test for git

#load packages
pacman::p_load(foreign, sandwich, writexl, finalfit, boot)

#Set working directory
setwd('C:/Users/ryorlets/Dropbox (Brown)/Brown SPH - Manuscripts/Calculations in R for RR and PR/Code')

#Load NHEFS data 
d <- read.dta('C:/Users/ryorlets/Dropbox (Brown)/Brown SPH - Manuscripts/Calculations in R for RR and PR/Data/nhefs.dta')

#Check for missing data
d %>% 
  missing_plot('wt82_71', 'death')
length(d$seq[is.na(d$wt82_71)])
length(d$seq[!is.na(d$wt82_71)])
#There are 1566 non-missing values here
length(d$seq[is.na(d$death)])
#0
length(d$seq[!is.na(d$death)])
#1629

#Consider a binary outcome of death by 1992 and a continuous exposure of weight gain between 1971 and 1982
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

#Bootstrap confidence intervals
#First, write a function that will take your data (ours is named 'd'), model, and index over each observation i in the data set
bootCI = function(d, formula, i){
  d = d[i,]
  formula = model
  return(coef(model))
}

#Set seed
set.seed(1)
#Use the boot() function combined with our function
boot_estimate = boot(d, bootCI, R=1000)
plot(boot_estimate)
boot.ci(boot.out = boot_estimate, type = c('norm', 'basic', 'perc'))
