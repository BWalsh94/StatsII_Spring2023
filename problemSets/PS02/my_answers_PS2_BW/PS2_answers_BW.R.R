#####################
# load libraries
# set wd
# clear global .envir
#####################

# remove objects
rm(list=ls())
# detach all libraries
detachAllPackages <- function() {
  basic.packages <- c("package:stats", "package:graphics", "package:grDevices", "package:utils", "package:datasets", "package:methods", "package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:", search()))==1, TRUE, FALSE)]
  package.list <- setdiff(package.list, basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package,  character.only=TRUE)
}
detachAllPackages()

# load libraries
pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,  "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg,  dependencies = TRUE)
  sapply(pkg,  require,  character.only = TRUE)
}

# here is where you load any necessary packages
# ex: stringr
# lapply(c("stringr"),  pkgTest)
lapply(c("mgcv"),  pkgTest)

# set wd for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#####################
# Problem 1
#####################

# load data
load(url("https://github.com/ASDS-TCD/StatsII_Spring2023/blob/main/datasets/climateSupport.RData?raw=true"))


# Converting the choice variables to dummy variables
climateSupport$choice <- ifelse(climateSupport$choice == 'Not supported', 0, 1)

# Converting the countries and sanctions to numeric variables
climateSupport$countries <- as.numeric(climateSupport$countries)
climateSupport$sanctions <- as.numeric(climateSupport$sanctions)

# Fitting the additive model
climate_additive_model <- glm(choice ~ countries + sanctions, data = climateSupport, 
                              family=binomial(logit))
# Printing the summary output
summary(climate_additive_model)

# Testing the null hypothesis
climate_null_model <- glm(choice ~ 1, data = climateSupport, family = binomial(logit))
anova(climate_additive_model, climate_null_model, test = "Chisq")


#####################
# Problem 2
#####################
climate_interaction_model <- glm(choice ~ countries + sanctions + countries*sanctions, data = climateSupport, 
                              family=binomial(logit))

summary(climate_interaction_model)

# Calculating the log odds ratio
odds_ratio <- exp(-0.12353)
100 * (odds_ratio - 1)

# Question 2(b)
Q2_coefficient <- -0.34540 + (0.32436 * 2) + (-0.12353 * 0) 
Q2_odds_ratio <- exp(Q2_coefficient)
100*(Q2_odds_ratio - 1)
