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
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg,  dependencies = TRUE)
  sapply(pkg,  require,  character.only = TRUE)
}

lapply(c("survminer", "survival", "ggplot2"),  pkgTest)


# set working directory for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Telling R not to use scientific notation
options(scipen=999)

# Problem 1 #
library(eha)
data(child)

# Making dummy variables of the 'sex' column, male = 1, female = 0
child$sex <- ifelse(child$sex == 'male', 1, 0)

# Fitting my additive Cox's proportional hazard model
Additive.hazard.model <- coxph(Surv(exit, event) ~ m.age + sex, data = child)

summary(Additive.hazard.model)


# Assessing the goodness of fit for the additive model
drop1(Additive.hazard.model, test = "Chisq")

# Changing the survival method to breslow and exact to test if they're better than efron
Breslow.add.hazard.model <- coxph(Surv(exit, event) ~ m.age + sex, data = child, method = "breslow")
summary(Breslow.add.hazard.model)
drop1(Breslow.add.hazard.model, test = "Chisq")

Exact.add.hazard.model <- coxph(Surv(exit, event) ~ m.age + sex, data = child, method = "exact")
summary(Exact.add.hazard.model)
drop1(Exact.add.hazard.model, test = "Chisq")

# Plotting the additive model
ggsurvplot(survfit(Additive.hazard.model, newdata = child), 
           data = child, 
           time = "exit", 
           color = "#DE4D05", 
           ggtheme = theme_minimal())

# Fitting my interactive Cox's proportional hazard model
Interactive.hazard.model <- coxph(Surv(exit, event) ~ m.age * sex, data = child)

summary(Interactive.hazard.model)

# Assessing the goodness of fit for the interactive model
drop1(Interactive.hazard.model, test = "Chisq")

# Changing the survival method to breslow and exact to test if they're better than efron
Breslow.inter.hazard.model <- coxph(Surv(exit, event) ~ m.age * sex, data = child, method = "breslow")
summary(Breslow.inter.hazard.model)
drop1(Breslow.inter.hazard.model, test = "Chisq")

Exact.inter.hazard.model <- coxph(Surv(exit, event) ~ m.age * sex, data = child, method = "exact")
summary(Exact.inter.hazard.model)
drop1(Exact.inter.hazard.model, test = "Chisq")
