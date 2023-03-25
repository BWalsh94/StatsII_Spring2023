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

# here is where you load any necessary packages
# ex: stringr
# lapply(c("stringr"),  pkgTest)
lapply(c("mgcv", "readr", "nnet", "MASS", "AER", "pscl", "OptimalCutpoints"),  pkgTest)


# set wd for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Telling R not to use scientific notation
options(scipen=999)

#####################
# Problem 1
#####################

# Loading the data
gdpChange <- read.csv("https://raw.githubusercontent.com/ASDS-TCD/StatsII_Spring2023/main/datasets/gdpChange.csv")

# Wrangling the data to produce three factor variables, Decrease, Increase, and No Change
gdpChange$GDPWdiff <- as.factor(ifelse(gdpChange$GDPWdiff < 0, 'Decrease',
                                 ifelse(gdpChange$GDPWdiff > 0, 'Increase', 'No Change')))

# Setting the reference level for the outcome
gdpChange$GDPWdiff <- relevel(gdpChange$GDPWdiff, ref = "No Change")

# Question 1.1 #

# Constructing the unordered/multinomial logit model
multinomial.GDPWdiff <- multinom(GDPWdiff ~ REG + OIL, data = gdpChange)

summary(multinomial.GDPWdiff)
exp(coef(multinomial.GDPWdiff))

multinomial.GDPWdiff

# Estimating the Cutoff Points
cutoffs_multinomial.GDPWdiff <- optimal.cutpoints(multinomial.GDPWdiff, data = gdpChange, method = "Youden") # ?????????????????????????
print(cutoffs_multinomial.GDPWdiff)

# Calculating the Odds Ratio
exp(coef(multinomial.GDPWdiff))

# Finding the cut points for the unordered model
polr(GDPWdiff ~ REG + OIL, data = gdpChange)


# Question 1.2 #

# Converting the response variable (GDPWdiff) from and unordered factor variable to an ordered factor variable
gdpChange$GDPWdiff <- factor(gdpChange$GDPWdiff, levels = c("Decrease", "No Change", "Increase"), ordered = TRUE)

# Constructing the ordered/proportional odds logistic model
ordered.GDPWdiff <- polr(GDPWdiff ~ REG + OIL, data = gdpChange, Hess = TRUE)

summary(ordered.GDPWdiff)
exp(coef(ordered.GDPWdiff))


#####################
# Problem 2
#####################

# Loading the data
MexicoMuni <- read.csv("https://raw.githubusercontent.com/ASDS-TCD/StatsII_Spring2023/main/datasets/MexicoMuniData.csv")

# Question 2a # 

# Running the Poisson model
poisson_Mexico <- glm(PAN.visits.06 ~ competitive.district + marginality.06 + PAN.governor.06, data = MexicoMuni, family = "poisson")

summary(poisson_Mexico)
exp(coef(poisson_Mexico))

# Performing a Chi-Squared Goodness of Fit Test to determine if the model fits the data
pchisq(991.25, 2403, lower.tail=FALSE)

# Running the dispersion test
dispersiontest(poisson_Mexico)

# Interactive Poisson
inter_poisson_Mexico <- glm(PAN.visits.06 ~ competitive.district * marginality.06 * PAN.governor.06, data = MexicoMuni, family = "poisson")

summary(inter_poisson_Mexico)

exp(coef(inter_poisson_Mexico))

# Question 2c #
Mexico.coefs <- coef(poisson_Mexico)

exp(Mexico.coefs[1] + Mexico.coefs[2]*1 + Mexico.coefs[3]*0 + Mexico.coefs[4]*1)

# Creating a data frame to check how robust my initial calculation was
means.Mexico <- data.frame(competitive.district = 1, 
                           marginality.06 = 0, 
                           PAN.governor.06 = 1)

mean_PAN.visits <- predict(poisson_Mexico, means.Mexico, type = "response")

mean_PAN.visits


