library(MASS)
library(nnet)
library(survival)
library(VGAM)
library(cshapes)
library(stargazer)


setwd("C:\\Users\\Breandán Breathnach\\Documents\\Stáidéar\\ASDS\\2. Téarma Hillary\\POP77004 Applied Statistical Analysis II\\Replication\\Greenhill Replication")
load("genocide data G.rda")

options(scipen=999)

# GREENHILL AND STRAUSZ'S ORIGINAL MODELS 
gendata$totalsq<-gendata$total^2
gendata$totalcu<-gendata$total^3

fulldata$totalsq<-fulldata$total^2
fulldata$totalcu<-fulldata$total^3

### Table 1 (regression models 1-3):

# Basic model with total:
model.logit1<-glm(ratified ~ total + neighbors + polity + newdemoc 
            + domestic.hurdles + british.legal +  + loggdppc 
            + nminorities, data=gendata, binomial(link = "logit")) 

# Add a squared term for total:
model.logit2<-glm(ratified ~ total + totalsq + neighbors + polity 
            + newdemoc + domestic.hurdles + british.legal +  + loggdppc 
            + nminorities, data=gendata, binomial(link = "logit")) 

# Add a cubed term for total:
model.logit3<-glm(ratified ~ total + totalsq + totalcu + neighbors + polity 
            + newdemoc + domestic.hurdles + british.legal +  + loggdppc 
            + nminorities, data=gendata, binomial(link = "logit"))

summary(model.logit1)
summary(model.logit2)
summary(model.logit3)

exp(coef(model.logit1))
exp(coef(model.logit2))
exp(coef(model.logit3))

# MY TWIST ON GREENHILL AND STRAUSZ'S MODELS

# Logistic Regression:
# Additive Probit
model.probit1<-glm(ratified ~ total + neighbors + polity + newdemoc 
            + domestic.hurdles + british.legal +  + loggdppc 
            + nminorities, data=gendata, binomial(link = "probit"))  

# Additive Probit with squared total
model.probit2<-glm(ratified ~ total + totalsq + neighbors 
            + polity + newdemoc + domestic.hurdles 
            + british.legal +  + loggdppc + nminorities, data=gendata, binomial(link = "probit"))

# Additive Probit with cubed total
model.probit3<-glm(ratified ~ total + totalsq + totalcu + neighbors 
            + polity + newdemoc + domestic.hurdles + british.legal +  + loggdppc 
            + nminorities, data=gendata, binomial(link = "probit"))  

summary(model.probit1)
summary(model.probit2)
summary(model.probit3)

exp(coef(model.probit1))
exp(coef(model.probit2))
exp(coef(model.probit3))


# Additive Cauchit
model.cauchit1<-glm(ratified ~ total + neighbors + polity + newdemoc 
                   + domestic.hurdles + british.legal +  + loggdppc 
                   + nminorities, data=gendata, binomial(link = "cauchit"))  

# Additive Cauchit with squared total
model.cauchit2<-glm(ratified ~ total + totalsq + neighbors 
                   + polity + newdemoc + domestic.hurdles 
                   + british.legal +  + loggdppc + nminorities, data=gendata, binomial(link = "cauchit"))

# Additive Cauchit with cubed total
model.cauchit3<-glm(ratified ~ total + totalsq + totalcu + neighbors 
                   + polity + newdemoc + domestic.hurdles + british.legal +  + loggdppc 
                   + nminorities, data=gendata, binomial(link = "cauchit")) 

summary(model.cauchit1)
summary(model.cauchit2)
summary(model.cauchit3)

exp(coef(model.cauchit1))
exp(coef(model.cauchit2))
exp(coef(model.cauchit3))


# Additive Gompit
model.gompit1<-glm(ratified ~ total + neighbors + polity + newdemoc 
                    + domestic.hurdles + british.legal +  + loggdppc 
                    + nminorities, data=gendata, binomial(link = "cloglog"))  

# Additive Gompit with squared total
model.gompit2<-glm(ratified ~ total + totalsq + neighbors 
                    + polity + newdemoc + domestic.hurdles 
                    + british.legal +  + loggdppc + nminorities, 
                   data=gendata, binomial(link = "cloglog"))

# Additive Gompit with cubed total
model.gompit3<-glm(ratified ~ total + totalsq + totalcu + neighbors 
                    + polity + newdemoc + domestic.hurdles 
                   + british.legal +  + loggdppc + nminorities, 
                   data=gendata, binomial(link = "cloglog")) 


summary(model.gompit1)
summary(model.gompit2)
summary(model.gompit3)

exp(coef(model.gompit1))
exp(coef(model.gompit2))
exp(coef(model.gompit3))

# Performing log likelihood tests on each model
logLik(model.logit1)
logLik(model.logit2)
logLik(model.logit3)
logLik(model.probit1)
logLik(model.probit2)
logLik(model.probit3)
logLik(model.cauchit1)
logLik(model.cauchit2)
logLik(model.cauchit3)
logLik(model.gompit1)
logLik(model.gompit2)
logLik(model.gompit3)


# Interactive Logistic Regression:
## I'm interacting variables I hypothesise are related

# Interactive logit:
inter.logit1<-glm(ratified ~ total + neighbors * polity * newdemoc 
                  + domestic.hurdles + british.legal +  + loggdppc 
                  + nminorities, data=gendata, binomial(link = "logit"))  

# Interactive logit with squared total:
inter.logit2<-glm(ratified ~ total + totalsq + neighbors * polity 
                  * newdemoc + domestic.hurdles + british.legal +  + loggdppc 
                  + nminorities, data=gendata, binomial(link = "logit"))  

# Interactive logit with cubed total:
inter.logit3<-glm(ratified ~ total + totalsq + totalcu + neighbors * polity 
                  * newdemoc + domestic.hurdles + british.legal +  + loggdppc 
                  + nminorities, data=gendata, binomial(link = "logit")) 

summary(inter.logit1)
summary(inter.logit2)
summary(inter.logit3)

exp(coef(inter.logit1))
exp(coef(inter.logit2))
exp(coef(inter.logit3))


# Interactive probit:
inter.probit1<-glm(ratified ~ total + neighbors * polity * newdemoc 
                   + domestic.hurdles + british.legal +  + loggdppc 
                   + nminorities, data=gendata, binomial(link = "probit"))  

# Interactive Probit with squared total
inter.probit2<-glm(ratified ~ total + totalsq + neighbors 
                   * polity * newdemoc + domestic.hurdles 
                   + british.legal +  + loggdppc + nminorities, data=gendata, binomial(link = "probit"))

# Interactive Probit with cubed total
inter.probit3<-glm(ratified ~ total + totalsq + totalcu + neighbors 
                   * polity * newdemoc + domestic.hurdles + british.legal +  + loggdppc 
                   + nminorities, data=gendata, binomial(link = "probit"))  

summary(inter.probit1)
summary(inter.probit2)
summary(inter.probit3)

exp(coef(inter.probit1))
exp(coef(inter.probit2))
exp(coef(inter.probit3))


# Interactive Cauchit
inter.cauchit1<-glm(ratified ~ total + neighbors * polity * newdemoc 
                    + domestic.hurdles + british.legal +  + loggdppc 
                    + nminorities, data=gendata, binomial(link = "cauchit"))  

# Interactive Cauchit with squared total
inter.cauchit2<-glm(ratified ~ total + totalsq + neighbors 
                    * polity * newdemoc + domestic.hurdles 
                    + british.legal +  + loggdppc + nminorities, data=gendata, binomial(link = "cauchit"))

# Interactive Cauchit with cubed total
inter.cauchit3<-glm(ratified ~ total + totalsq + totalcu + neighbors 
                    * polity * newdemoc + domestic.hurdles + british.legal +  + loggdppc 
                    + nminorities, data=gendata, binomial(link = "cauchit")) 

summary(inter.cauchit1)
summary(inter.cauchit2) 
summary(inter.cauchit3) 

exp(coef(inter.cauchit1))
exp(coef(inter.cauchit2))
exp(coef(inter.cauchit3))


# Interactive Gompit
inter.gompit1<-glm(ratified ~ total + neighbors * polity * newdemoc 
                   + domestic.hurdles + british.legal +  + loggdppc 
                   + nminorities, data=gendata, binomial(link = "cloglog"))  

# Interactive Gompit with squared total
inter.gompit2<-glm(ratified ~ total + totalsq + neighbors 
                   * polity * newdemoc + domestic.hurdles 
                   + british.legal +  + loggdppc + nminorities, 
                   data=gendata, binomial(link = "cloglog"))

# Interactive Gompit with cubed total
inter.gompit3<-glm(ratified ~ total + totalsq + totalcu + neighbors 
                   * polity * newdemoc + domestic.hurdles 
                   + british.legal +  + loggdppc + nminorities, 
                   data=gendata, binomial(link = "cloglog")) 


summary(inter.gompit1)
summary(inter.gompit2)
summary(inter.gompit3)

exp(coef(inter.gompit1))
exp(coef(inter.gompit2))
exp(coef(inter.gompit3))


# EXTRA LOGIT MODELS:
# Past genocide:

genocide.logit1<-glm(ratified ~ total + neighbors + polity + newdemoc + domestic.hurdles + british.legal +  + deathmag.max + loggdppc + nminorities, data=gendata, binomial(link = "logit")) # 
genocide.logit2<-glm(ratified ~ total + totalsq + neighbors + polity + newdemoc + domestic.hurdles + british.legal +  + deathmag.max + loggdppc + nminorities, data=gendata, binomial(link = "logit")) # 
genocide.logit3<-glm(ratified ~ total + totalsq +totalcu + neighbors + polity + newdemoc + domestic.hurdles + british.legal +  + deathmag.max + loggdppc + nminorities, data=gendata, binomial(link = "logit")) # 

summary(genocide.logit1)
summary(genocide.logit2)
summary(genocide.logit3)


# Sharia
sharia.logit1 <-glm(ratified ~ total + neighbors + polity + newdemoc + domestic.hurdles +  + loggdppc + nminorities + islamic.law, data=gendata, binomial(link = "logit")) #  
sharia.logit2 <-glm(ratified ~ total + totalsq + neighbors + polity + newdemoc + domestic.hurdles +  + loggdppc + nminorities + islamic.law, data=gendata, binomial(link = "logit")) #  
sharia.logit3 <-glm(ratified ~ total + totalsq + totalcu + neighbors + polity + newdemoc + domestic.hurdles +  + loggdppc + nminorities + islamic.law, data=gendata, binomial(link = "logit")) #  

summary(sharia.logit1)
summary(sharia.logit2)
summary(sharia.logit3)

exp(coef(sharia.logit3))

# Civil Law
civil.logit1 <-glm(ratified ~ total + neighbors + polity + newdemoc + domestic.hurdles +  + loggdppc + nminorities + civil.law, data=gendata, binomial(link = "logit")) #  
civil.logit2 <-glm(ratified ~ total + totalsq + neighbors + polity + newdemoc + domestic.hurdles +  + loggdppc + nminorities + civil.law, data=gendata, binomial(link = "logit")) #  
civil.logit3 <-glm(ratified ~ total + totalsq + totalcu + neighbors + polity + newdemoc + domestic.hurdles +  + loggdppc + nminorities + civil.law, data=gendata, binomial(link = "logit")) #  

summary(civil.logit1)
summary(civil.logit2)
summary(civil.logit3)

exp(coef(civil.logit1))
exp(coef(civil.logit2))
exp(coef(civil.logit3))


# Mixed Law
mixedlaw.logit1 <-glm(ratified ~ total + neighbors + polity + newdemoc + domestic.hurdles +  + loggdppc + nminorities + mixed.law, data=gendata, binomial(link = "logit")) #  
mixedlaw.logit2 <-glm(ratified ~ total + totalsq + neighbors + polity + newdemoc + domestic.hurdles +  + loggdppc + nminorities + mixed.law, data=gendata, binomial(link = "logit")) #  
mixedlaw.logit3 <-glm(ratified ~ total + totalsq + totalcu + neighbors + polity + newdemoc + domestic.hurdles +  + loggdppc + nminorities + mixed.law, data=gendata, binomial(link = "logit")) #  

summary(mixedlaw.logit1)
summary(mixedlaw.logit2)
summary(mixedlaw.logit3)

exp(coef(mixedlaw.logit1))
exp(coef(mixedlaw.logit2))
exp(coef(mixedlaw.logit3))
