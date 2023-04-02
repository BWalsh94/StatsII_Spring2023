# Replication script for Brian Greenhill & Michael Strausz, "Explaining Non-Ratification of the Genocide Convention: A Nested Analysis."

library(survival)
source("/Users/brian/Brian/new functions/report.progress.R")
source("/Users/brian/Brian/new functions/round.text.R")
source("/Users/brian/Brian/new functions/displaytable.R")
source("/Users/brian/Brian/new functions/logit.plot.R")
source("/Users/brian/Brian/new functions/Xbuilder.R")

load("genocide data G.rda")

gendata$totalsq<-gendata$total^2
gendata$totalcu<-gendata$total^3

fulldata$totalsq<-fulldata$total^2
fulldata$totalcu<-fulldata$total^3

### Table 1 (regression models 1-3):

# Basic model with total:
model1<-glm(ratified ~ total + neighbors + polity + newdemoc 
            + domestic.hurdles + british.legal +  + loggdppc 
            + nminorities, data=gendata, binomial(link = "logit")) #  

# Add a squared term for total:
model2<-glm(ratified ~ total + totalsq + neighbors + polity 
            + newdemoc + domestic.hurdles + british.legal +  + loggdppc 
            + nminorities, data=gendata, binomial(link = "logit")) #  

# Add a cubed term for total:
model3<-glm(ratified ~ total + totalsq + totalcu + neighbors + polity 
            + newdemoc + domestic.hurdles + british.legal +  + loggdppc 
            + nminorities, data=gendata, binomial(link = "logit")) #  





capture.output(displaytable(model1, model2, model3, coefnames=c("Constant", "Global Ratifications", "Neighborhood Effect", "Democracy", "New Democracy", "Institutional Hurdles", "Common Law", "GDP per capita (logged)", "Minorities at Risk", "Global Ratifications (Squared)", "Global Ratifications (Cubed)"), neworder=c(1,2, 10, 11, 3:9)), file="/Users/brian/Brian/Genocide paper/Paper drafts/tables/reg1.tex")



### Figure 1 (ratification map):

library(cshapes)
map<-cshp(date=as.Date("2008-6-30"), useGW=FALSE)
# add COW codes:
source("/Users/brian/Brian/new functions/countrymatcher.R")
map@data$cow<-countrymatcher(map@data$ISONAME)
map@data$cow[map@data$ISONAME=="Viet Nam"]<-"DRV"

# create a vector for use as the color key:
# first change GFR to GMY in ratdata (Germany)
ratdata$cow[ratdata$cow=="GFR"]<-"GMY"

colorkey<-rep(NA, nrow(map@data))
names(colorkey)<-map@data$cow

cx<-function(x) rgb(1-x,1-x,1)

colorkey<-rep(NA, nrow(map@data))
names(colorkey)<-map@data$cow
for (i in 1:nrow(ratdata)){
	cow.current<-as.character(ratdata$cow[i])
	if (!(cow.current %in% names(colorkey))) next
	ratyear<-as.numeric(substr(ratdata$stopdate[i],1,4))
	censored<-ratdata$censored[i]
	if (censored==0) colorkey[cow.current]<-cx((ratyear-1948)/(2011-1948)) else colorkey[cow.current]<-cx(1)
	}

pdf(width=12, height=12, file="/Users/brian/Brian/Genocide paper/Paper drafts/graphics/ratificationmap.pdf")
par(mfrow=c(2,1))
plot(map, col=colorkey)
f<-function(x) ((x-1948)/(2011-1948)*150)-30
yearstoplot<-c(1948, 1960, 1970, 1980, 1990, 2000)
for (y in 1948:2011) rect(f((y-0.5)), -47, f((y+0.5)), -55, border=NA, col=cx((f(y)+30)/150) )
text(f(yearstoplot), -56, yearstoplot, adj=c(0,1), cex=0.9)
text(f(2010), -56, "Not Yet Ratified", adj=c(0,1), cex=0.9)
text(f(1948), -46, "Year of Ratification:", adj=c(0,0), cex=0.9)

# create a count of the total number of ratifications:
yearlist<-sort(unique(fulldata$year))
totalrat<-rep(NA, length(yearlist))
possrat<-rep(NA, length(yearlist))

for (y in 1:length(yearlist)){
	totalrat[y]<-sum(fulldata$ratified[fulldata$year==yearlist[y]], na.rm=T)
	possrat[y]<-sum(!is.na(fulldata$ratified)[fulldata$year==yearlist[y]])
	} # close y loop

par(mar=c(10,10,0,4)+0.1, cex=0.9)
plot(yearlist, yearlist, type="n", ylim=c(0,140), las=1, bty="n", ylab="Cumulative Number of Ratifications", xlab="Year")
lines(yearlist, totalrat, lwd=2)
dev.off()


### Figure 2 (Marginal effect plots of models 1-3):

set.seed(1)
X1<-Xbuilder(model1, fulldata, "total", columns=100)

X2<-Xbuilder(model2, fulldata, "total", columns=100)
X2["totalsq",]<-X2["total",]^2

X3<-Xbuilder(model3, fulldata, "total", columns=100)
X3["totalsq",]<-X3["total",]^2
X3["totalcu",]<-X3["total",]^3

pdf(file="/Users/brian/Brian/Genocide paper/Paper drafts/graphics/totals.pdf", width=6, height=12)
par(mfrow=c(3,1))
logit.plot(model1, X1, xlab="Cumulative Number of Ratifications", ylab="Annual Probability of Ratification", main="Model 1")
logit.plot(model2, X2, xlab="Cumulative Number of Ratifications", ylab="Annual Probability of Ratification", main="Model 2")
logit.plot(model3, X3, xlab="Cumulative Number of Ratifications", ylab="Annual Probability of Ratification", main="Model 3")
dev.off()


### Figure 3 (S-curve):

x<-seq(-5, 5, 0.1)
y<-plogis(x)
pdf(width=6, height=4, file="/Users/brian/Brian/Genocide paper/Paper drafts/graphics/Scurve.pdf")
par(mar=c(0,0,0,0) + 0.1)
plot(x,y, bty="n", type="n", las=1, xaxt="n", yaxt="n", xlab="Time", ylab="Number of States", xlim=c(-6,6), ylim=c(-0.1, 1.1))
lines(x,y, lwd=3)
arrows(-5.3, 0, 5, 0, length=0.1, lwd=1)
arrows(-5, -0.05, -5, 1, length=0.1, lwd=1)
text(0, -0.1, "Time")
text(-6, 0.5, "Number of Ratifications", srt=90)
segments(x0=c(-3, 3), y0=c(0,0), x1=c(-3, 3), y1=c(1,1), lty=2)
text(c(-4, 0, 4), 0.8, c("1", "2", "3"))
dev.off()


### Figure 4 (marginal effect plots for all covariates):
pdf(width=12, height=6, file="/Users/brian/Brian/Genocide paper/Paper drafts/graphics/marginaleffects.pdf")
par(mfrow=c(2,4))
	
X<-Xbuilder(model3, fulldata, "total", columns=100)
X["totalsq",]<-X["total",]^2
X["totalcu",]<-X["total",]^3
logit.plot(model3, X, ylim=c(0,0.4), xlab="Total Ratifications", main="Global Ratifications")
logit.plot(model3, Xbuilder(model3, fulldata, "neighbors"), ylim=c(0,0.4), xlab="Neighbors", main="Neighborhood Effect")
logit.plot(model3, Xbuilder(model3, fulldata, "polity"), ylim=c(0,0.4), xlab="Polity 2", main="Democracy")
logit.plot(model3, Xbuilder(model3, fulldata, "newdemoc", columns=2), ylim=c(0,0.4), xlab="New Democracy", main="New Democracy")
logit.plot(model3, Xbuilder(model3, fulldata, "domestic.hurdles"), ylim=c(0,0.4), xlab="Institutional Hurdles", main="Institutional Hurdles")
logit.plot(model3, Xbuilder(model3, fulldata, "british.legal", columns=2), ylim=c(0,0.4), xlab="Common Law", main="Common Law")
logit.plot(model3, Xbuilder(model3, fulldata, "loggdppc"), ylim=c(0,0.4), xlab="GDP per capita (logged)", main="GDP per capita")
logit.plot(model3, Xbuilder(model3, fulldata, "nminorities"), ylim=c(0,0.4), xlab="Number of Minorities at Risk", main="Minorities at Risk")
dev.off()


### Figure 5 (Japan's neighboring influences)
Japan.plot<-function(variable, data=fulldata, label=""){
	data1<-boxplot(variable~data$year, range=0, plot=F)
	years<-as.numeric(data1$names)
	plot(years, years, xlim=c(1948,2009), ylim=range(variable, na.rm=T), type="n", xlab="Year", las=1, ylab=label, bty="n")
	polygon(x=c(years, rev(years)), y=c(data1$stats[1,], rev(data1$stats[5,])), border=NA, col="grey90")
	polygon(x=c(years, rev(years)), y=c(data1$stats[2,], rev(data1$stats[4,])), border=NA, col="grey80")
	lines(years, data1$stats[3,], lty=3)
	lines(years, variable[data$country=="Japan" & data$year %in% years], lwd=3)
	}

pdf(file="/Users/brian/Brian/Genocide paper/Paper drafts/graphics/japanneighbors.pdf")
Japan.plot(fulldata$neighbors, label="Neighborhood Effect")
dev.off()


### Misc. calculations:

# Calculate the probability of Japan having ratified by now:
a<-predict.glm(model3, newdata=subset(gendata, cow=="JPN"))
b<-plogis(a[!is.na(a)])
1-prod(1-b) # probability of 0.94

# Calculate the number of years covered by model 3:
x<-names(model3$coefficients)
x<-x[x!="(Intercept)"]
x<-c("country", "cow", "year","ratified",x)
newdata<-na.omit(gendata[,x])
range(newdata$year) # 1949-2001



### Appendix Figure 1:
oldstates<-ratdata$cow[ratdata$t.start==0]
gendata.old<-subset(gendata, cow %in% oldstates)

model1old<-glm(ratified ~ total  + neighbors + polity + newdemoc + domestic.hurdles + british.legal +  + loggdppc + nminorities, data=gendata.old, binomial(link = "logit")) #  
summary(model1old)

model2old<-glm(ratified ~ total + totalsq  + neighbors + polity + newdemoc + domestic.hurdles + british.legal +  + loggdppc + nminorities, data=gendata.old, binomial(link = "logit")) #  
summary(model2old)

model3old<-glm(ratified ~ total + totalsq + totalcu + neighbors + polity + newdemoc + domestic.hurdles + british.legal +  + loggdppc + nminorities, data=gendata.old, binomial(link = "logit")) #  
summary(model3old)

pdf(file="/Users/brian/Brian/Genocide paper/Paper drafts/graphics/totals-oldstates.pdf")
X3<-Xbuilder(model3old, fulldata, "total", columns=100)
X3["totalsq",]<-X3["total",]^2
X3["totalcu",]<-X3["total",]^3
logit.plot(model3old, X3, xlab="Cumulative Number of Ratifications", ylab="Annual Probability of Ratification")
dev.off()


### Appendix Table 1:

# Past genocide:
model4<-glm(ratified ~ total + totalsq +totalcu + neighbors + polity + newdemoc + domestic.hurdles + british.legal +  + deathmag.max + loggdppc + nminorities, data=gendata, binomial(link = "logit")) # 

# IGO Context
model5<-glm(ratified ~ total + totalsq +totalcu + neighbors + IGOc + polity + newdemoc + domestic.hurdles + british.legal +  + loggdppc + nminorities, data=gendata, binomial(link = "logit"))   

# Add the Mitchell and Powell legal system dummy variables (but at the same time exclude the common law dummy I already have)
model6<-glm(ratified ~ total + totalsq + totalcu + neighbors + polity + newdemoc + domestic.hurdles +  + loggdppc + nminorities + common.law + islamic.law + mixed.law, data=gendata, binomial(link = "logit")) #  

# Exconst:
model7<-glm(ratified ~ total + totalsq + totalcu + neighbors + polity + newdemoc + domestic.hurdles  +  + loggdppc + nminorities + exconst + common.law + islamic.law + mixed.law, data=gendata, binomial(link = "logit")) #  

# Council of Europe dummy variable:
model8<-glm(ratified ~ total + totalsq + totalcu + neighbors + polity + newdemoc + domestic.hurdles + british.legal +  + loggdppc + nminorities + coe, data=gendata, binomial(link = "logit")) #  
summary(model7)


capture.output(displaytable(model3, model4, model5, model6, model7, model8, coefnames=c("Constant", "Global Ratifications", "Global Ratifications Squared", "Global Ratifications Cubed", "Neighborhood Effect", "Democracy", "New Democracy", "Institutional Hurdles", "Common Law", "GDP per capita (logged)", "Minorities at Risk", "Genocide/Politicide","IGO Influences", "Common Law tradition", "Islamic Law tradition", "Mixed law tradition", "Constraints on Executive", "Council of Europe"), modelnames=paste("Model",3:8)), file="/Users/brian/Brian/Genocide paper/Paper drafts/tables/Appendixtable1.tex")


### Appendix Figure 2
testmodel<-function(model, data=fulldata, ...){
	X<-Xbuilder(model, data, "total", columns=100)
	if (any(rownames(X)=="totalsq")) X["totalsq",]<-X["total",]^2
	if (any(rownames(X)=="totalcu")) X["totalcu",]<-X["total",]^3
	logit.plot(model, X, xlab="Cumulative Number of Ratifications", ylab="Annual Probability of Ratification", ...)
	}

pdf(file="/Users/brian/Brian/Genocide paper/Paper drafts/graphics/appendixfig2.pdf")
par(mfrow=c(3,2))
testmodel(model3, main="Model 3", ylim=c(0,0.3))
testmodel(model4, main="Model 4", ylim=c(0,0.3))
testmodel(model5, main="Model 5", ylim=c(0,0.3))
testmodel(model6, main="Model 6", ylim=c(0,0.3))
testmodel(model7, main="Model 7", ylim=c(0,0.3))
testmodel(model8, main="Model 8", ylim=c(0,0.3))
dev.off()

