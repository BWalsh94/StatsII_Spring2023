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

lapply(c("stringr"),  pkgTest)

# set wd for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


#####################
# Problem 1
#####################

#Making data reproducible
set.seed(123)

# Creating data comprising 1,000 Cauchy random variables
data  <- rcauchy(1000, location = 0, scale = 1)

# Creating my own Kolmogorov-Smirnov test function
kol_smir.test <- function(x, y) {
  ECDF <- ecdf(x)
  empiricalCDF <- ECDF(x)
  D <- max(abs(empiricalCDF - pnorm(x)))
  p_value <- pt(q=D, df=length(x)-1, lower.tail = FALSE)
  return(c("D" = D, "p-value" = p_value))
}

# Performing my Kolmogorov-Smirnov test
kol_smir.test(data,"pnorm")

#####################
# Problem 2
#####################

# Creating data
set.seed(123)
data <- data.frame(x = runif(200, 1, 10))
data$y <- 0 + 2.75*data$x + rnorm(200, 0, 1.5)

# Creating a function to minimise residuals to use in the Newton-Raphson algorithm
minimise_residuals <- function(data, par) {
  with(data, sum((par[1] + par[2] * x - y)^2))
}

# Estimating an OLS regression using the Newton-Raphson algorithm
optim(par = c(0,1), fn=minimise_residuals, data=data,
      method = "BFGS")

# Comparing the results of the Newton-Raphson algorithm to the lm() function
lm(y ~ x, data=data)

