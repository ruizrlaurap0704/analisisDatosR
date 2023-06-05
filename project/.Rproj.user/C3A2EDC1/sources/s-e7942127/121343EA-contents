## ----Simple addition prints to screen------------------------------------
1+2

## ----Creating an object--------------------------------------------------
oneplustwo <- 1 + 2

## ----What kind of object is it?------------------------------------------
class(oneplustwo)
str(oneplustwo)
typeof(oneplustwo)

## ------------------------------------------------------------------------
str(as.character(oneplustwo))

## ----Making a vector-----------------------------------------------------
oneANDtwo <- c(1,2)
class(oneANDtwo)
str(oneANDtwo)
typeof(oneANDtwo)

## ------------------------------------------------------------------------
data.frame(oneANDtwo, oneplustwo)

## ----results='hide'------------------------------------------------------
oneANDtwo - 1

## ----Remove some objects-------------------------------------------------
rm(oneplustwo)
rm(oneANDtwo)

## ----install packages, eval = FALSE--------------------------------------
install.packages("pdfetch")
install.packages("xts")
install.packages("stargazer")
install.packages("zoo")
install.packages("ggplot2")
install.packages("knitr")

## ----Load packages, warning=FALSE, message=FALSE-------------------------
library(pdfetch)
library(stargazer)
library(xts)
library(zoo)
library(ggplot2)
library(knitr)

## ----Loading built in datasets-------------------------------------------
data("mtcars")
head(mtcars)

## ----Get Fred Data, results="hide"---------------------------------------
pdfetch_FRED("THREEFY10")

## ----Get Fred Data Into an Object----------------------------------------
treasury <- pdfetch_FRED(c("THREEFY10", "THREEFYTP10"))
plot(treasury)

## ----Write data to CSV---------------------------------------------------
write.csv(as.data.frame(treasury),"treasury.csv")

## ----Information about xts objects---------------------------------------
class(treasury)
dim(treasury)
names(treasury)
start(treasury)
end(treasury)
first(treasury)
last(treasury)
periodicity(treasury)

## ----Subsetting xts objects, results='hide'------------------------------
head(treasury$THREEFY10)
treasury["2000"]
treasury["2000-07"]
treasury_subset <- treasury["2008/2011"]

## ----Change periodicity, warning=FALSE-----------------------------------
THREEFY10.monthly <- to.monthly(treasury$THREEFY10)
THREEFYTP10.monthly <- to.monthly(treasury$THREEFYTP10)

## ----Change between ts and xts-------------------------------------------
gdp.FRED = pdfetch_FRED("A191RO1Q156NBEA")  # Real GDP growth: Percent Change from Quarter One Year Ago, Seasonally Adjusted
gdp = ts(gdp.FRED, start=start(to.quarterly(gdp.FRED)), end=end(to.quarterly(gdp.FRED)), frequency=4)
plot(gdp)

## ----Using autoplot.zoo--------------------------------------------------
p <- autoplot.zoo(treasury, facets=NULL)
p

## ----Customizing the plot------------------------------------------------
p1 <- p +  labs(title = "Ten Year Treasury Yield and Term Premium, 1990-2017", caption = "Sources: Federal Reserve Bank of New York, Federal Reserve", x = "Year") + scale_colour_grey()  + theme(legend.title = element_blank()) + theme(legend.justification=c(1,1), legend.position=c(.95,.95)) 
p1 + theme_bw()

## ----Making a regression model-------------------------------------------
model <- lm(mpg ~ wt + cyl, data = mtcars)
str(model)
class(model)
summary(model)

## ----Making a pretty table, results='asis'-------------------------------
stargazer(model, type="html")

