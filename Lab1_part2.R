#import data
EPI_data <- read.csv(file = "C:/Users/gaoy13/Desktop/6600/Lab/Lab1/EPI_Data.csv")
attach(EPI_data)
multivariate <- read.csv("C:/Users/gaoy13/Desktop/6600/Lab/Lab1/multivariate.csv")
attach(multivariate)

#fitting a distribution beyond histograms
plot(ecdf(EPI),do.points=FALSE,verticals = TRUE) #ECDF

par(pty="s") # square single chart mode

qqnorm(EPI)
qqline(EPI)

x <- seq(30,95,1)
qqplot(qt(ppoints(250),df=5),x,xlab="Q-Q plot for t dsn")
qqline(x)

# code...
plot(ecdf(EPI_data$EPI),do.points=FALSE, verticals = TRUE) #ECDF line
plot(ecdf(EPI_data$EPI),do.points=TRUE, verticals = TRUE) #ECDF line + points

par(pty="s")
help("qqnorm")
help("qqplot")

qqnorm(EPI_data$EPI) #q-q plot
qqline(EPI_data$EPI) #add auxiliary line to q-q plot

x <- seq(30,95,1)
x2 <- seq(30,95,2)
x3 <- seq(30,96,2)

qqplot(qt(ppoints(250),df=5),x, xlab = "Q-Q plot")
qqline(x)
qqplot(qt(ppoints(250),df=5),x2, xlab = "Q-Q plot")
qqline(x2)
qqplot(qt(ppoints(250),df=5),x3, xlab = "Q-Q plot")
qqline(x3)

#fitting a distribution
plot(ecdf(EPI_data$DALY),do.points=FALSE, verticals = TRUE)
qqnorm(EPI_data$DALY)
qqline(EPI_data$DALY)

plot(ecdf(EPI_data$WATER_H),do.points=FALSE, verticals = TRUE)
qqnorm(EPI_data$WATER_H)
qqline(EPI_data$WATER_H)

qqplot(EPI,DALY)

#comparing distributions
boxplot(EPI_data$EPI,EPI_data$DALY)

boxplot( EPI,ENVHEALTH, ECOSYSTEM, DALY, AIR_H,WATER_H, AIR_E, BIODIVERSITY)

#Multivariate.csv dataset
mm <- lm(Homeowners~Immigrant)
mm
help(lm)

summary(mm)

plot(Homeowners~Immigrant)
help(abline)
abline(mm) # add auxiliary line
abline(mm,col=2,lwd=3) #col color; lwd width

abline(mm,col=3,lwd=3) 
attributes(mm)
mm$coefficients

#In-Class Work: ggplot examples
plot(mtcars$wt,mtcars$mpg)
library(ggplot2) #ggplot2 is an open-source data visualization package
qplot(mtcars$wt,mtcars$mpg) #quick plot
qplot(wt,mpg,data = mtcars) #synonym
ggplot(mtcars,aes(x=wt,y=mpg))+geom_point() #synonym
plot(pressure$temperature,pressure$pressure,type="l")
#plot types:
#p	Points plot (default)
#l	Line plot
#b	Both (points and line)
#o	Both (overplotted)
#s	Stairs plot
#h	Histogram-like plot
#n	No plotting
points(pressure$temperature,pressure$pressure) #add points to plot
lines(pressure$temperature,pressure$pressure/2,col="red") #add lines to plot
points(pressure$temperature,pressure$pressure/2,col="blue")

library(ggplot2)
qplot(pressure$temperature,pressure$pressure, geom="line")
qplot(temperature,pressure,data = pressure, geom="line")
ggplot(pressure,aes(x=temperature,y=pressure))+geom_line()+geom_point()
ggplot(pressure,aes(x=temperature, y=pressure))+geom_line()+geom_point()

#Creating Bar graphs
barplot(BOD$demand, names.arg = BOD$Time)
table(mtcars$cyl) # count for each distinct value
barplot(table(mtcars$cyl)) # barplot by distinct values
qplot(mtcars$cyl) # consider the gap
qplot(factor(mtcars$cyl)) # not consider the gap

qplot(factor(cyl),data=mtcars) #synonym
ggplot(mtcars,aes(x=factor(cyl)))+geom_bar() #synonym

#Creating Histograms using ggplot
library(ggplot2)
plot(ToothGrowth$supp, ToothGrowth$len)
boxplot(len~supp,data=ToothGrowth)
boxplot(len~supp+dose,data=ToothGrowth)
qplot(ToothGrowth$supp,ToothGrowth$len,geom="boxplot")
qplot(supp,len,data=ToothGrowth,geom="boxplot")
ggplot(ToothGrowth,aes(x=supp,y=len))+geom_boxplot()
qplot(interaction(ToothGrowth$supp,ToothGrowth$dose),ToothGrowth$len,geom="boxplot")
ggplot(ToothGrowth,aes(x=interaction(supp,dose),y=len))+geom_boxplot()
