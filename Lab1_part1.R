# import data
EPI_data <- read.csv(file = "C:/Users/gaoy13/Desktop/6600/Lab/Lab1/EPI_Data.csv")

# view data in seperate window
View(EPI_data)

# add shortcuts to subcolumns of database
attach(EPI_data)
EPI

# A simple UI to fix data by hand
fix(EPI_data)

# identify na data
tf <- is.na(EPI)
tf
# select non-na data
E <- EPI[!tf]
E

# common commands
summary(EPI) #work for both col and database

fivenum(EPI,na.rm = TRUE) # five points to describe a col

stem(EPI) # stem and leaf plot

hist(EPI) # histogram
hist(EPI, seq(30.,95,1.0),prob=TRUE) #(lower_bound, higher_bound,unit length)
help(hist)
lines(density(EPI,na.rm=TRUE,bw=1.)) #line chart on histogram
lines(density(EPI,na.rm=TRUE,bw="SJ"))
help(lines)
rug(EPI)

plot(ecdf(EPI),do.points=FALSE,verticals=TRUE) #Empirical Cumulative Distribution Function

par(pty="s") # stitching charts 
help(par)
qqnorm(EPI) #Quantile-Quantile
qqline(EPI) #based on previous chart

x <- seq(30,95,1)
qqplot(qt(ppoints(250),df=5),x,xlab="Q-Q plot for t dsn")
qqline(x)

boxplot(EPI,DALY) # boxplot
