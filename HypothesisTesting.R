#Set the mean and variance for the normal distribution
mu <- 0
sigma <- 1

#Generate the 100 values for random variable X 
x <- seq(-4*sigma, 4*sigma, length.out=100)

#Calculate the normal distribution density
y <- dnorm(x, mean=mu, sd=sqrt(sigma))

#Plot the normal distribution
par(mar = c(3, 3, 3, 3)) #c(bottom, left, top, right)
plot(x, y, type='l', lwd = 2, cex=4, col = "black", xlab = "X", ylab = "f(X=x)")

#Load weather data
data <- read.csv("/Users/fatimabatool/downloads/HT/Hypothesis Testing data examples/weather2020.csv")


# The hypothesised value: mu = 7.144 is told. The hypothesised mean is taken form another year average from the past.

#From this sample of 366 weather averages for this location, test wheather the average hourly tempreture has differ from hypothesised average tempreture. 

#Since population variance is not known we estimate it from sample. 
vardata <- var(data$average_temperature_celsius)

#Calculate the T Statistics: T = sqrt(n)(Xbar - mu)/S ~ t(n-1) 
meandata <- mean(data$average_temperature_celsius)
n <- 366

Tstatistics <- sqrt(n)*(meandata-7.144)/sqrt(vardata)

#Calcuate the critical values for the two tail test
confidence_level <- 0.95
degree_of_freedom <- n-1
Ttabular <- qt((1-confidence_level)/2, df=degree_of_freedom)

#Calculating Pvalue, whats the probability of getting a T statistics as large as Ttabular?
#left-tailed test
Pvalue_lower <- pt(Ttabular, degree_of_freedom)

#right-tailed test
Pvalue_upper <- pt(Ttabular, degree_of_freedom, lower.tail = FALSE)


#Decision: Since Pvalue is less than the significance level (alpha=0.05), we reject the null hypothesis. Based on the data sample at hand, there is evidence that the average tempreture for year 2020 has changed form hypothesised year average value. 

pt(-1.35, df=15)
pt(2.89, df=46, lower.tail = FALSE) 
2*pt(-1.89, df=310)


##Set the df for t-distibution
df <- 310
sigma <- 1

#Generate the 100 values for random variable X 
x <- seq(-4*sigma, 4*sigma, length.out=100)

#Calculate the normal distribution density
y <- dt(x, df) 

#Plot the t dist distribution
par(mar = c(3, 3, 3, 3)) #c(bottom, left, top, right)
plot(x, y, type='l', lwd = 3, col = "blue", xlab = "X", ylab = "f(X=x)")


