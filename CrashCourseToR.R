# OPER 528 R Introduction 
# TIP: Command + Enter to run a line

# Assignment 
x = 1 
x <- 1

# Scalar assignment 
x = c(1,2,3)
x[1]
x = seq(1,100,length=100) 

x[10:15]
x[-1]

for(i in 1:100){
  x[i] = i^2
}

f=function(x, a=2){
  x^a - sin(x) + log(x)
}

# as length increases, it will look smoother
x = seq(1,5,length=5)
y = f(x)

# Make sure to use labels
plot(x,y,type="l", xlab = "x", ylab = "y")

mean(y)
var(y)
sqrt(var(y)) # standard deviation

# EXAMPLE 1: rnorm with 50 samples, mean of 1, standard deviation of 1.5
# Generates random samples for the normal distribution
x = rnorm(50,1,1.5) 
n = 50

# EXAMPLE 2: Build 95% confidence interval, see confidence interval equation
# To find the z value, you need to determine which confidence interval you want 
# REMEMBER: Find quartile:
alpha = 1 - 0.95 
qrt = 1 - alpha/2 # = 0.975
# qnorm: give us the quartile of a normal distribution 
# qnorm ( quartile (ex. 0.975), default mean = 0, default sd = 1)

# 1.5 gotten from rnorm inputs
# lower bound for 95% confidence interval = 0.6645536
mean(x) - qnorm(0.975)*sqrt(1.5^2/n) 
# upper bound for 95% confidence interval = 1.496096
mean(x) + qnorm(0.975)*sqrt(1.5^2/n)
# Therefore, confidence interval: (0.665,1.496)

# EXAMPLE 3: Build confidence interval when you do not know true mean/not normal distribution
# NOTE: if you do not know the true variance is, you can use the sample variance var() and t-value instead of z-value
# qt - need to specify degrees of freedom = n - 1 
df = n - 1
# lower bound for 95% confidence interval 
mean(x) - qt(0.975,df)*sqrt(var(x)^2/n) 
# upper bound for 95% confidence interval 
mean(x) + qt(0.975,df)*sqrt(var(x)^2/n)
# Therefore, confidence interval: (0.595334, 1.91825) when the population variance is unknown

# Crash Course to R Functions
#dt = density function of t distribution 
#pt = probability  
#qt = quantile 
#rt = random

# EXAMPLE 4: Exporting a CSV with our data
y = rt(50,10)
z = cbind(x,y)
colnames(z) = c("normal sample", "t sample") # change column names
write.csv( z, "528/Example.csv",row.names=FALSE) # write.csv(variable you want to save, "file name")

# EXAMPLE 5: Reading a CSV of data
data = read.csv("528/Example.csv", header=TRUE)
dim(data) # to know how many rows and columns
head(mydata) # to get first 6 rows

# Values in matrix
data[1, 1] 
data[1, ] # Row 1
data[ ,1] # Column 1
z1 = data$normal.sample # Column 1

# EXAMPLE 5
hist(z1, xlab = "Data in Column 1", 
     ylab = "# of observations", 
     main = "Histogram of Data", 
     col = "light blue") 

# to plot relative frequency (proportion) instead of raw count
hist = hist(z1, freq = FALSE, 
     xlab = "Data in Column 1", 
     ylab = "Proportion of Observations", 
     main = "Histogram of Data", 
     col = "light green") 

# to plot density curve
hist(z1, freq = FALSE, 
     xlab = "Data in Column 1", 
     ylab = "Proportion of Observations", 
     main = "Histogram of Data", 
     col = "light green")
lines(density(z1)) # can be plotted on top of histogram

# EXAMPLE 6: printing only positive numbers
# OPTION 1: 
for(i in 1:50){
  if(z1[i] > 0) {
    print(x[i])
  }
}

# OPTION 2:
z1[z1>0]
