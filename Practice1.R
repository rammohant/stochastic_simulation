# Tara Ram Mohan
# OPER 528 Homework 1 
# Due February 14, 2022

# 1. The pdf of a random variable X
# b. Plot the pdf of X in R.
pdf <- function(x){ 
  ifelse((0 < x & x<1),(x^2) + (2/3)*x + (1/3), ifelse((x>1 | x < 0),0, NA))
} 
plot(pdf,
     xlim=c(-1,3), ylim = c(-1, 5), 
     xlab = "x", 
     ylab = "Probability", 
     main = "PDF of F(x)", 
     col = "red",lwd=2,) 

# c. Plot the cdf of F(x) in R.
cdf <- function(x){ 
  ifelse((0 <= x & x<=1),(1/3)*(x^3) + (1/3)*(x^2) + (1/3)*(x), 
         ifelse((x>1),1,
                ifelse((x<0),0, NA)))
} 
plot(cdf,
     xlim=c(-1,3), ylim = c(-1, 5), 
     xlab = "x", 
     ylab = "Probability", 
     main = "CDF of F(x)", 
     col = "red",lwd=2) 

# QUESTION 4 ---------------------------------------------------------------------------
# 4. For the dataset “Unrealistic” on Canvas, do the following per group of data :
data <- read.csv("528/Unrealistic.csv",header=TRUE)

n = 100 # 100 rows
df = n - 1 # degrees of freedom

# GROUP 1 ---------------------------------------------------------------------------
# a. Find the sample mean, sample variance.
m.g1 = mean(data$Group.1) # Sample mean = 1.988805
v.g1 = var(data$Group.1) # Sample variance = 0.2899428

# b. Build a 95% confidence interval for the mean.
lower = m.g1 - qt(0.975,df)*sqrt(v.g1/n) 
upper = m.g1 + qt(0.975,df)*sqrt(v.g1/n)
# Confidence interval: (1.881962, 2.095648)

# c. Plot the density histogram as well as the density curve. 
hist(data$Group.1, freq = FALSE, 
     xlab = "Data in Group 1", 
     ylab = "Proportion of Observations", 
     main = "Density Histogram and Curve of Group 1", 
     col = "light green") 
lines(density(data$Group.1)) 

# Guess from what distribution the data arises: Uniform distribution

# GROUP 2 ---------------------------------------------------------------------------
m.g2 = mean(data$Group.2) # Sample mean = 0.9531702
v.g2 = var(data$Group.2) # Sample variance = 1.104647

# b. Build a 95% confidence interval for the mean.
lower = m.g2 - qt(0.975,df)*sqrt(v.g2/n) 
upper = m.g2 + qt(0.975,df)*sqrt(v.g2/n)
# Confidence interval: (0.7446247, 1.161716)

# c. Plot the density histogram as well as the density curve. 
hist(data$Group.2, freq = FALSE, 
     xlab = "Data in Group 2", 
     ylab = "Proportion of Observations", 
     main = "Density Histogram and Curve of Group 2", 
     col = "light green") 
lines(density(data$Group.2)) 

# Guess from what distribution the data arises: Normal distribution

# GROUP 3 ---------------------------------------------------------------------------
m.g3 = mean(data$Group.3) # Sample mean = 1.014875
v.g3 = var(data$Group.3) # Sample variance = 1.025814

# b. Build a 95% confidence interval for the mean.
lower = m.g3 - qt(0.975,df)*sqrt(v.g3/n) 
upper = m.g3 + qt(0.975,df)*sqrt(v.g3/n)
# Confidence interval: (0.8139089, 1.215842)

# c. Plot the density histogram as well as the density curve. 
hist(data$Group.3, freq = FALSE, 
     xlab = "Data in Group 3", 
     ylab = "Proportion of Observations", 
     main = "Density Histogram and Curve of Group 3", 
     col = "light green") 
lines(density(data$Group.3)) 

# Guess from what distribution the data arises: Log-normal distribution ( was actually exponential )

# GROUP 4 ---------------------------------------------------------------------------
m.g4 = mean(data$Group.4) # Sample mean = 3.116199
v.g4 = var(data$Group.4) # Sample variance = 3.714955

# b. Build a 95% confidence interval for the mean.
lower = m.g4 - qt(0.975,df)*sqrt(v.g4/n) 
upper = m.g4 + qt(0.975,df)*sqrt(v.g4/n)
# Confidence interval: (2.733757, 3.498641)

# c. Plot the density histogram as well as the density curve. 
hist(data$Group.4, freq = FALSE, 
     xlab = "Data in Group 4", 
     ylab = "Proportion of Observations", 
     main = "Density Histogram and Curve of Group 4", 
     col = "light green") 
lines(density(data$Group.4)) 

# Guess from what distribution the data arises: Erlang distribution
