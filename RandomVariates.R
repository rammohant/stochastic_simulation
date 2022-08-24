# Unit 9 Random Variate Generation
# 4.4.22

# Linear Congruential Generator 

# Want to generate 100 samples 
n = 100 

# Want to save samples somewhere 
sx = NULL 

# CDF of discrete random variable of interest 

# Option 1: can generate 1 standard variable n times 

for(i in 1:n){
  r = runif(1)
  
  # True probability (Standard uniform distribution)
  if (r <= 0.1) {
    x = 0 # generate a sample for the first bucket
  } else if (r > 0.1 & r <= 0.3) {
    x = 1
  } else if (r > 0.3 & r <= 0.55) {
    x = 2
  } else if (r > 0.55 & r <= 0.85) {
    x = 3
  } else if (r > 0.85 & r <= 0.95) {
    x = 4
  } else if (r > 0.95) {
    x = 5
  }
  
  # for each new x, attach the new sample to the end of the sx variable 
  sx = c(sx,x)

}

# Law of large number

# Find relative random frequency
# For 5, expected probability = 0.05, actual = 0.07
# Difference is due to finite sample size
length(sx[sx ==5])/n

# GOAL: Want to creat e2 events 
# Randomness is borrowed from random discrete distribution 

#--------------------------------------------------------------------------------
# Inverse Transformation Method
r = runif(n) # standard uniform random variate 

# r = x = response 

lambda = 2
x = -1/lambda * log(1 - r)
hist(x)

#--------------------------------------------------------------------------------
# Acceptance-Rejection Method

# If you want, you can set a,b,c arguments for triangular function 

# Create pdf function
f = function(x) {
  if (x < 2) {
    return(0)
  } else if (x > 8) {
    return(0)
  } else if (x >= 2 & x < 6) {
    return(-1/6 + x/12)
  } else if (x >= 6 & x <= 8) {
    return(4/3 - x/6)
  }
}

m = f(6)
n = 100 # number of samples that you have is going to be less than 100
xs = NULL 

for(i in 1:n){
  r = runif(2) # for each iteration, we generate 2 standard uniform variables
  xstar = 2 + (8-2)*r[1]
  
  if (r[2] <= f(xstar)/m) {
    xs = c(xs, xstar)
  }
}

hist(xs) # Ideally should be triangular distribution, but you can use the hypothesis test to test that 

# To ensure that you have the necessary number of samples, you should use a while loop
while(length(xs) < n){
  r = runif(2) # for each iteration, we generate 2 standard uniform variables
  xstar = 2 + (8-2)*r[1]
  
  if (r[2] <= f(xstar)/m) {
    xs = c(xs, xstar)
  }
}
hist(xs) # Ideally should be triangular distribution, but you can use the hypothesis test to test that 
#--------------------------------------------------------------------------------
# Convolution Method 

# When we apply Central Limit Theorem, sample size should be at least 30 (else, CLT cannot be applied)
r = 30 # represents the 'n' on top of the summation

# Generate 100 samples for N(mu, sigma^2)
n = 100 
mu = 1 
sigma = 1.5
x = NULL

for(i in 1:n){
  u = runif(r)
  z = (sum(u) - 0.5*r)/sqrt(r/12) #standard normal univariate
  x = c(x, mu+sigma*z)
}

hist(x) 
# 0.9179031
mean(x) # should not be far from population mean 

# EXAMPLE --------------------------------------------------------------------------------
# Convolution Method for Erlang Distribution 

# Works because erlang is the sum of exponential 

# Generate 100 samples for Erlang(k,mu)
n = 100
k = 3
mu = 1.5
x = NULL

for(i in 1:n){
  r = rexp(k, mu) # For 1 erlang, we need k exponential
  x = c(x, sum(r))
}

hist(x) 
mean(x) # 1.98908; should not be far from population mean 
# This is just one dimension, so marchov chain monte carlo is used for more complicated applications 

