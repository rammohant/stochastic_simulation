# OPER 528 Final Exam 
# Due May 4 2022

# PROBLEM 2 -------------------------------------------------------------------
n = 100 # 100 i.i.d. samples

# Generate x from Exp(2) using U[0,1] using inverse transformation method
r = runif(n) # standard uniform random variate 
lambda = 2
x = -1/lambda * log(1 - r)

# Function to random variable 𝑍 using 𝑌 from 𝑈[0, 1] and input x
f = function(x) {
  y = runif(1)
  if (y <= 0.6) {
    return (1.2 * x)
  } else if (y > 0.6) {
    return (0.8*x + 0.5)
  }
}

z = f(x)
hist(z)

# PROBLEM 3 -------------------------------------------------------------------
source = read.csv('528/Naive.csv')
data = source$Data

# (a) Give your hypothesis on the distribution: appear to be an Erlang distribution
hist(data)

# (b) Estimate the parameters that are required to specify the hypothesized distribution. 
# H_O: The samples are 100 i.i.d observations from a erlang distribution gamma(k,mu)

# We can use method of the moment to estimate parameters for erlang
k1 = mean(data)^2 /var(data) # shape parameter = 3.331545
mu = mean(data)/var(data) # mu = 2.272763

k = 6 # number of small intervals
n = length(data) # 36
m = n/k
r = 2  #number of parameters to specify the gamma distribution 

# Find starting and ending points of the intervals
intdiv = seq(0,0,length = k + 1)

for(i in 1:(k+1)) {
  intdiv[i] = qgamma(((i-1)/k),k1,mu) 
}

# Count the num of observations in each interval
x = seq(0,0, length=k)
for(i in 1:(k+1)) {
  x[i] = length(data[data <= intdiv[i+1]]) - length(data[data < intdiv[i]])
}

# You can verify that your num observations is correct by taking the sum 
sum(x) # = 36

# Get test statistics
teststat = sum(x^2/m) - n

# Now we compare this with the critical value
alpha = 0.05
criticalval = qchisq(1-alpha, k-r-1)

if(teststat > criticalval) {
  print("We reject H_O at level \alpha.")
} else {
  print("We fail to reject H_O at level alpha")
}

# Conclusion: "We fail to reject H_O at level alpha." Therefore, our hypothesis that the 100 i.i.d observations are from an erlang distribution is correct
# PROBLEM 4 -------------------------------------------------------------------
set.seed(99)

# Integrate the given function on interval (0,4)
f = function(x){
  4*sqrt(1-x^2)
}

# Expected value
e_val = 3.141592653589793

# Method 1: “Shooting Experiment” #############################################

# First we need to find the value ymax 
ymax = f(0)
a = 0
b = 1

# You can improve accuracy by generating more points
n = 10000
r = 30 
a_val_s = seq(0,0,length = r)
error_s = seq(0,0,length = n)

for (j in 1:r) {
  count = 0
  
  for(i in 1:n){
    x = runif(1,a,b)
    y = runif(1,0,ymax)
    
    if(y <= f(x)) {
      count = count + 1
    }
  }
  
  # Actual value 
  a_val_s[j] = count/n*(b-a)*ymax
}

# Actual value = 1.32672
actual_1 = mean(a_val_s)

# Average error 
error_1 =  ( abs(actual_1 - e_val) / e_val ) * 100

# Method 2: Mean Value Theorem #############################################

n = 10000 # num samples 
r = 30 
a_val_m = seq(0,0,length = r)
error_m = seq(0,0,length = r)

for (i in 1:r) {
  x = runif(n, a, b)
  
  # Actual
  a_val_m[i] = sum(f(x))/n*(b-a)
}

# Actual value = 1.330219
actual_2 = mean(a_val_m)

# Average error 
error_2 = ( abs(actual_2 - e_val) / e_val ) * 100

print(error_1)
print(error_2)

# PROBLEM 5 -------------------------------------------------------------------
library(triangle)

# Input given data tables
cost = 300 # million per mac
lifecycle = 3 # years 
discount_rate = 0.06
r = 500 
profit_generating = 0
npv = rep(0,r)

for(i in 1:r) {
  
  # Year 1 Profit
  u = runif(1) 
  
  if (u <= 0.25) {
    y1_profit = 90
  } else if (u > 0.25 & u <= 0.65) {
    y1_profit = 100
  } else if (u > 0.65) {
    y1_profit = 110
  }
  
  # Year 2 Profit
  increase_rate = rtriangle(1, 0, 0.4, 0.2)
  y2_profit = y1_profit*(1+increase_rate)
  
  # Year 3 Profit 
  decrease_rate = rtriangle(1, 0.2, 0.4, 0.3)
  y3_profit = y2_profit*(1+decrease_rate)
  
  profit = y1_profit + y2_profit + y3_profit
  npv[i] = (profit - cost)/(1+discount_rate)^i
  
  if (npv[i] > 0) {
    profit_generating = profit_generating + 1
  }
}

mean(npv) # 14.0567
(profit_generating/r)*100 # 99.8% of the time it is profit generating
