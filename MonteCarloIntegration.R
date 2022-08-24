# Unit 10 Monte Carlo Integration
# 4.13.22

# Method 1: “Shooting Experiment” - samples both axises (which is more resource heavy)

# Integrate the normal pdf of N(1,1) on interval (2,4)
f = function(x,mu=1,sigma=1){
  1/sqrt(2*pi*sigma^2)*exp(-(x-mu)^2/2/sigma^2)
}

# Test that the function is correct
dnorm(1,1,1)
f(1)

# First we need to find the value ymax 
ymax = f(2)

# You can improve accuracy by generating more points
n = 10000
count = 0
for(i in 1:n){
  x = runif(1,2,4)
  y = runif(1,0,ymax)
  
  if(y <= f(x)) {
    count = count + 1
  }
}

# Estimated value = 0.1558291
count/n*(4-2)*ymax

# True value = 0.1573054 using the cdf of the normal distribution
pnorm(4,1,1) - pnorm(2,1,1)

#--------------------------------------------------------------------------------
# Method 2: Mean Value Theorem 

# Integrate the normal pdf of N(1,1) on interval [2,4]
f = function(x,mu=1,sigma=1){
  1/sqrt(2*pi*sigma^2)*exp(-(x-mu)^2/2/sigma^2)
}

# Can improve accuracy by (1) increasing n value, (2) by run multiple replications, or (3) use the same seed value

n = 1000 # num samples 
r = 30 

x = runif(n, 2, 4)

# Actual
sum(f(x))/n*(4-2)

# Expected
pnorm(4,1,1) - pnorm(2,1,1)

# EXAMPLE: ESTIMATE THE VALUE OF PIE USING SHOOTING METHOD ###########
n = 1000
x = runif(n, -1, 1)
y = runif(n, -1, 1)
c = 0

for(i in 1:n){
  if(x[i]^2 + y[i]^2 <= 1)
    c = c+1
}

(c/n)*4 # Should converge to the probability pi/4 -> multiply by 4 to estimate pi 

