# Tara Ram Mohan
# OPER 528 Homework 4
# Due April 13, 2022

# 1. Show how to generate 100 samples from a Cauchy distribution using the inverse transform method. 
n = 100
r = runif(n) # standard uniform random variate 

# r = x = response 
x = tan(pi*r)
hist(x)

#--------------------------------------------------------------------------------
# 2. Show how to generate 100 samples from a triangular distribution with parameters (5, 8, 6) using the acceptance-rejection method.
a = 5 #min
b = 8 #max
c = 6 #mode

# If you want, you can set a,b,c arguments for triangular function 

# Create pdf function
f = function(x) {
  if (x < 5) {
    return(0)
  } else if (x > 8) {
    return(0)
  } else if (x >= 5 & x < 6) {
    print(1)
    return (2*(x-a))/((b-a)*(c-a))
  } else if (x >= 6 & x <= 8) {
    print(2)
    return (2*(b-x))/((b-a)*(b-c))
  }
}
m = f(7)
n = 100 # number of samples that you have is going to be less than 100
xs = NULL 

while(length(xs) < n){
  r = runif(2) # for each iteration, we generate 2 standard uniform variables
  xstar = 5 + (8-5)*r[1]
  
  if (r[2] <= f(xstar)/m) {
    xs = c(xs, xstar)
  }
}

hist(xs) # Ideally should be triangular distribution, but you can use the hypothesis test to test that 

#--------------------------------------------------------------------------------
# Problem 3: Use two Monte Carlo integration techniques to estimate

set.seed(99)
# Integrate the given function on interval (0,4)
f = function(x){
  1/(1+x^2)
}

# Method 1: “Shooting Experiment” #############################################

# First we need to find the value ymax 
ymax = f(0)

# You can improve accuracy by generating more points
n = 200
r = 30 
a_val_s = seq(0,0,length = r)
error_s = seq(0,0,length = n)

for (j in 1:r) {
  count = 0
  
  for(i in 1:n){
    x = runif(1,0,4)
    y = runif(1,0,ymax)
    
    if(y <= f(x)) {
      count = count + 1
    }
  }
  
  # Actual value 
  a_val_s[j] = count/n*(4-0)*ymax
  
  # Expected value
  e_val = 1.325818
}

# Actual value = 1.32672
actual_1 = mean(a_val_s)

# Average error 
error_1 =  ( abs(actual_1 - e_val) / e_val ) * 100

actual_1
error_1
#--------------------------------------------------------------------------------
# Method 2: Mean Value Theorem 

set.seed(99)
n = 10000 # num samples 
r = 30 
a_val_m = seq(0,0,length = r)
error_m = seq(0,0,length = r)

for (i in 1:r) {
  x = runif(n, 0, 4)
  
  # Actual
  a_val_m[i] = sum(f(x))/n*(4-0)
  
  # Expected value
  e_val = 1.325818
}

# Actual value = 1.330219
actual_2 = mean(a_val_m)

# Average error 
error_2 = ( abs(actual_2 - e_val) / e_val ) * 100

actual_2
error_2

# How many iterations do you need such that the error is below 1%? -------------------------- 
# Method 1: “Shooting Experiment” #############################################

set.seed(99)
# First we need to find the value ymax 
ymax = f(0)

# You can improve accuracy by generating more points
n = 1000
r = 30 
a_val_s = seq(0,0,length = r)
error_s = seq(0,0,length = n)
actual_1 = seq(0,0,length = n)
# Expected value
e_val = 1.325818

# Check for number of iterations needed for error below 1% 
for(k in 1:n){
  # Run multiple replications
  for (j in 1:r) {
    count = 0
    
    for(i in 1:k){
      x = runif(1,0,4)
      y = runif(1,0,ymax)
      
      if(y <= f(x)) {
        count = count + 1
      }
    }
    
    # Actual value 
    a_val_s[j] = count/k*(4-0)*ymax
    
  }
  actual_1[k] = mean(a_val_s)
  
  # Calculate error
  error_s[k] = ( abs(actual_1[k] - e_val) / e_val ) * 100
}
  
error_s

#--------------------------------------------------------------------------------
# Method 2: Mean Value Theorem 

set.seed(99)
n = 1000 # num samples 
r = 30 
a_val_m = seq(0,0,length = r)
error_m = seq(0,0,length = n)
actual_2 = seq(0,0,length = n)

# Alternatively, you can use a while loop with (approx n - approx n + 1 < epsilon) OR (approx - true > 1% * true) if we can calculate analytical solution
for(k in 1:n){
  for (i in 1:r) {
    x = runif(k, 0, 4)
    
    # Actual
    a_val_m[i] = sum(f(x))/k*(4-0)
  }
  
  # Calculate error
  error_m[k] =  format( ( abs(mean(a_val_m)- e_val) / e_val ) * 100 , scientific = FALSE)
}

error_m
