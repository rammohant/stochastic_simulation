# Lecture 8 
# Input Analysis for Discrete-Event Simulations

# Chi squared goodness of fit
data = read.csv('528/Unrealistic.csv')

# Normally, plot each group and predict which distribution is appropriate to model each group
head(data)
d3 = data$Group.3
hist(d3)

# This part goes in your report ->
# H_O: The samples are 100 i.i.d observations from an exponential distribution Exp(lambda).

length(d3)
# (1) We can use method of the moment to estimate parameters for exp
lambda = 1/mean(d3)

# (2) Calculate test statistics 

# General guidelines: m >= 5 and k <= 30
# We have n=100 observations, and if we make all m's equal where m = n/k
# For this case, given n=100, m = n/k >= 5 -> 100/k >= 5 -> k <= 20

# (2) When selecting k, it'll be easier if you pick a k that yield a whole number m = n/k
# k = 10 
k = 10
n = 100
m = n/k
r = 1  #number of parameters to specify the distribution

# We are ensuring that the area underneath the curve (total area/probability = 1) is equal to 0.1 (1/k) or ten small intervals
# Expected number of observations = 10 - 0.1
# To find quantile, q+name of distr (EX. qexpo(0.1) or qgamma)

# (3) Find starting and ending points of the intervals
intdiv = seq(0,0,length = k + 1)

for(i in 1:(k+1)) {
  intdiv[i] = qexp(((i-1)/k),lambda) #First point is (1-1/k) for 0th quartile, and then 10th quantile...
}

# (4) Now, we need to count the num of obversations in each interval
x = seq(0,0, length=k)
for(i in 1:(k+1)) {
  x[i] = length(d3[d3 <= intdiv[i+1]]) - length(d3[d3 < intdiv[i]])
}

# You can verify that your num observations is correct by taking the sum 
sum(x) # = 100

teststat = sum(x^2/m) - n

# (5) Now we compare this with the critical value
alpha = 0.05
criticalval = qchisq(1-alpha, k-r-1)

if(teststat > criticalval) {
  print("We reject H_O at level \alpha.")
} else {
  print("We fail to reject H_O at level alpha")
}

# Conclusion will stay not consistent for different k's

# ------------------------------------------------------------------------------------------------------------------
# NOW IT'S TIME FOR AN ERLANG DISTRIBUTION

# Normally, plot each group and predict which distribution is appropriate to model each group
head(data)
d3 = data$Group.4
hist(d3) # Appears to be erlang

# This part goes in your report ->
# H_O: The samples are 100 i.i.d observations from an exponential distribution Gamma(k, my).
# Gamma allows for decimal values in case of noninteger erlang k
length(d3)
# (1) We can use method of the moment to estimate parameters for exp
lambda = 1/mean(d3)

# (2) When selecting k, it'll be easier if you pick a k that yield a whole number m = n/k
# This value must be an integer
# In most situations, do not round. Only round if you know ex. there are 3 servers
k1 = mean(d3)^2 / var(d3) # shape parameter
mu = mean(d3)/var(d3)

k = 20 # number of small intervals
n = 100
m = n/k
r = 2  #number of parameters to specify the distribution (k1 and mu)

# We are ensuring that the area underneath the curve (total area/probability = 1) is equal to 0.1 (1/k) or ten small intervals
# Expected number of observations = 10 - 0.1
# To find quantile, q+name of distr (EX. qexpo(0.1) or qgamma)

# (3) Find starting and ending points of the intervals
intdiv = seq(0,0,length = k + 1)

for(i in 1:(k+1)) {
  intdiv[i] = qgamma(((i-1)/k),k1,mu) #First point is (1-1/k) for 0th quantile, and then 10th quantile...
}

# (4) Now, we need to count the num of observations in each interval
x = seq(0,0, length=k)
for(i in 1:(k+1)) {
  x[i] = length(d3[d3 <= intdiv[i+1]]) - length(d3[d3 < intdiv[i]])
}

# You can verify that your num observations is correct by taking the sum 
sum(x) # = 100

# (5) Get test statistics
teststat = sum(x^2/m) - n

# (5) Now we compare this with the critical value
alpha = 0.05
criticalval = qchisq(1-alpha, k-r-1)

if(teststat > criticalval) {
  print("We reject H_O at level \alpha.")
} else {
  print("We fail to reject H_O at level alpha")
}

# Best solution when its close to collect more data
# Check how similar the difference between the teststat criticalval, bigger difference means more right
# If you're unsure, "I recommend that" because one option is more appropriate to do

# ------------------------------------------------------------------------------------------------------------------
# Assessing Sample Independence
# 3/30/22

# Method 2: Scatterplot method 

n = 50 # num samples
# If there are n samples, we need to shift the sequence n-1 times (i.e. calculate n-1 correlations or plot n-1 scatterplots)
x = rexp(n,1)

corr = seq(0,0,length=n-1)
y = seq(0,0,length=n)

# i = num of shifts that you've made 
# j = location in current sequence y
for(i in 1:(n-1)){
  for(j in 1:n){
    if(j <= n-i) {
      y[j] = x[j+i]
    } else if (j > (n-i)){
      y[j] = x[j+i-n] 
      # y[j] = (j+i)%%n you can also use modulus here
    }
  }
  corr[i] = cor(x,y)
}

plot(y,x)
plot(corr,  ylim =c(-1,1), xlab = "# of shifts", ylab = "Correlation")

z1 = seq(0.32,0.32, length = n-1)
z2 = -z1
lines(z1,lty=2, col="red")
lines(z2,lty=2, col="red")

#Range of -0.2825723 to 0.3213798 -> Therefore, we can suggest independence
min(corr)
max(corr)
median(corr)


