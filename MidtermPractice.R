# Tara Ram Mohan 
# Midterm 2 R Code 
# Due April 6 2022

data = read.csv('528/Food Cart.csv')

# (b) Find the interarrival times of the customers ---------------------------------------------------------------------
r = nrow(data)
interarrival_times = seq(0,0,length = r - 1)
for (i in 1:(r-1)) {
  interarrival_times[i] = data$Arrivals.time..hour.[i+1] - data$Arrivals.time..hour.[i]
}
# INCORRECT: Should include the first arrival time as first interarrival time

# Find the service times of Server 1
service_times_1 = seq(0,0,length = r)
for (i in 1:r) {
  service_times_1[i] = data$Time.finishing.ordering.food.with.Server.1..hour.[i] - max(data$Time.starting.ordering.food.with.Server.1..hour.[i],data$Time.starting.ordering.food.with.Server.1..hour.[i-1])
}

# Find the service times of Server 2
service_times_2 = seq(0,0,length = r)
for (i in 1:r) {
  service_times_2[i] = data$Time.leaving.the.system..hour.[i] - max(data$Time.finishing.ordering.food.with.Server.1..hour.[i],data$Time.leaving.the.system..hour.[i-1])
}

# Assess the independence of the interarrival times
n = length(interarrival_times) # num samples
# If there are n samples, we need to shift the sequence n-1 times (i.e. calculate n-1 correlations or plot n-1 scatterplots)
x = interarrival_times
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

#Range of -0.2944198 to 0.2083892 -> Therefore, we can suggest independence for interarrival times
min(corr)
max(corr)
median(corr)

plot(y,x)
plot(corr,  ylim =c(-1,1), xlab = "Interarrival times", ylab = "Correlation")

z1 = seq(0.32,0.32, length = n-1)
z2 = -z1
lines(z1,lty=2, col="red")
lines(z2,lty=2, col="red")

# Assess the independence of service times of Server 1 times 
n = length(service_times_1) # num samples
# If there are n samples, we need to shift the sequence n-1 times (i.e. calculate n-1 correlations or plot n-1 scatterplots)
x = service_times_1
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

#Range of -0.2613721 to 0.2621473 -> Therefore, we can suggest independence for service times for Server 1
min(corr)
max(corr)
median(corr)

plot(y,x)
plot(corr,  ylim =c(-1,1), xlab = "Service Times for Server 1", ylab = "Correlation")

z1 = seq(0.32,0.32, length = n-1)
z2 = -z1
lines(z1,lty=2, col="red")
lines(z2,lty=2, col="red")


# Assess the independence of service times of Server 2 times 
n = length(service_times_2) # num samples
# If there are n samples, we need to shift the sequence n-1 times (i.e. calculate n-1 correlations or plot n-1 scatterplots)
x = service_times_2
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

#Range of -0.2527927 to 0.1603976 -> Therefore, we can suggest independence for service times for Server 2
min(corr)
max(corr)
median(corr)

plot(y,x)
plot(corr,  ylim =c(-1,1), xlab = "Service Times for Server 2", ylab = "Correlation")

z1 = seq(0.32,0.32, length = n-1)
z2 = -z1
lines(z1,lty=2, col="red")
lines(z2,lty=2, col="red")

# (c) What distributions appear to be appropriate for modeling the arrival process and the service process? ---------------------------------------------------------------------
# (the possibilities are: exponential, uniform, triangular, Erlang)

# Interarrival times appear to have an exponential distribution
hist(interarrival_times, breaks = 8)

# Service times for server 1 appear to have an exponential distribution
hist(service_times_1)

# Service times for server 2 appears to have an erlang distribution
hist(service_times_2)

# (d) Estimate the parameters of your hypothesized distribution above and (e) Apply statistical test to verify your choice of the distribution. ---------------------------------------------------------------------

####################### INTERARRIVAL HYPOTHESIS TEST ###############################
# H_O: The samples are 35 i.i.d observations from an exponential distribution exp(k).

# Use method of the moment to estimate parameters for exp
lambda = 1/mean(interarrival_times)

k = 5 # number of small intervals
n = length(interarrival_times) # 35
m = n/k # 7 -> ideally should be a whole number 
r = 1  #number of parameters to specify the exp distribution (lambda)

# Find starting and ending points of the intervals
intdiv = seq(0,0,length = k + 1)

for(i in 1:(k+1)) {
  intdiv[i] = qexp(((i-1)/k),lambda) #First point is (1-1/k) for 0th quantile, and then 10th quantile...
}

# Count the num of observations in each interval
x = seq(0,0, length=k)
for(i in 1:(k+1)) {
  x[i] = length(interarrival_times[interarrival_times <= intdiv[i+1]]) - length(interarrival_times[interarrival_times < intdiv[i]])
}

# Verify that your num observations is correct by taking the sum 
sum(x) # = 35

# Get test statistics
teststat = sum(x^2/m) - n

# Now we compare this with the critical value
alpha = 0.05
criticalval = qchisq(1-alpha, k-r-1)

if(teststat > criticalval) {
  print("Interarrival Times: We reject H_O at level \alpha.")
} else {
  print("Interarrival Times: We fail to reject H_O at level alpha")
}

####################### SERVICE 1 HYPOTHESIS TEST ###############################
# H_O: The samples are 36 i.i.d observations from an exponential distribution exp(lambda)

# We can use method of the moment to estimate parameters for exp
lambda = 1/mean(service_times_1)

k = 6 # number of small intervals
n = length(service_times_1) # 36
m = n/k # 6
r = 1  

# (3) Find starting and ending points of the intervals
intdiv = seq(0,0,length = k + 1)

for(i in 1:(k+1)) {
  intdiv[i] = qexp(((i-1)/k),lambda) #First point is (1-1/k) for 0th quantile, and then 10th quantile...
}

# (4) Now, we need to count the num of observations in each interval
x = seq(0,0, length=k)
for(i in 1:(k+1)) {
  x[i] = length(service_times_1[service_times_1 <= intdiv[i+1]]) - length(service_times_1[service_times_1 < intdiv[i]])
}

# You can verify that your num observations is correct by taking the sum 
sum(x) # = 36

# Get test statistics
teststat = sum(x^2/m) - n

# Now we compare this with the critical value
alpha = 0.05
criticalval = qchisq(1-alpha, k-r-1)

if(teststat > criticalval) {
  print("Service Times for Server 1: We reject H_O at level \alpha.")
} else {
  print("Service Times for Server 1: We fail to reject H_O at level alpha")
}

####################### SERVICE 2 HYPOTHESIS TEST ###############################
# H_O: The samples are 36 i.i.d observations from a erlang distribution gamma(k,mu)

# We can use method of the moment to estimate parameters for erlang
k1 = mean(service_times_2)^2 /var(service_times_2) # shape parameter
mu = mean(service_times_2)/var(service_times_2)

k = 6 # number of small intervals
n = length(service_times_2) # 36
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
  x[i] = length(service_times_2[service_times_2 <= intdiv[i+1]]) - length(service_times_2[service_times_2 < intdiv[i]])
}

# You can verify that your num observations is correct by taking the sum 
sum(x) # = 36

# Get test statistics
teststat = sum(x^2/m) - n

# Now we compare this with the critical value
alpha = 0.05
criticalval = qchisq(1-alpha, k-r-1)

if(teststat > criticalval) {
  print("Service Times for Server 2: We reject H_O at level \alpha.")
} else {
  print("Service Times for Server 2: We fail to reject H_O at level alpha")
}

# Since the arrival rate is less than the service rate (i.e. 9 < 14), this queue will reach a steady state eventually. 

# (g) Simulate the system. Express your simulation result in terms of confidence intervals. ---------------------------------------------------------------------
library(queuecomputer)

set.seed(123)
r = 500 # number of simulations 

lambda = 1/mean(interarrival_times) # paramaters for arrival process
lambda2 = 1/mean(service_times_1)  # parameters for server 1 time
k1 = mean(service_times_2)^2 / var(service_times_2) # shape parameter for server 2 time
mu = mean(service_times_2)/var(service_times_2) # parameter for server 2 time
  
sn1 = 1 # num servers for server 1
sn2 = 2 # num servers for server 2

c = 4 # simulate for 4 hours during lunch hour (11:00 – 15:00).

w1 = seq(0,0,length=r)
w2 = w1
w3 = w1
q = w1
cost = w1

for (i in 1:r){
  
  # FIRST QUEUE ---------------------------------------------------------------
  n = 4000 # 500*4 -> 2000
  
  arrivals = cumsum(rexp(n, lambda))

  # add only arrivals that work
  arrivals = arrivals[arrivals <= c]

  #update n 
  n = length(arrivals)

  # generate service times
  service1 = rexp(n, lambda2)
  sort(service1)
    
  x = queue_step(arrivals, service1, servers = sn1)
  y = summary(x) # to get statistics
  
  # SECOND QUEUE ---------------------------------------------------------------
  
  # Now, we want to get the departure times of the first queue and use them as arrival times for second queue
  arrivals2 = x$departures
  n1 = length(arrivals2)
  
  service2 = rgamma(n1, shape=k1, rate=mu)

  x1 = queue_step(arrivals2, service2, servers = sn2)
  y1 = summary(x1)

  w1[i] = y$mrt # avg time with server 1 (ordering food)
  w2[i] = y1$mrt # avg time with server 2 (making food)
  
  # average queue length = total length of each line in system  / num of queues
  q[i] = (y$qlength_mean + y1$qlength_mean)/2
  # INCORRECT: Should report the two queue lengths separately OR only consider 1st queue (No queue after ordering food)
  
  # average time spent in system per customer = total time spent by all cars / num of customers 
  w3[i] = (y$mrt*n + n1*y1$mrt) / n # avg time in entire system -> related to 3 queues
  
  # delay cost of $1 is incurred per minute (i.e. $60 per hour)
  # $10 per hour to hire a counterman 
  # $15 per hour to hire a cook
  cost[i] = sn1*(10)*c + sn2*(15)*c + (60)*((y$mwt*n + n*y1$mwt) / n)
}

# What is the average queue length?
mean(q) - qt(0.975, r-1)*sqrt(var(q)/r) 
mean(q) + qt(0.975, r-1)*sqrt(var(q)/r) 
# (0.2198309,0.2469847)

# What is the average time that a customer spends in the system (from entering the queue to leaving)? 
mean(w3) - qt(0.975,r-1)*sqrt(var(w3)/r)
mean(w3) + qt(0.975,r-1)*sqrt(var(w3)/r) 
# (0.1252004,0.1299536)

# (h) From your simulation, between ordering food and making food, which part of the service is more time consuming on average? ---------------------------------------------------------------------
# Confidence interval for ordering food: (0.02596086, 0.02701261)
mean(w1) - qt(0.975, r-1)*sqrt(var(w1)/r) 
mean(w1) + qt(0.975, r-1)*sqrt(var(w1)/r) 

# Confidence interval for making food: (0.09885033,0.1033302)
mean(w2) - qt(0.975, r-1)*sqrt(var(w2)/r) 
mean(w2) + qt(0.975, r-1)*sqrt(var(w2)/r)

# Therefore, making food is more time consuming on average

# (i)  To reduce the sum of service cost and delay cost, should another server be hired? If so, a counterman or a cook? 
# Current cost (sn1 = 1, sn2 = 1): (102.6703, 102.9358)
mean(cost) - qt(0.975, r-1)*sqrt(var(cost)/r) 
mean(cost) + qt(0.975, r-1)*sqrt(var(cost)/r) 

# Additional counterman (sn1 = 2, sn2 = 1): (142.3796, 142.6317)
mean(cost) - qt(0.975, r-1)*sqrt(var(cost)/r) 
mean(cost) + qt(0.975, r-1)*sqrt(var(cost)/r) 

# Additional cook (sn1 = 1, sn2 = 2): (160.4711, 160.5261)
mean(cost) - qt(0.975, r-1)*sqrt(var(cost)/r) 
mean(cost) + qt(0.975, r-1)*sqrt(var(cost)/r) 

# No, we should not hire another counterman or cook. 

# (j) How does this action of posting the menu publicly affect the total cost? Should the food cart do this or not in terms of reducing the total cost?
set.seed(123)

r = 500 # number of simulations 

lambda = 1/mean(interarrival_times) # arrival rate for arrival process
lambda2 = 1.5*(1/mean(service_times_1))  # service rate for server 1 increased by 50% 
k1 = mean(service_times_2)^2 / var(service_times_2) # shape parameter for server 2 time 
mu = 1.2*(mean(service_times_2)/var(service_tim-es_2)) # service rate parameter for server 2 increased by 20% 

sn1 = 1 # num servers for server 1
sn2 = 1 # num servers for server 2

c = 4 # simulate for 4 hours during lunch hour (11:00 – 15:00).

w1 = seq(0,0,length=r)
w2 = w1
w3 = w1
q = w1
cost = w1

for (i in 1:r){
  
  # FIRST QUEUE ---------------------------------------------------------------
  n = 2000 # 500*4 -> 4000
  
  arrivals = cumsum(rexp(n, lambda))
  
  # add only arrivals that work
  arrivals = arrivals[arrivals <= c]
  
  #update n 
  n = length(arrivals)
  
  # Split into 80|20 sub sequences using Bernoulli
  
  # Generate a standard uniform variable (a value between 0 and 1) for each customer 
  route = runif(n)
  
  # If value is <= 0.2, the customer will just read the posted menu and leave directly
  arrivals0 = arrivals[route <= 0.2]
  
  # Else, value is > 0.2, then the customer enters the system
  arrivals1 = arrivals[route > 0.2]
  
  n1 = length(arrivals1) 
  
  # generate service times
  service1 = rexp(n1, lambda2)

  x = queue_step(arrivals1, service1, servers = sn1)
  y = summary(x) # to get statistics
  
  # SECOND QUEUE ---------------------------------------------------------------
  
  # Now, we want to get the departure times of the first queue and use them as arrival times for second queue
  arrivals2 = x$departures
  n2 = length(arrivals2)
  
  service2 = rgamma(n2, shape=k1, rate=mu)
  
  x1 = queue_step(arrivals2, service2, servers = sn2)
  y1 = summary(x1)
  
  w1[i] = y$mrt # avg time with server 1 (ordering food)
  w2[i] = y1$mrt # avg time with server 2 (making food)
  
  q[i] = (y$qlength_mean + y1$qlength_mean)/2
  
  # average time spent in system = total time spent by all cars / num of cars 
  w3[i] = (y$mrt*n1 + n2*y1$mrt) / n # avg time in entire system -> related to 3 queues
  
  # delay cost of $1 is incurred per minute (i.e. $60 per hour)
  # $10 per hour to hire a counterman 
  # $15 per hour to hire a cook
  cost[i] = sn1*(10)*c + sn2*(15)*c + (60)*((y$mwt*n1 + n2*y1$mwt) / n)
}

# Confidence interval for cost: (100.8266, 100.9254)
mean(cost) - qt(0.975, r-1)*sqrt(var(cost)/r) 
mean(cost) + qt(0.975, r-1)*sqrt(var(cost)/r) 
# This action results in a reduction in the total cost (as seen below), meaning that the food cart should post the menu publicly to everyone. 
