# Tara Ram Mohan
# OPER 528 Homework 3
# Due March 28, 2022

# QUESTION 1 ---------------------------------------------------------------------------
# Simulating Complex System Example
library(queuecomputer) # no bulking or reneging 

r = 30 # run multiple replications to reduce uncertainty
lambda = 100 # arrival rate per hour 
mu = 50 # service rate per hour

sn = 7 # number of servers -> don't know this value, so we test it out
cost = seq(0,0,length=r) # to save values into 

c = 8 # time limit 5:00 AM to 1:00 PM in hours

for(i in 1:r) {
  # First generate long enough arrival sequence
  # 100*8 = 800 
  n = 1000
  arrivals = cumsum(rexp(n, lambda))
  
  # Abandon the late arrivals 
  arrivals = arrivals[arrivals <= c]
  
  # Update number of arrivals
  n = length(arrivals)
  
  # Now generate service times 
  service = rexp(n, mu)
  
  x = queue_step(arrivals, service, servers = sn)
  
  # Use summary to get statistics 
  y = summary(x)
  
  # Save cost into the ith element of cost (simulated result of ith replication)
  # cost of server for 1 hour = 5 
  # cost of a customer waiting in the line for 1 hour = $20
  cost[i] = sn*5 + 20*y$mwt*n
  # CORRECTION: cost[i] = sn*5*8 + 20*y$mwt*n
}

mean(cost) 
sqrt(var(cost)) 

# Confidence interval 
mean(cost) - qt(0.975, r - 1)*sqrt(var(cost)/r) 
mean(cost) + qt(0.975, r - 1)*sqrt(var(cost)/r) 

# QUESTION 2 ---------------------------------------------------------------------------
# Tandem System Example

library(queuecomputer)
set.seed(123)
r = 30 # number of replications 

lambda = 400/60  # arrival rate

mu = 150/60 # service rate for x-ray machines

# Given number of servers
sn = 3 # num servers for x-ray station
sn2 = 3 # num servers for wanding 

c = 4*60 # simulate from 5:00 AM to 9:00 AM 

w1 = seq(0,0,length=r)
w2 = w1

for (i in 1:r){
  
  # FIRST QUEUE (X-Ray Machines) ---------------------------------------------------------------
  n = 2000 # 400*4 = 1600 -> 2000
  
  arrivals = cumsum(rexp(n, lambda))
  
  # add only arrivals that work
  arrivals = arrivals[arrivals <= c]
  
  #update n 
  n = length(arrivals)
  
  # generate service times
  service = rexp(n,mu)
  
  x = queue_step(arrivals, service, servers = sn)
  # To see attributes attributes(x)
  
  y = summary(x) # to get statistics
  
  # SECOND QUEUE ---------------------------------------------------------------
  
  # Now, we want to get the departure times of the first queue and use them as arrival times for second queue
  arrivals = x$departures
  
  # Split into 90|10 sub sequences 
  # Bernoulli (Binomial with n = 1) 
  
  # Generate a standard uniform variable (a value between 0 and 1) for each passenger 
  route = runif(n)
  
  # If value is <= 0.9, the passenger is free to go to flight (i.e. no added time)
  arrivals1 = arrivals[route <= 0.9]
  
  # Else, value is > 0.9, then the passenger must get wanded
  arrivals2 = arrivals[route > 0.9]
  
  n2 = length(arrivals2) 
  
  # Wanding follows an Erlang distribution with mean of 4 minutes and standard deviation of 2 minutes.
  mean= 4 
  variance = 2^2
  k = (mean^2)/variance
  mu2 = mean/variance
  
  service2 = rgamma(n2, shape=k, rate = mu2)
  
  x2 = queue_step(arrivals2, service2, servers = sn2)
  y2 = summary(x2)
  
  w1[i] = y$mrt #avg time in engine installation station 
  
  # average time spent in system = total time spent by all passengers / num of passengers 
  w2[i] = (y$mrt*n + y2$mrt*n2) / n # avg time in entire system -> related to 3 queues
}

# a. How long does it take the average passenger to pass through security (entire system)?
mean(w2) - qt(0.975,r-1)*sqrt(var(w2)/r)
mean(w2) + qt(0.975,r-1)*sqrt(var(w2)/r)
# 1.959023, 2.469226

# b. If there were no wanding, how long would it take the average passenger to pass through security?
mean(w1) - qt(0.975,r-1)*sqrt(var(w1)/r)
mean(w1) + qt(0.975,r-1)*sqrt(var(w1)/r)
# 1.147606, 1.545923

# c. Which would improve the situation more: -----------------------------------
# Option 1: adding an X-ray machine 
mean(w2) - qt(0.975,r-1)*sqrt(var(w2)/r) 
mean(w2) + qt(0.975,r-1)*sqrt(var(w2)/r)
# 1.240207, 1.533185

# Option 2: adding an additional person to perform wanding
mean(w2) - qt(0.975,r-1)*sqrt(var(w2)/r) 
mean(w2) + qt(0.975,r-1)*sqrt(var(w2)/r)
# 1.61978, 2.032566