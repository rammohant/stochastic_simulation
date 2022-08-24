# Lecture 7 
# Simulating Complex System 
# 3/16/22

library(queuecomputer) # no bulking or reneging 

r = 30 # run multiple replications to reduce uncertainty
lambda = 2 # arrival rate per min 
mu = 0.5 # service rate per min = 30 customers / 60 min 

sn = 7 # number of servers -> don't know this value, so we test it out
cost = seq(0,0,length=r) # to save values into 

c = 8*60 # time limit in min 

for(i in 1:r) {
  # First generate long enough arrival sequence
  # 2*60*8 = 960 --> rounded up to 1,000 (this is a waste of resource, we'll abandon those that don't fit )
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
  cost[i] = sn*72 + 0.05*y$mwt*n
}

mean(cost) #34219.23
sqrt(var(cost)) #3134.714

# Confidence interval 
mean(cost) - qt(0.975, r - 1)*sqrt(var(cost)/r) #33048.71
mean(cost) + qt(0.975, r - 1)*sqrt(var(cost)/r) #35389.75

# ---------------------------------------------------------------------------
# Tandem System: Auto Assembly
# 3/21/22

library(queuecomputer)

r = 30 # number of replications 

# If 54 per hour is too large, we can make it smaller by converting per hour to per minute
lambda = 54/60  # arrival rate

mu = 60/60 # service rate for engine station 
mu1 = 60/3/60 # service rate for tire stations 

sn = 1 # num servers for engine station 
sn1 = 4 # num servers for first tire station 
sn2 = 3 # num servers for second tire station

c = 400*60 # simulate for 400 hours 

w1 = seq(0,0,length=r)
w2 = w1

for (i in 1:r){
  
  # FIRST QUEUE ---------------------------------------------------------------
  n = 100000 # 24000 -> 100,000
  
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
  
  # Split into 60|40 sub sequences 
  # Bernoulli (Binomial with n = 1) 

  # Generate a standard uniform variable (a value between 0 and 1) for each car 
  route = runif(n)
  
  # If value is <=0.6, the car gets sent to the first
  arrivals1 = arrivals[route <= 0.6]
  
  # Else, value is >0.6, then it is a failure and gets sent to second 
  arrivals2 = arrivals[route > 0.6]
  
  n1 = length(arrivals1)
  n2 = n - n1 
  
  service1 = rexp(n1,mu1)
  service2 = rexp(n2,mu1)
  
  x1 = queue_step(arrivals1, service1, servers = sn1)
  x2 = queue_step(arrivals2, service2, servers = sn2)

  y1 = summary(x1)
  y2 = summary(x2)
  
  w1[i] = y$mrt #avg time in engine installation station 
  
  # average time spent in system = total time spent by all cars / num of cars 
  w2[i] = (y$mrt*n + n1*y1$mrt + n2*y2$mrt) / n # avg time in entire system -> related to 3 queues

}

# Q1: How long does it take for a car to have the engine installed on average?
mean(w1) - qt(0.975,r-1)*sqrt(var(w1)/r)
mean(w1) + qt(0.975,r-1)*sqrt(var(w1)/r)

# Q2: How long does it take for a car to have both the engine and tires installed on average?
mean(w2) - qt(0.975,r-1)*sqrt(var(w2)/r)
mean(w2) + qt(0.975,r-1)*sqrt(var(w2)/r) 