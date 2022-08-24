# 528 Transient Behavior of Queues
# Basketball Game Example
# 2/23/22 

# Install "queuecomputer" package
install.packages("queuecomputer")
library(queuecomputer) 
# Note that this package is not able to handle the situations of balking and releaving

lambda = 7 # arrival rate 

mu = 12 # service rate

ns = 1 # number of servers 
ns_3 = 2 # number of servers for q3
 
c = 16 # time limit

r = 50 # number of replications Q: Arbitrary number - How does this number affect accuracy?
# If you want to do distr approx, need at least 30 

w = seq(0,0,length = r)
q = w
# All of this is one replication, so we need to put it into a for loop
# Expect to get r replications 
for (i in 1:r) {

  # Interarrival time:
  # if it follows an exponential distribution, then we know the interarrival times follow an exponential distr
  # rexp(number of sample you want to generate, rate that samples are generated)
  # n = 10000 # this is almost impossible to occur on a given day
  n = 10000 # Does not affect accuracy -> just make sure that n is large enough
  interarrival_times = rexp(n, lambda)
  
  # Arrival time: Use interarrival to calculate arrival time
  arrival = cumsum(interarrival_times) # includes arrivals that are later than time limit
  arrival = arrival[arrival<=c] # keep reasonable arrivals 
  
  # Service time: 
  n = length(arrival) # how there are n customers, so we need n service times
  service = rexp(n,mu)
  
  x = queue_step(arrival, service, servers = ns_3)
  y = summary(x) # Mean number of customers in system = r
  
  w[i] = y$mwt # an observation average weighting time for this replication
  q[i] = y$qlength_mean
  
  # w = c(w, y$mrt) -> another option for the above statement 
}

# Use w output to build a confidence interval 
# 1. What is a 95% CI for the average time in line for the basketball game with one goal?
mean(w) - qt(0.975, r-1)*sqrt(var(w)/r) # lower bound = 0.09065459
mean(w) + qt(0.975, r-1)*sqrt(var(w)/r) # upper bound = 0.124103

# if we don't want to use this method, we can use 'arrival = c arrival'. 
# generate only arrivals that meet the time limit to save computational space

# 2. What is a 95% CI for the average line length?
mean(q) - qt(0.975, r-1)*sqrt(var(q)/r) # lower bound = 0.6440636
mean(q) + qt(0.975, r-1)*sqrt(var(q)/r) # upper bound = 0.8381354

# 3. What are 95% CI’s for the average waiting time in line and line length with two goals?
# MM2 queue
mean(w) - qt(0.975, r-1)*sqrt(var(w)/r) # lower bound = 0.006120925
mean(w) + qt(0.975, r-1)*sqrt(var(w)/r) # upper bound = 0.008735981
mean(q) - qt(0.975, r-1)*sqrt(var(q)/r) # lower bound = 0.03833763
mean(q) + qt(0.975, r-1)*sqrt(var(q)/r) # upper bound = 0.05644697

# -------------------------------------------------------------------------------------
# Balking Example 3/1/22 - CODE DOES NOT WORK -> MUST USE Balking.R

install.packages("simmer")
library("simmer")

# Monte Carlo random num generation method: ensures that the exact same random values are generated
# Possess random property, but are technically pseudorandom numbers
set.seed(1234) 
rnorm(10) 

ns = 1 # number of servers 
qc = 1 # queue capacity 
lambda = 7 # arrival rate 
mu = 12 # service rate 
c = 16 # time limit

# We will create an object called player to simulate what a customer would go through in the system
# %>% ensures that you're still in the system

# 'Customer' is an object in the class 'trajectory' in the system 'game'
customer <- 
  trajectory("Customer's path") %>%  # Now we describe the experience of the customer object 
  log_("Here I am!") %>% # When the customer first arrives, we let the customer say something
  set_attribute("start_time", function() {now(game)}) %>%  # Record when customer first enters the system
  seize ( 
    "server",
    continue = FALSE, # customer will quit waiting if some condition happens... if continue = TRUE, then customers have infinite patience
    reject = # another trajectory for balked customers 
      trajectory("Balked customers") %>%
      log_("Balking!")
  ) %>% # let the customer get the server resource from the system 
  log_( function() { paste("Waited: ", 
                       now(game) - get_attribute(game, "start_time")) } ) %>% # outputs waiting time, if the customer has successfuly got a server resource
  timeout(function() {rexp(1,mu)}) # generate service time (for one customer only)
  release("server") %>% # when the service is done, the server is released
  log_(function() {paste("Finished: ", now(game))})

game <- # NOTE: can change this name to something else if you want, name of the simulator
  simmer("game") %>% # creates a simulator environment, 'game' is the class of the simulator
  add_resource("server", 
               capacity = ns, 
               queue_size = qc)  %>% # system capacity 
  # NOTE: Put 300 instead of 10,000 this time because we need enough customers for only 16 hours given 7 ph arrival rate
  add_generator("Customer", customer, function() { c(0, rexp(300, lambda), -1 )})   # NOTE: customer is both text and class here
  # 0 is the time of the first arrival, rexp to generate the other arrival times, -1 stops the generator

# To run this until time limit is reached
game %>% run(until=c) 

game %>% 
  get_mon_arrivals() %>% # to get monitoring statistics
  transform(waiting_time = end_time - start_time - activity_time)
  
# ------------------------------------------------------------------------------------
# Reneging Example 3/14/22

library(simmer)
set.seed(1234)

ns = 1
# qc = 1 # queue capacity -> for balking 
lambda = 7
mu = 12
c = 16
maxwait = 2/60 # max waiting time = 2 min

customer <-
  trajectory("Customer's path") %>%
  log_("Here I am!") %>%
  set_attribute("start_time", function() {now(game)}) %>%
  renege_in(maxwait, # customer will renege if not served in maxwait time
            out = trajectory("Reneging Customers") %>% # trajectory for customers who give up
            log_(function() {paste("Waited", now(game) - get_attribute(game, "start_time"), "I am off" )} ))  %>%
  seize("server") %>% # get the server resource
  renege_abort() %>% # stay if served 
  log_(function() {paste("Waited: ", 
                         now(game) - get_attribute(game, "start_time"))}) %>%
  timeout(function() {rexp(1, mu)}) %>% # generate service time
  release("server") %>%
  log_(function() {paste("Finished:", now(game))})

game <-
  simmer("game") %>% # create a simulator environment
  add_resource("server", 
               capacity = ns) %>% # system capacity
  add_generator("Customer", customer, function() {c(0, rexp(300, lambda), -1)})
# 0 is the time of first arrival, -1 stops the generator

game %>% run(until=c)

game %>%
  get_mon_arrivals() %>% # get monitoring statistics
  transform(waiting_time = end_time - start_time - activity_time) #Q: Why does this calculation work?
# Computation error from the computer -> use round function to fix this 

# compute waiting time






