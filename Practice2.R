# Tara Ram Mohan 
# Homework 2 Question 4

data = read.csv("528/Ordering_Food.csv", header=TRUE)

# a. What are the interarrival times?
r = nrow(data)
interarrival_times = seq(0,0,length = r - 1)
for (i in 1:(r-1)) {
  interarrival_times[i] = data$Arrivals.time..hour.[i+1] - data$Arrivals.time..hour.[i]
}

# b. What are the service times of each customer with server 1? 
service_times_1 = seq(0,0,length = r)
for (i in 1:r) {
  service_times_1[i] = data$Time.finishing.ordering.food.with.Service.1..hour.[i] - data$Time.starting.ordering.food.with.Server.1..hour.[i]
}

# With server 2?
service_times_2 = seq(0,0,length = r)

# First customer doesn't have to wait on a previous customer, so server 2 starts immediately after server 1 ends
service_times_2[1] =  data$Time.leaving.the.system..hour.[1] - data$Time.finishing.ordering.food.with.Service.1..hour.[1]

# Service time is the amount of time between when previous customer leaves system (Server 2 frees up) and current customer leaves system (finishes receiving food)
for (i in 2:r) {
  service_times_2[i] = data$Time.leaving.the.system..hour.[i] - max(data$Time.leaving.the.system..hour.[i-1],data$Time.finishing.ordering.food.with.Service.1..hour.[i])
}
# NOTE: forgot to include finish order. Should be max (finish order, leaving system)

# c. How long does each customer wait to get service from server 1? 
wait_times_1 = seq(0,0,length = r)
for (i in 1:r) {
  wait_times_1[i] = data$Time.starting.ordering.food.with.Server.1..hour.[i] - data$Arrivals.time..hour.[i]
}

# From server 2?

# Wait time is the amount time between when current customer finishes ordering food from Server 1 
# and previous customer finishes getting their food from Server 2 (leaves the system and Server 2 is free to serve another customer)
wait_times_2 = seq(0,0,length = r)

# Wait time for customer 1 will be 0. Server 2 will immediately serve them (not waiting on any previous order)
wait_times_2[1] = 0 
for (i in 2:r) {
  wait_times_2[i] = data$Time.leaving.the.system..hour.[i-1] - data$Time.finishing.ordering.food.with.Service.1..hour.[i]
  if (wait_times_2[i] < 0) {
    wait_times_2[i] = 0
  }
}

