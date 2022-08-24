library(simmer)

set.seed(1234)

ns = 1

qc = 1 # queue capacity

lambda = 7

mu = 12

c = 16

customer <-
  trajectory("Customer's path") %>%
  log_("Here I am!") %>%
  set_attribute("start_time", function() {now(game)}) %>%
  seize(
    "server",
    continue = FALSE, # quit
    reject = # another trajectory for balked customers
      trajectory("Balked customers") %>%
      log_("Balking!")
  ) %>% # get the server resource
  log_(function() {paste("Waited: ", 
                       now(game) - get_attribute(game, "start_time"))}) %>%
  timeout(function() {rexp(1, mu)}) %>% # generate service time
  release("server") %>%
  log_(function() {paste("Finished:", now(game))})

game <-
  simmer("game") %>% # create a simulator environment
  add_resource("server", 
               capacity = ns,
               queue_size = qc) %>% # system capacity
  add_generator("Customer", customer, function() {c(0, rexp(300, lambda), -1)})
# 0 is the time of first arrival, -1 stops the generator

game %>% run(until=c)

game %>%
  get_mon_arrivals() %>% # get monitoring statistics
  transform(waiting_time = end_time - start_time - activity_time) #Q: Why does this calculation work?
# Computation error from the computer -> use round function to fix this 

# compute waiting time


