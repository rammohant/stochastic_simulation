# Unit 11: Monte Carlo Simulation
# 4.18.22

# The News Vendor Problem: Ordering Calendars -------------------------------------------------------------------
# If you want to represent the uncertainty in the system, you have to use random samples to generate random factors in system
r = 500 
x = 300 # decision/control variable
uc = 2 # unit cost
up = 4.5 # unit price
lp = 0.75 # leftover price

profit = seq(0,0,length=r)
avg_profit = seq(0,0,length = 201) # records avg profit returned for each value of x
for (j in 1:201) {
  x = 99 + j 
  
  for (i in 1:r) {
    u = runif(1)
    
    if(u <= 0.3) 
      d = 100 
    else if(u > 0.3 & u <= 0.5)
      d = 150
    else if(u > 0.5 & u <= 0.8)
      d = 200
    else if(u > 0.8 & u <= 0.95) 
      d = 250
    else if(u > 0.95)
      d = 300
    
    if(x<=d) 
      profit[i] = up*x - uc*x
    else 
      profit[i] = up*d - uc*x + lp*(x-d)
  }
  avg_profit[j] = mean(profit)
}

plot(avg_profit, type = "l")
max(avg_profit) # 366.4175
which.max(avg_profit) + 99 # 203

# Food Cart Problem -------------------------------------------------------------------

# NOTE: it is possible to have no customers order chicken wraps -> min of 0 
# Therefore, range of x: 0 <= x <= 25 (50/2)

r = 500 
x = seq(0,25,length=51)
mean_x = x
sd_x = x

for (j in 1:51) {
  profit = rep(0,r) # or use seq(0,0,length...)
  for(i in 1:r) {
    u = runif(1)
    
    # STEP 1: Generate num of customers 
    if (u <= 0.15)
      d = 30
    else if (u > 0.15 & u <= 0.25)
      d = 35
    else if (u > 0.25 & u <= 0.5)
      d = 40
    else if (u > 0.5 & u <= 0.85)
      d = 45
    else if (u > 0.85)
      d = 50
    
    # STEP 2: Determine number of customers that order chicken wraps
    
    # Generate d standard univariates
    u1 = runif(d)
    
    # Count how many of them are less than 0.78 probability 
    dc = length(u1[u1 <= 0.78])
    
    profit[i] = min(2*x[j], dc)*4 - 0.99*x[j]
  }
  
  mean_x[j]  = mean(profit)
  sd_x[j] = sd(profit)
  # And also 95% confidence interval
  
}

plot(x,mean_x, type="b")
max(mean_x)
x[which.max(mean_x)]

# NPV: Truck Production -------------------------------------------------------------------

library(triangle)

# Input given data tables 
gnp = c(0.03, 0.05, 0.04)
inf1 = c(0.04, 0.07, 0.03) # note that inf = infinity, so we can't use that variable name
sp = c(15,16,17) # sales price in thousands 
vc = c(12,13,14) # variable cost in thousands

r = 500 # num of replications 
rate= 0.1

d = rep(0,r)

for(i in 1:r) { # for each replication
  
  c = 2 # num of competitors 
  for (j in 1:3) {# for each year j
    gr = rtriangle(1, gnp[j]*0.91, gnp[j]*1.09, gnp[j])
    ir = rtriangle(1, inf1[j]*0.92, inf1[j]*1.08, inf1[j])
    
    tr = 500 + 50*gr - 40*ir # real t (not estimate)
    t = 500 + 50*gnp[j] - 40*inf1[j] # t est
    
    u = runif(1) # generate probability 
    
    if (u <= 0.5) {
      nc= 0
    } else if (u > 0.5 & u <= 0.8) {
      nc = 1
    } else if (u > 0.8 & u <=0.9) {
      nc = 2
    } else if (u > 0.9) {
      nc = 3
    }
    
    c = c*nc
    
    mr = tr*0.5*0.9^c # real market share
    m = t*0.5*0.9^c # est market share
    
    profit = (sp[j]*min(mr,m) - vc[j]*m)/(1+rate)^j # revenue - cost
    
    d[i] = d[i] + profit 
    
    # at the end of each year, there is a 20 percent chance that the competitor will leave the industry 
    # normal distribution of the competitors who will stay in the business 
    #c = rbinom(1, c, 1-0.5) # for q2
    c = rbinom(1, c, 1-0.2) # for q1
    
  }
}
  
mean(d) #*1000*1000 # in billions
var(d) #*1000*1000


# OLD INCORRECT QUESTION 2 --------------------------------------------------------------------------------
library(triangle)

set.seed(1234)
# NOTE: Everything is in thousands 

# depreciation per year = total amount / number of years. fixed development cost of $1.4 billion at year 0
plot(dev_cost, type = "l")

# variable cost of producing the car increases 4% a year
var_cost_per_car = c(10000, 10000*1.04, 10000*(1.04)^2, 10000*(1.04)^3, 10000*(1.04)^4)
revenue_per_car = 15000
tax_rate = .4
cf_discount = .15 
r = 1000

sales_per_year = rep(0,5)
profit = rep(0,5)
cash_flow = rep(0,5)
npv = rep(0,r)
revenue = rep(0,5)
cost = rep(0,5)
count_av = 0 # count of the number of times the new model adds value to GM

for (j in 1:r) { 
  # Calculate sales
  if (i == 1) {
    sales = rtriangle(1, 100000, 170000, 150000)
    sales_per_year[1] = sales 
  } else {
    sales_decay_rate = rtriangle(1, 0.05, 0.1, 0.08)
    sales_per_year[i] = sales_per_year[i-1] * (1-sales_decay_rate) 
  }
  
  for(i in 1:5) { 
    revenue = sales_per_year[i]*revenue_per_car
    cost = sales_per_year[i]*var_cost_per_car[i]
    
    depreciation = (1400000000/5)
    # profit = revenue - cost - depreciation
    profit = revenue - cost - depreciation
    
    # cash flow = after-tax profit + depreciation
    cash_flow[i] = ( profit*(1-tax_rate) + depreciation)/(1+cf_discount)^i  
    
    npv[j] = npv[j] + cash_flow[i]
  }
  
  net_profit = npv[j] - 1400000000
  print(net_profit)
  
  if (net_profit > 0) {
    count_av = count_av+1
  }
}

fraction = (count_av/r) * 100
fraction



