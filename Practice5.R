# OPER 528 Homework 5
# Tara Ram Mohan 
# Due May 2, 2022

# QUESTION 1 --------------------------------------------------------------------------------
set.seed(123)

r = 500 
tax_rate = 0.4
income = 75000

di = seq(0,0,length=r)
avg_di = seq(0,0,length = 4001) # records avg di returned for each value of x
avg_di_sd = avg_di

for (j in 1:4001) { # for each possible TSM value
  x = 2999 + j # dollar amount you put into TSB 
  
  for (i in 1:r) { # for each replication
    u = runif(1)

    # Generate d = dollar amount of medical expenses for next year
    if(u <= 0.2) 
      d = 3000
    else if(u > 0.2 & u <= 0.4)
      d = 4000
    else if(u > 0.4 & u <= 0.6)
      d = 5000
    else if(u > 0.6 & u <= 0.8) 
      d = 6000
    else if(u > 0.8)
      d = 7000
    
    # For logging purposes
    di1 = (income - x)*(1-tax_rate) - (d-x)
    di2 = (income - x)*(1-tax_rate) 
    print(c(x, di1, di2))
  
    # Objective function
    if(x<=d) 
      di[i] = (income - x)*(1-tax_rate) - (d-x)
    else 
      di[i] = (income - x)*(1-tax_rate)
  }
  
  # Store mean and standard deviation of each possible TSM value
  avg_di[j] = mean(di)
  avg_di_sd[j] = sqrt(var(di))
}

plot(avg_di, type = "l")
max(avg_di) 
z = which.max(avg_di) + 2999
avg_di[z-2999]
avg_di_sd[z-2999]

# QUESTION 2--------------------------------------------------------------------------------
library(triangle)
set.seed(98)

var_cost_per_car = c(10000, 10000*1.04, 10000*(1.04)^2, 10000*(1.04)^3, 10000*(1.04)^4) # variable cost of producing the car increases 4% a year
revenue_per_car = 15000
depreciation = (1400000000/5) # depreciation per year = total amount / number of years. fixed development cost of $1.4 billion at year 0
tax_rate = .4
cf_discount = .15 
r = 1000 # num of replications

sales_per_year = rep(0,5)
profit = rep(0,5)
cash_flow = rep(0,5)
npv = rep(0,r)
revenue = rep(0,5)
cost = rep(0,5)
count_av = 0

for (j in 1:r) { 
  
  # year 1 sales: worst case of 100,000 units, most likely case of 150,000 units, and best case of 170,000 units
  y1_sales = rtriangle(1, 100000, 170000, 150000)
  
  # years 2–5 sales decay rate: best case of 5%, most likely case of 8%, and worst case of 10
  decay_rate = 1 - rtriangle(1, 0.05, 0.1, 0.08)
  
  for(i in 1:5) { 
    sales = y1_sales*(decay_rate^(i-1))
    revenue = sales*revenue_per_car
    cost = sales*var_cost_per_car[i]
    profit = revenue - cost - depreciation
    
    # cash flow = after-tax profit + depreciation
    cash_flow[i] = ( profit*(1-tax_rate) + depreciation)/(1+cf_discount)^i  
    npv[j] = npv[j] + cash_flow[i] # total npv 
  }
  
  net_profit = npv[j] - 1400000000
  # count of the number of times the new model adds value to GM
  if (net_profit > 0) {
    count_av = count_av+1
  }
}

# (a) Simulate 1,000 times to estimate the mean NPV of the cash flows from the new car.
mean(npv)
sqrt(var(npv))

# (b) What fraction of the time will the new model add value to GM? 
# count of the number of times the new model adds value to GM / total number of replications 
(count_av/r) * 100

