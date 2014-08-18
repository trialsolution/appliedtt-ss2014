# Problem 14: Econometric specification of a PE model
# 
# The excel sheet 14_apple trade model.xlsx contains an example of how statistically (econometrically) 
# the functional forms of supply and demand have been specified for a country. Use these functions to solve the following problems:

# a) What are equilibrium price and market quantity in autarky (remember: supply=demand)?
# b) From the data we see, that the country has developed over time to a (small) exporting country: 
#    write down the formula for the excess supply function and calculate its functional values.
# c) If the international apple price is 600 LCU/ton and there are no market distortion measures: 
#    what are the production, consumption and exporting quantity when it opens up to trade?


library("ggplot2")

# set environment and load data table
setwd("c://Users/himics/Desktop/applied_trade/appliedtt-ss2014/")
x <- read.table(file="prob12_data.txt", header=TRUE)

# supply and demand functions are simple linear models
supply <- lm(prod_p~price, data=x)
demand <- lm(cons~price_r, data=x)

# draw a plot with the relationships
p <- ggplot(x)
p + geom_point(aes(x=prod_p, y=price), color="blue") + geom_point(aes(x=cons, y=price_r), color="red") + xlab("quantity") + ylab("price")

# another plot with the fitted values
x$fitted_supply <- supply$fitted.values
x$fitted_demand <- demand$fitted.values
p <- ggplot(x)
p + geom_point(aes(x=prod_p, y=price), color="blue") + 
  geom_point(aes(x=cons, y=price_r), color="red") + geom_line(aes(x=fitted_supply, y=price), color="blue") + 
  geom_line(aes(x=fitted_demand, y=price_r), color="red") + xlab("quantity") + ylab("price")


# find the intersetion where supply=demand
# solve it as a system of equations:   
#    slope_supply * price_supply - quantity_supply = - intercept_supply    (1)
#    slope_demand * price_demand - quantity_demand = - intercept_demand    (2)
A <- array(c(coef(supply)[2],coef(demand)[2],-1,-1), c(2,2))
b <- c(-coef(supply)[1],-coef(demand)[1])
price_eq <- solve(A,b)[1]
quantity_eq <- solve(A,b)[2]


# The excess supply is simply the difference between supply and demand:
x$ES  <-  x$prod_p - x$cons
# Have a look at how it evolved over time:
p <- ggplot(x, aes(x=year, y=ES))
p+geom_point()+geom_smooth()


# c) simply use the estimated linear equations above and substitute the price...


