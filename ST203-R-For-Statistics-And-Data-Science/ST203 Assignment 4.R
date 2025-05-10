##Assignment 4 - Colm Mooney - 20325583

##Construct the matrix seen in 1 (a)
a <- c(2,1,3,7,6)
A <- cbind(1,a, a^2,a^3)
A

#b) Write a function powermat which when given a vector x and a positive integer m (??? 2) constructs a
#matrix where the first column is a vector of ones, the second column is x, the third column is x
#2 and soon until the m-th column which is xm???1

powermat <- function(x, m)
{
  if(m < 2) #If m is smaller than 2
  {
    #Nothing happens
  }
  else #Cus question says m (??? 2)
  {
    sapply(0:(m-1), function(i) x^i)
  }
}

powermat(x = a, 5)

#(c) Test the function with x = (1,2,3,4,0) and m = 5.
powermat(x = c(1,2,3,4,0), m=5)

#Q2
#Create a vector y containing a total of n = 20 observations. The first 10 observations should be simulated
#from a normal distribution with mean 20 and standard deviation 3, and the remaining 10 observations from
#a normal distribution with mean 10 and standard deviation 3. "Begin by assigning gl(2, 10) to the variable x"
#and create the design matrix X using X <- model.matrix(~ x)



n <- 20
y1 <- rnorm(10,20,3) #First 10 observations. mean of 20. Standard deviation 3
y2<- rnorm(10,10,3) #Second 10 observations. Mean of 10. Standard deviation 3

y <- c(y1,y2) #Paired together
x <- gl(2,10) #"Begin by assigning gl(2, 10) to the variable x"
X <- model.matrix(~ x) #create the design matrix X using X <- model.matrix(~ x)

#a) Assign to object beta.hat the results of equation and compare to coef(lm(y - x))
beta.hat <- solve(crossprod(X))%*%crossprod(X,y)
coef(lm(y ~ x))

#(b) Compare the 18 d.f. thing with anova(lm(y ~ x))$"Mean Sq"[2]

?inverse
#YtY (Part 1) - yTXBeta.hat (Part 2) - Beta.hattXty (Part 3) + Beta.hatTXTXBeta  (Part 4)/18
xTx <- crossprod(X,X)
yTy <- crossprod(y,y) #Part 1
xTy <- crossprod(X,y)

yTxBeta <- crossprod(y,X) %*% beta.hat #Part 2
Beta.xTy <- crossprod(beta.hat,xTy) #Part 3
Beta.xTx <- crossprod(beta.hat,xTx) %*% beta.hat #Part 4

sigma2 <- (yTy - yTxBeta - Beta.xTy + Beta.xTx)/18
sigma2
anova(lm(y ~ x))$"Mean Sq"[2]

#Both return the same value in Console However when compared answer is false.
#Despite having the same values, one is a matrix and other is numeric.

all.equal(sigma2, anova(lm(y ~ x))$"Mean Sq"[2])
as.numeric(sigma2) == as.numeric(anova(lm(y ~ x))$"Mean Sq"[2])

#(c) Get SE's for estimates b.hat, given by sq. root of diagonal elementts of the matrix H = (X>X)???1??^2
#Compare the results with
#summary(lm(y ~ x))$coef[,2]
H <- solve((xTx)^-1*as.numeric(sigma2))
H
summary(lm(y ~ x))$coef[,2]
H == summary(lm(y ~ x))$coef[,2] #Very close but not the same


#3 (a) rewrite them using pipes and check that you get the same answer:
dat <- c(3,5,7,9,11,13,15)
median(exp((log(dat)^2)/5)*0.4)
test1 <- median(exp((log(dat)^2)/5)*0.4)
library(tidyverse)
library(magrittr)

test2 <- dat %>% log() %>% raise_to_power(2) %>% divide_by(5) %>% exp() %>% multiply_by(0.4)  %>% median()
test2
test1 == test2

# Find the % of cars in the mtcars dataset where mpg is greater than 20
sum(mpg$cty > 20)/length(mpg$cty)
test3 <- mtcars$cty %>% mutate(mpg_20 = mpg < 20) %>% divide_by(length(mpg$cty))



#4
#Make a vector of:
mu <- 3
sigma <- 5
x <- rnorm(100, mu, sigma)

#(a) Not a chance in hell I'm write the questions for 4 and 5. I have better things to do
g <- rep(1:20,each = 5)

#data.frame(x,g)
y <- tapply(x, g)
y
#(b) For loop to give an alternative construction for vector y.
i <- 1:5
for(i in 1:20)
{
  length(i)
}
#(c)

index <- seq(2.5, 97.5, length = 20)
plot(index, y, ylim = range(x)*1.1, type = "o", col = "red")
points(x, cex = .8, pch = 21, bg = "lightgray")

#(d)
ul <- mu + 1.96*sigma
ll <- mu - 1.96*sigma
abline(h = c(mu,ul,ll), lty = 2)

#(e) - (i)

#Q (5)
#(a)
library(tidyverse)
library(nycflights13)
flights

flights %>% str()
flights %>% View()

#(b)
flights2 <- flights %>% filter(!is.na(dep_delay) & !is.na(arr_delay))
flights2
#(c)

flights3 <- flights2 %>% mutate(gain = dep_delay - arr_delay, 
                                air_hour = air_time,
                                gain_per_hour = gain/air_hour ) %>% select(gain_per_hour, origin) 
flights3
flights3 %>% View()

#(d)
library(ggplot2)
ggplot(flights3, aes(x = gain_per_hour, fill = origin)) +
  geom_density()

ggplot(flights3, aes(x = gain_per_hour, fill = origin)) +
  geom_point() +
  geom_smooth(method = lm, se = FALSE) 
