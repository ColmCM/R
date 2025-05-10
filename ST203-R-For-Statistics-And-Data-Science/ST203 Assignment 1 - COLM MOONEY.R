#ST203 Assignment - Colm Mooney

#Q1 - Complete
x <- 1:10
x * (2/3) * (3/2) - x
## This is a rounding error. Decimal numbers can't be measured in binary, (which is how numbers in a computer is represented) leading to the result you see.

#Q2 - Complete
2 + 2 == 4 # [1] TRUE
## This is asking the question does 2 + 2 equal 4. The console is responding it does. if I typed 2+2 equaled any other number, it would say [1] FALSE.
## 2 + 2 = 4 would lead to an error. If you run the code 2 + 2, the answer in console will say TRUE.

sqrt(2) * sqrt(2) == 2 #[1] FALSE
## This is a rounding error. While typing sqrt(2) * sqrt(2) = 2 would be [1] TRUE, sqrt(2) * sqrt(2) doesn't exactly equal to. It is very close, but it is
## slightly off

all.equal(sqrt(2) * sqrt(2), 2) #[1] TRUE
##This question is 2 the nearest whole number closest to sqrt(2) * sqrt(2). And it is. Therefore, the answer is [1] TRUE

#Q3 - Complete
1:5 + rep(0:4,each=5)

#Q4 - Complete
sum_n <- function(n) sum(1:n)
n <- 100
sum_n(100)
sum_n(200)
sum_n(400)
sum_n(800)

sapply(c(100,200,400,800),sum_n)
sapply(100*2^(0:3), function(n) n*(n+1)/2)


  
  #Best way to compare with n(n + 1)/2
  sapply(c(100,200,400,800), sum_n)
sapply(100*2^(0:3), function(n) n*(n+1)/2)

sapply(c(100,200,400,800), sum_n) == sapply(100*2^(0:3), function(n) n*(n+1)/2)

#Q5 - Not sure on this

x <- 1/3
n <- 10
sum_n(10)
sapply(c(10), sum_n)

m <- n+1
sum((1-x^m)/(1-x))

sapply(c(10), sum_n) == sapply(c(10), sum_n)

#Q6 #(A) Cars whose dist is between 20 & 25 - Complete

?cars
cars[1:50,]
is.data.frame(cars)
cars[cars$dist>=20&cars$dist<=25,]


#(B) Average speed of cars whose dist is between 20 & 25
mean(cars[cars$dist>=20&cars$dist<=25, "speed"])

#(C)Subset to get cars whose dist is between 20 & 25
subset(cars, dist >= 20 & dist <= 25)

#Q7 - COMPLETED
##(A)
solar_radiation <- c(11.1, 10.6, 6.3, 8.8, 10.7, 11.2, 8.9, 12.2)
##(B)
mean(solar_radiation)
median(solar_radiation)
var(solar_radiation)
##(C)

sr10 <- solar_radiation+10
mean(sr10)
median(sr10)
var(sr10)
##Mean changes from 9.975 to 19.975
##Median changes from 10.65 to 20.65
##Variance does not change

sr2 <- solar_radiation*-2
mean(sr2)
median(sr2)
var(sr2)
##Mean changes from 9.975 to -19.95
##Median changes from 10.65 to -21.3
##Variance has changed to 14.1

hist(solar_radiation)
hist(sr10)
hist(sr2)

