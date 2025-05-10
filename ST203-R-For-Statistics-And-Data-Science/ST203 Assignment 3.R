#ST203 ASSIGNMENT 3, 20325583, Colm Mooney
#Q1
qroots <- function(a, b, c) {
  if(a == 0)
  {
    roots <- -c/b
  }
  else
  {
    d <- b^2 -4*a*c
    
    if(d > 0) {
      d1 <- sqrt(d)
      roots <- c((-b-d1), (-b+d1))/(2*a)
    } 
    else if(d==0) {
      r <- -b/(2*a)
      roots <- c(r,r)
    } else {
      roots <- c()
    }
  }
  return(roots)
  
}

## what if a = 0?
qroots(0,4,1)
#When a = 0, the values returned are -Infinity and Not a number. We've added an if statement that deals with that (2-5)

#Question 2 
#(a)
n <- 30
fib <- numeric(n)
fib[1:2] <- 1
for(i in 3:n) {
  fib[i] <- fib[i-1] + fib[i-2]
}
fib
#(b)
n <- 15
fib <- numeric(n)
fib[1:2] <- 1
for (i in 3:n) fib[i] <- fib[i-1] + fib[i-2]
fn <- fib[-1]
fn1 <- fib[-n]
plot(fn/fn1)


#(c) Compute the golden ratio (1 + ???5)/2. Is the sequence converging to this ratio?
n <- 15
fib <- numeric(n)
fib[1:2] <- 1
for (i in 3:n) fib[i] <- fib[i-1] + fib[i-2]
fn <- fib[-1]
fn1 <- fib[-n]
plot(fn/fn1)
abline(h = (1+sqrt(5))/2, col="red")
#yes, it does follow to this ratio.

#Question 3
#Make a 5x5 Hankel matrix using:
?matrix
?seq
?rep
n <- 5
matrix(seq(1:n) + rep(0:n,each=n), nrow = n, ncol = n)
n <- 10
matrix(seq(1:n) + rep(0:n,each=n), nrow = n, ncol = n)
n <- 12
matrix(seq(1:n) + rep(0:n,each=n), nrow = n, ncol = n)

#Question 4. (a)

mat <- matrix(nrow = 4,c(1,2,3,4,1^2,2^2,3^2,4^2))
mat

#(b)
?crossprod
tmat <- t(mat)
tmat2<- tmat%*%mat
tanswer <- solve(tmat2)
tanswer
#(c)
#Get inverse of the inverse thingy
transposedmatrix <- mat%*%tmat
transposedmatrix
solve(transposedmatrix)
det(transposedmatrix)
#The answer says the determinant is 0. That That's not the case, it only says zero because it can solve the transposed matrix.

#Question 5

#(a) How long does it take to clear the debt (in months)?

nmonths <- 0
loan <- 1000
rate <- (1+0.11/12)
money <- 12
while(loan > 0)
{
  loan <- (rate*loan - money)
  nmonths <- nmonths + 1
}
loan
nmonths # It takes 159 months.

#(b) Draw a plot showing the amount owed every month.
?plot

nmonths2 <- 1
loan2 <- 1000
rate2 <- (1+0.11/12)
money <- 12
test <- 0
while(loan2[nmonths2] > test)
{
  loan2 <- c(loan2, rate2*loan2[nmonths2] - money)
  nmonths2 <- nmonths2 + 1
}
plot(0:(nmonths2 - 1), loan2, type = "l")

#(c) How much total interest is paid in the end?
159 * 12 - 1000 -9.73 == 898.27 
#159 months, 12 euros, 1000 = first loan, -9.73, overpay from the last month.
