#ST203 assignment 2
#Question 1 (DONE)
VADeaths
barplot(VADeaths, beside = TRUE,
        main = "Death rates in Virgina",
        ylab = "Deaths per 1000", legend = TRUE)
?barplot

#Question 2 (DONE) (a)
setwd("SharedFiles/ST203/")
eupop <- read.table("eupop.txt", header = TRUE, row.names = 1)
head(eupop)

#Question 2 (b)
#The legend might not be understandable to some so I'll explain it.
#It's grouped population data. Ages 0-14,15-44,45-64 and 65+.
?barplot
eupop <- as.matrix(eupop)
eupop
barplot(t(eupop[c("UK","Ireland"),-5]),
        main = "Population of UK and Ireland",
        beside = TRUE, legend = TRUE)

#Question 2 (c) 
barplot(t(eupop[c("Ireland"),-5]), ylim = c (-40,40),col = 3, xlab = "Population Ages")
barplot(-t(eupop[c("UK"),-5]),col = 4, add = TRUE, main = "Population of UK")

#Question 2 (d)
#Draw divided (stacked) barcharts comparing the population breakdowns of all countries.
barplot(eupop, main = "Population of All Countries",beside = FALSE, legend = TRUE)


#Question 3 (DONE) (a)
?rnorm()
x <- rnorm(100)
x
?rbinom
y <- rbinom(20,20,.25)
y
#Question 3 (b)
?qqplot
qqplot(x,y)
qqnorm(y) 
#Looks like a steady rise, 20 dots in total, More dots appear around the middle due to the higher probability of it being there.
##The further away from the middle it gets, the less common the dots become.

#Question 3 (c)
x1 <- rnorm(100)
y1 <- rbinom(20, 20, .5)
qqplot(x1,y1)
qqnorm(y1)

#Question 4 (DONE) (a)
?choose
#Given inputs k, n,p, calculate the sum.
fbinom <- function(k,n,p)
{
  sum(choose(n,k)*(p*k) * (1-p)^(n-k))
}
#Question 4 (b)
fbinom(5,10,.3)
fbinom(30,100,.3)
##Question 4 (c) & (d)
pbinom(5,10,.3)
pbinom(30,100,.3)


#Question 5 (DONE) (a) 
?rpois
180/5
rpois(1, 2)
q<- 180/5
rpois(q, 2)

num1 <- cumsum(rpois(q,2))
num2 <- cumsum(rpois(q,2))
num3 <- cumsum(rpois(q,2))

#Question 5 (b) & (c)
?plot
plot(0:q, c(0, num1), ylim=c(0,max(num1, num2, num3)), type = "l", col = "red", main = "Supermarket Customers"
     , ylab = "People", xlab = "Arrivals every 5 minutes")
lines(0:q, c(0, num2), col= "blue", lty = 2)
lines(0:q, c(0, num3), col = "yellow", lty = 3)
legend("topleft", col = 1:3, lty = 1:3, ncol = 1, paste0(1:3))

# Question 6 (DONE) (a)

#x <- c(1, 15, 12, 5, 18, 11, 12, 15, 18, 25) #Test values
Geom_mean <- function(x) prod(x)^(1/length(x))
Geom_mean(x)

#Question 6 (b)
?exp
test2<- function(x) exp(mean(log(x)))
test2(x)

#Question 6 (c)
set.seed(20325583)
x <- rexp(1000, rate = 1/100)
Geom_mean(x) ; test2(x)

#Question 6(d)
?replicate
differences <- function(x) mean(x) - test2(x)
DifferencesMeans <- replicate(1000, diffs.mean(rexp(100, 1)))

#Question 6(e)
?hist
hist(DifferencesMeans)
mean(DifferencesMeans)
