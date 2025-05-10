#20325583, Colm Mooney.

#Suppose a Normal Distribution has µ = 100 and ?? = 10.
#(a) Plot the probability density function for this distribution. Use values in the range 50 to 150 for creating the plot.
test <- dnorm(50:150, mean = 100, sd = 10)
graph <- data.frame("Range:" = 50:150, "Density" = test)
plot(graph, main = "Probability Density Function")

#(b) Calculate the area 
#(i) below 90, 
?pnorm
pnorm(90, mean = 100, sd = 10)
#(ii) above 105, 
pnorm(105, mean = 100, sd = 10)
#(iii) between 80 and 107.
pnorm(107, mean = 100, sd = 10) - pnorm(80, mean = 100, sd = 10)

#(c) Plot the cumulative probability distribution function. Use the same range of values that you used in part (a).

x <- seq(50,150,by = 1)
cdf <- pnorm(x)
plot(x,cdf, main = "Cumulative Probability Distribution Function")

#(d) Find the 2.5th and 97.5th percentile values. Provide an interpretation.
qnorm(0.025, mean = 100, sd = 10)
qnorm(0.975, mean = 100, sd = 10)

abline(v = qt(0.025, df = N-1))
abline(v = qt(.975, df = N-1))


#Load this dataset into R and answer the following questions.
beers <- readRDS("~/SharedFiles/ST201/beers.rds")

beers$Before
beers$After
#(a) Create a new variable which is the difference between the After reaction times and the Before reaction
#times. What is the mean difference based on this sample?
p1 <- mean(beers$Before)
p2 <-mean(beers$After)

p3 <- p2 -p1 
p3 # 0.5015 seconds
#(b) What is the 90% confidence interval for the population mean difference? Provide an interpretation.


N <- 20

se <- p3/sqrt(N)
p3 - qt(0.9, df = N-1)*se
p3 + qt(0.9, df = N-1)*se

#(c) What conclusions can you draw from (b) about the mean difference in reaction times?

#That drinking alcohol leads to slower reaction times.
#The reaction times of those  drinking is faster than after drinking.
