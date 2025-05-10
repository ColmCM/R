#Colm Mooney 23255803 3pm 19/11/21
# Suppose the turning pattern follows a binomial distribution.
#size = 16, Probability .38

#(a)Chance exactly 7 mice turn right?
dbinom(x = 7,size = 16, prob = .38)

#(b) Chance that 8 or fewer turn right?
1-dbinom(x = 8,size = 16,prob = .38)

#(c) Chance that 8 or more turn right?
sum(dbinom(8:16, size = 16, prob = .38))

#(d) Chance that from 5 to 7 turn right?
sum(dbinom(5:7, size = 16, prob = .38))

#(e) What is the 80th percentile value for the number of mice that turn right? Provide an interpretation
# qunif(0.8,0,16) ?qpois qpois(0.8, lambda = 16, prob = .38)
qbinom(0.8, 16, .38)
p4<- qbinom(0.8, 16, .38)
plot(p4)
#Due to how probability works, It is next to impossible for the 16 mice to turn right when the chance of one them going right
##is 38% per rat. This leads to a loop of 38% X 38% X 38% etc. with 16 38%s. However, the 80th percentile is about 8 rats.
##You would think that it is 12.4 rats, but that's only 80% of the rats that still have the option to go left or right.



#houses sold by an estate agent follows a Poisson distribution with a mean of 2 per week.
#Find the probability that in the next 4 weeks the estate agent sells:
#(i) exactly 5 houses
?ppois
ppois(q = 5, lambda = 8, lower.tail = TRUE)

#(ii) more than 5 houses

1 - ppois(q = 5, lambda = 8)

#(b) The estate agent monitors sales on a monthly basis (every 4 weeks) and considers a month to be a
#success if more than 5 houses are sold (and a failure if 5 or less are sold). If the estate agent monitors
#sales each month for 12 months, what is the probability that 6 or more months are successful?
sum(dbinom(6:12,size = 12, prob = 0.6866257))
