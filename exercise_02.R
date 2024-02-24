# Q1
# X: probability that relevant to number of matches occured in Bill's ticket
# P(X=0): none of match occured
# P(X>0) : at least one matching number
# P(X>0) = 1 - P(X=0)
# According to the provided probability distribution function 
P_X0 = (choose(6,0)*choose(43,6))/ choose(49,6) # P(X=0)
cat('P(X=0) = ',P_X0)
#P(X>0) = 1 - (choose(6,0)*choose(43,6))/ choose(49,6) 
P_Xover0 = 1 - (choose(6,0)*choose(43,6))/ choose(49,6) #P(X>0)
cat('\n')
cat('P(X>0) = ',P_Xover0)
E = 0.7347 #E(X)
SD = 0.76 #SD(X)
n = 52 # number of tickets Bills buys a year
UL = E + z*SD/sqrt(n)
LL = E - z*SD/sqrt(n)
cat("95% CI for average number of matches E that Bill can expect per ticket is", "(", LL, ",", UL, ")"))
cat('\n')
cat('Although the probability of getting at least one matching number in 52 draws ( one draw / week) is 0.564 which is greater that the probability of getting no matching numbers which is 0.436.
That does not guarantee at least one match on every week of a year. The CI indicates that that there will be weeks where Billy does not match any number, but Bill might expect that it is more likely than not that he will have at least one match on a ticket.')