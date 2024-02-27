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


# Q2
# create the bootstrap distribution of the sample mean
# library(mosaic)
LC50 = c(16, 5, 21, 19, 10, 5, 8, 2, 7, 2, 4, 9)
M = do(2000)*mean(sample(LC50,replace = TRUE))
dim(M)
head(M)
ggplot(M, aes(x=M$mean)) + 
  geom_histogram(col='black', fill='blue', binwidth=0.5, na.rm=TRUE) + 
  xlab("LC50, n = 12") + ylab("Frequency") + 
  ggtitle("Distribution of sample mean LC50 on a certain species of fish - bootstrap")

#b compute the 95% bootstrap (percentile) confidence interval for the mean LC50 measurement for DDT
UL = quantile(M$mean, 0.975)
LL = quantile(M$mean, 0.025)
cat("95% CI for mean LC50 measurement - bootstrap technique is", "(", LL, ",", UL, ")")
print("\n")

#c using the other confidence interval -  t approach with assumption of Normality with population distribution
n = length(LC50)
t = qt(0.975,n-1)
UL = mean(LC50) + t*sd(LC50)/sqrt(n)
LL = mean(LC50) - t*sd(LC50)/sqrt(n)
cat("95% CI for mean LC50 measurement - t approach is", "(", LL, ",", UL, ")")
print('\n')
print("c. To compute a 95% confidence interval (CI) for the mean LC50 measurement using the t-technique, we assumed as below :
# random sampling - this ensures hat the sample is representative of the larger population.
# independence - the measurement of one item should not influence or be dependent on the measurement of another.
# normality with population distribution
# the accuracy of CI increases as the sample size increases 
# unknown population sd")

# d 
print("d. while the provided confidence intervals are the same for both techniques in this specific case, it does not guarantee that the data follows a normal distribution.
    it's essential to consider factors like outliers which can distort the shape of data distribution")

# Q3

HS = do(1000)*mean(resample(c(rep(1,348),rep(0,322)),670));
quantile(HS$mean,0.025);
quantile(HS$mean,0.975);
ggplot(data = HS, aes(x=HS$mean)) + geom_histogram(color = "orange", fill = "green") + 
  xlab("Values of Bootstrap proportion_HS") + ylab("Count") + ggtitle("Distribution of Bootstrap Statistic_HS: Sample proportion") + 
  geom_vline(xintercept = quantile(HS$mean, 0.025), color="red") + geom_vline(xintercept = quantile(HS$mean, 0.975), color="red")


UN = do(1000)*mean(resample(c(rep(1,274),rep(0,102)),376));
quantile(UN$mean,0.025);
quantile(UN$mean,0.975);
ggplot(data = UN, aes(x=UN$mean)) + geom_histogram(color = "orange", fill = "blue") + 
  xlab("Values of Bootstrap proportion_UN") + ylab("Count") + ggtitle("Distribution of Bootstrap Statistic_UN: Sample proportion") + 
  geom_vline(xintercept = quantile(UN$mean, 0.025), color="red") + geom_vline(xintercept = quantile(UN$mean, 0.975), color="red") 


HS01 = c(rep(1,348),rep(0,322))
UN01 = c(rep(1,274),rep(0,102))
library(mosaic)
#re-sampling with replacement
prop_HS01 = do(1000)*mean(resample(HS01, replace = TRUE))
prop_UN01 = do(1000)*mean(resample(UN01, replace = TRUE))
#distribution of the difference between two proportions
prop_diff = prop_UN01 - prop_HS01

#visualizing the distribution
ggplot(prop_diff,aes(x=mean)) + geom_histogram(col='black',fill = "red")+
  xlab("phat_UN - phat_HS") + ylab("frequency") +
  ggtitle("Bootstrap Distribution of difference between two proportions") +
  geom_vline(xintercept = quantile(prop_diff$mean, 0.025), color="blue") + geom_vline(xintercept = quantile(prop_diff$mean, 0.975), color="blue") 
quantile(prop_diff$mean,c(0.025,0.975))
print('\n')
print("The 95% confidence intervals is to the right of 0, with the center of distribution is around 0.2. This would be suggested that in most boostrap samples, phat_UN is greater than phat_HS based on this histogram with the bootstrap technique involved")
