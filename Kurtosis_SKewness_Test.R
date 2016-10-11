
# test ofkurtosis for gre score from admits dataset

# installing and loading moments package for evaluating kurtosis
install.packages("moments")

library(moments)


# loading admit data 
admit <- read.csv("C:/Users/gvykr/Desktop/admit.csv")

# plotting the density plot of gre scores
plot(density(admit$gre))

# evaluating the tstat for admit
tstat <- kurtosis(admit$gre)
tstat

# evaluating the size of the sample wehave 
size <- nrow(admit)

# Hypothesis
# H0 : kurtosis=3
# Ha : Kurtosis<> 3

# Kurtosis for a normal distribution curve is normal 
# so we can use rnorm with sample meand sd while generating smapling distribution


# function to take samples to draw sampling distribution

f1 <- function(){
  # sample genration for sampling dist
  x<- rnorm(n = size,mean = mean(admit$gre),sd = sd(admit$gre))
  # returning the kurtosis value of the sample
  return(kurtosis(x))
}

# replicating the sample 10000 times
sdist<- replicate(10000,f1())

# plotting the sampling distribution
plot(density(sdist))

# evaluating the gap 
gap<- abs(mean(sdist)-tstat)

# ploting the two threshold lines on the distribution 
# to get a visual feel of what proportion our sdist satisfies H0 
abline(v = mean(sdist)-gap)
abline(v = mean(sdist)+gap)

p<- length(sdist[sdist < mean(sdist)-gap | sdist > mean(sdist)+gap]) / length(sdist)
p

# here p value is 0.159 which is greater than the threshold so we fail to reject the null hypothesis
# though our p value suggest that a small proportion our data is in tails 
# but it is not significant enough to say that distribution has heavy tails 
