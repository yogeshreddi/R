task1 <- rnorm(n = 10000,mean = 5, sd = 4)

task2 <- runif(n = 10000, max = 7, min =3)

time <- task1 + task2

s1 <- time[time<=10]
p <- length(s1)/length(time)

# the value range for probability being 75%
t75 <- quantile(x = time,probs = 0.75)
# checking if its true
s2<- time[time <=t75]
p2 <- length(s2)/length(time)



# doing tasks in parallel instead of sequntially 

task1 <- rnorm(n = 10000,mean = 5, sd = 4)

task2 <- runif(n = 10000, max = 7, min =3)

time2 <- ifelse(task1<=task2,task2,task1)

plot(density(time2))

s3 <- time2[time2<=10]
p3 <- length(s3)/length(time2)

median(time2)

####################

n1 <- rnorm(n =50, mean = 5, sd = 3)

plot(density(n1))

summary(n1)


# sampling error is inntroduced while taking a small sample from a population

# steps fro performing a statistical test

# 1. First thing we care bout is, lets say we care about test statistic (mean)
# 2. Then we hypothesize about the population (hypothesis is firstdefined based on business problem)
# 3. Using sample we define other parameters as a approximation to population
# 4. Take a small sample of same size zillion times and evaluate the test statistic in picture
#    and plot the distribution (sampling distribution) and see where the hypothesized mean fall in the distribution


# Statistical test for mean

admission <- read.csv("E:/Study/uconn/Fall2016/R/Session 4/Data/admission.csv")

tstat <- mean(admission$GMAT)
sd
# hypothesis
# normal , 
m = 510
std = sd(admission$GMAT)
sizee <- nrow(admission)

f1<- function(){
  x <- rnorm(n = sizee,mean = m, sd = std)
  return(mean(x))  
}

sdist <- replicate(10000,f1())
plot(density(sdist))

gap <- abs(mean(sdist)-tstat)
abline(v = mean(sdist),col = 'green')
abline(v = mean(sdist)+gap,col = 'red')
abline(v = mean(sdist)-gap,col='red')

s1<- sdist[sdist<mean(sdist)-gap | sdist>mean(sdist)+gap ]

p<- length(s1)/length(sdist)
p


####################### test of Sandard deviation ###########################

admission <- read.csv("E:/Study/uconn/Fall2016/R/Session 4/Data/admission.csv")

tstat <- sd(admission$GMAT)
# hypothesis
# normal , 
sd = 78
mean <- mean(admission$GMAT)
sizee <- nrow(admission)

f2<- function(){
  x <- rnorm(n = sizee,mean = mean(admission$GMAT), sd = 78)
  return(sd(x))  
}

sdist <- replicate(10000,f2())
plot(density(sdist))

gap <- abs(mean(sdist)-tstat)
abline(v = mean(sdist),col = 'green')
abline(v = mean(sdist)+gap,col = 'red')
abline(v = mean(sdist)-gap,col='red')

s2<- sdist[sdist<mean(sdist)-gap | sdist>mean(sdist)+gap ]

p<- length(s2)/length(sdist)
p


############################## test for median ######################################

admission <- read.csv("E:/Study/uconn/Fall2016/R/Session 4/Data/admission.csv")

tstat <- median(admission$GMAT)

# hypothesis
# normal , 
sd = sd(admission$GMAT)
mean <- median(admission$GMAT)
sizee <- nrow(admission)

f3<- function(){
  x <- rnorm(n = sizee,mean = 500, sd = sd(admission$GMAT))
  return(median(x))  
}

sdist <- replicate(10000,f3())
plot(density(sdist))

gap <- abs(mean(sdist)-tstat)
abline(v = mean(sdist),col = 'green')
abline(v = mean(sdist)+gap,col = 'red')
abline(v = mean(sdist)-gap,col='red')

s3<- sdist[sdist<mean(sdist)-gap | sdist>mean(sdist)+gap ]

p<- length(s3)/length(sdist)
p




############################ 

tstat <- quantile(x = admission$GMAT,probs = 0.75)

# hypothesis about population
sd <- sd(admission$GMAT)
mean <- 
sizee <- nrow(admission)

qnorm(0.75,mean = 545,sd = sd(admission$GMAT))

f3<- function(){
  x <- rnorm(n = sizee,mean = 545, sd = sd(admission$GMAT))
  return(quantile(x,0.75))  
}

sdist <- replicate(10000,f3())
plot(density(sdist))

gap <- abs(mean(sdist)-tstat)
abline(v = mean(sdist),col = 'green')
abline(v = mean(sdist)+gap,col = 'red')
abline(v = mean(sdist)-gap,col='red')

s3<- sdist[sdist<mean(sdist)-gap | sdist>mean(sdist)+gap ]

p<- length(s3)/length(sdist)
p


polygon(density(sdist),col = ifelse(sdist<=mean(sdist)-gap,'green','white') )


#####




























