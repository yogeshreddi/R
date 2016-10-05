# Statistics from ground

# distribution

# discrete distribution 

#tossing a coin
v = c('H','T')
p = c(0.5,0.5)
sample(x = v, prob = p,size = 10,replace = T)

# rolling a die
v=seq(1,6)
p=rep(1/6,6)
sample(x = v, prob = p,size = 5,replace = T)

# Continous
# normal distribution
r1<-rnorm(10000)
plot(density(r1))

#unifrom distribution
r2<- runif(10000)
plot(density(r2))

# log normal distribution 
# only positive and is skewed to the right
y = exp(r1)
plot(density(r1))


r1<- rnorm(10000)
r2<- rnorm(10000)
r3<- rnorm(10000)
r4<- rnorm(10000)
y = r1+r2+r3+r4
plot(density(y))
y = r1^2+r2^2+r3^2+r4^2
plot(density(y))

r5<- rnorm(10000)
r6<- rnorm(10000)
r7<- rnorm(10000)
r8<- rnorm(10000)
y <- r1^2+r2^2+r3^2+r4^2+r5^2+r6^2+r7^2+r8^2
plot(density(y))


# if you see the skewnees is reducing as we are adding more and more varibale..
# so adding more varibales with chi square distribution will result in a normal distribution at some threshold.

u1<- runif(10000)
u2<- runif(10000)

y<- u1+u2

plot(density(y))


u1<- runif(10000)
u2<- runif(10000)
u3<- runif(10000)
u4<- runif(10000)
u5<- runif(10000)
u6<- runif(10000)
u7<- runif(10000)
u8<- runif(10000)

y<- u1+u2+u3+u4+u5+u6+u7+u8

plot(density(y))


#################################

# for an x on the distribution dnorm will give the density value

dnorm(x =3,mean = 5, sd = 3)

# gives the are which fall under that particular poiint
pnorm(3, mean =5, sd = 3)

# does the opposite to what pnorm does
qnorm(0.25,5,3)



######################### distributions


f2<- function(){
  rn <- rnorm(25,mean = 100,sd = 15)
  return(mean(rn))
}


sdist <- replicate(4000,f2())
  
summary(sdist)

mean(sdist)

sd(sdist)
psd <- 15/sqrt(25)

plot(density(sdist))

####################


f3<- function(){
  rn <- rnorm(25,mean = 100,sd = 15)
  return(median(rn))
}


sdist <- replicate(4000,f3())

summary(sdist)

mean(sdist)

sd(sdist)
psd <- 15/sqrt(25)

plot(density(sdist))


####################


f3<- function(){
  rn <- rnorm(25,mean = 100,sd = 15)
  return(quantile(probs = 0.75,x= rn))
}


sdist <- replicate(4000,f3())

summary(sdist)

mean(sdist)

sd(sdist)
psd <- 15/sqrt(25)

plot(density(sdist))
