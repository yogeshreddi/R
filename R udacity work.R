data(mtcars)

mtcars$mpg
mean(mtcars$mpg)

#loading the data

data(mtcars)

getwd()
#setwd("C:/Users/s/Downloads")
setwd("E:/yogesh/UDACITY")


!complete.cases(bd)

statesinfo<-read.csv("stateData.csv")
statesoinfo2<-read.table("stateData.csv")
st3<-tbl_df(mtcars)

class(statesinfo)
class(statesoinfo2)

str(statesoinfo2)
str(statesinfo)

#subseting
#dataset[ROWS,COLUMNS]
subset(statesinfo,state.region=1)

statesinfo[statesinfo$state.region!=1,]

statesinfo$area_class <- ifelse(statesinfo$area>30000,"big","small")
###statesinfo<-statesinfo[,- statesinfo$area_class]--- error
statesinfo<-subset(statesinfo,select=-area_class)

#FactorVariables & Factor ordering
reddit<-read.csv("reddit.csv")
str(reddit)
summary(reddit)
table(reddit$age.range)
levels(reddit$age.range)
plot(reddit$age.range)
library(ggplot2)
qplot(data=reddit,age.range)

levels(reddit$cheese)
levels(reddit$income.range)
reddit$income.range<-factor(reddit$income.range, levels = c("Under $20,000",
                                                            "$20,000 - $29,999",
                                                            "$30,000 - $39,999",
                                                            "$40,000 - $49,999",
                                                            "$50,000 - $69,999",
                                                            "$70,000 - $99,999",
                                                            "$100,000 - $149,999",
                                                            "$150,000 or more"), ordered = T)
library(ggplot2)
qplot(data=reddit,income.range)


#Data Munging

#Data Loading..
income <- read.csv("data/ACS_13_5YR_S1903/ACS_13_5YR_S1903.csv")
#Column class manipulation..
sapply(income, class)
income <- read.csv("data/ACS_13_5YR_S1903/ACS_13_5YR_S1903.csv",sep = ",",colClasses = c("HC02_EST_VC41"="logical"))
sapply(income, class)

#Loading data through a URL..
income <- read.csv("http://datasets.flowingdata.com/tuts/2015/load-data/ACS_13_5YR_S1903.csv",
                   stringsAsFactors=FALSE, sep=",", colClasses=c("GEO.id2"="character"))

# selecting observations with NA's in a particular column
income[is.na(income$HC01_MOE_VC40),]
# selecting observations without NA's in a particular column
income[!is.na(income$HC01_MOE_VC41),]
# Omissions of na's from dataset
income_new<-na.omit(income)
#subsetting 
income_new2<-income[,1:10]
head(income_new2)
str(income_new2)
summary(income_new2)
#changing col names
names(income_new2) <- c("id", "FIPS", "name", "households", "households_moe", "med_income", "med_income_moe","yogesh","aditya","shaik")
colnames(income_new2) <- c("id", "FIPS", "name", "households", "households_moe", "med_income", "med_income_moe","yogesh","aditya","shaik")
summary(income_new2)

#Merging two datasets

# Load two datasets
income2008 <- read.csv("data/ACS_08_3YR_S1903/ACS_08_3YR_S1903.csv",
                       stringsAsFactors=FALSE, sep=",", colClasses=c("GEO.id2"="character"))
income2013 <- read.csv("data/ACS_13_5YR_S1903/ACS_13_5YR_S1903.csv",
                       stringsAsFactors=FALSE, sep=",", colClasses=c("GEO.id2"="character"))


# Merging

income0813 <- merge(income2008, income2013, by="FIPS")

#Exploring/visualization one varibale in datast
pf<-read.csv("pseudo_facebook.tsv",sep="\t")
library(ggplot2)

#scaling the x axis/dob_day properly
qplot(x=dob_day,data=pf)+
  scale_x_discrete(breaks=1:31)

#Faceting
# facet_wrap(formula) , facet_wrap(~variables)
#facet_grid(formula), facet_grid(vertical~horizontal)

qplot(x=dob_day,data=pf)+
  scale_x_discrete(breaks=1:31)+
  facet_wrap(~dob_month,ncol=3)


#limitting the axis
qplot(data=pf,x=friend_count,xlim = c(0,1000))
#or
qplot(data=pf,x=friend_count)+
  scale_x_continuous(limits=c(0,1000))
#or
qplot(data=pf[pf$friend_count<1000,],x=friend_count)

#modifying the binwidth/barwidth & scaling the x axis properly
qplot(data=na.omit(pf),x=friend_count,binwidth=25)+
  scale_x_continuous(breaks=seq(0,1000,50),limits = c(0,1000))

#Spliting the histograms based on other columns using facet_wrap
qplot(data=pf[!is.na(pf$gender),],x=friend_count,binwidth=25)+
  scale_x_continuous(breaks=seq(0,1000,50),limits = c(0,1000))+
  facet_wrap(~gender,ncol=1)
#or
qplot(data=na.omit(pf),x=friend_count,binwidth=25)+
  scale_x_continuous(breaks=seq(0,1000,50),limits = c(0,1000))+
  facet_wrap(~gender,ncol=1)

#female vs male friend count
table(pf$gender)

by(pf$friend_count,pf$gender,summary)

#Using Color in histo

qplot(data=pf,x=tenure/365,color=I('white'),fill=I("RED"))+
  scale_x_discrete(breaks=1:7)

qplot(data=pf,x=tenure/365,color=I('white'),fill=I("RED"),xlab="Tnure in years",ylab="COUNT")+
  scale_x_discrete(breaks=1:10,limits=seq(0,7,1))

#applying bin breaks limits and lable

qplot(data=pf,x=age,color=I("white"),binwidth=1)+
  scale_x_continuous(breaks=seq(13,max(pf$age),5),limits=c(13,113))


#transforming

qplot(data=pf,x=ifelse(abs(friend_count)<=1,0,log10(friend_count)),color=I("white"),xlab="logarithmic trnasform of friend count to base 10",ylab="Number of facebook users")
#  scale_x_log10()

summary(log(pf$friend_count+1,10))

#scaling
library(ggplot2)
qplot(data=pf[pf$friend_count<2000,],x=friend_count,color=I("white"))
qplot(data=pf,x=log10(friend_count),color=I("white"),fill=I("blue"))
qplot(data=pf,x=sqrt(friend_count),color=I("white"),fill=I("blue"))



pf<-read.csv("pseudo_facebook.tsv",sep="\t")
library(ggplot2)
library(gridExtra)
#gridExtra
#Provides a number of user-level functions to work with "grid" graphics, 
#notably to arrange multiple grid-based plots on a page, and draw tables.

#Arrange multiple grobs on a page
grid.arrange(qplot(data=pf[pf$friend_count<2000,],x=friend_count,color=I("white")),
             qplot(data=pf,x=log10(friend_count+1),color=I("white"),fill=I("blue")),
             qplot(data=pf,x=sqrt(friend_count),color=I("white"),fill=I("red")),ncol=3)

#or

p1<-ggplot(aes(x=friend_count),data=pf)+geom_histogram(color=I("white"))
grid.arrange(p1,p1+scale_x_log10(),p1+scale_x_log10()+scale_x_sqrt(),ncol=3)


# Frequency Polygon
p1<-ggplot(aes(x=friend_count),data=pf)+geom_freqpoly(color=I("blue"))

#2 freqpoly on same plot

qplot(data=pf[!is.na(pf$gender),],x=friend_count,color=gender,geom='freqpoly',binwidth=10)+
  scale_x_continuous(limits=c(0,1000),breaks=seq(0,1000,50))
  #facet_wrap(~gender)


qplot(x=www_likes,data=pf[!is.na(pf$gender),],geom='freqpoly',color=gender)+
  scale_x_continuous(limits=c(0,2000),breaks=seq(0,2000,200))+
  scale_x_log10()

by(pf$www_likes,pf$gender,sum)

#Box plot
# y - continous, x - catogorical

qplot(geom='boxplot',y=friend_count,x=gender,data=pf[!is.na(pf$gender),])+
  scale_y_continuous(limits=c(0,1000))
#in abve data points ar ignored after 1000, so box plot will be different from original

#coord_cartesian

qplot(geom='boxplot',y=friend_count,x=gender,data=pf[!is.na(pf$gender),])+
  coord_cartesian(ylim=c(0,250))

qplot(y=friendships_initiated,x=gender,data=pf[!is.na(pf$gender),],geom='boxplot')+
  coord_cartesian(ylim=c(0,150))

by(pf$friendships_initiated,pf$gender,summary)

by(pf$mobile_likes,pf$gender,summary)

pf$mobile_check_ins<-NA
pf$mobile_check_ins<-ifelse(pf$mobile_likes==0,1,0)
pf$mobile_check_ins<- factor(pf$mobile_check_ins)

summary(pf$mobile_check_ins)

63947/(63947+35056)

#problem set 3 Udacity

data("diamonds")

dim(diamonds)
head(diamonds)
str(diamonds)
?diamonds

library(ggplot2)
qplot(data=diamonds,x=price,color=I("white"))

sum(diamonds$price>=15000)


library(ggplot2)
qplot(data=diamonds,x=price,color=I("white"),binwidth=1)+
  scale_x_continuous(limits=c(0,1800),breaks=seq(0,1800,150))


qplot(x=price,data=diamonds,color=I("black"),binwidth=1)+
  facet_wrap(~cut,ncol=1)

by(diamonds$price,diamonds$cut,Summary)
by(diamonds$price,diamonds$cut,max)
by(diamonds$price,diamonds$cut,min)

facet_wrap()

qplot(x=price,data=diamonds,color=I("black"),binwidth=1)+
  facet_wrap(~cut,ncol=1,scales = ("free_y"))


colnames(diamonds)
head(diamonds)


qplot(x=price/carat,data=diamonds,color=I("black"),fill=I("blue"),binwidth=600)+
  scale_x_continuous(limits=c(0,10000))+
  facet_wrap(~cut,scales=("free_y"))

qplot(x =log10(price/carat), data = diamonds, color = I('black'), fill = I("blue"), binwidth = 0.05) +
  scale_x_continuous(limits=c(0,10000))+
  facet_wrap(~cut, scales="free_y") 


rbind(c(1,2),c(3,4))

c(c(1,2),c(3,4))


round(10/3,2)


# Create a histogram of price per carat
# and facet it by cut. You can make adjustments
# to the code from the previous exercise to get
# started.

# Adjust the bin width and transform the scale
# of the x-axis using log10.

# Submit your final code when you are ready.

# ENTER YOUR CODE BELOW THIS LINE.
# ===========================================================================

qplot(x=log10(price/carat),data=diamonds,color=I("white"),binwidth=0.05)+
  facet_wrap(~cut,ncol=1)

#or

qplot(x=price/carat,data=diamonds,color=I("white"))+
  scale_x_log10()+
  facet_wrap(~cut,ncol=1)


# Investigate the price of diamonds using box plots,
# numerical summaries, and one of the following categorical
# variables: cut, clarity, or color.

# There won’t be a solution video for this
# exercise so go to the discussion thread for either
# BOXPLOTS BY CLARITY, BOXPLOT BY COLOR, or BOXPLOTS BY CUT
# to share you thoughts and to
# see what other people found.

# You can save images by using the ggsave() command.
# ggsave() will save the last plot created.
# For example...
#                  qplot(x = price, data = diamonds)
#                  ggsave('priceHistogram.png')

# ggsave currently recognises the extensions eps/ps, tex (pictex),
# pdf, jpeg, tiff, png, bmp, svg and wmf (windows only).

# Copy and paste all of the code that you used for
# your investigation, and submit it when you are ready.
# =================================================================

qplot(y=price,data=diamonds,x=color)+
  scale_y_continuous(breaks=seq(0,max(diamonds$price),1000))+
  geom_boxplot()

ggsave('priceclaritybox.png')


######

summary(diamonds$color)

summary(diamonds[diamonds$color=="J",][,"price"])


diamonds

library(ggmap)

?ggmap

?get_all_vars()

?maptools

?map2SpatialPolygons

dc[1,]

testPoints <- data.frame(x = c(-90, -120), y = c(44, 44))

latlong2state(testPoints)

library(sp)
names(dc)

points <- SpatialPoints(dc[,5])

paste(c(1,2,3),c(4,5,6),sep="/",collapse=",")

substr(dc[1,5],3,last=",")

paste(1:12, c("st", "nd", "rd", rep("th", 9)))

strsplit(as.character(dc[1,5]),",")[1]

as.character()

strsplit()

sapply(dc,class)

SpatialPoints()