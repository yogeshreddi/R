# R Basics and visualizations
# Author : Venkata Gongati , vyg14001

# Question 1

# Creating a vector of 500 Random normal values
x = rnorm(500)

# Creating a vector of 20 random uniform values
y = runif(500)

# Plotting normal "x" vs uniform "y" with mentioned pch, color and labels
plot(x,y,xlab = "normal",ylab = "uniform",pch = 7,col = "red")


# Question 2

# 2.a
# snippit to repeat "x" "y" "z" 6 times
rep(c("x","y","z"),6)

#2.b
# snippit to repeat "x" 5 times, then "y" once, and then "z" twice
rep(c("x","y","z"),c(5,1,2))

# Installing and loading package ISwR
install.packages("ISwR")
library("ISwR")

# viewing thw jull2 dataset
View(juul2)

# checking the dimension of the dataset
dim(juul2)

# Question 1
# Calculating mean,median, and 75th Percentile of age from the dataset juul2
mean(juul2$age,na.rm = T)
median(juul2$age,na.rm = T)
quantile(juul2$age,0.75,na.rm=T)

# Question 2
# t-test to evaluate the hypothesis that population mean of weight variable is 45
t.test(juul2$weight,mu=45)
# P value = 0.4812 greater than the default significance level so we fail to reject the null hypothesis

# Question 3
# Subsetting the datatframe to select only rows for which age < 20
juul2_new <- juul2[juul2$age<20,]
# Plotting the age vs igf1 from the juul2_new dataframe
plot(juul2_new$age,juul2_new$igf1)

# Question 4
# Creating a subset of juul2 dataframe such that the subset contans only girls with age over 20
# and also observations with NA values in sex and age are removed
s1 <- juul2[juul2$age>20 & juul2$sex == 2 & !is.na(juul2$age) & !is.na(juul2$sex),]

# Question 5
# Calculating median value of igf1 in the new dataframe s1
median(s1$igf1,na.rm = T)

#Install and loading the package plm, loading the data Males.
install.packages("plm")
library(plm)
data(Males)

##############################################################################

###################............a..............################################
#ataaching males dataset
attach(Males)
#frequency table between industry and residence. 
table(industry,residence)

#or 

ftable(table(industry,residence))

#Qns and ans

#Qn - Where do people in manufacturing mostly live and by what proportion? 
x <- table(industry,residence,useNA = "always")

#To find the percentage or propotion columnwise
prop.table(x,1)

#ans - People in manufacturing industry live mostly in nothern_central with a ~40% of the total in manufaturing 

#Qn - What proportion of people in the south are in Transportation? 
# row wise propotion 
row_prop <- prop.table(x,2)

# proportion for people in south with transportation
row_prop["Transportation","south"]

#ans - there ~10.423% of south people are in transportation 

#Qn - What is the most popular industry for people living in rural_area?

row_prop[,"rural_area"] == max(row_prop[,"rural_area"])

row_prop[row_prop[,"rural_area"] == max(row_prop[,"rural_area"]),]
#ans - In rural are with ~24.7% of people Trade industry is most popular

############################################################################

##############.................b.....................#######################

#Qn - Compute average wage based on industry and married using tapply(). 
s1 <- tapply(wage, list(industry,married),mean)
print(s1)

s1[,"yes"]==max(s1[,"yes"])

#Qn - Which industry has the highest wage on average among married people?
#ans -  among married people, people in Transportation have the highest wage


###########################################################################
################..................c..................######################

# dividing the experience into range of values 
careerlevel <- cut(exper,c(-Inf,3,8,Inf))
careerlevel

# Calculating the average wage grouping by careerlevel and married
tapply(wage,list(careerlevel,married), mean)

###########################################################################
################..................d..................######################

# installing and loading dplyr package
install.packages("dplyr")
library(dplyr)
#--------------------------------d.1-----------------------------------
#subsettig Males dataset to select "school", "residency", "wage" and saving it to s1
s1 <- select(Males,school,residence,wage)

#--------------------------------d.2-----------------------------------
#formula for filtering the s1 dataset for data of north_east residence and with wage greater than 1.5 
f1 <- residence == "north_east" & wage > 1.5

#filtering the s1 using formula f1 and save the resultant dataset into s2
s2 <- filter(s1,f1)

#--------------------------------d.3-----------------------------------
#renaming the school variable in s2 dataset as education_years
s2 <- rename(s2,education_years = school)

#ordering the s2 datset rows by wage decreasing
s2 <- arrange(s2,desc(wage))

#detaching males dataset
detach(Males)

#############################################################
#########################....2....##########################
##############################################################

# loading datasets libraray

library(datasets)

#attaching quakes dataset
attach(quakes)

#..........................a.........................................

#Spliting the plotting region into 1 row and 3 columns with grey background and blue color 
opar <- par()
par(bg = "grey",col = "blue",mfrow = c(1,3))
#plotting the following graphs: lat and long, depth and mag, stations and mag.
plot(lat,long)
plot(depth,mag)
plot(stations,mag)
par(opar)

#.........................b..........................................

#histogram for mag with depth on the y-axis. 
hist(mag,freq = F)

#Adding a vertical line to indicate the mean of mag in red with line width 3. 
abline(v = mean(mag),col = "red",lwd=3)

# Giving the line an appropriate label using paste method.
lab <- paste("Mean of mag = ",mean(mag))
text(4.7,0.4,lab,srt = 90)


# R Assignment 4 : 09/15/2016
# Author : Venkata Gongati , vyg14001

#Creating a vector called x and place values 10 to 1000. 
x <- seq(10,1000)

#Creating a vector y that takes the square root of the log of numbers in x. 
y <- sqrt(log(x))

#Ploting x and y
plot(x,y)

#using expression & text methods to display the required expression on the graph
exp <- expression(alpha+beta^2+gamma^3)
text(100,1.6,exp)
