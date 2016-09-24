# A few Vizulizations using ggplot2 and plotly : 08/31/2016
# Author : Venkata Gongati , vyg14001

# clearing the environment variables
rm(list = ls())

# Installing MASS package
ininstall.packages("MASS")

# Loading MASS package
library(MASS)

#attaching survey data
attach(survey)

# looking at the data
summary(survey)
View(survey)

# knowing more about the data
?survey


#################################......1st Plot.........##############################################

### Bubble plot showing Writing hand span and Non Writting hand span for females and males
# here Size of the bubble is age , 
# color marking is for sex and 
# Writing hand span and non writing hand span are on x and y axis respectively

symbols(Wr.Hnd,NW.Hnd,circles=Age,inches=0.5, fg="white", bg= ifelse(Sex == "Male","blue","pink"),
        xlab="Writing Hand Span", ylab="Non Writting Hand Span")

#to add legend for sex
legend(
  "bottomright", 
  legend=c("Male", "Female"), 
  pch = 21,
  bty = "n",
  col = "black",
  pt.cex = 4,
  pt.bg = ifelse(Sex == "Male","pink","blue")         
)     


#Alternative using plot_ly method from plotly package

install.packages("plotly")
library(plotly)
plot_ly(data = survey[complete.cases(survey),] ,x = NW.Hnd, y = Wr.Hnd,
        mode = "markers", size = Age,color = Sex)


# from the above plot the ket take aways are ,
# 1 . Writing hand span and non hand writing span are postively correlated.
# 2 . Hand span for males is more than that of females
# 3 . Age doesn't seem to make any difference to the hand span. This is may be due to the fact that majority of people are between 15 to 20 



#################################......2nd Plot.........##############################################

# lets try to plot box plots of pulse by peoples smoking behaviour

# for that we can use qplot from ggplot2

#installing and loadung ggplot2
install.packages("ggplot2")
library(ggplot2)

#to change the default size of the font in qplot using theme_set() method 
theme_set(theme_gray(base_size = 18))

#plotting using qplot function
qplot(Smoke,Pulse,data = survey[complete.cases(survey$Smoke),],geom=c("boxplot","jitter"),main="Pulse rate by Student Smoking Habbit",fill = Smoke,
      xlab="how much the student smoke", ylab="pulse rate of student (beats per minute).")

# we can see clearly that most of the data is of people who never smoke.
# From the plot there doesn't seem to be much difference in pusle of students because of their smoking habbit

#################################......3rd Plot.........##############################################

# Similar plot to the above, just we will keep studen excerise habbit on x axis and color of the box for smoking habbits

qplot(Exer,Pulse,data = survey[complete.cases(survey$Smoke),],geom=c("boxplot","jitter"),main="Pulse rate by Student Smoking Habbit & Excerise",fill = Smoke,
      xlab="how much the student excerise", ylab="pulse rate of student (beats per minute).")

# there doesn't seem to be a definite trend in the pusle of the students with smoking habbits and excerise habbits.


#################################......4th Plot.........##############################################

#Regression line of Writting hand and non writting hand span for female and male 

qplot(NW.Hnd, Wr.Hnd, geom=c("point", "smooth"), col = Sex ,
      main="Regression of Writting Hand span and Non Writting hand span", 
      xlab="Non Writting Hand Span", ylab="Writing Hand Span")


# From the above plots we can say that both female and male have similar regression line, 
# so irrespective of weather a student is male or female regression equation is about the same 