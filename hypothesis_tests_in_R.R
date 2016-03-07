#setting the directory
getwd()
#enter the direction of your own folder where you are going to keep your research documents - file management is important
setwd("D:/Users/Karol/Desktop/R")
#opening the file in R
world<-read.table("world.csv", header=TRUE, sep=",")
#displaying the structure of the data frame
str(world)
#attaching the file to the memory of R
attach(world)

#Shapiro test shows whether the variables have a normal distribution
shapiro.test(lifeexpf)
shapiro.test(lifeexpm)
shapiro.test(birth_rt)
shapiro.test(death_rt)
shapiro.test(world[,2])
shapiro.test(world[,3])
shapiro.test(world[,5])
#We are alomst 100% sure that NONE of the variables follows a normal distribution.

#Chi-squared test shows whether two variables are independent
chisq.test(region,death_rt)
#We are 99,96% sure that Ho is wrong and that the two variables are NOT independent. Hence, the death rate depends upon the region.

#Conducting t-tests in R.
t.test(death_rt)
t.test(lifeexpf, lifeexpm)
t.test(lifeexpf, lifeexpm, var.equal=T)
t.test(lifeexpf, lifeexpm, paired=T)

#A function for creating random examples of t-tests
ttest.for.examination <- function(x,y,z,k)
{
  subjects <- x
  mean1 <- y
  mean2 <- z
  standarddev <- k
  print( c("Number of measurements:   ", x))
  print( c("Mean of group 1: ", y))
  print( c( "Mean of group 2: ",z))
  print( c("Standard deviation:  ", k))
  group1 <- round(rnorm(x, y, k))
  group2 <- round(rnorm(x, z, k))
  framedata <- cbind(group1, group2)
  print(framedata)
  print( list (t.test(group1, group2, var.equal = T), t.test(group1, group2, var.equal = T, paired = T)))
}
#Now you can run the new function.
ttest.for.examination(13,90,105,10)
ttest.for.examination(13,92,100,10)
#Try running the same arguments twice
ttest.for.examination(13,92,100,10)

#Visualising Hypothesis Tests in R
#The red distribution is what you can expect to see if you plot repeated samples when the null hypothesis is true.
#You can recognize the Ho because it sounds like: "there was no difference", for instance: "The intervention did not affect the tumor marker."

#one tailed test   
#rare in health sciences, more common in industrial process control
x=seq(50,140,length=200)
y1=dnorm(x,80, 10)
plot(x,y1,type='l',lwd=2,col='red')
y2=dnorm(x,110, 10)
lines(x,y2,type='l',lwd=2,col='blue')
abline(v=qnorm(0.95,80,10)) 

#two tailed test is common in the health sciences, because in most cases, both an increase or a decrease in a variable would affect health
x=seq(50,140,length=200)
y1=dnorm(x,80, 10)
plot(x,y1,type='l',lwd=2,col='red')
y2=dnorm(x,110, 10)
lines(x,y2,type='l',lwd=2,col='blue')
abline(v=qnorm(0.025,80,10))
abline(v=qnorm(0.975,80,10))

#colour the rejection area, also referred to as alpha.
#If the p-value is equal to or lower than alpha - reject the null hypothesis
x=seq(50,140,length=200)
y1=dnorm(x,80, 10)
plot(x,y1,type='l',lwd=2,col='red')
y2=dnorm(x,110, 10)
lines(x,y2,type='l',lwd=2,col='blue')
cord.x1 <- c((round(qnorm(0.975, 80, 10))),seq((round(qnorm(0.975, 80, 10))), 120,1),120) 
cord.y1 <- c(0,dnorm(seq((round(qnorm(0.975, 80, 10))), 120, 1), 80, 10),0) 
polygon(cord.x1,cord.y1,col='red')
cord.x2 <- c(50,seq(50,round(qnorm(0.025, 80, 10),1)),round(qnorm(0.025, 80, 10))) 
cord.y2 <- c(0,dnorm(seq(50,round(qnorm(0.025, 80, 10),1)), 80, 10),0) 
polygon(cord.x2,cord.y2,col='red')

#Imagine that the alternative hypothesis were true. 
#Beta is the risk that you will keep (=not reject) the false null hypothesis
x=seq(50,140,length=200)
y1=dnorm(x,80, 10)
plot(x,y1,type='l',lwd=2,col='red')
y2=dnorm(x,110, 10)
lines(x,y2,type='l',lwd=2,col='blue')
cord.x2<- c(0,seq((round(1-qnorm(0.025,110,10))),100,1),100)
cord.y2 <- c(0,dnorm(seq((round(1-qnorm(0.025, 110, 10))), 100, 1), 110, 10),0) 
polygon(cord.x2,cord.y2,col='red')
abline(v=round(qnorm(0.975, 80, 10, lower.tail=T)))
abline(v=round(qnorm(0.025, 80, 10, lower.tail=T)))
text(95,0.005, "ß ",xpd=5)

#Statistical power, 1-beta, is the probability to reject a false null hypothesis
x<- seq(50,140,length=200)
y1<- dnorm(x,80, 10)
plot(x,y1,type='l',lwd=2,col='red')
y2<- dnorm(x,110, 10)
lines(x,y2,type='l',lwd=2,col='blue')
cord.x2<- c(0,seq((round(1-qnorm(0.025,110,10))),100,1),100)
cord.y2 <- c(0,dnorm(seq((round(1-qnorm(0.025, 110, 10))), 100, 1), 110, 10),0) 
polygon(cord.x2,cord.y2,col='red')
abline(v=round(qnorm(0.975, 80, 10, lower.tail=T)))
abline(v=round(qnorm(0.025, 80, 10, lower.tail=T)))
cord.x1 <- c(100,seq(round(qnorm(0.975, 80, 10, lower.tail=T)), 140,1),140) 
cord.y1 <- c(0,dnorm(seq(round(qnorm(0.975, 80, 10, lower.tail=T)),140, 1), 110, 10),0) 
polygon(cord.x1,cord.y1,col='6')
text(95,0.005, "ß ",xpd=5)
text(115,0.005, "1-ß ",xpd=5)

#non-parametric tests in R
dengue.fewer <-c(3000,3200,3500,5068,5679,6200,6300,7020)
scrub.typhus<-c(4400,4500,5900,6839,7561,9047,12300,14000)

wilcox.test(dengue.fewer,scrub.typhus)
t.test(dengue.fewer,scrub.typhus,var.equal=T)


ttest.wilcox.examples <- function(x,y,z,k)
{
  subjects <- x
  mean1 <- y
  mean2 <- z
  standarddev <- k
  print( c("Number of measurements:   ", x))
  print( c("Mean of group 1: ", y))
  print( c( "Mean of group 2: ",z))
  print( c("Standard deviation:  ", k))
  group1 <- rnorm(x, y, k)
  group2 <- rnorm(x, z, k)
  framedata <- cbind(group1, group2)
  print(framedata)
  print( list (t.test(group1, group2, var.equal = T), wilcox.test(group1, group2)))
}

set.seed(570)
ttest.wilcox.examples(13,90,100,10)


#try

A <- c(91,91,93,106,97,108,97,105,106,103,105,96,105,95,90,101)
B <- c(90,89,85,99,93,104,89,103,102,95,103,95,105,87,86,101)
t.test(A,B,paired=T)
wilcox.test(A,B, paired=T)