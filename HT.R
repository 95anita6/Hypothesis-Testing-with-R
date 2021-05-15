setwd("C:/Users/AnitaM/Desktop/R Language")

# Distributions functions in R

# d : given a number , it gives its probability

# p : cdf , given a number it gives prob of rv being smaller than that

# q : compliment to cdf , given a prob returns the number 

# r : generate random number from the distribution

n=100
p=0.1
# d : given a number , it gives its probability
dbinom(4,n,p)

# p : cdf , given a number it gives prob of rv being smaller than that
pbinom(10,n,p)

# q : compliment to cdf , given a prob returns the number 
qbinom(0.58,n,p)

# r : generate random number from the distribution
rbinom(4,n,p)

# exercise : simulate some data[ 10000 obs ] for a large clinical 
# trial where there were 1000 patients and probability of
# success was 20% and visualise distribution

x=rbinom(10000,1000,0.5)
df=data.frame(x=x)

library(ggplot2)
ggplot(df,aes(x=x))+geom_histogram(bins=100)
ggplot(df,aes(x=x))+geom_density()

# whats the probability that 200 of the patients will be cured
dbinom(200,1000,0.2)

# whats the probability that upto 200 patients will be cured
pbinom(210,1000,0.2)

# whats the probability that number of patients cured 
# will be between 190 and 210
pbinom(210,1000,0.2)-pbinom(190,1000,0.2)

# what is the probability that more than 200 patients will be cured
1-pbinom(200,1000,0.2)

# poisson distribution : dpois,ppois,rpois

# a dr's clinic recieves on an avg 20 patients per hr 
# they need to do special arrangements if that number exceeds 25 for any hr
# what are the chances that they'd need to go for special arrangements

# whats the parameter of poisson distribution : rate /unit time

# what will this give me : dpois(10,20) : probability of seeing 10 patients in an hr

# what will this give me : ppois(10,20) : probability of seeing upto 10 patients in an hr

# what will this give me : 1-ppois(10,20) : probability of seeing more than 10 pts in an hr

1-ppois(25,20)

# exponential dist:
# on an average it takes 3 minutes for taking a blood sample 
# whats the probability that it will take less than 2 minutes

pexp(2,1/3)

# what does a normal distribution look like

x=rnorm(10000,2,0.25)
d=data.frame(x=x)

ggplot(d,aes(x=x))+geom_histogram(bins=100)

## Hypothesis Testing:
## One sample tests:
## ------------------------------------------------------------------------

# EXAMPLE 1:

wq=read.csv("C:/Users/AnitaM/Downloads/winequality-white.csv",
            sep=";")
## ------------------------------------------------------------------------

# here the null hypothesis is that the average alcohol content in white wines is 10 units
# default alternate is of simple inequality

t.test(wq$alcohol,mu = 10)

## ------------------------------------------------------------------------
t.test(wq$alcohol,mu =10,alternative = 'greater')

## ------------------------------------------------------------------------
t.test(wq$alcohol,mu = 10,alternative ="less" )

## ------------------------------------------------------------------------
t.test(wq$alcohol,mu = 10.5,alternative ="greater" )


# EXAMPLE 2:

# exercise : make use of diabetic_data.csv
data=read.csv('C:/Users/AnitaM/Downloads/Data (1)/Data/diabetic_data.csv',stringsAsFactors = F)

# verify this claim : each patient goes through on an average 45 lab procedures: Null Hypothesis
t.test(data$num_lab_procedures,mu=45,alternative = 'less')

# in favour of null hypothesis here

# while hospitalise they recieve less 10 different medications:
# Null hypothesis : They receive 10 different medications
# Alternate hypothesis : They receive less than 10 different medications
t.test(data$num_medications, mu=10, alternative = 'less')

# here p-value > alpha; in favour of null hypothesis; i.e. the number of medications on avg is not less than 10

## 2 Sample test:
## ------------------------------------------------------------------------

# EXAMPLE 1:

library(sas7bdat)
d=read.sas7bdat("C:/Users/AnitaM/Downloads/Data (1)/Data/hsb2.sas7bdat")

# null : difference in mean is zero.
# according the p-value : null is true
t.test(d$read,d$write,paired = TRUE)

## for t-distribution find out which limits for the interval with 95% probability 
## with df 199

# Ans: 95 percent confidence interval:
# -1.7841424  0.6941424

### unpaierd test
read_f=d$read[d$female==1]
read_m=d$read[d$female==0]

# f-test:
var.test(read_f,read_m) # H_0 : ratio of variance = 1

t.test(read_f,read_m,paired = FALSE,var.equal = TRUE)

## using diabetic data comment if there is difference between 
# average weight between genders [ consider only those patients 
# where this data is available]
library(dplyr)
library(tidyr)
data = data.frame(data)

data = data %>% drop_na(weight)

weight_f = data$weight[data$gender=='Female']
weight_m = data$weight[data$gender=='Male']

# in this case null hypothesis is that the difference between the avg weigts of both genders is 0.
t.test(weight_f,weight_m, paired = FALSE, var.equal = TRUE)


## ----
fit = aov(alcohol ~ quality ,data=wq)
summary(fit)

## ------------------------------------------------------------------------
pairwise.t.test(wq$alcohol, wq$quality, p.adj = "bonf")


# does ethnicity has any impact on time in hosptial? 

### catagorical variable tests

table(d$race)

prop.table(table(d$race))

## ------------------------------------------------------------------------
chisq.test(table(d$race),p=c(0.2,0.1,0.1,0.6)) # Chi-squared test for given probabilities

## ------------------------------------------------------------------------
chisq.test(table(d$ses,d$female))  # Pearson's Chi-squared test

table(d$race,d$ses)
## ------------------------------------------------------------------------
fisher.test(table(d$race,d$ses))

# same type of tests on the diabetic data:
# Its a general assumption that among the patients where insulin
# level is checked [ insulin is not recorded as "No"]
# proportion of [Down,Steady , Up] is roughly [15%, 60%,25%]
# verify this

table(data$insulin)
chisq.test(table(data$insulin), p=c(0.15,0,0.6,0.25))
# acc to p-value : in favour of null hypothesis i.e. given propertions are true

prop.table(table(data$insulin))

# does insulin level has any impact on readmission?
chisq.test(table(data$readmitted,data$insulin))
# acc to p-value insulin levels do have an impact on readmission

## Dont ignore business context , and dont trust results of hypothesis tests blindly 

#-------------------------------------------------------------------------------------------------
# Other test:

x=runif(400)

hist(x)

shapiro.test(x)

## --------------------------------------------------------------
y=rbeta(6000,2,8)

shapiro.test(y)
library(nortest)
ad.test(y)

## ----
library(ggplot2)
df=data.frame(x,y)

ggplot(df,aes(x))+geom_density(color="red")+
  stat_function(fun=dnorm,args=list(mean=mean(df$x),sd=sd(df$x)),color="green")+
  ggtitle("Visual Normality Test for x ")

ggplot(df,aes(y))+geom_density(color="red")+
  stat_function(fun=dnorm,args=list(mean=mean(df$y),sd=sd(df$y)),color="green")+
  ggtitle("Visual Normality Test for y ")

## ------------------------------------------------------------------------
set.seed(1)
v1=rlnorm(20,0,0.4)
shapiro.test(v1)

df=data.frame(v1)
ggplot(df,aes(x=v1))+geom_density(color="red")+
  stat_function(fun=dnorm,args=list(mean=mean(v1),sd=sd(v1)),color="green")+
  ggtitle("Visual Normality Test for v1 ")


## ------------------------------------------------------------------------
set.seed(1)
v2 = rt(60000,29)
ad.test(v2)

df=data.frame(v2)
ggplot(df,aes(x=v2))+geom_density(color="red")+
  stat_function(fun=dnorm,args=list(mean=mean(v2),sd=sd(v2)),color="green")+
  ggtitle("Visual Normality Test for v2 ")
