###Reading the dataset ####
test <- read.csv("//Users//sky//Downloads//data.csv")

library(sqldf)
x = sqldf("Select score from test where name ='Arielle'")
y = sqldf("Select score from test where name ='Boris'")
x_r = sqldf("Select score from test where name ='Arielle' and hand ='R'")
y_r = sqldf("Select score from test where name ='Boris' and hand ='R'")
x_l = sqldf("Select score from test where name ='Arielle' and hand ='L'")
y_l = sqldf("Select score from test where name ='Boris' and hand ='L'")

#NULL Hypothesis - Difference between the means of both samples is 0 i.e. both Boris and Arielle have same level of performance
#Alternate Hypothesis - Difference between the means of both samples is not equal to 0 i.e. Arielle and Boris have difference in performance
#----- Hypothesis Testing ---------#

#------------------- Test -1 -----------------------#
#Test to see if there is a significant difference between performance of Arielle and Boris based on their overall performance ##
## Checking data distribution
qqnorm(y$score)
qqline(y$score,col="red")
#Checking if variance between the samples are similar##
var.test(x$score, y$score)
### t-test to check for difference between means of Arielle's and Boris's performance and see if the difference is significant ##
t.test(x,y,var.equal = T)
#Results show that p-value is >0.05 i.e. we fail to reject the null hypothesis that both Boris and Arielle perform at the same level


#-------------------------- Test-2 ---------------------------# 
#Test to see if there is a significant difference between performance of Arielle and Boris-- 
# when Arielle is using his right hand vs Boris's left #
#Checking if variance between the samples are homogenous using Fisher's f-test##
var.test(x_r$score, y_l$score)
### t-test to check for difference between means of Arielle's and Boris's performance and see if the difference is significant ##
t.test(x_r,y_l,var.equal = T)
#testing power of t-test ####
sd(x_r$score)
sd(y_l$score)
power.t.test(n=35,delta=2,sig.level = 0.05,sd=2.17)
# Results show that p < 0.05 and hence the null hypothesis can be rejected i.e. Arielle and Boris have significant difference in 
#performance when Arielle uses right and Boris uses left hand with Arielle having the better performance

#--------------------------- Test- 3 ------------------#
#Test to see if there is a significant difference between performance of Arielle and Boris-- 
# when Arielle is using his left hand vs Boris's right #
#Checking if variance between the samples are homogenous using Fisher's f-test##
var.test(x_l$score, y_r$score)
### t-test to check for difference between means of Arielle's and Boris's performance and see if the difference is significant ##
t.test(x_l,y_r,var.equal = T)
#testing power of t-test ####
sd(x_l$score)
sd(y_r$score)
power.t.test(n=15,delta=4.46667,sig.level = 0.05,sd=2.04)
# Results show that p < 0.05 and hence the null hypothesis can be rejected i.e. Arielle and Boris have significant difference in 
#performance when Arielle uses left and Boris uses right hand with Boris having the better performance


#---------------------------- Test 4 -----------------------#
#Test to see if there is a significant difference between performance of Arielle and Boris-- 
# when both Arielle and Boris are using their right hands  #
#Checking if variance between the samples are homogenous using Fisher's f-test##
var.test(x_r$score, y_r$score)
### t-test to check for difference between means of Arielle's and Boris's performance and see if the difference is significant ##
t.test(x_r,y_r,var.equal = T)
#testing power of t-test ####
sd(x_r$score)
sd(y_r$score)
pwr.t2n.test(n1 = 35 , n2= 15 , d = 0.3 , sig.level =0.05, power = NULL)
# Results show that p < 0.05 i.e. there is evidence to reject the null hypothesis however power of the
#t-test is much less than 80% suggesting the presence of 


#---------------------------- Test 5 -----------------------#
#Test to see if there is a significant difference between performance of Arielle and Boris-- 
# when both Arielle and Boris are using their left hands  #
#Checking if variance between the samples are homogenous using Fisher's f-test##
var.test(x_l$score, y_l$score)
### t-test to check for difference between means of Arielle's and Boris's performance and see if the difference is significant ##
t.test(x_l,y_l,var.equal = T)
#testing power of t-test ####
sd(x_l$score)
sd(y_l$score)
pwr.t2n.test(n1 = 15 , n2= 35 , d = 0.2 , sig.level =0.05, power = NULL)
# Results show that p > 0.05 i.e. we fail to reject the null hypothesis