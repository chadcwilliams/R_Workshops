###############################################################################
## Written by Chad C Williams, 2021                                          ##
## www.chadcwilliams.com                                                     ##
###############################################################################
#### (0) Setup Environment                                                  ####
set.seed(123)

#### (1) Multiple Regression                                                ####
#Create Multiple Regression Data
regression_data = data.frame( #Create data frame
  Age = round(runif(100,10,100)), #Create age variable ranging from 10 to 100
  Education = round(runif(100,1,5)) #Create education variable ranging from 1 to 5 years
)
regression_data$IQ = (((rnorm(100,regression_data$Age,20))/5)+80)+(((10-regression_data$Education)/10)*regression_data$Education)+(regression_data$Education*2) #Create IQ variable that has main effects of Age and Education

####  (i) Plot Regression Data                                              ####
library(ggplot2)
regression_plot = ggplot(aes(x=Age,y=IQ,color=Education,group=Education),data=regression_data)+
  geom_point(alpha = .5)+
  geom_smooth(method=lm,size=1,se=FALSE)+
  theme_classic()
regression_plot

####  (ii) Run Multiple Regression                                          ####
regression_output = lm(IQ~Age*Education,data=regression_data)
summary(regression_output)

#### (2) Moderation Regression                                              ####

####  (i) Create Moderation Data                                            ####
moderation_data = data.frame(
  hours_sleep = abs(rnorm(100, 6, 4)), #IV
  added_variance = abs(rnorm(100, 60, 30)), #Noise variable
  coffee = rnorm(100, 30, 8) #Moderator
)
moderation_data$attention = abs((-0.8*moderation_data$hours_sleep) * (0.2*moderation_data$coffee) - 0.5*moderation_data$hours_sleep - 0.4*moderation_data$added_variance + 10 + rnorm(100, 0, 3)) #DV; Attention Paid
moderation_data$hours_sleep_centered = moderation_data$hours_sleep-mean(moderation_data$hours_sleep) #Center IV
moderation_data$coffee_centered = moderation_data$coffee-mean(moderation_data$coffee) #Center Moderator

####  (ii) Run Moderation Regression                                        ####
moderation_regression = lm(attention~hours_sleep_centered*coffee_centered,data=moderation_data)
summary(moderation_regression)

####  (iii) Plot Moderation Regression                                      ####
library(rockchalk)
plotSlopes(moderation_regression,plotx='hours_sleep_centered',modx='coffee_centered',modxVals='std.dev')

#### (3) Mediation Regression                                               ####
####  (i) Create Mediation Data                                             ####
mediation_data = data.frame( #Create data frame
  hours_dawn = rnorm(100, 175, 7)) #Create hours until dawn variable
mediation_data$coffee = 0.7*mediation_data$hours_dawn + rnorm(100, 0, 5) #Create mediating coffee variable
mediation_data$wakefulness = 0.4*mediation_data$coffee + rnorm(100, 0, 5) #Create wakefullness variable

####  (ii) Run Mediation Regression                                         ####
library(mediation)
mediation_PathA = lm(coffee~hours_dawn,mediation_data) #Path A
mediation_PathCPrime = lm(wakefulness~hours_dawn+coffee,mediation_data) #Path C'

mediation_output = mediate(mediation_PathA,mediation_PathCPrime,treat='hours_dawn',mediator='coffee',boot=TRUE)
summary(mediation_output) #ACME is the mediation results (should be sig), ADE is C Prime results (should not be sig)


#### (Appendix A) Package to Check Assumptions                              ####

#library(gvlma)
#gvlma(mediation_PathA)
#gvlma(mediation_PathCPrime)

#### (Appendix B) Reference                                                 ####
#The moderation and mediation code was taken from:
#https://ademos.people.uic.edu/Chapter14.html