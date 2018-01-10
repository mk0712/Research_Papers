############################

# Packages:
#install.packages("car")
#library(car)
#install.packages("lme4")
library(lme4)
#install.packages("simr")
library(simr)
#install.packages("lmerTest")
library(lmerTest)
#install.packages("RLRsim")
library(RLRsim)

# Revision 3:
install.packages("MuMIn")
library(MuMIn)

############################

# Load data:
data <- read.csv(".....csv", sep=";", dec=",")

############################

# Transform data types:
data$SI_RecChosen_binary <- as.logical(data$SI_RecChosen_binary)
data$Gender <- as.factor(data$Gender)
data$EmployeeKey <- as.factor(data$EmployeeKey )

############################

# Focus on correctly answered questions:
data <- data[data$SufficientMotivation_ieQuestionAnsweredCorrectly == "yes",]

# Create data_pos (--> dept, country) and data_prox (--> cohesion, hierarchy):
data_pos <- data
data_pos <- data_pos[data_pos$DeptLvl !="none",]
vectorWithColumns_pos <- c("EmployeeKey","SI_RecChosen_binary","elaboration","DeptLvl", "CountryLvl", "Age", "Gender", "Nationality", "Culture_FirstLanguage")
data_pos <- data_pos[vectorWithColumns_pos]
data_pos$DeptLvl <- factor(data_pos$DeptLvl)
data_pos$CountryLvl <- factor(data_pos$CountryLvl)

data_prox <- data
data_prox <- data_prox[data_prox$SocialCohesionHigh !="none",]
vectorWithColumns_prox <- c("EmployeeKey","SI_RecChosen_binary","elaboration","SocialCohesionHigh", "HierarchyLvl", "Age", "Gender", "Nationality", "Culture_FirstLanguage")
data_prox <- data_prox[vectorWithColumns_prox]
data_prox$SocialCohesionHigh <- factor(data_prox$SocialCohesionHigh)
data_prox$HierarchyLvl <- factor(data_prox$HierarchyLvl)

############################

# Interaction plots:

##### Note: interaction plots for elaboration are developed in Excel

# Position:
## Department * Elaboration:

## Country * Elaboration:

# Proximity:
## Cohesion * Hierarchy:
interaction.plot(data_prox$SocialCohesionHigh, data_prox$HierarchyLvl, data_prox$SI_RecChosen_binary, 
                 type="b", # type of the plot: lines or points or both
                 col=c(1:5), # color of the plot
                 leg.bty="o", # legend 
                 leg.bg="beige", # legend background color
                 lwd=2, # line width
                 pch=c(18,24,22, 19, 23),  # point character
                 ylab="Probability to chose recommendation", 
                 xlab="Proximity", 
                 main="Interaction Plot: Proximity * Hierarchy")


## Cohesion * Elaboration:


## Hierarchy * Elaboration:


############################

# Hypothesis Tests:

# Loglinear analysis using glm:
# http://ww2.coastal.edu/kingw/statistics/R-tutorials/loglin.html 

# Proximity:
# GLM with binomial variance and probit link
#proximity_glm_binomial = glm( data_prox$SI_RecChosen_binary ~ data_prox$SocialCohesionHigh + data_prox$HierarchyLvl + data_prox$SocialCohesionHigh * data_prox$HierarchyLvl + data_prox$SocialCohesionHigh * data_prox$elaboration + data_prox$HierarchyLvl * data_prox$elaboration + data_prox$SocialCohesionHigh * data_prox$HierarchyLvl * data_prox$elaboration, family=binomial(link=probit) )
#summary( proximity_glm_binomial )
#anova(proximity_glm_binomial, test="Chisq")

# Position:
# GLM with binomial variance and probit link
#position_glm_binomial = glm( data_pos$SI_RecChosen_binary ~ data_pos$DeptLvl + data_pos$CountryLvl + data_pos$DeptLvl * data_pos$elaboration + data_pos$CountryLvl * data_pos$elaboration, family=binomial(link=probit) )
#summary( position_glm_binomial )
#anova(position_glm_binomial, test="Chisq")





##### Model with lme4 (paper revision):
### No moderators (+):
proximity_glmer_binomial = glmer( data_prox$SI_RecChosen_binary ~ data_prox$SocialCohesionHigh + data_prox$HierarchyLvl + data_prox$elaboration + (1|data_prox$EmployeeKey), 
                                  family=binomial )
proximity_glmer_binomial = glmer( SI_RecChosen_binary ~ SocialCohesionHigh + HierarchyLvl + elaboration + (1|EmployeeKey), 
                                  family=binomial, data=data_prox)
proximity_glmer_binomial = glmer( SI_RecChosen_binary ~ HierarchyLvl + (1|EmployeeKey), 
                                  family=binomial, data=data_prox)
summary( proximity_glmer_binomial )
r.squaredGLMM( proximity_glmer_binomial )
#r.squaredLR( proximity_glmer_binomial )

# Standardized odds ratio:
fixef(proximity_glmer_binomial)
exp(fixef(proximity_glmer_binomial)) # =estimate
plot(ranef(proximity_glmer_binomial)) # =odds ratio
# Power analysis with simr:
fixef(proximity_glmer_binomial)
fixef(proximity_glmer_binomial)["data_prox$SocialCohesionHighyes"] <- 0.05
fixef(proximity_glmer_binomial)["data_prox$SocialCohesionHighyes"]
fixef(proximity_glmer_binomial)["HierarchyLvl_medium"] <- 0.05
fixef(proximity_glmer_binomial)["HierarchyLvl_medium"]
fixef(proximity_glmer_binomial)["HierarchyLvlhigh"] <- 0.05
fixef(proximity_glmer_binomial)["HierarchyLvlhigh"]
fixef(proximity_glmer_binomial)["data_prox$elaboration"] <- 0.05
fixef(proximity_glmer_binomial)["data_prox$elaboration"]
fixef(proximity_glmer_binomial)["data_prox$"] <- 0.05
fixef(proximity_glmer_binomial)["data_prox$"]
powerSim(proximity_glmer_binomial, sim=2)

#powerSim(proximity_glmer_binomial,test=fixed(xname="data_prox$HierarchyLvl",method="z"),nsim=1)
powerSim(proximity_glmer_binomial,test=fixed(xname="HierarchyLvl",method="z"),nsim=1)
lastResult()$errors

## Only for testing:
model1 = glmer( z ~ x + (1|g), family="poisson", data=simdata )
summary(model1)
fixef(model1)["x"] <- -0.05
powerSim(model1, nsim=20) # default of nsim is 1000
powerSim(model1, test=fixed(xname="x",method="z"),nsim=20) # default of nsim is 1000
pc1  <- powerCurve(model1)
plot(pc1)
##

#### With moderators (social cohesion, hierarchy, elaboration):
# Elaboration (detailed, float):
proximity_glmer_binomial_moderator = glmer( data_prox$SI_RecChosen_binary ~ 
                                            data_prox$SocialCohesionHigh * data_prox$HierarchyLvl * data_prox$elaboration + 
                                            (1|data_prox$EmployeeKey), 
                                          family=binomial )
# Elaboration (interval):
#proximity_glmer_binomial_moderator = glmer( data_prox$SI_RecChosen_binary ~ 
 #                                             data_prox$SocialCohesionHigh * data_prox$HierarchyLvl * data_prox$elaboration_interval + 
  #                                            (1|data_prox$EmployeeKey), 
   #                                         family=binomial )

# Log odds:
summary( proximity_glmer_binomial_moderator )
r.squaredGLMM( proximity_glmer_binomial_moderator )

# Standardized odds ratio:
fixef(proximity_glmer_binomial_moderator)
exp(fixef(proximity_glmer_binomial_moderator))
summary(proximity_glmer_binomial_moderator)$varcor

#ranef(proximity_glmer_binomial_moderator)
plot(ranef(proximity_glmer_binomial_moderator))

res_glmer = residuals(proximity_glmer_binomial_moderator)
plot(res_glmer)
qqnorm(res_glmer)
qqline(res_glmer)
plot(proximity_glmer_binomial_moderator)

#### No moderators (Dept, country):
pos_glmer_binomial = glmer( data_pos$SI_RecChosen_binary ~ 
                                        data_pos$DeptLvl + data_pos$CountryLvl + data_pos$elaboration 
                                      + (1|data_pos$EmployeeKey), family=binomial )
summary( pos_glmer_binomial )
exp(fixef(pos_glmer_binomial))
r.squaredGLMM( pos_glmer_binomial )

#### With moderators (Dept, Country, elaboration):
pos_glmer_binomial_moderator = glmer( data_pos$SI_RecChosen_binary ~ 
                                        data_pos$DeptLvl * data_pos$CountryLvl * data_pos$elaboration 
                                      + (1|data_pos$EmployeeKey), family=binomial )
summary( pos_glmer_binomial_moderator )
exp(fixef(pos_glmer_binomial_moderator))
r.squaredGLMM( pos_glmer_binomial_moderator )

pos_glmer_binomial_moderator_2a = glmer( data_pos$SI_RecChosen_binary ~ 
                                           data_pos$DeptLvl * data_pos$elaboration 
                                         + (1|data_pos$EmployeeKey), family=binomial )
summary( pos_glmer_binomial_moderator_2a )
exp(fixef(pos_glmer_binomial_moderator_2a))


pos_glmer_binomial_moderator_2b = glmer( data_pos$SI_RecChosen_binary ~ 
                                           data_pos$CountryLvl * data_pos$elaboration 
                                         + (1|data_pos$EmployeeKey), family=binomial )
summary( pos_glmer_binomial_moderator_2b )
exp(fixef(pos_glmer_binomial_moderator_2b))

############################

# Interaction plot: Cohesion * Hierarchy * Elaboration: 
# Problem: I need to plot 4 dimensions: IVs (cohesion, hierarchy, elaboration) + DVs (SI_Rec_chosen)

# Transform factor to numeric:
data_prox_2 <- data
data_prox_2 <- data_prox_2[data_prox_2$SocialCohesionHigh !="none",]
vectorWithColumns_prox_2 <- c("SI_RecChosen_binary","elaboration","SocialCohesionHigh_num", "HierarchyLvl_num" )
data_prox_2 <- data_prox_2[vectorWithColumns_prox_2]

# Show plot:
persp(data_prox_2$elaboration, data_prox_2$HierarchyLvl_num, data_prox_2$SI_RecChosen_binary, theta = 30, phi = 30, expand = 0.5, col = "lightblue")

############################

# Control model:

# Proximity (Social Cohesion) and Power with GLMER:
control_proximity_glmer_binomial = glmer( data_prox$SI_RecChosen_binary 
                                          ~ data_prox$SocialCohesionHigh * data_prox$HierarchyLvl * data_prox$elaboration
                                          + data_prox$Age + data_prox$Gender + data_prox$Nationality + data_prox$Culture_FirstLanguage
                                          + (1|data_prox$EmployeeKey),
                                          family=binomial(link=probit) )
summary( control_proximity_glmer_binomial )
exp(fixef(control_proximity_glmer_binomial))

# Proximity (Social Cohesion) and Power with GLM:
control_proximity_glm_binomial = glm( data_prox$SI_RecChosen_binary ~ data_prox$SocialCohesionHigh + data_prox$HierarchyLvl + data_prox$SocialCohesionHigh * data_prox$HierarchyLvl + data_prox$SocialCohesionHigh * data_prox$elaboration + data_prox$HierarchyLvl * data_prox$elaboration + data_prox$Age + data_prox$Gender + data_prox$Nationality + data_prox$Culture_FirstLanguage + data_prox$SocialCohesionHigh * data_prox$HierarchyLvl * data_prox$elaboration, family=binomial(link=probit) )
summary( control_proximity_glm_binomial )
anova(control_proximity_glm_binomial, test="Chisq")


# Position (Institutional Isomorphism) with GLMER:
control_position_glmer_binomial = glmer( data_pos$SI_RecChosen_binary 
                                          ~ data_pos$DeptLvl * data_pos$elaboration * data_pos$CountryLvl
                                          + data_pos$Age + data_pos$Gender + data_pos$Nationality + data_pos$Culture_FirstLanguage
                                          + (1|data_pos$EmployeeKey),
                                          family=binomial(link=probit) )
summary( control_position_glmer_binomial )
exp(fixef(control_position_glmer_binomial))

# Position (Institutional Isomorphism) with GLM:
control_position_glm_binomial = glm( data_pos$SI_RecChosen_binary ~ data_pos$DeptLvl + data_pos$CountryLvl + data_pos$DeptLvl * data_pos$elaboration + data_pos$CountryLvl * data_pos$elaboration + data_pos$Age + data_pos$Gender + data_pos$Nationality + data_pos$Culture_FirstLanguage, family=binomial(link=probit) )
summary( control_position_glm_binomial )
anova(control_position_glm_binomial, test="Chisq")


############################
