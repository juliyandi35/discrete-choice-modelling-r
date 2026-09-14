### Estimating Discrete Choice Model in R ###
library(dplyr)
library(AER)
library(mlogit)

data("TravelMode", package = "AER") #load the data
head(TravelMode) #see the head of the data

nrow(TravelMode) #number of rows
length(unique(TravelMode$individual)) #how many individuals?
table(is.na(TravelMode)) #is there any missing data
table(filter(TravelMode, choice=="yes")$mode)/(nrow(TravelMode)/4) #average frequency of choices

## Discrete Choice Model
# Likelihood Function: Example
theta = c(1,2,3,-0.001,-0.003,-0.005) #generate one set of parameters
sample<-filter(TravelMode,individual==1)  #filter one individual
sample$constant<-0
sample$constant[sample$mode=="air"] = theta[1]
sample$constant[sample$mode=="train"] = theta[2]
sample$constant[sample$mode=="bus"] = theta[3]
sample$utility = theta[4]*sample$gcost+theta[5]*sample$wait+theta[6]*sample$travel+sample$constant 
sample

real_choice<-filter(sample,choice=="yes")
real_choice
real_choice$utility
exp(sample$utility)
sum(exp(sample$utility))
choice_prob=exp(real_choice$utility)/sum(exp(sample$utility))
choice_prob

# Choice Probability Function for each individual
# Suppose we have the utility for each alternative
choice.prob<-function(sample){
  x = filter(sample,choice=="yes") #filter the choice 
  prob = exp(x$utility)/sum(exp(sample$utility)) #caculate probability
  return(prob)
}

group = group_by(TravelMode,individual) #group the data by individual
summarise(group,avg_travel = mean(travel)) # calculate the avg travel time for each individual

TravelMode$utility<-runif(nrow(TravelMode),0,1) #generate some random utilities
Probability <- TravelMode  %>% group_by(individual)  %>% do(data.frame( prob= choice.prob(.))) #apply choice.prob() function on each individuals
Probability # a data.frame, each row contains the choice probability for each individual
sum(log(Probability$prob)) #sum up the log likelihoods

# Likelihood function for the whole data
Likelihood<-function(theta){
  # caclulating utility for each alternative for all the individuals
  TravelMode$constant=0
  TravelMode$constant[TravelMode$mode=="air"] = theta[1]
  TravelMode$constant[TravelMode$mode=="train"] = theta[2]
  TravelMode$constant[TravelMode$mode=="bus"] = theta[3]
  TravelMode$utility = theta[4]*TravelMode$gcost+theta[5]*TravelMode$wait
  +theta[6]*TravelMode$travel+TravelMode$constant 
  
  # caclulating choice probability for each individual
  Probability=TravelMode %>% 
    group_by(individual) %>% # group the data by individual
    do(data.frame(prob=choice.prob(.)))  # use Choice() function on each individual
  return(-sum(log(Probability$prob))) # return the sum log likelihood 
}

# Maximum Likelihood Estimation
est<-optim(c(4.0,4.0,4.0,-0.01,-0.01,-0.01),Likelihood,method = "BFGS") 
est$par

# Using mlogit Package
# formatting data
TM <- mlogit.data(TravelMode, choice = "choice", shape = "long", 
                  chid.var = "individual", alt.var = "mode", drop.index = TRUE)
head(TM)

ml.TM <- mlogit(choice ~ gcost +wait +travel, TM, reflevel = "car")
summary(ml.TM)

#how fitted choice probability match with data
apply(fitted(ml.TM, outcome=FALSE), 2, mean) # fitted mean choice probability
ml.TM$freq/sum( ml.TM$freq) # mean choice probability in data
