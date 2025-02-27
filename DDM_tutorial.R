# Drift Diffussion model tutorial
# generating data, calculating cost, recovering paramaters and fitting your own data

#------------------------------------------------------------------------------#
## Initialization ## 

# first we set the working directory and clear any existing variables
#install.packages("rstudioapi")
setwd(dirname(rstudioapi::getSourceEditorContext()$path))
rm(list=ls())

# now we install some libraries we will need (uncomment the ones you don't have yet)
# install.packages("RcppZiggurat")
# install.packages("Rcpp", type = "source")
# install.packages("DEoptim")
# install.packages("ggplot2")
library(ggplot2)
library(Rcpp)
library(DEoptim)

# here we source the drift diffusion function as well as the cost function
sourceCpp("DDM_3params.cpp")
source("DDM_fit_functions_tutorial.R")

#------------------------------------------------------------------------------#
## Defining the value of the parameters to use in the model ##

## the parameters of the DDM
# The DDM we are fitting here has 3 parameters: the drift rate (v), boundary (a)
# and the non-decision time (ter).
# the drift rate determines the speed at which information is accumulated, the 
# boundary how much accumulation is necessary to make a decision, and the 
# non-decision time how much time passes for processing unrelated to the decision
# (e.g. visual processing, motor preparation).

## define DDM parameters
v<-0.8 #drift rate
a<-0.75 #bound
ter<-0.4 #non-decision time (seconds)

#------------------------------------------------------------------------------#
## Create data using these parameters ## 

## have a look at the DDM_3params.cpp function to see how these parameters are 
# used to create DVs, choice accuracy and reaction times
# on line 54, you can see the formula used for evidence accumulation:
# evidence = evidence + v * dt + s * sqrt(dt) * zigg.norm()

# evidence is being added to the DV at every timestep (dt) with magnitude v. 
# Some noise is also added with magnitude s (fixed at 1).

## Let's now use the .cpp function to generate some data
Gen_Data<-DDM_3params(v,a,ter)
colnames(Gen_Data$Data) <- c("RT", "accuracy")

# replace zeros in DV and time with NA
Gen_Data$DVs[Gen_Data$Dvs == 0] <- NA
Gen_Data$time[Gen_Data$time == 0] <- NA

# replace first zero, which is actually time
Gen_Data$time[ ,1] = 0

# have a look at the content of Gen_Data
str(Gen_Data)

#------------------------------------------------------------------------------#
## plot a couple of DVs ##

# now we would like to plot a couple of decision variables to see how a choice 
# is being made on a single trial

# set this to the trials you would like to plot, note that we only keep the DVs 
# of the first 21 trials to save memory.
trials_toplot = c(3,4,7,15,18)

# plot those trials and the boundary values
plot(t(Gen_Data$time[trials_toplot, ]), t(Gen_Data$DVs[trials_toplot, ]), type ="l", 
     ylim = c(-a-0.1,a+0.1), xlab = 'Time (s)', ylab = 'Decision variable')
abline(h = a, col = "red")
abline(h = -a, col = "red")

# Q1 why does the DV start with a flat line before accumulation?

# Q2 what does each DV have in common? When do they stop accumulating?

# Q3 can you deduce for how many of these trials the choice was correct?

# Q4 what was the (approximate) reaction time for each of the trials you plotted?

#------------------------------------------------------------------------------#
## plot behavioural results ##

# when plotting the DVs, we can see a direct representation of the parameters on 
# the decison being made on a single trial. But how do the parameters affect 
# overall behaviour, such as accuracy or reaction time?

# we are usually interested in the distribution of reaction times for the 
# correct and incorrect choices. We use these distributions as a way to fit our 
# predicted behaviour given some set of parameters to actual participant behaviour.

# let's look at the reaction time distributions of correct and incorrect choices.

# create a dataframe with our "reaction times" and "choice accuracy"
D <- data.frame(Gen_Data$Data)
D$accuracy <- as.factor(D$accuracy)

# plot histogram in red and green for correct and incorrect reaction times
correct_fill_color <- adjustcolor("#2A9D8F", alpha.f = 0.25)
error_fill_color <- adjustcolor("#E76F51", alpha.f = 0.25)
tempC <- hist(D$RT[D$accuracy == 1],
              prob = F, col = correct_fill_color,
              breaks = 50,
              xlab="Reaction time", ylab="Occurence",
              border = "white", main = "")
tempE <- hist(D$RT[D$accuracy == 0], , add=TRUE,
              prob = F, col = error_fill_color,
              breaks = 50,
              border = "white", main = "")

legend("topright",fill=c("white","white","#2A9D8F","#E76F51"),border=F,
       legend=c("Correct trials","Incorrect trials"),
       col=c("#2A9D8F","#E76F51"),bty='n',lwd=c(1,1,-1,-1))

## Q5 what do you notice about the number of trials that are correct and incorrect?

## Q6 what do you notice about the reaction times on correct and incorrect trials?


#------------------------------------------------------------------------------#
## playing around with the parameters ##

# now try and change some parameter values and consider their effect on the DVs 
# and behavioural results.
# you can adapt the parameter values and then run the block of code creating 
# trials and plotting the DVs and behavioural results

# usually in experiments, we find parameter values in this range, so it's best 
# to stay within them when changing the parameters:
# drift rate: 0 - 2
# boundary: 0.25 - 4
# non-decision time: 0 - 1

# change here
v<-0.8 #drift rate
a<-0.75 #bound
ter<-0.4 #non-decision time

# run here
Try_Data<-DDM_3params(v,a,ter)
Try_Data$DVs[Try_Data$Dvs == 0] <- NA
Try_Data$time[Try_Data$time == 0] <- NA
Try_Data$time[ ,1] = 0
colnames(Try_Data$Data) <- c("RT", "accuracy")

par(mfrow = c(1,2))
trials_toplot = c(1,2,3,4,7,11,21)
plot(t(Try_Data$time[trials_toplot, ]), t(Try_Data$DVs[trials_toplot, ]), type ="l",
ylim = c(-a-0.1,a+0.1), xlab = 'Time (s)', ylab = 'Decision variable')
abline(h = a, col = "red")
abline(h = -a, col = "red")

D <- data.frame(Try_Data$Data)
D$accuracy <- as.factor(D$accuracy)
hist(D$RT[D$accuracy == 1],
              prob = F, col = correct_fill_color,
              breaks = 50,
              xlab="Reaction time", ylab="Occurence",
              border = "white", main = "")
hist(D$RT[D$accuracy == 0], , add=TRUE,
              prob = F, col = error_fill_color,
              breaks = 50,
              border = "white", main = "")

legend("topright",fill=c("white","white","#2A9D8F","#E76F51"),border=F,
       legend=c("Correct trials","Incorrect trials"),
       col=c("#2A9D8F","#E76F51"),bty='n',lwd=c(1,1,-1,-1))

par(mfrow = c(1,1))

## Q7 what happens to the DVs when you make the boundary higher?

## Q8 how does that affect behaviour (reaction times and accuracy)?

## Q9 what happens when you increase the drift rate while keeping the high boundary?

## Q10 what happens if you decrease the non-decision time? How does that affect behaviour?

## Q11 what happens if you choose a parameter value that's outside of the "normal
# range"?


#------------------------------------------------------------------------------#
## if we'd like to compare data we predicted based on a set of parameters to 
# data we collected from participants, we need a way to quantify the difference 
# between the distributions from generated and collected data.

## have a look at the function "Cost_ddm" inside DDM_fit_functions_tutorial.R
# and answer the questions.

# this script computes 5 quantiles (containing a proportion of 0.1, 0.3, 0.5, 
# 0.7 and 0.9 of reaction times) in the observed data and  then computes the 
# probability of both a correct and incorrect choice within these quantiles.
# The cost (or chi square) is then computed on line 45:

# (obs_prop_cor - pred_prop_cor)^2 / pred_prop_cor + (obs_prop_incor - pred_prop_incor)^2 / pred_prop_incor

# make the input variables to the function
Pred_Data <-DDM_3params(0.9,0.6,0.3); # we predict the data is well described by these parameters
obs_RT <- Gen_Data$Data[ ,1]
obs_acc <- Gen_Data$Data[ ,2]
pred_RT <- Pred_Data$Data[ ,1]
pred_acc <- Pred_Data$Data[ ,2]

# now run the function Cost_ddm line by line

# if you'd like to understand the cost function better, have a look at the 
# Chi-Square Fitting Method described on page 9 of:

# Ratcliff, R., & Tuerlinckx, F. (2002). Estimating parameters of the diffusion 
# model: Approaches to dealing with contaminant reaction times and parameter 
# variability. Psychonomic Bulletin & Review, 9(3), 438–481. 
# https://doi.org/10.3758/BF03196302

#------------------------------------------------------------------------------#
# to illustrate the use of the cost function, we will now create some "observed"
# data with known parameters and compare it to estimated data with the same or 
# different parameters.

# run 10 simulations in which data is created from the original parameters or 
# the original parameters with some added noise

cost_same <- vector()
cost_diff <- vector()
for(sim in 1:10){
  Pred_Data <-DDM_3params(v,a,ter); # generate data with same parameters, should be low cost
  cost_same[sim] <- Cost_ddm(Gen_Data$Data[ ,1], Gen_Data$Data[ ,2], Pred_Data$Data[ ,1], Pred_Data$Data[ ,2])
  
  Pred_Data <-DDM_3params(v+(rnorm(1)/10),a+rnorm(1),ter+(rnorm(1)/10)); # generate data with different parameters, should be high cost
  cost_diff[sim] <- Cost_ddm(Gen_Data$Data[ ,1], Gen_Data$Data[ ,2], Pred_Data$Data[ ,1], Pred_Data$Data[ ,2])
}

## look at the values for cost_diff and cost_same, which ones are on average higher? Why?
plot(c(1:10), cost_same, ylim = c(min(c(cost_diff, cost_same)), max(c(cost_diff, cost_same))), col = 'blue')
points(c(1:10), cost_diff, col = 'red')

# manual changes in parameter values, look at cost
# plot also the distribution given these parameters over the generated data

#------------------------------------------------------------------------------#
# in a real experiment we do not know the underlying parameters for the data we 
# collected, but we wish to estimate them.
# we could of course manually try different parameters and look at the cost for
# each combination, but this might take a long time.
# instead we use an differential evolution optimization which finds the best 
# fitting combination for us

# grid search vs optimization

# have a look at the function Iterate_fit in DDM_fit_functions.R
# in this function, some data is generated using the inputted parameter values,
# after which this predicted data is compared to the observed data we input to it.
# the cost of this comparison is given as output.

# now we would like to use this function to minimize the cost, using the DEoptim
# function in which R uses a differential evolution optimization.

# first, we should define the upper and lower boundaries of our parameters (v, a, ter)
# so the algorithm  doesn't look in impossible parameter space

L<- c(0,0,0)
U<- c(3,4,1) # drift rate, boundary, non-decision time (seconds)

## now fit the best parameters using the optimization function
optimal_params <- DEoptim(Iterate_fit,  # Function to optimize
                          lower = L,  
                          upper = U,
                          control = c(itermax = 1000, strategy = 2, steptol = 50, reltol = 1e-8),
                          Gen_Data$Data)

## look at the optimal parameters the fit function finds back:
summary(optimal_params)

## compare these optimal parameters to the ones we used to generate data ##
# (v = 0.8, a = 0.75, ter = 0.4)

# Note that this assumes  you never overwrote Gen_Data using different parameter 
# values, if you did you should compare the optimal parameters to those values.

# plot the fitted parameters over generated data

#------------------------------------------------------------------------------#
# We seem to be able to find back  the generative parameter values we created 
# this data with. But how about fitting the model to behavioural data for which 
# we don't know the parameter values yet?

# load your data 

# or load data that we gave you

# set your data in a format that our function expects (two columns, one with 
# reaction time in seconds and one with accuracy in 0/1 coding)

# make behavioral plot showing distributions?
# make your own code

# run the optimization algorithm with your own data
L<- c(0,0,0)
U<- c(3,4,1) # drift rate, boundary, non-decision time (seconds)

## now fit the best parameters using the optimization function
optimal_params <- DEoptim(Iterate_fit,  # Function to optimize
                          lower = L,  
                          upper = U,
                          control = c(itermax = 1000, strategy = 2, steptol = 50, reltol = 1e-8),
                          ...)

# look at the optimal parameters to describe your data
summary(optimal_params)

#------------------------------------------------------------------------------#
# extra oefening: voeg starting point toe
# model comparison, bic
# note: starting point should be dependent on boundary

# compare the fit of different models to your data by calculating the bic for our
# basic DDM (and what other model?)