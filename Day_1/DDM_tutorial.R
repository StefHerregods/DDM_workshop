# Drift Diffusion model tutorial
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
# on line 50, you can see the formula used for evidence accumulation:
# evidence = evidence + v * dt + s * sqrt(dt) * zigg.norm()

# evidence is being added to the DV at every timestep (dt) with magnitude v. 
# Some noise is also added with magnitude s (fixed at 1).

# in a real experiment, the drift rate should depend on the stimulus. For example,
# in a random dot motion stimulus where the dots are moving to the left, the 
# drift rate should be negative, so that evidence accumulation is towards the 
# lower bound. If the dots are moving to the right, the drift rate should be
# positive. 
# Given that we are just simulating data for the moment, we do not have the 
# information on which choice would be correct and which sign the drift
# rate should have.Therefor, we are randomly deciding the "correct choice" for
# each trial. Later, we will use the experimental design to inform the correct choice.

# create "correct choice" for 1000 trials, random for now
CC <- sample(c(1,-1), 1000, replace = TRUE, prob = c(0.5,0.5))

## Let's now use the .cpp function to generate some data
Gen_Data<-DDM_3params(v,a,ter,CC)
colnames(Gen_Data$Data) <- c("RT", "accuracy")

# replace zeros in DV and time with NA
Gen_Data$DVs[Gen_Data$Dvs == 0] <- NA
Gen_Data$time[Gen_Data$time == 0] <- NA

# replace first zero, which is actually time
Gen_Data$time[ ,1] = 0

# have a look at the content of Gen_Data
str(Gen_Data)

# Gen_Data contains 3 variables, DVs, time & data (with columns RT and accuracy)
# DVs and time are used to plot the evidence accumulation on a couple of trials
# (see below). Data contains the predicted behaviour, reaction time (RT) and 
# performance (accuracy).

#------------------------------------------------------------------------------#
## plot a couple of DVs ##

# now we would like to plot a couple of decision variables to see how a choice 
# is being made on a single trial

# set this to the trials you would like to plot, note that we only keep the DVs 
# of the first 21 trials to save memory. You can also plot all trials by setting 
# this to c(1:21) but it might be a bit harder to see each trace.
trials_toplot = c(1,3,4,7,15,18)

# plot those trials and the boundary values
plot(t(Gen_Data$time[trials_toplot, ]), t(Gen_Data$DVs[trials_toplot, ]), type ="l", 
     ylim = c(-a-0.1,a+0.1), xlab = 'Time (s)', ylab = 'Decision variable')
abline(h = a, col = "red")
abline(h = -a, col = "red")

# Q1 why does the DV start with a flat line before accumulation?

# Q2 what does each DV have in common? When do they stop accumulating?

# Q3 can you deduce for how many of these trials the participant chose to move right/left?

# Q4 what was the (approximate) reaction time for each of the trials you plotted?

#------------------------------------------------------------------------------#
## plot behavioural results ##

# when plotting the DVs, we can see a direct representation of the parameters on 
# the decision being made on a single trial. But how do the parameters affect 
# overall behaviour, such as accuracy or reaction time?

# we are usually interested in the distribution of reaction times for the 
# correct and incorrect choices. We use these distributions as a way to fit our 
# predicted behaviour (given some set of parameters) to actual participant behaviour.

# let's look at the reaction time distributions of correct and incorrect choices.
correct_fill_color <- adjustcolor("#2A9D8F", alpha.f = 0.25)
error_fill_color <- adjustcolor("#E76F51", alpha.f = 0.25)
df_observed <- data.frame(Gen_Data$Data)
ggplot() +
  geom_histogram(data = df_observed, aes(x = RT, y = ..count.., fill = as.factor(accuracy)),
                 binwidth = 0.05, color = "black", alpha = 0.5, position = "identity") +
  scale_fill_manual(name = "Accuracy", values = c(error_fill_color, correct_fill_color), labels = c("Incorrect", "Correct")) +
  scale_color_manual(name = "Accuracy", values = c(error_fill_color, correct_fill_color), labels = c("Incorrect", "Correct"))

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
Try_Data<-DDM_3params(v,a,ter,CC)
Try_Data$DVs[Try_Data$Dvs == 0] <- NA
Try_Data$time[Try_Data$time == 0] <- NA
Try_Data$time[ ,1] = 0
colnames(Try_Data$Data) <- c("RT", "accuracy")

par(mfrow = c(1,2))
trials_toplot = c(1,2,3,4,5,6)
plot(t(Try_Data$time[trials_toplot, ]), t(Try_Data$DVs[trials_toplot, ]), type ="l",
ylim = c(-a-0.1,a+0.1), xlab = 'Time (s)', ylab = 'Decision variable')
abline(h = a, col = "red")
abline(h = -a, col = "red")

D <- data.frame(Try_Data$Data)
D$accuracy <- as.factor(D$accuracy)
hist(D$RT[D$accuracy == 1],
              col = correct_fill_color, breaks = 50,
              xlab="Reaction time", ylab="Occurence",
              border = "white", main = "")
hist(D$RT[D$accuracy == 0], , add=TRUE,
              col = error_fill_color, breaks = 50,
              border = "white", main = "")

legend("topright",fill=c("white","white","#2A9D8F","#E76F51"),border=F,
       legend=c("Correct trials","Incorrect trials"),
       col=c("#2A9D8F","#E76F51"),bty='n',lwd=c(1,1,-1,-1))

par(mfrow = c(1,1))

## Q7 what happens to the DVs when you make the boundary higher?

## Q8 how does that affect behaviour (reaction times and accuracy)?

## Q9 what happens when you increase the drift rate while keeping the high boundary?
# why do you think that happens?

## Q10 what happens if you decrease the non-decision time? How does that affect behaviour?

## Q11 what happens if you choose a parameter value that's outside of the "normal
# range"?


#------------------------------------------------------------------------------#
## if we'd like to compare data we predicted based on a set of parameters to 
# data we collected from participants, we need a way to quantify the difference 
# between the distributions from generated and collected data.

## we're going to have a look at the function "Cost_ddm" inside DDM_fit_functions_tutorial.R

# First, we make the input variables to the function

# how many trials should the estimated distribution have? This is typically more
# than the observed trials, because that will create a smoother distribution. Let's
# set it to 50000 (we originally simulated 1000 "observed" trials)
n_est_trials = 50000

# redefine "correct choice" variable
CC <- sample(c(1,-1), n_est_trials, replace = TRUE, prob = c(0.5,0.5))

Pred_Data <-DDM_3params(0.9,0.6,0.3,CC,ntrials = n_est_trials); # we predict the data is well described by these parameters
obs_RT <- Gen_Data$Data[ ,1]
obs_acc <- Gen_Data$Data[ ,2]
pred_RT <- Pred_Data$Data[ ,1]
pred_acc <- Pred_Data$Data[ ,2]
plotting <- 1

# now run the function Cost_ddm line by line and answer the questions.

#------------------------------------------------------------------------------#
# Cost_ddm computes 5 quantiles (containing a proportion of 0.1, 0.3, 0.5, 
# 0.7 and 0.9 of reaction times) in the observed data and  then computes the 
# probability of both a correct and incorrect choice within these quantiles.
# Then the cost (or chi square) is computed.

# We call this the cost, because it computes how different the observed and predicted
# distributions are, and this is what we'd like to reduce as much as possible. A
# low cost means that the distributions are quite similar, and the parameters
# used to generate the data are close to those that underlie the observed data.
# therefor, when fitting a model, we always try to minimize the cost. That is
# what we will do later when fitting the model to some data. For now, we are just
# working on understanding the cost.

# if you'd like to understand the cost function better, have a look at the 
# Chi-Square Fitting Method described on page 9 of:

# Ratcliff, R., & Tuerlinckx, F. (2002). Estimating parameters of the diffusion 
# model: Approaches to dealing with contaminant reaction times and parameter 
# variability. Psychonomic Bulletin & Review, 9(3), 438–481. 
# https://doi.org/10.3758/BF03196302

#------------------------------------------------------------------------------#
# to illustrate the use of the cost function, we will look at our "observed"
# data with known parameters and compare it to estimated data with the same or 
# different parameters.

# set some parameters here that you wish to compare to the data
v_try<-0.1 # original drift rate: 0.8
a_try<-0.6 # original bound: 0.75
ter_try<-0.2  #original non-decision time: 0.4 seconds

# we will predict some data using those parameters
Pred_Data <- DDM_3params(v_try, a_try, ter_try, CC, ntrials = n_est_trials)
colnames(Pred_Data$Data) <- c("RT", "accuracy")

# and calculate the cost of this prediction compared to the original data
cost <- Cost_ddm(Gen_Data$Data[ ,1], Gen_Data$Data[ ,2], Pred_Data$Data[ ,1], Pred_Data$Data[ ,2], 0)

# plot our original data & prediction following from the parameters, with cost indicated
df_observed <- data.frame(Gen_Data$Data)
df_predicted <- data.frame(Pred_Data$Data)
ggplot() +
  geom_histogram(data = df_observed, aes(x = RT, y = ..count../nrow(df_observed), fill = as.factor(accuracy)),
                 binwidth = 0.05, color = "black", alpha = 0.5, position = "identity") +
  geom_freqpoly(data = df_predicted, aes(x = RT, y = ..count../nrow(df_predicted), color = as.factor(accuracy)),
                binwidth = 0.05, linewidth = 1, alpha = 1, inherit.aes = FALSE) + 
  scale_fill_manual(name = "Accuracy", values = c(error_fill_color, correct_fill_color), labels = c("Incorrect", "Correct")) +  
  scale_color_manual(name = "Accuracy", values = c(error_fill_color, correct_fill_color), labels = c("Incorrect", "Correct")) +
  ggtitle(paste("Cost =", cost))

# this figure shows our "observed" data that we generated at the beginning of the
# tutorial in a histogram, and a line representing the prediction following
# from the parameters you set. Does the line follow the data in the histogram?

## Q16 try to change the parameter values until you get a low cost and a good
# fit of the predicted data onto the original data (the lines should follow the
# histogram distribution closely)

## Q17 which parameter values fit the original data best? Why do you think that is?

#------------------------------------------------------------------------------#
# in a real experiment we do not know the underlying parameters for the data we 
# collected, but we wish to estimate them.
# we could of course manually try different parameters and look at the cost for
# each combination, but this might take a long time.
# instead we use an differential evolution optimization which finds the best 
# fitting combination for us.

# have a look at the function Iterate_fit in DDM_fit_functions_tutorial.R
# in this function, some data is generated using the inputted parameter values,
# after which this predicted data is compared to the observed data we input to it.
# the cost of this comparison is given as output.

# now we would like to use this function to minimize the cost, using the DEoptim
# function in which R uses a differential evolution optimization.

# first, we should define the upper and lower boundaries of our parameters (v, a, ter)
# so the algorithm  doesn't look in impossible parameter space

L<- c(0,0,0)
U<- c(3,4,1) # drift rate, boundary, non-decision time (seconds)

# choose number of estimated trials
n_est_trials = 5000

# redefine "correct choice" variable
CC <- sample(c(1,-1), n_est_trials, replace = TRUE, prob = c(0.5,0.5))

## now fit the best parameters using the optimization function
optimal_params <- DEoptim(Iterate_fit,  # Function to optimize
                          lower = L,  
                          upper = U,
                          control = c(itermax = 1000, strategy = 2, steptol = 50, reltol = 1e-8),
                          Gen_Data$Data, CC, ntrials = n_est_trials)

# the output printed to the console represents the iteration (how many times the
# algorithm has tried different parameter values), the cost associated with this
# iteration, and the 3 parameter values it used in this iteration (v, a, ter).

## look at the optimal parameters the fit function finds back:
summary(optimal_params)

## Q18 compare the optimal parameters the fit function finds to the ones we 
# originally used to generate data (v = 0.8, a = 0.75, ter = 0.4).
# Did the fitting procedure find back similar parameter values?

# Note that this assumes  you never overwrote Gen_Data using different parameter 
# values, if you did you can rerun line 36-74 before running the optimization.

#------------------------------------------------------------------------------#
## let's look at how predicted data from these optimal parameters compares to 
# our original data ##

# generate data using the optimal parameter values from our fitting procedure
Pred_Data <- DDM_3params(optimal_params$optim$bestmem[1], optimal_params$optim$bestmem[2], optimal_params$optim$bestmem[3], CC, ntrials = n_est_trials)
colnames(Pred_Data$Data) <- c("RT", "accuracy")
cost <- Cost_ddm(Gen_Data$Data[ ,1], Gen_Data$Data[ ,2], Pred_Data$Data[ ,1], Pred_Data$Data[ ,2], 0)

# plot our original data & prediction following from the optimal parameters
df_observed <- data.frame(Gen_Data$Data)
df_predicted <- data.frame(Pred_Data$Data)
ggplot() +
  geom_histogram(data = df_observed, aes(x = RT, y = ..count../nrow(df_observed), fill = as.factor(accuracy)),
                 binwidth = 0.05, color = "black", alpha = 0.5, position = "identity") +
  geom_freqpoly(data = df_predicted, aes(x = RT, y = ..count../nrow(df_predicted), color = as.factor(accuracy)),
                binwidth = 0.05, linewidth = 1, alpha = 1, inherit.aes = FALSE) + 
  scale_fill_manual(name = "Accuracy", values = c(error_fill_color, correct_fill_color), labels = c("Incorrect", "Correct")) +  
  scale_color_manual(name = "Accuracy", values = c(error_fill_color, correct_fill_color), labels = c("Incorrect", "Correct")) +
  ggtitle(paste("Cost =", cost))

## Q19 What do you think of the fit these optimal parameters provide? Does it 
# match your expectations?

#------------------------------------------------------------------------------#
# We seem to be able to find back  the generative parameter values we created 
# this data with. But how about fitting the model to behavioural data for which 
# we don't know the parameter values yet? Here, we will do the fitting procedure
# on some real data. You can use data your brought, or data we provided (to be 
# found in the repository with the code)

# load your data 
data <- ...

# or load data that we gave you
data <- read.csv("DDM_data.csv")

# set your data in a format that our function expects (two columns, one with 
# reaction time in seconds and one with accuracy in 0/1 coding).
# below we do this for the example dataset, but you should write it yourself
# for your own data
D <- data.frame(data)
names(D)[names(D) == "rt"] <- "RT"

# create a new variable names observations which has only the RT and accuracy column
# (so no confidence or stimulus information) for the optimization. Again, write this
# yourself for your own data.
observations <- matrix(nrow = length(D$RT), ncol = 2)
observations[,1] <- D$RT
observations[,2] <- D$accuracy

# make a behavioral plot showing the reaction time distributions of correct and incorrect trials
df_observed <- D
ggplot() +
  geom_histogram(data = df_observed, aes(x = RT, y = ..count.., fill = as.factor(accuracy)),
                 binwidth = 0.05, color = "black", alpha = 0.5, position = "identity") +
  scale_fill_manual(name = "Accuracy", values = c(error_fill_color, correct_fill_color), labels = c("Incorrect", "Correct")) +
  scale_color_manual(name = "Accuracy", values = c(error_fill_color, correct_fill_color), labels = c("Incorrect", "Correct"))

# note that up to now, we set the drift rate to positive or negative
# at random, because we do not have an actual correct boundary.Now that you have
# real data, there is a correct answer, and the drift rate should reflect this.
# Define CC here as the correct answer, the direction the dots were really going
# in if you use our example data or other data with an RDM stimulus.

# define the correct choice variable
CC <- 

# run the optimization algorithm with your own data
L<- c(0,0,0)
U<- c(3,4,1) # drift rate, boundary, non-decision time (seconds)

## now fit the best parameters using the optimization function
optimal_params <- DEoptim(Iterate_fit,  # Function to optimize
                          lower = L,  
                          upper = U,
                          control = c(itermax = 1000, strategy = 2, steptol = 50, reltol = 1e-8),
                          observations, CC, ntrials = nrow(observations))

# look at the optimal parameters to describe your data
summary(optimal_params)

#------------------------------------------------------------------------------#
## now plot the prediction from the optimal parameters over the observed data ##

# generate data using the optimal parameter values from our fitting procedure
CC <- sample(c(1,-1), n_est_trials, replace = TRUE, prob = c(0.5,0.5))
Pred_Data <- DDM_3params(optimal_params$optim$bestmem[1], optimal_params$optim$bestmem[2], optimal_params$optim$bestmem[3], CC, ntrials = n_est_trials)
colnames(Pred_Data$Data) <- c("RT", "accuracy")
cost <- Cost_ddm(D$RT, D$accuracy, Pred_Data$Data[ ,1], Pred_Data$Data[ ,2], 0)

# plot the original data & prediction following from the optimal parameters
df_predicted <- data.frame(Pred_Data$Data)
ggplot() +
  geom_histogram(data = df_observed, aes(x = RT, y = ..count../nrow(df_observed), fill = as.factor(accuracy)),
                 binwidth = 0.05, color = "black", alpha = 0.5, position = "identity") +
  geom_freqpoly(data = df_predicted, aes(x = RT, y = ..count../nrow(df_predicted), color = as.factor(accuracy)),
                binwidth = 0.05, linewidth = 1, alpha = 1, inherit.aes = FALSE) + 
  scale_fill_manual(name = "Accuracy", values = c(error_fill_color, correct_fill_color), labels = c("Incorrect", "Correct")) +  
  scale_color_manual(name = "Accuracy", values = c(error_fill_color, correct_fill_color), labels = c("Incorrect", "Correct")) +
  ggtitle(paste("Cost =", cost))

## Q20 How do the parameters values compare to the ones we used to generate data for
# the tutorial? What do you think could cause this difference?
# what do you think of the fit between the observed data and the predictions
# following our optimal parameters? Do you think fitting real data works as well
# as fitting generated data?
#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#

#------------------------------------------------------------------------------#
## extra assignment for if you have time: adding a parameter to the model ##
#------------------------------------------------------------------------------#

# currently, the model has 3 parameters: drift rate, boundary & non-decision time
# in experiments, we sometimes see that participants have a preference for one
# choice over the other, regardless of the sensory input they receive. This results
# in a bias, where they choose one option more often than the other over an entire
# block or whole experiment. This bias can not be captured by the 3 parameters we
# used until now.
# a drift-diffusion model can have an additional parameter that accounts for this 
# bias, it's called the starting point. As you might imagine from its name, the 
# starting point affects where the accumulation starts. Now, the accumulation 
# always started from zero, but it can also start closer to either the upper or
# lower bound to make one bound more likely to be reached than the other.

# make a new function (DDM_4params.cpp), in which the starting point can be altered.
# you can start with the DDM_3params.cpp file and adapt it (make sure to change the
# function name inside the file as well as the file name). Note that the starting 
# point should be dependent on the boundary value.

# Q21 how did you implement the starting point? Which values correspond to a bias 
# for which boundary? For what value of starting point is there no bias? What are 
# the limits of your parameter?

## now run your new function to generate some data and plot a couple of DVs

# make sure to source your new function first
sourceCpp("DDM_4params.cpp")

# define parameters
v<-0.8 #drift rate
a<-0.75 #bound
ter<-0.4 #non-decision time (seconds)
z <- ... #starting point

# create "correct choice", random for now
CC <- sample(c(1,-1), 1000, replace = TRUE, prob = c(0.5,0.5))

# generate data
Gen_Data<-DDM_4params(v,a,ter,CC,z) # make sure the order of the inputs matches how you wrote it in your function
colnames(Gen_Data$Data) <- c("RT", "accuracy")

# replace zeros in DV and time with NA
Gen_Data$DVs[Gen_Data$Dvs == 0] <- NA
Gen_Data$time[Gen_Data$time == 0] <- NA 
Gen_Data$time[ ,1] = 0

## plot a couple of DVs to see what your starting point did
trials_toplot = c(1:20)
plot(t(Gen_Data$time[trials_toplot, ]), t(Gen_Data$DVs[trials_toplot, ]), type ="l", 
     ylim = c(-a-0.1,a+0.1), xlab = 'Time (s)', ylab = 'Decision variable')
abline(h = a, col = "red")
abline(h = -a, col = "red")

## Q22 what do you notice about the DVs when trying different values of the starting
# point variable? Does your model work as expected?

#------------------------------------------------------------------------------#
## Now, let's see if we can recover the starting point parameter ##
# after generating data, we can see if our optimization algorithm and cost function
# also finds back the correct value for our starting point parameter.

# create "correct choice", random for now
CC <- sample(c(1,-1), 1000, replace = TRUE, prob = c(0.5,0.5))

# make sure to set the upper and lower limit of your starting point parameter so 
# the algorithm knows where to search
L<- c(0,0,0,...)
U<- c(3,4,1,...) # drift rate, boundary, non-decision time (seconds), starting point

## now fit the best parameters using the optimization function
optimal_params <- DEoptim(Iterate_fit_4params,  # Function to optimize
                          lower = L,
                          upper = U,
                          control = c(itermax = 1000, strategy = 2, steptol = 50, reltol = 1e-8),
                          Gen_Data$Data, CC)

# look at the optimal parameters to describe your data
summary(optimal_params)

## Q23 did the optimization algorithm find your starting point parameter back?
# how about the other parameters? 

## Q24 If you run the optimization multiple times, does the starting point 
# parameter always have the same sign? Why do you think that is?
