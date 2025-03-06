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
plotting <- 1

# now run the function Cost_ddm line by line

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
v_try<-0.8 # original drift rate: 0.8
a_try<-0.75 # original bound: 0.75
ter_try<-0.4  #original non-decision time: 0.4 seconds

# we will predict some data using those parameters
Pred_Data <- DDM_3params(v_try, a_try, ter_try)

# and calculate the cost of this prediction compared to the original data
cost <- Cost_ddm(Gen_Data$Data[ ,1], Gen_Data$Data[ ,2], Pred_Data$Data[ ,1], Pred_Data$Data[ ,2], 0)

# plot our original data & prediction following from the parameters, with cost indicated
D <- data.frame(Gen_Data$Data)
D$accuracy <- as.factor(D$accuracy)
hist(D$RT[D$accuracy == 1],
     col = correct_fill_color,
     breaks = 50, freq = FALSE,
     xlab="Reaction time", ylab="Occurence",
     border = "white", main = paste("Cost =", cost))

lines(x = density(x = Pred_Data$Data[(Pred_Data$Data[,2]==1),1]), col = correct_fill_color)

hist(D$RT[D$accuracy == 0], , add=TRUE,
              col = error_fill_color,
              breaks = 50, freq = FALSE,
              border = "white", main = "")

lines(x = density(x = Pred_Data$Data[(Pred_Data$Data[,2]==0),1]), col = error_fill_color)

legend("topright",fill=c("white","white","#2A9D8F","#E76F51"),border=F,
       legend=c("Correct trials","Incorrect trials"),
       col=c("#2A9D8F","#E76F51"),bty='n',lwd=c(1,1,-1,-1))


## Q16 try to change the parameter values untill you get a low cost and a good
# fit of the predicted data onto the original data (the lines should follow the
# histogram distribution closely)

## Q17 which parameter values fit the original data best? Why do you think that is?

#------------------------------------------------------------------------------#
# in a real experiment we do not know the underlying parameters for the data we 
# collected, but we wish to estimate them.
# we could of course manually try different parameters and look at the cost for
# each combination, but this might take a long time.
# instead we use an differential evolution optimization which finds the best 
# fitting combination for us

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

## Q18 compare the optimal parameters the fit function finds to the ones we 
# originally used to generate data (v = 0.8, a = 0.75, ter = 0.4).
# Did the fitting procedure find back similar parameter values?

# Note that this assumes  you never overwrote Gen_Data using different parameter 
# values, if you did you should compare the optimal parameters to those values.

#------------------------------------------------------------------------------#
## let's look at how predicted data from these optimal parameters compares to 
# our original data ##

Pred_Data <- DDM_3params(optimal_params$optim$bestmem[1], optimal_params$optim$bestmem[2], optimal_params$optim$bestmem[3])
cost <- Cost_ddm(Gen_Data$Data[ ,1], Gen_Data$Data[ ,2], Pred_Data$Data[ ,1], Pred_Data$Data[ ,2], 0)

# plot our original data & prediction following from the optimal parameters
D <- data.frame(Gen_Data$Data)
D$accuracy <- as.factor(D$accuracy)
hist(D$RT[D$accuracy == 1],
     col = correct_fill_color,
     breaks = 50, freq = FALSE,
     xlab="Reaction time", ylab="Occurence",
     border = "white", main = paste("Cost =", cost))

lines(x = density(x = Pred_Data$Data[(Pred_Data$Data[,2]==1),1]), col = correct_fill_color)

hist(D$RT[D$accuracy == 0], , add=TRUE,
     col = error_fill_color,
     breaks = 50, freq = FALSE,
     border = "white", main = "")

lines(x = density(x = Pred_Data$Data[(Pred_Data$Data[,2]==0),1]), col = error_fill_color)

legend("topright",fill=c("white","white","#2A9D8F","#E76F51"),border=F,
       legend=c("Correct trials","Incorrect trials"),
       col=c("#2A9D8F","#E76F51"),bty='n',lwd=c(1,1,-1,-1))

## Q19 What do you think of the fit these optimal parameters provide? Does it 
# match your expectations?

#------------------------------------------------------------------------------#
# We seem to be able to find back  the generative parameter values we created 
# this data with. But how about fitting the model to behavioural data for which 
# we don't know the parameter values yet?

# load your data 

# or load data that we gave you
data <- read.csv("DDM_data.csv")

# set your data in a format that our function expects (two columns, one with 
# reaction time in seconds and one with accuracy in 0/1 coding)
D <- data.frame(data)
names(D)[names(D) == "rt"] <- "RT"
observations <- ...

# make a behavioral plot showing the reaction time distributions of correct and incorrect trials
hist(D$RT[D$accuracy == 1],
     col = correct_fill_color,
     breaks = 50, freq = FALSE,
     xlab="Reaction time", ylab="Occurence",
     border = "white", main = "")
hist(D$RT[D$accuracy == 0], , add=TRUE,
     col = error_fill_color,
     breaks = 50, freq = FALSE,
     border = "white", main = "")
legend("topright",fill=c("white","white","#2A9D8F","#E76F51"),border=F,
       legend=c("Correct trials","Incorrect trials"),
       col=c("#2A9D8F","#E76F51"),bty='n',lwd=c(1,1,-1,-1))

# note that in the current model, we set the drift rate to positive or negative
# at random, because we do not have an actual correct boundary.Now that you have
# real data, there is a correct answer, and the drift rate should reflect this.
# go into the DDM_3params.cpp function and change the line 33-35 to something
# that reflects the actual correct decision for your data. Hint: you will need
# to add an input variable to the function and to line 388 in this script.

# define the correct choice variable

# run the optimization algorithm with your own data
L<- c(0,0,0)
U<- c(3,4,1) # drift rate, boundary, non-decision time (seconds)

## now fit the best parameters using the optimization function
optimal_params <- DEoptim(Iterate_fit,  # Function to optimize
                          lower = L,  
                          upper = U,
                          control = c(itermax = 1000, strategy = 2, steptol = 50, reltol = 1e-8),
                          observations)

# look at the optimal parameters to describe your data
summary(optimal_params)

## Q20 How do these parameters compare to the ones we used to generate data for
# the tutorial? What do you think could cause this difference?

#------------------------------------------------------------------------------#
## extra assignment for if you have time: adding a parameter to the model ##
# currently, the model has 3 parameters: drift rate, boundary & non-decision time
# in experiments, we sometimes see that participants have a preference for one
# choice over the other, regardless of the sensory input they receive. This results
# in a bias, where they choose one option more often than the other over an entire
# block or whole experiment.
# a drift-diffusion model can have an additional parameter that account for this 
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
Gen_Data<-DDM_4params(v,a,ter,CC,z)
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
## let's fit this new model on our data and see what the estimate of starting point is ##

# first we have to adjust the Iterate_fit function so that it takes our new model
# open the DDM_fit_functions_tutorial.R script and change the Iterate_fit function
# so that it works with DDM_4params instead of DDM_3params.

# (re)define the correct choice variable using the data
CC <- ...

# run the optimization algorithm with your own data

# define the upper and lower boundary of your starting point parameter
L<- c(0,0,0,...)
U<- c(3,4,1,...) # drift rate, boundary, non-decision time (seconds), starting point

## now fit the best parameters using the optimization function
optimal_params <- DEoptim(Iterate_fit,  # Function to optimize
                          lower = L,  
                          upper = U,
                          control = c(itermax = 1000, strategy = 2, steptol = 50, reltol = 1e-8),
                          observations, CC)

# look at the optimal parameters to describe your data
summary(optimal_params)

## Q23 was a bias present in your data? For which side?

#------------------------------------------------------------------------------#
## Let's say we are not sure if our experiment resulted in a bias for participants,
# and we would like to see if it makes sense to add the 4th parameter to our model.
# we can do this using model comparison.

