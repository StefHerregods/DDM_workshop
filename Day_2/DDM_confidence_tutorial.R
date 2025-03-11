
# Goal: Compare model fit of different post-decision confidence stopping rules


# Set-up ------------------------------------------------------------------

setwd(dirname(rstudioapi::getSourceEditorContext()$path))
rm(list=ls())

library(ggplot2)
library(Rcpp)
library(DEoptim)
library(ggpubr)  # Install with install.packages('ggpubr')

observations <- read.csv('DDM_data.csv')  # Replace with your own observations 
# if possible. Data needs columns: rt (reaction time), cor (accuracy), rtconf 
# (confidence reaction time), and cj (binary confidence judgments; 1 or 0)

sourceCpp('DDM_2DSD.cpp')  # Contains model with time-based stopping rule


# Time-based stopping rule ------------------------------------------------

# First, get some insight into the parameters of a model with time-based 
# stopping rule for confidence judgments (also known as the 2DSD model). 
# The model has four free parameters:

v <- 0.8  # Drift rate
a <- 0.75  # Decision bound
ter <- 0.4  # Non-decision time (in seconds)
tau <- 1  # Interjudgment time (in seconds)
          # = time between making a decision and confidence judgment

# Try out a few combinations of parameters. Use the functions below to simulate
# and plot results.

DDM_2DSD(...)
plot; color code left right

ggplot(data = predictions, aes(x = time, y = evidence))

# [Q1]  The confidence judgment on each trial depends on the accumulated 
#       post-decision evidence. Can you see how continuous post-decision 
#       evidence is split into binary confidence judgments in the plot below?

ggplot

# [Q2]  Take a look at the model fit. Plot decision reaction time histograms of 
#       your observations with overlapping model predictions. Can you find a set 
#       of parameters that works?

v <- 0.8  
a <- 1
ter <- 0.4  
tau <- 1 

predictions <- data.frame(DDM_2DSD(v=v, a=a, ter=ter, tau=tau))
names(predictions) <- c('rt', 'accuracy', 'rtconf', 'cj')  # decision RT, accuracy, confidence RT, confidence judgment

ggplot() +
  geom_histogram(data = observations, aes(x = rt, fill = as.factor(accuracy), 
                                          y = after_stat(..count.. / sum(..count..))),
                 binwidth = 0.2, color = "black", alpha = 0.5, position = "identity") +
                 scale_fill_manual(values = c("0" = "darkred", "1" = "darkgreen")) +
  geom_freqpoly(data = predictions, aes(x = rt, color = as.factor(accuracy), 
                                        y = after_stat(..count.. / sum(..count..))),
                binwidth = 0.2, size = 1.5) +
                scale_color_manual(values = c("0" = "darkred", "1" = "darkgreen")) +
  xlim(c(0,3)) +
  xlab('Decision reaction time') +
  ylab('Density') +
  labs(fill = 'Observed accuracy', color = 'Predicted accuracy')

# [Q3]  Try the same for confidence reaction times. Why is it difficult to find 
#       a set of parameters that works well?

v <- 0.8  
a <- 1
ter <- 0.4  
tau <- 1 

predictions <- data.frame(DDM_2DSD(v=v, a=a, ter=ter, tau=tau))
names(predictions) <- c('rt', 'accuracy', 'rtconf', 'cj')  # decision RT, accuracy, confidence RT, confidence judgment

ggplot() +
  geom_histogram(data = observations, aes(x = rtconf, fill = as.factor(accuracy), 
                                          y = after_stat(..count.. / sum(..count..))),
                 binwidth = 0.2, color = "black", alpha = 0.5, position = "identity") +
  scale_fill_manual(values = c("0" = "darkblue", "1" = "darkorange")) +
  geom_freqpoly(data = predictions, aes(x = rtconf, color = as.factor(accuracy), 
                                        y = after_stat(..count.. / sum(..count..))),
                binwidth = 0.2, size = 1.5) +
  scale_color_manual(values = c("0" = "darkblue", "1" = "darkorange")) +
  xlim(c(0,3)) +
  xlab('Confidence reaction time') +
  ylab('Density') +
  labs(fill = 'Observed confidence judgment', color = 'Predicted Confidence judgment')

# Evidence-based stopping rule --------------------------------------------

# The previous stopping rule did not capture the typically observed positively 
# skewed distribution of confidence reaction times. One alternative is to move 
# from a time-based to an evidence-based stopping rule. A schematic overview of 
# such a model is shown in 'Evidence-based_stopping.png'.

# [Q4]  Time to get our hands dirty. Implement this model in a new cpp file.
#       (Hint: you can use the 2DSD cpp file as inspiration)
#       Load the new script once finished.

sourceCpp('')

# [Q5]  Take a look at the model fit again. Plot decision reaction time 
#       histograms of your observations with overlapping model predictions. Can 
#       you find a set of parameters that provides good fit?

v <- 0.8  
a <- 1
ter <- 0.4  
a2 <- 1 

predictions <- data.frame(new_DDM_function(v=v, a=a, ter=ter, a2=a2))  # Don't forget to change the name of the function to your new model's name
names(predictions) <- c('rt', 'accuracy', 'rtconf', 'cj')  # decision RT, accuracy, confidence RT, confidence judgment

ggplot() +
  geom_histogram(data = observations, aes(x = rt, fill = as.factor(accuracy), 
                                          y = after_stat(..count.. / sum(..count..))),
                 binwidth = 0.2, color = "black", alpha = 0.5, position = "identity") +
  scale_fill_manual(values = c("0" = "darkred", "1" = "darkgreen")) +
  geom_freqpoly(data = predictions, aes(x = rt, color = as.factor(accuracy), 
                                        y = after_stat(..count.. / sum(..count..))),
                binwidth = 0.2, size = 1.5) +
  scale_color_manual(values = c("0" = "darkred", "1" = "darkgreen")) +
  xlim(c(0,3)) +
  xlab('Decision reaction time') +
  ylab('Density') +
  labs(fill = 'Observed accuracy', color = 'Predicted accuracy')

# [Q6]  Try the same for confidence reaction times as well.

v <- 0.8  
a <- 1
ter <- 0.4  
a2 <- 1 

predictions <- data.frame(new_DDM_function(v=v, a=a, ter=ter, a2=a2))  # Don't forget to change the name of the function to your new model's name
names(predictions) <- c('rt', 'accuracy', 'rtconf', 'cj')  # decision RT, accuracy, confidence RT, confidence judgment

ggplot() +
  geom_histogram(data = observations, aes(x = rtconf, fill = as.factor(accuracy), 
                                          y = after_stat(..count.. / sum(..count..))),
                 binwidth = 0.2, color = "black", alpha = 0.5, position = "identity") +
  scale_fill_manual(values = c("0" = "darkblue", "1" = "darkorange")) +
  geom_freqpoly(data = predictions, aes(x = rtconf, color = as.factor(accuracy), 
                                        y = after_stat(..count.. / sum(..count..))),
                binwidth = 0.2, size = 1.5) +
  scale_color_manual(values = c("0" = "darkblue", "1" = "darkorange")) +
  xlim(c(0,3)) +
  xlab('Confidence reaction time') +
  ylab('Density') +
  labs(fill = 'Observed confidence judgment', color = 'Predicted Confidence judgment')


# Model fitting -----------------------------------------------------------

# Next, we will estimate the optimal parameters of both models. Load the cost 
# function below. Test the function on your current observations and 
# predictions. 

source("DDM_confidence_fit_function.R")
Cost_ddm_confidence(obs_RT = observations$rt, obs_acc = observations$accuracy, obs_RTconf = observations$rtconf, obs_cj = observations$cj,
                    pred_RT = predictions$rt, pred_acc = predictions$accuracy, pred_RTconf = predictions$rtconf, pred_cj = predictions$cj,
                    plotting = T)

# [Q7]  For the DDM, the cost function computed model fit based on quantile 
#       optimization of two distributions (RT of correct trials, RT of error 
#       trials), not taking into account confidence data. What distributions are 
#       used to compute fit in the new function? (Note that this is only one of 
#       many possible implementations of a cost function)

# Load the function for a single iteration of the fitting procedure.
# Note that this function is specifically written for the 2DSD model.

Iterate_fit_confidence_2DSD <- function(params, observations, ntrials = 1000){
  names(params) <- c('v', 'a', 'ter', 'tau')
  predictions <- data.frame(DDM_2DSD(v = params['v'], a = params['a'], ter = params['ter'], tau = params['tau'], ntrials = ntrials))
  names(predictions) <- c('rt', 'accuracy', 'rtconf', 'cj')  # decision RT, accuracy, confidence RT, confidence judgment
  cost <- Cost_ddm_confidence(obs_RT = observations$rt, obs_acc = observations$accuracy, obs_RTconf = observations$rtconf, obs_cj = observations$cj,
                              pred_RT = predictions$rt, pred_acc = predictions$accuracy, pred_RTconf = predictions$rtconf, pred_cj = predictions$cj)
  return(cost)
}

# Load the function for a single iteration for your newly created model. 
# Make sure that the model function name matches the one from your cpp file.

Iterate_fit_confidence_new_model <- function(params, observations, ntrials = 1000){
  names(params) <- c('v', 'a', 'ter', 'a2')
  predictions <- data.frame(new_DDM_function(v = params['v'], a = params['a'], ter = params['ter'], a2 = params['a2'], ntrials = ntrials))
  names(predictions) <- c('rt', 'accuracy', 'rtconf', 'cj')  # decision RT, accuracy, confidence RT, confidence judgment
  cost <- Cost_ddm_confidence(obs_RT = observations$rt, obs_acc = observations$accuracy, obs_RTconf = observations$rtconf, obs_cj = observations$cj,
                              pred_RT = predictions$rt, pred_acc = predictions$accuracy, pred_RTconf = predictions$rtconf, pred_cj = predictions$cj)
  return(cost)
}

# Fit both models using an evolutionary algorithm.
# First, define reasonable parameter ranges for both models.

LowerLimit_2DSD <- c(0, 0, 0, 0)  # drift rate, decision boundary, non-decision time, interjudgment time
UpperLimit_2DSD <- c(5, 4, 1, 5) 

LowerLimit_new_model <- c(0, 0, 0, 0)  # drift rate, decision boundary, non-decision time, confidence boundary
UpperLimit_new_model <- c(5, 4, 1, 5) 

# Call the optimization function

optimal_params_2DSD <- DEoptim(Iterate_fit_confidence_2DSD,  # Function to optimize
                               lower = LowerLimit_2DSD,  # Lower limit of parameter values
                               upper = UpperLimit_2DSD,  # Upper limit of parameter values
                               control = c(itermax = 1000, strategy = 2, steptol = 50, reltol = 1e-8),
                               observations, ntrials = 1000)

optimal_params_new_model <- DEoptim(Iterate_fit_confidence_new_model,  # Function to optimize
                               lower = LowerLimit_new_model,  # Lower limit of parameter values
                               upper = UpperLimit_new_model,  # Upper limit of parameter values
                               control = c(itermax = 1000, strategy = 2, steptol = 50, reltol = 1e-8),
                               observations, ntrials = 1000)


# Model comparison --------------------------------------------------------

# look at the optimal parameters to describe your data

params_2DSD <- optimal_params_2DSD$optim$bestmem
names(params_2DSD) <- c('v', 'a', 'ter', 'tau')
print(params_2DSD)

params_new_model <- optimal_params_new_model$optim$bestmem
names(params_new_model) <- c('v', 'a', 'ter', 'a2')
print(params_new_model)


# [Q8] Take a look at the model fit. Which model captures your data best?

predictions_2DSD <- data.frame(DDM_2DSD(v=params_2DSD['v'], a=params_2DSD['a'], ter=params_2DSD['ter'], tau=params_2DSD['tau'])) 
names(predictions_2DSD) <- c('rt', 'accuracy', 'rtconf', 'cj')  # decision RT, accuracy, confidence RT, confidence judgment

predictions_new_model <- data.frame(new_DDM_function(v=params_new_model['v'], a=params_new_model['a'], ter=params_new_model['ter'], a2=params_new_model['a2']))  # Don't forget to change the name of the function to your new model's name
names(predictions_new_model) <- c('rt', 'accuracy', 'rtconf', 'cj')  # decision RT, accuracy, confidence RT, confidence judgment

p1 <- ggplot() +
  geom_histogram(data = observations, aes(x = rt, fill = as.factor(accuracy), 
                                          y = after_stat(..count.. / sum(..count..))),
                 binwidth = 0.2, color = "black", alpha = 0.5, position = "identity") +
  scale_fill_manual(values = c("0" = "darkred", "1" = "darkgreen")) +
  geom_freqpoly(data = predictions_2DSD, aes(x = rt, color = as.factor(accuracy), 
                                        y = after_stat(..count.. / sum(..count..))),
                binwidth = 0.2, size = 1.5) +
  scale_color_manual(values = c("0" = "darkred", "1" = "darkgreen")) +
  xlim(c(0,3)) +
  xlab('Decision reaction time') +
  ylab('Density') +
  labs(fill = 'Observed accuracy', color = 'Predicted accuracy')

p2 <- ggplot() +
  geom_histogram(data = observations, aes(x = rtconf, fill = as.factor(accuracy), 
                                          y = after_stat(..count.. / sum(..count..))),
                 binwidth = 0.2, color = "black", alpha = 0.5, position = "identity") +
  scale_fill_manual(values = c("0" = "darkblue", "1" = "darkorange")) +
  geom_freqpoly(data = predictions_2DSD, aes(x = rtconf, color = as.factor(accuracy), 
                                        y = after_stat(..count.. / sum(..count..))),
                binwidth = 0.2, size = 1.5) +
  scale_color_manual(values = c("0" = "darkblue", "1" = "darkorange")) +
  xlim(c(0,3)) +
  xlab('Confidence reaction time') +
  ylab('Density') +
  labs(fill = 'Observed confidence judgment', color = 'Predicted Confidence judgment')

ggarrange(p1, p2, ncol = 2) %>%
  annotate_figure(top = text_grob('2DSD model fit', face = "bold", size = 14))

p3 <- ggplot() +
  geom_histogram(data = observations, aes(x = rt, fill = as.factor(accuracy), 
                                          y = after_stat(..count.. / sum(..count..))),
                 binwidth = 0.2, color = "black", alpha = 0.5, position = "identity") +
  scale_fill_manual(values = c("0" = "darkred", "1" = "darkgreen")) +
  geom_freqpoly(data = predictions_new_model, aes(x = rt, color = as.factor(accuracy), 
                                             y = after_stat(..count.. / sum(..count..))),
                binwidth = 0.2, size = 1.5) +
  scale_color_manual(values = c("0" = "darkred", "1" = "darkgreen")) +
  xlim(c(0,3)) +
  xlab('Decision reaction time') +
  ylab('Density') +
  labs(fill = 'Observed accuracy', color = 'Predicted accuracy')

p4 <- ggplot() +
  geom_histogram(data = observations, aes(x = rtconf, fill = as.factor(accuracy), 
                                          y = after_stat(..count.. / sum(..count..))),
                 binwidth = 0.2, color = "black", alpha = 0.5, position = "identity") +
  scale_fill_manual(values = c("0" = "darkblue", "1" = "darkorange")) +
  geom_freqpoly(data = predictions_new_model, aes(x = rtconf, color = as.factor(accuracy), 
                                             y = after_stat(..count.. / sum(..count..))),
                binwidth = 0.2, size = 1.5) +
  scale_color_manual(values = c("0" = "darkblue", "1" = "darkorange")) +
  xlim(c(0,3)) +
  xlab('Confidence reaction time') +
  ylab('Density') +
  labs(fill = 'Observed confidence judgment', color = 'Predicted Confidence judgment')

ggarrange(p3, p4, ncol = 2) %>%
  annotate_figure(top = text_grob('New model fit', face = "bold", size = 14))

# [Q9]  Finally, we can also compare BIC values across the models. 
#       Do the BIC values align with your expectations?

# Defining BIC function
compute_BIC <- function(n_parameters, n_trials, cost){
  BIC <- n_parameters * log(n_trials) + n_trials * log(cost/n_trials)
  return(BIC)
}

# Defining 2DSD values
n_parameters_2DSD <- 4  # Number of free parameters
n_trials_2DSD <- nrow(observations)  # Number of trials in observations
cost_2DSD <- Cost_ddm_confidence(obs_RT = observations$rt, obs_acc = observations$accuracy, obs_RTconf = observations$rtconf, obs_cj = observations$cj,
                                 pred_RT = predictions_2DSD$rt, pred_acc = predictions_2DSD$accuracy, pred_RTconf = predictions_2DSD$rtconf, pred_cj = predictions_2DSD$cj)

# Defining new model values
n_parameters_2DSD <- 4  # Number of free parameters
n_trials_2DSD <- nrow(observations)  # Number of trials in observations
cost_2DSD <- Cost_ddm_confidence(obs_RT = observations$rt, obs_acc = observations$accuracy, obs_RTconf = observations$rtconf, obs_cj = observations$cj,
                                 pred_RT = predictions_new_model$rt, pred_acc = predictions_new_model$accuracy, pred_RTconf = predictions_new_model$rtconf, pred_cj = predictions_new_model$cj)

# Compute BIC
BIC_2DSD <- compute_BIC(n_parameters = n_parameters_2DSD, n_trials = n_trials_2DSD, cost = cost_2DSD)
BIC_new_model <- compute_BIC(n_parameters = n_parameters_new_model, n_trials = n_trials_new_model, cost = cost_new_model)
print(paste("The model BICs are:", "2DSD =", round(BIC_2DSD,2), "and 'New model' =", round(BIC_new_model,2)))

