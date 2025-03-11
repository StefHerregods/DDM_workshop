# calculating cost of observed and predicted data
# based on RT as well as probability correct
# input: observed RT, observed probability correct, predicted RT, predicted probability correct
# output: cost (chi square)

Cost_ddm <- function(obs_RT, obs_acc, pred_RT, pred_acc, plotting){
  
  # define quantiles from observed RTs
  quantiles_cor <- quantile(obs_RT[obs_acc == 1], probs = c(.1,.3,.5,.7,.9), names = FALSE)
  quantiles_incor <- quantile(obs_RT[obs_acc == 0], probs = c(.1,.3,.5,.7,.9), names = FALSE)

  if(plotting)
  {
    par(mfrow = c(2,1))
    # plot the distributions of correct and incorrect observations and add the quantiles
    hist(obs_RT[obs_acc == 1],
         prob = F, col = "green",
         breaks = 50,
         xlab="Reaction time", ylab="Occurence", main = "")
    abline(v = quantiles_cor, col = "green")
    hist(obs_RT[obs_acc == 0], , add=TRUE,
         prob = F, col = "red",
         breaks = 50, main = "")
    abline(v = quantiles_incor, col = "red")
  }
  
  # The distributions show the reaction times for the correct (green) and 
  # incorrect choices (red). The lines are the edges of the quantiles, which 
  # contain a proportion of 0.1, 0.3, 0.5, 0.7 and 0.9 of all trials in each.
  
  # Q12 are the quantiles different for the correct and incorrect trials? Why 
  # do you think so?
  
  # now we use the same quantiles but instead of plotting the observed data, we 
  # plot the predicted data we made from our set of parameters.
  if(plotting)
  {
  hist(pred_RT[pred_acc == 1],
       prob = F, col = "green",
       breaks = 50,
       xlab="Reaction time", ylab="Occurence",, main = "")
  abline(v = quantiles_cor, col = "green")
  hist(pred_RT[pred_acc == 0], , add=TRUE,
       prob = F, col = "red",
       breaks = 50, main = "")
  abline(v = quantiles_incor, col = "red")
  
  par(mfrow = c(1,1))
  }
  
  # This plot shows the same lines (representing the quantiles), but different
  # distributions. These distributions come not from the observed, but from
  # the predicted data based on our parameters.
  
  # Q13 compare the histogram with the observed data to the histogram with the 
  # predicted data. Does the same proportion of trials fall within the quantiles
  # for both sets of data? If not, how are they different?
  
  # now we will compute the number of responses that fall within the quantiles
  # for both observed and predicted data.
  
  # for observed data
  C_prob <- ecdf(obs_RT[obs_acc == 1]) # get cumulative probability distribution
  obs_prop_cor <- diff(c(0,C_prob(quantiles_cor),1)) * length(obs_RT[obs_acc == 1]) # sample proportion of probability distribution within quantiles

  C_prob <- ecdf(obs_RT[obs_acc == 0]) # same for observed incorrect
  obs_prop_incor <- diff(c(0,C_prob(quantiles_incor),1)) * length(obs_RT[obs_acc == 0])
  
  # obs_prop_cor and obs_prop_incor tell us how many trials fall within each quantile
  # for the correct and incorrect distributions of observed data.

  # now we compute the same for the predicted data
  # Note that because these data are generated, we sometimes have very little 
  # (in)correct trials, which we have to account for by setting the proportion 
  # to zero. Hence, the if statements.
  
  if(sum(pred_acc == 1) > 5){ 

  C_prob <- ecdf(pred_RT[pred_acc == 1])
  pred_prop_cor <- diff(c(0,C_prob(quantiles_cor),1)) * length(pred_RT[pred_acc == 1])
  } else{ # less than 5 correct responses: no quantiles possible
    pred_prop_cor <- c(0,0,0,0,0)}


  if(sum(pred_acc == 0) > 5){
  C_prob <- ecdf(pred_RT[pred_acc == 0]) 
  pred_prop_incor <- diff(c(0,C_prob(quantiles_incor),1)) * length(pred_RT[pred_acc == 0])
  } else{ # less than 5 errors: no quantiles possible
    pred_prop_incor <- c(0,0,0,0,0)}
  
  # change 0 proportions to very small number to prevent Inf output
  pred_prop_cor[pred_prop_cor == 0] <- 0.0001
  pred_prop_incor[pred_prop_incor == 0] <- 0.0001
  
  # we now have the number of trials within each quantile for the 4 distributions
  # (observed/predicted and correct/incorrect). But because we generated more 
  # predicted data than we actually observed, we have to correct for that by
  # dividing over the number of trials we have.
  
  # control for number of trials in observed and predicted data being different
  obs_prop_cor <- obs_prop_cor / length(obs_RT)
  obs_prop_incor <- obs_prop_incor / length(obs_RT)
  pred_prop_cor <- pred_prop_cor / length(pred_RT)
  pred_prop_incor <- pred_prop_incor / length(pred_RT)
  
  # Q14 compare obs_prop_cor to pred_prop_cor, the proportion of correct trials
  # in each quantile for the observed and predicted data. Which quantile has the 
  # highest proportion of trials for both?
  obs_prop_cor
  pred_prop_cor
  
  # we can see that the proportion of trials is not equal for the observed and 
  # predicted data, but how can we quantify how much they differ? For this,
  # we compute the chi-square, as below:

  # get chi-square measure as cost
  
  # compute chi square using proportions of correct and incorrect trials in observed
  # and predicted data
  Chi_square <- sum(c((obs_prop_cor - pred_prop_cor)^2 / pred_prop_cor, (obs_prop_incor - pred_prop_incor)^2 / pred_prop_incor), na.rm=TRUE)

  # Q15 how would the chi-square change if the number of (in)correct trials in 
  # a quantile become more similar? How about if they became more different?
  
  return(Chi_square)
}

# end of Cost_ddm function, return to the main script (DDM_tutorial.R)
#------------------------------------------------------------------------------#

# single iteration of fitting procedure, generate data given set of parameters and calculate cost
# input: observations (2xntrials matrix, column 1 RT, column 2 accuracy)
Iterate_fit <- function(params, Observations, CC, ntrials = 1000){
  
  names(params) <- c('v', 'a', 'ter')
  
  sim_data <-DDM_3params(v = params['v'],a = params['a'],ter=params['ter'], CC, ntrials = ntrials)
  
  cost<-Cost_ddm(Observations[ ,1], Observations[ ,2], sim_data$Data[ ,1], sim_data$Data[ ,2],0) # no plotting
  
  return(cost)
  
}
