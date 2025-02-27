# calculating cost of observed and predicted data
# based on RT as well as probability correct
# input: observed RT, observed probability correct, predicted RT, predicted probability correct
# output: cost (chi square)

Cost_ddm <- function(obs_RT, obs_acc, pred_RT, pred_acc){
  
  # define quantiles from observed RTs
  quantiles_cor <- quantile(obs_RT[obs_acc == 1], probs = c(.1,.3,.5,.7,.9), names = FALSE)
  quantiles_incor <- quantile(obs_RT[obs_acc == 0], probs = c(.1,.3,.5,.7,.9), names = FALSE)

  # plot dists & quantiles, ask question
  
  # get proportion of responses within these quantiles for observed and predicted
  C_prob <- ecdf(obs_RT[obs_acc == 1]) # get cumulative probability distribution
  obs_prop_cor <- diff(c(0,C_prob(quantiles_cor),1)) * length(obs_RT[obs_acc == 1]) # sample proportion of probability distribution within quantiles

  C_prob <- ecdf(obs_RT[obs_acc == 0]) # same for observed incorrect
  obs_prop_incor <- diff(c(0,C_prob(quantiles_cor),1)) * length(obs_RT[obs_acc == 0])

  # same for predicted RTs
  if(sum(pred_acc == 1) > 5){ 

  C_prob <- ecdf(pred_RT[pred_acc == 1])
  pred_prop_cor <- diff(c(0,C_prob(quantiles_cor),1)) * length(pred_RT[pred_acc == 1])}
  
  else{ # less than 5 correct responses: no quantiles possible
    pred_prop_cor <- c(0,0,0,0,0)}


  if(sum(pred_acc == 0) > 5){
  C_prob <- ecdf(pred_RT[pred_acc == 0]) 
  pred_prop_incor <- diff(c(0,C_prob(quantiles_cor),1)) * length(pred_RT[pred_acc == 0])}
  else{ # less than 5 errors: no quantiles possible
    pred_prop_incor <- c(0,0,0,0,0)}
  

  # get chi-square measure as cost
  
  # change 0 proportions to very small number to prevent Inf output
  pred_prop_cor[pred_prop_cor == 0] <- 0.0001
  pred_prop_incor[pred_prop_incor == 0] <- 0.0001
  
  # compute chi square using both correct and incorrect trials
  Chi_square <- sum(c((obs_prop_cor - pred_prop_cor)^2 / pred_prop_cor, (obs_prop_incor - pred_prop_incor)^2 / pred_prop_incor), na.rm=TRUE)

  return(Chi_square)
}

# single iteration of fitting procedure, generate data given set of parameters and calculate cost
# input: observations (2xntrials matrix, column 1 RT, column 2 accuracy)
Iterate_fit <- function(params, Observations){
  
  names(params) <- c('v', 'a', 'ter')
  
  sim_data <-DDM_3params(v = params['v'],a = params['a'],ter=params['ter'])
  
  cost<-Cost_ddm(Observations[ ,1], Observations[ ,2], sim_data$Data[ ,1], sim_data$Data[ ,2])
  
  return(cost)
  
}