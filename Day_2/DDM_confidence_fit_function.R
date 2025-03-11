# calculating cost of observed and predicted data
# input: observed RT, observed accuracy, observed RTconf, observed CJ, predicted RT, predicted accuracy, predicted RTconf, predicted CJ
# output: cost (chi square)

Cost_ddm_confidence <- function(obs_RT, obs_acc, obs_RTconf, obs_cj, pred_RT, pred_acc, pred_RTconf, pred_cj, plotting = F){
  
  # define quantiles from observed RTs and confidence RTs
  quantiles_cor <- quantile(obs_RT[obs_acc == 1], probs = c(.1,.3,.5,.7,.9), names = FALSE)
  quantiles_incor <- quantile(obs_RT[obs_acc == 0], probs = c(.1,.3,.5,.7,.9), names = FALSE)
  quantiles_highCJ <- quantile(obs_RTconf[obs_cj == 1], probs = c(.1,.3,.5,.7,.9), names = FALSE)
  quantiles_lowCJ <- quantile(obs_RTconf[obs_cj == 0], probs = c(.1,.3,.5,.7,.9), names = FALSE)
  
  if(plotting)
  {
    par(mfrow = c(2,2))
    
    # plot the distributions of correct and incorrect observed decision RT's and add the quantiles
    hist(obs_RT[obs_acc == 1],
         prob = F, col = "green",
         breaks = 50,
         xlab="Reaction time", ylab="Occurence", main = "Observed decision RT's")
    abline(v = quantiles_cor, col = "green")
    hist(obs_RT[obs_acc == 0], add=TRUE,
         prob = F, col = "red",
         breaks = 50, main = "")
    abline(v = quantiles_incor, col = "red")
    
    # plot the distributions of correct and incorrect simulated decision RT's and add the quantiles
    hist(pred_RT[pred_acc == 1],
         prob = F, col = "green",
         breaks = 50,
         xlab="Reaction time", ylab="Occurence", main = "Simulated decision RT's")
    abline(v = quantiles_cor, col = "green")
    hist(pred_RT[pred_acc == 0], add=TRUE,
         prob = F, col = "red",
         breaks = 50, main = "")
    abline(v = quantiles_incor, col = "red")
    
    # plot the distributions of correct and incorrect observed confidence RT's and add the quantiles
    hist(obs_RTconf[obs_cj == 1],
         prob = F, col = "orange",
         breaks = 50,
         xlab="Reaction time", ylab="Occurence", main = "Observed confidence RT's")
    abline(v = quantiles_highCJ, col = "orange")
    hist(obs_RTconf[obs_cj == 0], add=TRUE,
         prob = F, col = "blue",
         breaks = 50, main = "")
    abline(v = quantiles_lowCJ, col = "blue")
    
    # plot the distributions of correct and incorrect simulated confidence RT's and add the quantiles
    hist(pred_RTconf[pred_cj == 1],
         prob = F, col = "orange",
         breaks = 50,
         xlab="Reaction time", ylab="Occurence", main = "Simulated confidence RT's")
    abline(v = quantiles_highCJ, col = "orange")
    hist(pred_RTconf[pred_cj == 0], add=TRUE,
         prob = F, col = "blue",
         breaks = 50, main = "")
    abline(v = quantiles_lowCJ, col = "blue")
    
    par(mfrow = c(1,1))
  }
  
  # Now we will compute the number of responses that fall within the quantiles
  # for both observed and predicted data.
  
  # For observed data
  C_prob <- ecdf(obs_RT[obs_acc == 1]) # get cumulative probability distribution
  obs_prop_cor <- diff(c(0,C_prob(quantiles_cor),1)) * length(obs_RT[obs_acc == 1]) # sample proportion of probability distribution within quantiles
  
  C_prob <- ecdf(obs_RT[obs_acc == 0]) # same for observed incorrect decision RT's
  obs_prop_incor <- diff(c(0,C_prob(quantiles_incor),1)) * length(obs_RT[obs_acc == 0])
  
  C_prob <- ecdf(obs_RTconf[obs_cj == 1]) # same for observed high cj confidence RT's
  obs_prop_highCJ <- diff(c(0,C_prob(quantiles_highCJ),1)) * length(obs_RTconf[obs_cj == 1]) 
  
  C_prob <- ecdf(obs_RTconf[obs_cj == 0]) # same for observed low cj confidence RT's
  obs_prop_lowCJ <- diff(c(0,C_prob(quantiles_lowCJ),1)) * length(obs_RTconf[obs_cj == 0])
  
  # for predicted data (note that because these data are generated, we sometimes 
  # have very little (in)correct or low confidence trials, which we have to account for by setting 
  # the proportion to zero. Hence, the if statements)
  
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
  
  if(sum(pred_cj == 1) > 5){
    C_prob <- ecdf(pred_RTconf[pred_cj == 1])
    pred_prop_highCJ <- diff(c(0,C_prob(quantiles_highCJ),1)) * length(pred_RTconf[pred_cj == 1])
  } else{ # less than 5 high CJ responses: no quantiles possible
    pred_prop_highCJ <- c(0,0,0,0,0)}
  
  if(sum(pred_cj == 0) > 5){
    C_prob <- ecdf(pred_RTconf[pred_cj == 0]) 
    pred_prop_lowCJ <- diff(c(0,C_prob(quantiles_lowCJ),1)) * length(pred_RTconf[pred_cj == 0])
  } else{ # less than 5 low CJ responses: no quantiles possible
    pred_prop_lowCJ <- c(0,0,0,0,0)}
  
  
  # change 0 proportions to very small number to prevent Inf output
  pred_prop_cor[pred_prop_cor == 0] <- 0.0001
  pred_prop_incor[pred_prop_incor == 0] <- 0.0001
  pred_prop_highCJ[pred_prop_highCJ == 0] <- 0.0001
  pred_prop_lowCJ[pred_prop_lowCJ == 0] <- 0.0001
  
  # control for number of trials in observed and predicted data being different
  obs_prop_cor <- obs_prop_cor / length(obs_RT)
  obs_prop_incor <- obs_prop_incor / length(obs_RT)
  obs_prop_highCJ <- obs_prop_highCJ / length(obs_RTconf)
  obs_prop_lowCJ <- obs_prop_lowCJ / length(obs_RTconf)
  pred_prop_cor <- pred_prop_cor / length(pred_RT)
  pred_prop_incor <- pred_prop_incor / length(pred_RT)
  pred_prop_highCJ <- pred_prop_highCJ / length(pred_RTconf)
  pred_prop_lowCJ <- pred_prop_lowCJ / length(pred_RTconf)
  
  # we can see that the number of trials is not equal for the observed and 
  # predicted data, but how can we quantify how much they differ? For this,
  # we compute the chi-square, as below:
  
  # get chi-square measure as cost
  
  # compute chi square using both correct and incorrect trials
  Chi_square <- sum(c((obs_prop_cor - pred_prop_cor)^2 / pred_prop_cor, (obs_prop_incor - pred_prop_incor)^2 / pred_prop_incor, (obs_prop_highCJ - pred_prop_highCJ)^2 / pred_prop_highCJ, (obs_prop_lowCJ - pred_prop_lowCJ)^2 / pred_prop_lowCJ), na.rm=TRUE)

  # Q15 how would the chi-square change if the number of (in)correct trials in 
  # a quantile become more similar?
  
  return(Chi_square)
}

