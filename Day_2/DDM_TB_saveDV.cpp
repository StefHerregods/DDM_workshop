#include <Rcpp.h>
// [[Rcpp::depends(RcppZiggurat)]]
#include <Ziggurat.h>
using namespace Rcpp;
static Ziggurat::Ziggurat::Ziggurat zigg;

// [[Rcpp::export]]
List DDM_TB_saveDV(double v, double a, double ter, double tau, int ntrials = 10000, double s = 1, double dt = 0.01) { 
  
  // v: drift rate
  // a: bound
  // ter: non-decision time
  // tau: interjudgment time
  
  // ntrials: number of simulated trials
  
  // s: within trial noise
  // dt: precision (in seconds)
  
  // Initialize matrices for output
  NumericMatrix DATA(ntrials, 5);
  NumericMatrix DVs(ntrials,(10/dt)); // initialize matrix with length 10 seconds
  NumericMatrix time(ntrials, (10/dt)); 
  
  // Loop over trials
  for (int i = 0; i < ntrials; i++) {
    
    // Assuming a balanced design (50% with stimulus A, 50% with stimulus B)
    if (zigg.norm() > 0){
      v = -1 * v;
    }
    
    // Initialize variables
    int acc = -1;  // Accuracy
    int choice = -1;  // Choice (1 = upper bound reached, -1 = lower bound reached)
    int cj = -1;  // Confidence judgment
    double evidence = 0;  // Accumulated evidence
    double t = 0;  // Decision RT
    int t_idx = 0;
    double t2 = 0;  // Confidence RT
    
    // Decisional processing
    while (evidence < a && evidence >-a){
      
      t = t + dt;
      t_idx = t_idx +1;

      // After the non-decision time, evidence begins accumulating 
      if (t >= ter) {
        evidence = evidence + v * dt + s * sqrt(dt) * zigg.norm();
      } else if (t < ter) {
        evidence = evidence; 
      }
      
      if((i<20) & (t < 3)){
        time(i,t_idx) = t;
        DVs(i,t_idx) = evidence;}
      
      // If either boundary is hit, determine accuracy of the decision using drift rate
      if (evidence >= a){
        if(v>0) {acc = 1;}
        if(v<0) {acc = 0;}
        choice = 1;
        break;
      } else if (evidence <= -a) {
        if(v>0) {acc = 0;}
        if(v<0) {acc = 1;}
        choice = -1;
        break;
      }
    }
    
    // Post-decisional processing for interjudgment time tau
    while (t2 < tau){
      t2 = t2 + dt;
      t_idx = t_idx +1;
      evidence = evidence + v * dt + s * sqrt(dt) * zigg.norm();
      
      if((i<20) & (t < 5)){
        time(i,t_idx) = t+t2;
        DVs(i,t_idx) = evidence;}
    }
    
    // A high confidence judgment is given if evidence towards the choice increased
    // A low confidence judgment is given if evidence towards the choice decreased
    if (choice == 1){
      if (evidence > a){
        cj = 1;
      } else {
        cj = 0;
      }
    } else if (choice == -1){
      if (evidence < -a){
        cj = 1;
      } else {
        cj = 0;
      }
    }
    
    // Add results to the matrix
    DATA(i,0) = t;  // Decision RT
    DATA(i,1) = choice; // Choice
    DATA(i,2) = acc;  // Accuracy
    DATA(i,3) = t2;  // Confidence RT
    DATA(i,4) = cj;  // Confidence judgment
  }
  
  List return_vars;
  return_vars["DVs"] = DVs;
  return_vars["time"] = time;
  return_vars["Data"] = DATA;
  
  return return_vars; 
}
