// in r: install Rcpp library and ZigguratRCPP library, then source this script and the function becomes available (sourceCpp("DDM_basic.cpp"))
#include <Rcpp.h>
// [[Rcpp::depends(RcppZiggurat)]]
#include <Ziggurat.h>
using namespace Rcpp;
static Ziggurat::Ziggurat::Ziggurat zigg;

// [[Rcpp::export]]
List DDM_3params_withData(double v, double a, double ter, NumericVector CC, int ntrials = 1000, double s = 1, double dt = 0.01) { 
  
  // parameter names:
  // v: drift rate
  // a: bound
  // ter: non-decision time

  // constants:
  // ntrials: number of simulated trials
  
  // fixed variables:
  // s: within trial noise
  // dt: precision (in seconds)

  // initialize matrices for output
  NumericMatrix DATA(ntrials,2);
  NumericMatrix DVs(ntrials,(10/dt)); // initialize matrix with length 10 seconds
  NumericMatrix time(ntrials, (10/dt)); 
  
  // loop over trials
  for (int i = 0; i < ntrials; i++) {
    
    // make drift rate negative for half of the trials
    // use experimental design to determine negative/positive
    v = CC[i] * v;
    
    // initialize variables
    int acc = 0;
    double evidence = 0;
    double t = 0;
    int t_idx = 0;
    DVs(i,t_idx) = evidence;
    
    // Decisional processing
    while (evidence < a && evidence >-a){
      
      t = t + dt;
      t_idx = t_idx +1;
      
      // After the non-decision time, evidence begins accumulating (plus noise)
      if (t >= ter) {
        evidence = evidence + v * dt + s * sqrt(dt) * zigg.norm();
      } else if (t < ter) {
        evidence = evidence; 
      }
      
      // Save time and DV only for the first 20 trials and max 3 seconds
      if((i<20) & (t < 3)){
      time(i,t_idx) = t;
      DVs(i,t_idx) = evidence;}
      
      // if either boundary is hit, determine accuracy of the decision using drift rate
      if (evidence >= a){
        //DVs(i,t_idx) = a;
        if(v>0) {acc = 1;}
        if(v<0) {acc = 0;}
        break;
      } else if (evidence <= -a) {
        //DVs(i,t_idx) = -a;
        if(v>0) {acc = 0;}
        if(v<0) {acc = 1;}
        break;
      }
    }
    
    // save the time and accuracy of the decision
    DATA(i,0) = t;
    DATA(i,1) = acc;
    
  }
  
  // return the DVs, reaction times and accuracy 
  List return_vars;
  return_vars["DVs"] = DVs;
  return_vars["time"] = time;
  return_vars["Data"] = DATA;
  
  return return_vars; 
}
