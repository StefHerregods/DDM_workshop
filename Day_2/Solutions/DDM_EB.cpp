#include <Rcpp.h>
// [[Rcpp::depends(RcppZiggurat)]]
#include <Ziggurat.h>
using namespace Rcpp;
static Ziggurat::Ziggurat::Ziggurat zigg;

// [[Rcpp::export]]
NumericMatrix DDM_2DSD_EB_YV(double v, double a, double ter, double a2, int ntrials = 10000, double s = 1, double dt = 0.01) { 
  
  // v: drift rate
  // a: bound
  // ter: non-decision time
  // tau: confidence boundary
  
  // ntrials: number of simulated trials
  
  // s: within trial noise
  // dt: precision (in seconds)
  
  // Initialize matrices for output
  NumericMatrix DATA(ntrials, 4);
  
  // Loop over trials
  for (int i = 0; i < ntrials; i++) {
    
    // Assuming a balanced design (50% with stimulus A, 50% with stimulus B)
    if (zigg.norm() > 0){
      v = -1 * v;
    }
    
    // Initialize variables
    int acc = -1;  // Accuracy
    int choice = -1;  // Choice (1 = upper bound reached, 0 = lower bound reached)
    int cj = -1;  // Confidence judgment
    double evidence = 0;  // Accumulated evidence
    double t = 0;  // Decision RT
    double t2 = 0;  // Confidence RT
    double SP_conf = a; // Starting point of confidence accumulation

    // Decisional processing
    while (evidence < a && evidence >-a){
      
      t = t + dt;

      // After the non-decision time, evidence begins accumulating 
      if (t >= ter) {
        evidence = evidence + v * dt + s * sqrt(dt) * zigg.norm();
      } else if (t < ter) {
        evidence = evidence; 
      }
      
      // If either boundary is hit, determine accuracy of the decision using drift rate
      if (evidence >= a){
        if(v>0) {acc = 1;}
        if(v<0) {acc = 0;}
        choice = 1;
        SP_conf = a;
        break;
      } else if (evidence <= -a) {
        if(v>0) {acc = 0;}
        if(v<0) {acc = 1;}
        choice = 0;
        SP_conf = -a;
        break;
      }
    }
    
    // Post-decisional processing for interjudgment time tau
    while (evidence < SP_conf + a2 && evidence > SP_conf - a2){ // keep accumulating untill either the uppper or lower boundary is hit
      t2 = t2 + dt;
      evidence = evidence + v * dt + s * sqrt(dt) * zigg.norm();
    }
    
    // A high confidence judgment is given if evidence towards the choice increased
    // A low confidence judgment is given if evidence towards the choice decreased
    if (choice == 1){
      if (evidence > SP_conf + a2){
        cj = 1;
      } else if (evidence < SP_conf - a2) {
        cj = 0;
      }
    } else if (choice == 0){
      if (evidence < SP_conf - a2){
        cj = 1;
      } else if (evidence > SP_conf + a2){
        cj = 0;
      }
    }
    
    // Add results to the matrix
    DATA(i,0) = t;  // Decision RT
    DATA(i,1) = acc;  // Accuracy
    DATA(i,2) = t2;  // Confidence RT
    DATA(i,3) = cj;  // Confidence judgment
  }
  
  return DATA; 
}
