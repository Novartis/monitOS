#include <Rcpp.h>
using namespace Rcpp;


// [[Rcpp::export]]
double mod2(double a){
  return a - floor(a/2.0)*2.0;
}

// [[Rcpp::export]]
double sum_sug(NumericVector x){
  return sum(x);
}

// // [[Rcpp::export]]
// void rcpp_rprintf(NumericVector v){
//   // printing values of all the elements of Rcpp vector
//   for(int i=0; i<v.length(); ++i){
//     Rprintf("%f ", v[i]);
//   }
// }

// [[Rcpp::export]]
double exactt(double events, double n0, double n1, double crit, double hr){

  NumericVector vec(events);
  for (int i = 0; i < events; i++) {
    vec[i] = pow(2, events - i - 1);
  }

  NumericVector output(events);
  int nump = pow(2, events);
  NumericVector pvalue(nump);
  double logrank_num = 0;
  double logrank_den = 0;
  double prob = 1;
  double logrank;
  double atRiskT;
  double atRiskC;
  double prob0;

  for(int i = 0; i < nump; i++) {
    logrank_num = 0;
    logrank_den = 0;
    prob = 1;

    for (int k = 0; k < events; k++) {
      output[k] = mod2(floor(i / vec[k]));
      // Rprintf("i: %i, vec[k]: %f, output[k]: %f \n",i,  vec[k], output[k]);
    }

    for (int k = 0; k < events; k++) {
      if (k == 0) {
        atRiskT = n1;
        atRiskC = n0;
      } else {
        NumericVector s(k);
        s = seq(0, k - 1);

        atRiskT = n1 - sum_sug(output[s]);
        atRiskC = n0 - k + sum_sug(output[s]);
      }
      prob0 = atRiskT / (atRiskC + atRiskT);
      logrank_num = logrank_num + (output[k] - prob0);
      logrank_den = logrank_den + prob0 * (1 - prob0);
      prob = prob * ((hr * atRiskT * output[k] + atRiskC * (1 - output[k])) / (hr * atRiskT + atRiskC));
      // Rprintf("k: %i, atRiskT: %f, atRiskC: %f \n", k,  atRiskT, atRiskC);
    }

    logrank = logrank_num / sqrt(logrank_den);
    if (logrank > crit) {
      pvalue[i] = prob;
    } else {
      pvalue[i] = 0.0;
    }
    // Rprintf("\n %f ", pvalue[i]);
  }

  double res = sum_sug(pvalue);
  return res;
}
