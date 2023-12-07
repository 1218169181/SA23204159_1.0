#include <Rcpp.h>
#include <iostream>
#include <algorithm>
#include<stdlib.h>
#include <cstdlib>

using namespace Rcpp;

//' @title  construct studentized bootstrap CI
//' @description  construct studentized bootstrap CI
//' @param X the variable that you want to construct CI,must be a vector
//' @param B the number of times that replicate
//' @return lower and upper bound of CI
//' @examples
//' \dontrun{
//' data(lung)
//' attach(lung)
//' ##construct CI for patients' survival month,use lung data
//' data<-lung$存活时间
//' studentized_bootstrap_ci(data)
//' }
//' @export
// [[Rcpp::export]]
NumericVector studentized_bootstrap_ci_C(NumericVector X, int B=500) {
  NumericVector ci(2);
  int n=X.size();
  NumericVector theta_star(B);
  NumericVector se_star(B);
  NumericVector x_star(n);
  NumericVector theta_star_second(B);
  NumericVector x_star_second(n);
  int ind1,ind2;
  double sum;
  double mean;
  int ind;
  double variance;
  for(int i = 0; i < B; i++) {
    for(int j=0;j<n;j++){
      ind=0 + rand()%n;
      x_star[j]=X[ind];
    }
    sum = std::accumulate(std::begin(x_star), std::end(x_star), 0.0);
    theta_star[i]=sum/n;
    for(int k=0;k<B;k++){
      for(int j=0;j<n;j++){
      ind=rand()%n;
      x_star_second[j]=x_star[ind];
    }
    sum = std::accumulate(std::begin(x_star_second), std::end(x_star_second), 0.0);
    theta_star_second[k]=sum/n;
    }
    sum = std::accumulate(std::begin(theta_star_second), std::end(theta_star_second), 0.0);
    mean=sum/B;
    variance = 0.0;
    std::for_each(std::begin(theta_star_second), std::end(theta_star_second), [&](const double d) {
        variance += pow(d-mean, 2);
    });
    variance /= B;
    se_star[i]=sqrt(variance);
  }
  double theta;
  sum = std::accumulate(std::begin(X), std::end(X), 0.0);
  theta=sum/n;

  sum = std::accumulate(std::begin(theta_star), std::end(theta_star), 0.0);
  mean=sum/B;
  variance = 0.0;
  std::for_each(std::begin(theta_star_second), std::end(theta_star_second), [&](const double d) {
      variance += pow(d-mean, 2);
  });
  variance /= B;
  double se1=sqrt(variance);
  NumericVector t(B);
  for(int k=0;k<B;k++){
      t[k]=(theta_star[k]-theta)/se_star[k];
  }
  std::sort(t.begin(),t.end());
  ind1=round(0.025*n-1);
  ind2=round(0.975*n-1);
  double q1=t[ind1];
  double q2=t[ind2];
  ci[0]=theta-q2*se1;
  ci[1]=theta-q1*se1;
  return(ci);
}

