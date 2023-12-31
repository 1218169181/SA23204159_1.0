# Generated by using Rcpp::compileAttributes() -> do not edit by hand
# Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#' @title  construct studentized bootstrap CI
#' @description  construct studentized bootstrap CI
#' @param X the variable that you want to construct CI,must be a vector
#' @param B the number of times that replicate
#' @return lower and upper bound of CI
#' @examples
#' \dontrun{
#' data(lung)
#' attach(lung)
#' ##construct CI for patients' survival month,use lung data
#' data<-lung$存活时间
#' studentized_bootstrap_ci(data)
#' }
#' @export
studentized_bootstrap_ci_C <- function(X, B = 500L) {
    .Call('_SA23204159_studentized_bootstrap_ci_C', PACKAGE = 'SA23204159', X, B)
}

