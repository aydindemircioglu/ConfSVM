#' ConfSVM: Conformal SVM 
#'
#' ConfSVM package implements a kernel learning via conformal scaling (see [1]).
#' The conformal scaling is applied by using the method proposed by Williams (see [2]).
#' We further speeding up kernel learning via the divide-and-conquer approach (see [3]).
#' This yields a substantial speed-up with similar accuracy.
#'
#' The underlying ideas are in the following papers:
#' 
#' 1. Wu, S., & Amari, S. I. (2002). Conformal transformation of kernel functions: A data-dependent way to improve support vector machine classifiers. Neural Processing Letters, 15(1), 59-67.
#  2. Williams, P., Li, S., Feng, J., & Wu, S. (2007). A geometrical method to improve performance of the support vector machine. Neural Networks, IEEE Transactions on, 18(3), 942-947.
#' 3. Hsieh, C. J., Si, S., & Dhillon, I. S. (2013). A divide-and-conquer solver for kernel support vector machines. arXiv preprint arXiv:1311.0914.
#'
#' @section ConfSVM functions:
#' confSVM
#' confDCSVM
#'
#' @docType package
#' @name ConfSVM
NULL
