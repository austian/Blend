#' @title Loss function
#' 
#' @description
#' Computes the root mean squared difference or percentage difference of two numeric or two two-level factors respectively.
#' 
#' @details
#' The function takes two vectors, which it expects to be of the same type, and computes the root mean squared difference
#' in the numeric case and the percentage difference in the two-level factor case.
#' 
#' @param vector1 first vector
#' @param vector2 second vector
#' 
#' @author Alex Ustian <alex.l.ustian@@gmail.com>
#' @export
loss <- function(vector1, vector2)  {
  if(class(vector1) != class(vector2))  {
    print("Incompatible data types")
  } 
  else if(class(vector1) == "numeric")  {
    diff <- vector1 - vector2
    sq <- diff*diff
    return(sqrt(mean(sq)))
  } 
  else if(class(vector1) == "factor")  {
    mean(!(vector1 == vector2))
  }
}