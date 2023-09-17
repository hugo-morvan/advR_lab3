#' Euclidean's Algorithmm
#'

#' @description This function calculates the greatest common divisor, that is the
#' number that divides both inputs without remainder
#' @param input1 a numeric scalar
#' @param input2 a numeric scalar
#' @return The function returns the greatest common divisor of the given two inputs
#' @seealso \href{https://en.wikipedia.org/wiki/Euclidean_algorithm}{Euclidean Algorithm}
#' @export
#' @examples
#' euclidean(123612, 13892347912)

euclidean <- function(input1, input2){
  # Asserting that inputs are numeric scalars
  stopifnot(
    is.numeric(input1), is.vector(input1), length(input1) == 1,
    is.numeric(input2), is.vector(input2), length(input2) == 1
  )

  min_inp <- min(input1, input2) #the max from the input
  max_inp <- max(input1, input2) #the min from the input
  remainder <- max_inp %% min_inp #initial remainder

  repeat{
    if (remainder != 0){
      max_inp <- min_inp
      min_inp <- remainder
      remainder <- max_inp %% min_inp
    } else{
      return(abs(min_inp))
      break
    }
  }
}
