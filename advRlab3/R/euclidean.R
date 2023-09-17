euclidean <-
function(input1, input2){
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
