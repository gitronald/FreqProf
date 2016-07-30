#' Generate a Sequence of Dates
#' 
#' @param startDate a character string in the format "YYYY-MM-DD"
#' @param nDays length of output vector
#' @param by days or weeks
#'   
#' @return Sequence of dates starting at startDate with length nDays by daily or
#'   weekly increments
#' @export
#' 
#' @examples 
#' generate_dates("2016-06-05", 31, by = "day")
#'
generate_dates = function(startDate, nDays, by = "day") {

startDate = as.Date(startDate)
nDays = nDays
data1 = seq(startDate, by = by, length.out = nDays)
return(data1)
}


#' Generate Random Binary Vectors
#'
#' @param nBehaviors number of columns to generate
#' @param nrow bumber of rows to generate
#' @param prStart starting probability
#' @param prEnd ending probability
#' @param prBy probability to sequence by
#' 
#' @importFrom stats rbinom
#'
#' @return a matrix of binary values
#' @export
#'
#' @examples
#' generate_behaviors(10, 10)
generate_behaviors = function(nrow, nBehaviors,
                            prStart = 0.1, prEnd = 0.3, prBy = 0.001){

pr = sample(seq(prStart, prEnd, prBy), 
            nBehaviors, 
            replace = T)
data1 = sapply(pr, function(x) rbinom(nrow, 1, x))
return(data1)
}
