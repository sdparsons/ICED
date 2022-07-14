#' converts a ICED measurement structure data.frame and a vector
# of variances into an expected covariance matrix
#' 
#' helper function to generate an expected covariance matrix from an ICED measurement structure and vector of variances. Not expected to be called directly, but used within sim_ICED
#' 
#' @param structure data.frame describing the structure of the data, with each variable convering a design aspect - see example. Note: currently the first variable must be time and include a different value for each repeated measure.
#' @param variances list of variances for each source of variance
#' @param e_label sting label for error variance. defaults to "e"
#' 
#' @return returns a matrix
#' 
#' @export
#' 
#' @examples
#' \dontrun{
#' struc <- data.frame(time = c("T1", "T2", "T3", "T4"),
#' day = c("day1","day1","day2","day2"),
#' session = c("ses1", "ses1","ses2", "ses3"))
#'  
#' str2cov(struc,
#' list(time = 10,
#'      day = 2,
#'      session = 1,
#'      e = 3))
#' }


str2cov <- function(structure,
                    variances,
                    e_label = "e") {
  
  
  # tests
  
  if(!is.data.frame(structure)) {warning("structure must be a data.frame")}
  if(!is.list(variances)) {warning("variances must be a list")}
  if((ncol(structure)+1) != length(variances)) {warning("structure must contain 1 fewer variable than the list of variances (structure excludes error)")}
  if(!all(c(colnames(structure),e_label) ==  names(variances))) {warning("variances list must contain the same variable names as structure, plus e_label")}
  
  # other tests to add

  
  # time variance component
  time1 <- matrix(rep(variances$time, nrow(structure)*nrow(structure)),
                  nrow = nrow(structure),
                  ncol = nrow(structure),
                  dimnames = list(structure$time,
                                  structure$time))
  
  # other (non error) variance components
  latents <- c(colnames(structure)[-1])
  
  # for loop creates new matrix for each latent and adds to the existing
  # covariance matrix
  for(lat in latents){
    temp <- matrix(rep(0, nrow(structure)*nrow(structure)),
                   nrow = nrow(structure),
                   ncol = nrow(structure),
                   dimnames = list(structure$time,
                                   structure$time))
    
    for(i in 1:nrow(time1)) {
      for(j in 1:ncol(time1)) {
        if(structure[i,lat] == structure[j,lat]) {
          temp[i,j] <- variances[[lat]]
        }
      }
    }
    
    time1 <- time1 + temp
    
  }
  
  # error variance
  temp <- matrix(rep(0, nrow(structure)*nrow(structure)),
                 nrow = nrow(structure),
                 ncol = nrow(structure),
                 dimnames = list(structure$time,
                                 structure$time))
  
  for(i in 1:nrow(temp)) {
    for(j in 1:ncol(temp)) {
      if(i == j) {
        temp[i,j] <- variances[[e_label]]
      }
    }
  }
  time1 <- time1 + temp
  
  # return the expected covariance matrix
  
  return(time1)
  
}
