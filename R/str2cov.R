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
#' str2cov(struc,
#' list(time = 10,
#'      day = 2,
#'      session = 1,
#'      error = 3))
#' }


str2cov <- function(structure,
                    variances,
                    e_label) {
  
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