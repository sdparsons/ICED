# str2cov converts a ICED measurement structure data.frame and a vector
# of variances into an expected covariance matrix

str2cov <- function(structure,
                    variances) {
  
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
        temp[i,j] <- variances$error
      }
    }
  }
  time1 <- time1 + temp
  
  # return the expected covariance matrix
  
  return(time1)
  
}