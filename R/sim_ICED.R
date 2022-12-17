#' simulates data based on ICED model structure and list of variances
#' 
#'  sim_ICED simulates n x p data frame based on ICED model structure,
#' selected variance components, and specified n
#' @param structure data.frame describing the structure of the data, with each variable covering a design aspect - see example. Note: currently the first variable must be time and include a different value for each repeated measure.
#' @param variances list of variances corresponding to each latent variable specified in strucutre
#' @param n number of participants to simulate
#' @param check_recovery runs run_ICED to extract variance components in order to check the variance parameter recovery
#' @return list including simulated data
#' 
#' @examples
#' # compare recovery of variance parameters
#' 
#' # ICED structure
#' struc <- data.frame(time = c("T1", "T2", "T3", "T4"),
#' day = c("day1","day1","day2","day2"),
#' session = c("ses1", "ses1","ses2", "ses3"))
#'
#' sim_ICED(struc,
#' variances = list(time = 10,
#'                  day = 2,
#'                  session = 1,
#'                  error = 3),
#' n = 20,
#' check_recovery = TRUE)
#' 
#' sim_ICED(struc,
#'          variances = list(time = 10,
#'                           day = 2,
#'                           session = 1,
#'                           error = 3),
#'          n = 2000,
#'          check_recovery = TRUE)
#' 
#' 
#' @importFrom MASS mvrnorm
#' @importFrom stats optimise
#' @importFrom stats quantile
#' @importFrom utils capture.output
#' @importFrom utils combn
#' @export

sim_ICED <- function(structure,
                     means = NULL,
                     variances,
                     n,
                     check_recovery = FALSE) {
  
# tests
  if(!is.data.frame(structure)) {warning("structure must be a data.frame")}
  if(is.null(means)) {means <- rep(0, nrow(structure))}
  if(!is.numeric(means)) {warning("means must be a numeric vector")}
  if(length(means) != nrow(structure)) {warning("the means vector must be of the same length as the number of repeated measures")}
  if(!is.list(variances)) {warning("variances must be a list")}
  if((ncol(structure)+1) != length(variances)) {warning("structure must contain 1 fewer variable than the list of variances (structure excludes error)")}
  if(!all(colnames(structure) %in% names(variances))) {warning("variances list must contain the same variable names as structure, plus e_label")}
  if(!is.numeric(n)) {warning("n must be numeric")}
  if(!is.logical(check_recovery)) {warning("check_recovery must be TRUE or FALSE")}
  
  
  
  #  prepare output object 
  
  out <- list(call = list(structure = structure,
                          means = means,
                          variances = variances,
                          n = n,
                          check_recovery = check_recovery))  
  
  # get e_label
  e_label <- names(variances)[length(variances)]
  
  # get expected covariance structure
  sim_cov <- str2cov(structure = structure,
                     variances = variances,
                     e_label = e_label)
  
  # simulate data based on this structure
  # sim <- MASS::mvrnorm(n = n,
  #                      mu = rep(0, nrow(sim_cov)),
  #                      Sigma = sim_cov)
  
   sim <- MASS::mvrnorm(n = n,
                        mu = means,
                        Sigma = sim_cov)
  
  sim <- as.data.frame(sim)
  
  # add data and expected covariance to output object 
  
  out$data <- sim
  out$expected_covariance <- sim_cov
  
  if(check_recovery == TRUE) {
    
    # print simulation parameters
    print(paste("n = ",
                n,
                "data simulated"))
    print(paste("data simulated based on ICC1 = ",
                variances$time / (sum(unlist(variances))) # ICC1
    ))
    print(unlist(variances))
    
    # print recovered parameters
    capture.output({
      sim_syntax <- iced_syntax(structure)
      
      sim_result <- run_ICED(model = sim_syntax,
                             data = sim)
    })
    
    out$cov_mat <- sim_result$est_cov
    
    
    print("model parameters recovered:")
    print(paste("ICC1 =", sim_result$ICC))
    print(unlist(sim_result[3:(2+length(variances))]))
    
    out$recovery <- sim_result[3:(2+length(variances))]
  }
  
  
   
  return(out)
  
}

# test it works
#sim1 <- sim_ICED(structure = structure3,
#                 variances = variances,
#                 n = 100)