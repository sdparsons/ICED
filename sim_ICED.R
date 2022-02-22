# sim_ICED simulates n x p data frame based on ICED model structure,
# selected variance components, and specified n


sim_ICED <- function(structure,
                     variances,
                     n,
                     check_recovery = FALSE) {
  
  # get expected covariance structure
  sim_cov <- str2cov(structure = structure,
                     variances = variances)
  
  # simulate data based on this structure
  sim <- MASS::mvrnorm(n = n,
                       mu = rep(0, nrow(sim_cov)),
                       Sigma = sim_cov)
  
  sim <- as.data.frame(sim)
  
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
    
    print("model parameters recovered:")
    print(paste("ICC1 =", sim_result$ICC))
    print(unlist(sim_result[3:(2+length(variances))]))
  }
  
  return(sim)
  
}

# test it works
#sim1 <- sim_ICED(structure = structure3,
#                 variances = variances,
#                 n = 100)