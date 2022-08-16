# ICED_boot

# internal functions for bootstrapping the ICED models to obtain CIs around the ICC and ICC2

# library("boot")

#ICC

ICC_boot_fun <- function(model, data, indices){
  
  data.select = data[indices, ]
  
  result1 <- lavaan::lavaan(data = data.select, # for testing
                            model = model,
                            missing = "FIML")  
  
  var_list <- unique(lavaan::parTable(result1)$label)
  var_list <- var_list[var_list != ""]
  
  var_values <- data.frame(id = 1:length(var_list),
                           id_row = 1:length(var_list),
                           id_n = 1:length(var_list),
                           est = 1:length(var_list))
  
  count <- 1
  for(i in var_list) {
    var_values[count, "id"] <- paste(i, "est", sep = "")
    var_values[count, "id_n"] <- sum(lavaan::parTable(result1)$label==i)
    var_values[count, "id_row"] <- which(lavaan::parTable(result1)$label==i)[1]
    
    count <- count + 1
  }
  
  count <- 1
  for(i in var_list) {
    var_values[count, "est"] <- lavaan::parTable(result1)$est[var_values[count, "id_row"]]
    count <- count + 1
    
  }
  
  # calculate ICC
  
  ICC <- var_values[1, "est"] / sum(var_values[1:nrow(var_values), "est"])
  
  return(ICC)
  
}

#ICC2

ICC2_boot_fun <- function(model, data, indices) {
  
  data.select = data[indices, ]
  
  result1 <- lavaan::lavaan(data = data.select, # for testing
                            model = model,
                            missing = "FIML")  
  
  var_list <- unique(lavaan::parTable(result1)$label)
  var_list <- var_list[var_list != ""]
  
  var_values <- data.frame(id = 1:length(var_list),
                           id_row = 1:length(var_list),
                           id_n = 1:length(var_list),
                           est = 1:length(var_list))
  

  
  cov1 <- lavaan::fitted(result1)$cov
  
  result0 <- result1
  
  m1_params <- lavaan::lav_model_get_parameters(result0@Model)
  m1_params[1] <- 0 # set true score variance to 0
  
  result0@Model <- lavaan::lav_model_set_parameters(result0@Model,
                                                    m1_params)
  
  lavaan::lav_model_get_parameters(result0@Model)[1] == 0 # check
  
  cov0 <- lavaan::lav_model_implied(result0@Model)$cov[[1]]
  
  
  
  #Then, you compute the chi-square non-centrality parameter from these two models M0 and M (this is the lambda-equation in the Appendix). This is basically the Saris-Satorra-power approximation idea
  mtr <- function(m) {sum(diag(m))}
  lambda <- (log(det(cov0))+mtr(solve(cov0)%*%cov1)-log(det(cov1))-dim(cov0)[1])
  
  # Then you compute reliability (this is the ECR value in the second equation in Appendix 3)
  loss.func <- function(x, chiByN, N) {
    ratio <- 1/(1-x)
    loss = (chiByN-(ratio-log(ratio)-1))^2
    return(loss)
  }
  
  compute.numeric <- function(chiByN) {
    ecr <- optimise(loss.func, c(0,1), chiByN=chiByN, tol = 1e-8)
    return(ecr$minimum)
  }
  
  ECR <- compute.numeric(lambda) # .737 (expected .935)
  
  # Finally (see Appendix 2), compute effective error from reliability knowing that reliability=true_variance / (true_variance + error) and knowing both reliability and true_variance
  
  effective.error.from.ecr<-function(abs.effect, ecr.value)
  {
    abs.effect <- abs(abs.effect)
    return(abs.effect*(1/ecr.value-1))
  }
  
  efferr <- effective.error.from.ecr(var_values[1, "est"], ECR) # 2.62   (expected .506)
  
  ICC2 <- var_values[1, "est"] / (var_values[1, "est"] + efferr)
  
  return(ICC2)
}

