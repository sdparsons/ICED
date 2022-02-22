# run_ICED

# Wrapper function for lavaan to run an ICED model generated with ICED_syntax(), or manually coded

# library("tidyverse)
# library("lavaan")
# library("boot")

# source("ICED_syntax.R")
# source("ICED_boot.R)




run_ICED <- function(model = NULL,
                     data = NULL,
                     boot = NULL) {

result1 <- lavaan::lavaan(data = data, # for testing
                  model = model,
                  missing = "FIML")

# generalised code to extract variance components from the lavaan model

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


# calculate ICC2
## effective error

# extract model-implied covariance matrix
cov1 <- lavaan::fitted(result1)$cov

#Set true-score variance to zero and compute the model-implied matrix from that model as M0 (here cov0)

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


# bootstrapped CIs

if(!is.null(boot)) {
# ICC
ICC_booted <- boot::boot(data = data,
                         model = model,
                         statistic = ICC_boot_fun,
                         R = boot,
                         parallel = "multicore")

quantile(ICC_booted$t, c(.025, .975))

# hist(ICC_booted$t)

ICC_booted_CIs <- boot::boot.ci(ICC_booted,
                          type = "perc")

ICC_lower <- ICC_booted_CIs$percent[4]
ICC_upper <- ICC_booted_CIs$percent[5]

# ICC2
ICC2_booted <- boot::boot(data = data,
                          model = model,
                          statistic = ICC2_boot_fun,
                          R = boot,
                          parallel = "multicore")

quantile(ICC2_booted$t, c(.025, .975))

# hist(ICC2_booted$t)

ICC2_booted_CIs <- boot::boot.ci(ICC2_booted,
                           type = "perc")

ICC2_lower <- ICC2_booted_CIs$percent[4]
ICC2_upper <- ICC2_booted_CIs$percent[5]
}

# print everything

output <- list()

output$ICC <- ICC
if(!is.null(boot)) {
output$ICC_CIs <- c(ICC_lower, ICC_upper)
}

output$ICC2 <- ICC2
if(!is.null(boot)) {
output$ICC2_CIs <- c(ICC2_lower, ICC2_upper)
}

for(i in var_values$id){
  output[[i]] <- var_values[var_values["id"] == i,"est"]
}

output$EffectiveError <- efferr

absolute <- sum(var_values$est[2:nrow(var_values)] / var_values$id_n[2:nrow(var_values)])

output$AbsoluteError <- absolute

phi <- var_values$est[1] / 
  (var_values$est[1] + absolute)

output$phi_dependability <- phi

output$lavaan <- result1

print(output)

}



# build bootstrapped estimates into this - perhaps also compare the bootstrapped to the blavaan estimates (or offer functionality to do either)

# have more detailed outputs
## include:
## - all variance estimates
## - model fit stuff
## - model comparisons(compare nested models - do the additional sources of variance help model fit?)

