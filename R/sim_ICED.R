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
#' @importFrom dplyr select mutate rename relocate
#' @importFrom tidyr pivot_longer pivot_wider
#' @importFrom magrittr %>%

sim_ICED <- function(structure,
                     means = NULL,
                     variances,
                     n,
                     growth = FALSE,
                     int_slope_covariance = FALSE,
                     check_recovery = FALSE) {
  
# tests
  if(!is.data.frame(structure)) {warning("structure must be a data.frame")}
  if(is.null(means)) {means <- rep(0, nrow(structure)) }
  if(!is.numeric(means)) {warning("means must be a numeric vector")}
  if(!is.list(variances)) {warning("variances must be a list")}
  if(growth == FALSE) {
    if((ncol(structure)+1) != length(variances)) {warning("structure must contain 1 fewer variable than the list of variances (structure excludes error)")}
  }
  if(growth == TRUE) {if((ncol(structure)+2) != length(variances)) {warning("structure must contain 2 fewer variable than the list of variances (structure excludes slope and error)")}
    }
  if(!all(colnames(structure) %in% names(variances))) {warning("variances list must contain the same variable names as structure, plus e_label")}
  if(!is.numeric(n)) {warning("n must be numeric")}
  if(!is.logical(check_recovery)) {warning("check_recovery must be TRUE or FALSE")}
  
  # checks for growth = TRUE
  if(growth == TRUE) {
    if(is.null(int_slope_covariance)) {warning("for growth models, the intercept slope covariance must be set, e.g. int_slope_covariance = .2")}
    if(names(variances)[2] != "slope") {warning("for growth models, the second variance paramater must be the slope variance, e.g. slope = 5")}
  }
  
  
  
  #  prepare output object 
  
  out <- list(call = list(structure = structure,
                          means = means,
                          variances = variances,
                          n = n,
                          check_recovery = check_recovery))  
  
  # get e_label
  e_label <- names(variances)[length(variances)]
  
  ####### create the variance covariance matrix Psi #######
  
  # add error latents to structure
  structure2 <- data.frame(structure,
                       error = paste("E", 1:nrow(structure), sep = ""))
  
  # add slope latent parameter for growth models
  if(growth == TRUE) {
    structure2$slope <- "slope"
    
    structure2 <- structure2 %>%
      dplyr::relocate(slope, .after = time)
  }
  
  # begin vector of all latent's variances with time
  variances2 <- c(variances$time) 
  
  # create full vector of variances of latents 
  # (assumes the same variance for each latent group)
  for(i in names(variances)[-1]) {
    variances2 <- c(variances2,
                    rep(variances[[i]], 
                        length(unique(structure2[[i]]))))
  }
  
  # create variance covariance matrix Psi containing all variances
  Psi <- diag(variances2)
  
  if(growth == TRUE) {
    Psi[1,2] <- int_slope_covariance
    Psi[2,1] <- int_slope_covariance
  }
  
  ####### create the factor loading matrix, Lambda #######
  
  # make a vector of all latent variables
  all_latents <- 
    c(colnames(structure2[1]),
      unique(unlist(structure2[,-1])))
  
  # number variables
  structure2$n <- 1:nrow(structure2)
  
  # restructure the latent variables into the Lambda loading matrix
  Lambda <- structure2[,-1] %>%
    tidyr::pivot_longer(cols = !n) %>%
    dplyr::mutate(name = 1) %>%
    tidyr::pivot_wider(id_cols = n,
                       names_from = value,
                       values_from = name,
                       values_fill = 0) %>%
    dplyr::rename(time = n) %>%
    dplyr::mutate(time = 1) %>%
    {if(growth == TRUE) mutate(., slope = 0:(nrow(structure2)-1)) else .}  %>%
    dplyr::select(all_of(all_latents)) %>%
    as.matrix()
  
  ####### incorporate mean structures #######
  
  
  if(is.numeric(means)) {
    if(growth == FALSE) {
      if(length(means) != nrow(structure)) {warning("the means vector must be of the same length as the number of repeated measures")}
      
      # set latent means of time as 1, all other latents as zero
      alpha <- c(1, rep(0, length(all_latents)-1))
      # set nu vector of item intercepts to zero
      nu <- means
    }
    
    if(growth == TRUE) {
      if(length(means) != 2) {warning("for growth models the means vector must be of length 2, e.g. c(1, 2), for a mean intercept of 1 and slope of 2")}
      
      # set latent means of intercept and slope as 1, all other latents as zero
      alpha <- c(means, rep(0, length(all_latents)-2))
      # set nu vector of item intercepts to zero
      nu <- c(0, 0, 0, 0)
    }
  }
  

  # simulate data
  sim_dat <- as.data.frame(MASS::mvrnorm(n,
                       mu = Lambda %*% alpha + nu, 
                       Sigma = Lambda %*% Psi %*% t(Lambda),
                       empirical = TRUE))
 # notes: 
  
  # ensure variable names are the same as specified
  colnames(sim_dat) <- structure[,1]
  
  # add data and expected covariance to output object 
  
  out$data <- sim_dat
  #out$expected_covariance <- sim_cov
  
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
                             data = sim_dat)
    })
    
    out$cov_mat <- sim_result$est_cov
    
    
    print("model parameters recovered:")
    print(paste("ICC1 =", sim_result$ICC))
    print(unlist(sim_result[3:(2+length(variances))]))
    
    out$recovery <- sim_result[3:(2+length(variances))]
  }
  
  
   
  return(out)
  
}

# # test it works
# 
# 
# structure <- data.frame(time = c("T1", "T2", "T3", "T4"),
#                      day = c("day1","day1","day2","day2"),
#                      session = c("ses1", "ses1","ses2", "ses3"))
# 
# sim_ICED(structure,
#          variances = list(time = 10,
#                           day = 2,
#                           session = 1,
#                           error = 3),
#          n = 200,
#          means = c(1,20,30,40),
#          check_recovery = TRUE)
#                     
# sim_ICED(structure,
# variances = list(time = 10,
#                  slope = 5,
#                 day = 2,
#                 session = 1,
#                 error = 3),
# growth = TRUE,
# int_slope_covariance = .2,
# means = c(1,10),
# n = 200,
# check_recovery = TRUE)
# 
# 
# 
# 
# structure <- data.frame(time = c("T1", "T2", "T3", "T4",
#                                  "T5", "T6", "T7", "T8",
#                                  "T9", "T10", "T11", "T12"))
# 
# library(tidyverse)
# library(ggthemes)
# library(ggthemr)
# dat1 <- sim_ICED(structure,
#          variances = list(time = 10,
#                           error = .2),
#          growth = FALSE,
#          #int_slope_covariance = -.2,
#          means = c(10, 9.5, 9, 8.5,
#                    7.8,7,6.3,5.5,
#                    4.5,3.4,2.5,1.2),
#          n = 20,
#          check_recovery = FALSE)
# 
# dat1$data %>%
#   mutate(n = 1:nrow(dat1$data)) %>%
#   pivot_longer(cols = c("T1", "T2", "T3", "T4",
#                         "T5", "T6", "T7", "T8",
#                         "T9", "T10", "T11", "T12"),
#                names_to = "Age",
#                values_to = "Cortical Thickness") %>%
#   mutate(Age = as.numeric(gsub('T','', Age))) %>%
#   ggplot(aes(x = Age,
#              y = `Cortical Thickness`)) +
#   #geom_line() +
#   #stat_smooth()
#   stat_summary(inherit.aes = FALSE,
#                aes(x = Age,
#                    y = `Cortical Thickness`),
#                    fun = mean,
#                geom = "line",
#                linewidth = 3) +
#   theme_pander() +
#   theme(axis.text = element_blank())
# 
# ggthemr("sky")
# ggthemr_reset()
# 
# 
# dat1$data %>%
#   mutate(n = 1:nrow(dat1$data)) %>%
#   pivot_longer(cols = c("T1", "T2", "T3", "T4",
#                         "T5", "T6", "T7", "T8",
#                         "T9", "T10", "T11", "T12"),
#                names_to = "Age",
#                values_to = "Cortical Thickness") %>%
#   mutate(Age = as.numeric(gsub('T','',Age))) %>%
#   ggplot(aes(x = Age,
#              y = `Cortical Thickness`,
#              group = n)) +
#   geom_line() +
#   theme_light() +
#   theme(axis.text = element_blank())
# 
# ###
# 
# dat2 <- sim_ICED(structure,
#                  variances = list(time = 10,
#                                   error = 4),
#                  growth = FALSE,
#                  #int_slope_covariance = -.2,
#                  means =  c(10, 9.5, 9, 8.5,
#                             7.8,7,6.3,5.5,
#                             4.5,3.4,2.5,1.2),
#                  n = 20,
#                  check_recovery = FALSE)
# 
# dat2$data %>%
#   mutate(n = 1:nrow(dat2$data)) %>%
#   pivot_longer(cols = c("T1", "T2", "T3", "T4",
#                         "T5", "T6", "T7", "T8",
#                         "T9", "T10", "T11", "T12"),
#                names_to = "Age",
#                values_to = "Cortical Thickness") %>%
#   mutate(Age = as.numeric(gsub('T','',Age))) %>%
#   ggplot(aes(x = Age,
#              y = `Cortical Thickness`,
#              group = n)) +
#   #geom_line() +
#   stat_summary(inherit.aes = FALSE,
#                aes(x = Age,
#                    y = `Cortical Thickness`),
#                fun = mean,
#                geom = "line",
#                linewidth = 3) +
#   theme(axis.text = element_blank())
# 
# 
# dat2$data %>%
#   mutate(n = 1:nrow(dat2$data)) %>%
#   pivot_longer(cols = c("T1", "T2", "T3", "T4",
#                         "T5", "T6", "T7", "T8",
#                         "T9", "T10", "T11", "T12"),
#                names_to = "Age",
#                values_to = "Cortical Thickness") %>%
#   mutate(Age = as.numeric(gsub('T','',Age))) %>%
#   ggplot(aes(x = Age,
#              y = `Cortical Thickness`,
#              group = n)) +
#   geom_line() +
#   theme_light() +
#   theme(axis.text = element_blank(),
#         axis.title.y = element_text(face = "italic"))
# 
# 
# ##########
# 
# 
# dat3 <- sim_ICED(structure,
#                  variances = list(time = 20,
#                                   slope = 5,
#                                   error = 1),
#                  growth = TRUE,
#                  int_slope_covariance = -.1,
#                  means =  c(1, -2.5),
#                  n = 20,
#                  check_recovery = FALSE)
# 
# dat3$data %>%
#   mutate(n = 1:nrow(dat3$data)) %>%
#   pivot_longer(cols = c("T1", "T2", "T3", "T4",
#                         "T5", "T6", "T7", "T8",
#                         "T9", "T10", "T11", "T12"),
#                names_to = "Age",
#                values_to = "Cortical Thickness") %>%
#   mutate(Age = as.numeric(gsub('T','',Age))) %>%
#   ggplot(aes(x = Age,
#              y = `Cortical Thickness`,
#              group = n,
#              colour = n)) +
#   geom_line(linewidth = .8) +
#   scale_color_viridis_c(option = "mako",
#                         begin = .2,
#                         end = .7) +
#   theme_light() +
#   theme(axis.text = element_blank(),
#         legend.position = "none")
# 
# library(Cairo)
# 
# Cairo("trajectories.png", 
#       dpi = 600,
#       width = 2400,
#       height = 2000)
# 
# dat3$data %>%
#   mutate(n = 1:nrow(dat3$data)) %>%
#   pivot_longer(cols = c("T1", "T2", "T3", "T4",
#                         "T5", "T6", "T7", "T8",
#                         "T9", "T10", "T11", "T12"),
#                names_to = "Age",
#                values_to = "Score") %>%
#   mutate(Time = as.numeric(gsub('T','',Age))) %>%
#   ggplot(aes(x = Time,
#              y = `Score`,
#              group = n,
#              colour = n)) +
#   geom_line(linewidth = .8) +
#   scale_color_viridis_c(option = "mako",
#                         begin = .2,
#                         end = .7) +
#   theme_light() +
#   theme(axis.text = element_blank(),
#         legend.position = "none")
# 
# dev.off()
