#' iced_syntax function - generates lavaan syntax for ICED models
#' 
#'  The function takes a dataframe describing the data structure and returns lavaan syntax to run the model
#' 
#' @param structure data.frame describing the structure of the data, with each variable covering a design aspect - see example. Note: currently the first variable must be time and include a different value for each repeated measure.
#' @param fix_lower_bounds fixes error variance estimates to be positive, defaults to TRUE
#' @param set_variances allows the user to specify a list of variances for each latent variable
#' @param e_label user defined variable name of the error variance. defaults to "e"
#' @param e_variance_equality defaults to TRUE, indicating that the error variances are set to be equal - setting to FALSE allows the error variances to vary
#' @param print option to print the syntax to the console. defaults to TRUE
#' @param groups allows the user to specify a number or list of group names. The syntax will generate separate latent variable variances to estimate for each group
#' @param groups_inequality allows the user to specify which variance components they wish to allow to vary between groups. Useful for model comparisons. 
#' 
#' @return returns a character string for the ICED model following lavaan syntax
#' @examples 
#' ## see online documentation for full examples
#' # https://github.com/sdparsons/ICED
#' structure <- data.frame(time = c("T1", "T2", "T3", "T4"),
#'                         day = c("day1","day1","day2","day2"),
#'                         session = c("session1", "session1","session2", "session3"))
#' 
#' 
#' @importFrom stringr str_replace_all
#'
#' @export                          


iced_syntax <- function(structure, 
                        fix_lower_bounds = TRUE, 
                        set_variances = NULL,
                        e_label = "e",
                        e_variance_equality = TRUE,
                        print = TRUE,
                        growth = FALSE,
                        groups = NULL,
                        groups_inequality = NULL) {

  
# tests (more may be needed)
  if(!is.data.frame(structure)) {warning("structure must be a data.frame")}
  if(!is.logical(fix_lower_bounds)) {warning("fix_lower_bounds must be TRUE or FALSE")}
  if(!is.null(set_variances) & !is.list(set_variances)) {warning("set_variances must be list")}  
  if(!is.character(e_label)) {warning("e_label must be a character string")}
  if(!is.logical(print)) {warning("print must be TRUE or FALSE")}
  if(!is.null(set_variances)) {
  if((ncol(structure)+1) != length(set_variances)) {warning("structure must contain 1 fewer variable than the list of variances (structure excludes error)")}
  }
  if(is.null(set_variances)) {
  if(!all(c(colnames(structure),e_label) == names(set_variances))) {warning("variances list must contain the same variable names as structure, plus e_label")}
  }
  
  if(!is.null(groups_inequality) & !is.character(groups_inequality)) {warning("groups_inequality must be vector of strings")}
  
  # new tests to incorporate growth models

  
structure[] <- lapply(structure, as.character)
  
## regressions =~

if(growth == FALSE) {

# time latent - assumes first variable in structure is time
lat_time <- paste("T",
                  " =~ ", 
                  paste("1*", structure$time, sep = ""),
                  sep = "",
                  collapse = '\n')
}

if(growth == TRUE) {
  
  # slope (slope needs to come first for ECR purposes, later)
  lat_slope <- paste("S",
                     " =~ ", 
                     paste(0:(length(structure$time)-1), "*", structure$time, sep = ""),
                     sep = "",
                     collapse = '\n')

    slope_resid <- "S ~~ slope_var*S"
  slope_mean <- "S ~ slope_mean*1"
  
  # intercept
  lat_intercept <- paste("I",
                         " =~ ", 
                         paste("1*", structure$time, sep = ""),
                         sep = "",
                         collapse = '\n')

  intercept_resid <- "I ~~ intercept_var*I"
  intercept_mean <- "I ~ intercept_mean*1"
  
  # intercept slope cor
    I_S_cor <- "I ~~ iscor*S"
}

# other latent variables, i.e. potential sources of variance
lat_structure <- NULL

if(ncol(structure) > 1) {
lat_structure <- paste(unlist(structure[,2:length(structure)]),
      " =~ 1*",
      structure$time,
      sep = "",
      collapse = '\n')
}

# errors - one error term per measure
lat_errors_list <- 1:length(structure$time)
lat_errors <- paste("E",
                    lat_errors_list,
                    " =~ 1*",
                    structure$time,
                    sep = "",
                    collapse = '\n')

## residuals, variances, covariances

if(growth == FALSE) {
# time variance
time_resid <- paste("T ~~ ",
                    colnames(structure)[1],
                    "*T",
                    sep = "")

}

# covariances - constrained to equal variances
co_vars <- NULL

if(ncol(structure) > 1) {
  for(i in 2:(ncol(structure))){
    co_vars <- paste(co_vars,
      paste(as.vector(unlist(unique(structure[i]))), 
            " ~~ ",
            colnames(structure)[i],
            "*",
            as.vector(unlist(unique(structure[i]))),
            sep = "",
            collapse = '\n'
            ),
        sep = "",
        collapse = '\n')
    
      if(i < ncol(structure)){
        co_vars <- paste(co_vars,
              '\n',
              sep = "")
    }
  }
}

# error covariances - constrained to equal variance
if(e_variance_equality == TRUE){
co_err <- paste(
  paste("E",1:length(structure$time), sep = ""),
  " ~~ ",
  e_label, "*",
  paste("E",1:length(structure$time), sep = ""),
  sep = "",
  collapse = '\n')
}

if(e_variance_equality == FALSE){
  co_err <- paste(
    paste("E",1:length(structure$time), sep = ""),
    " ~~ ",
    paste(e_label,1:length(structure$time), sep = ""),
    "*",
    paste("E",1:length(structure$time), sep = ""),
    sep = "",
    collapse = '\n')
}

  
# constrain loadings for each latent to each other to 0
# dont think this is actually needed

# if(ncol(structure == 1)){
# latents <- as.vector(c("T",
#                        paste("E",1:length(structure$time), sep = "")
# ))
# }
# 
# if(ncol(structure) > 1) {
# latents <- as.vector(c("T",
#                        as.character(unique(unlist(structure[,2:length(structure)]))),
#                        paste("E",1:length(structure$time), sep = "")
#                      ))
# }
# 
# 
# latents_combined <- combn(latents, 2)
# latents_syntax <- paste(latents_combined[1,1],
#                         " ~~ 0*",
#                         latents_combined[2,1],
#                         sep = "")
# 
# for(i in 2:ncol(latents_combined)){
#   latents_syntax <- paste(latents_syntax,
#     paste(latents_combined[1,i],
#           " ~~ 0*",
#           latents_combined[2,i],
#           sep = ""),
#     sep = "\n",
#     collapse = "\n"
#   )
# }

# observed means
obs_means <- paste(structure$time, 
      "~1",
      sep = "",
      collapse = '\n')


# combine syntax
if(growth == FALSE){
final_syntax <- paste("! regressions",
               lat_time,
               lat_structure,
               lat_errors,
               "! residuals, variances and covariances",
               time_resid,
               co_vars,
               co_err,
               #latents_syntax,
               "! observed means",
               obs_means,
               sep = "\n",
               collapse = '\n'
               )
}


if(growth == TRUE){
  final_syntax <- paste("! regressions",
                        lat_slope,
                        slope_resid,
                        lat_intercept,
                        intercept_resid,
                        slope_mean,
                        intercept_mean,
                        lat_structure,
                        lat_errors,
                        "! residuals, variances and covariances",
                        I_S_cor,
                        co_vars,
                        co_err,
                        #latents_syntax,
                        "! observed means",
                        obs_means,
                        sep = "\n",
                        collapse = '\n'
  )
}

# syntax manipulation for multi group 

if(!is.null(groups)) {
  if(!is.numeric(groups) && !is.character(groups)) {stop("groups must be numeric or vector of character")}
  
  
  # groups inequality conditions
  # tests/checks
  
  
  
  if(is.null(groups_inequality)) {
    lat_list <- c(colnames(structure), e_label)
  }
  
  if(!is.null(groups_inequality)) {
    if(!all(groups_inequality %in% c(colnames(structure), e_label))) {warning("variables in groups_inequality must be variables in structure")}
    
    lat_list <- groups_inequality
  }
  
  
  if(is.numeric(groups)) {
    groups_list <- 1:groups
  }
  if(is.character(groups)) {
    groups_list <- groups
  }  
  
  for(i in lat_list) {
    
    final_syntax <- 
      stringr::str_replace_all(string = final_syntax,
                           pattern = paste(" ", i,"\\*",
                                           sep = ""),
                           replacement = paste(" c(",
                                            paste(paste("lat",i,sep=""),
                                                  groups_list,
                                                  collapse = ",", 
                                                  sep = ""),
                                            ")*",
                                            sep = ""))
    
  }
  
}





# fixes a lower bound to the variance of the latent variables
if(fix_lower_bounds == TRUE &
   is.null(set_variances)) {
  
  if(is.null(groups)) {
    if(e_variance_equality == TRUE){
    lower <- paste("\n",
        c(colnames(structure), e_label),
        " > 0.0001 ",
        sep = "",
        collapse = "")
    }
    
    if(e_variance_equality == FALSE){
      lower <- paste("\n",
                c(colnames(structure), 
                paste(e_label, 1:length(structure$time), sep = "")),
                " > 0.0001 ",
                sep = "",
                collapse = "")
    }
  
  
  
  if(growth == TRUE) {
    if(e_variance_equality == TRUE){
        lower <- paste("\n",
                   "slope_var > 0.0001",
                   "\n",
                   "intercept_var > 0.0001",
                   "\n",
                   paste(e_label, " > 0.0001", sep = ""),
                   sep = "")
    }
  }
  
  }
  
  # for groups
  if(is.numeric(groups)) {
  lower <- paste("\n",
                 paste("lat",
                       rep(lat_list, each = groups), 
                       groups_list,
                       sep = ""),
                 " > 0.0001 ",
                 sep = "",
                 collapse = "")
  }
  
  # for groups
  if(is.character(groups)) {
  lower <- paste("\n",
                 paste("lat",
                       rep(lat_list, each = length(groups)), 
                       groups_list,
                       sep = ""),
                 " > 0.0001 ",
                 sep = "",
                 collapse = "")  
  }
  
  final_syntax <- paste(final_syntax,
                        "\n!set lower bounds of variances",
                        lower,
                        sep = "")
  
}

if(!is.null(set_variances)) {
  
  if(typeof(set_variances) == "double") {
    if(e_variance_equality == TRUE){
      variances <- c(colnames(structure), 
                     e_label)
    }
    if(e_variance_equality == FALSE){
      variances <- c(colnames(structure), 
                     paste(e_label,
                           1:length(structure$time),
                           sep = ""))
    }
  
  if(length(variances) != length(set_variances) ) {
    warning("cannot run - set_variances is of incorrect length")
  }
  
  set_variances <- paste("\n",
        variances,
        "==",
        set_variances,
        collapse = "")
  }
  
  if(typeof(set_variances) == "character") {
    set_variances <- paste("\n", 
                           set_variances, 
                           collapse = "")
    
    
  }
  
  final_syntax <- paste(final_syntax,
                        "\n!set variances",
                        set_variances,
                        sep = "")
      
}

if(print == TRUE) {
cat(final_syntax)
}
  
return(final_syntax)
}


#########################################################################
# The following code offers some tests of the syntax function



# # testing ICED model from Brandmaier et al (2018) fig 5 
# 
# structure <- data.frame(time = c("T1", "T2", "T3"),
#                          p = c("P1", "P1","P2"))
 
#syntax <- iced_syntax(structure, e_variance_equality = FALSE)
# 
# 
# # testing fig 6
# 
# structure2 <- data.frame(time = c("T1","T2","T3","T4","T5",
#             "T6","T7","T8","T9","T10",
#             "TX"))
# 
# iced_syntax(structure2)
# 
# # figure 3
# structure3 <- data.frame(time = c("T1", "T2", "T3", "T4"),
#                         day = c("day1","day1","day2","day2"),
#                         session = c("session1", "session1","session2", "session3"))
# 
# iced_syntax(structure3)
# 
# # testing the structure of Noble et al. 
# 
# structure_noble <- data.frame(time = paste("T",1:24, sep = ""),
#                               session = rep(c("session1","session2","session3","session4"), each = 6),
#                               day = rep(c("day1","day2"), each = 12),
#                               scanner = rep(c("scanner1","scanner2"), each = 6, length.out = 24))
 
# iced_syntax(structure_noble)
# 
# 
