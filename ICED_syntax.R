# iced_syntax function. 
# intended to recreate lavaan syntax for ICED models

# assumes a structure data.frame similar to the following. With each variable being a source of variance. The first is expected to be time. 
# structure <- data.frame(time = c("T1", "T2", "T3", "T4"),
#                         day = c("day1","day1","day2","day2"),
#                         session = c("session1", "session1","session2", "session3"))


iced_syntax <- function(structure, 
                        fix_lower_bounds = TRUE, 
                        set_variances = NULL,
                        e_label = "e",
                        print = TRUE,
                        groups = NULL) {

structure[] <- lapply(structure, as.character)
  
## regressions =~

# time latent - assumes first variable in structure is time
lat_time <- paste("T",
                  " =~ ", 
                  paste("1*", structure$time, sep = ""),
                  sep = "",
                  collapse = '\n')

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

# time variance
time_resid <- paste("T ~~ ",
                    colnames(structure)[1],
                    "*T",
                    sep = "")

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
co_err <- paste(
  paste("E",1:length(structure$time), sep = ""),
  " ~~ ",
  e_label, "*",
  paste("E",1:length(structure$time), sep = ""),
  sep = "",
  collapse = '\n')
  
  
# constrain loadings for each latent to each other to 0

if(ncol(structure == 1)){
latents <- as.vector(c("T",
                       paste("E",1:length(structure$time), sep = "")
))
}

if(ncol(structure) > 1) {
latents <- as.vector(c("T",
                       as.character(unique(unlist(structure[,2:length(structure)]))),
                       paste("E",1:length(structure$time), sep = "")
                     ))
}


latents_combined <- combn(latents, 2)
latents_syntax <- paste(latents_combined[1,1],
                        " ~~ 0*",
                        latents_combined[2,1],
                        sep = "")

for(i in 2:ncol(latents_combined)){
  latents_syntax <- paste(latents_syntax,
    paste(latents_combined[1,i],
          " ~~ 0*",
          latents_combined[2,i],
          sep = ""),
    sep = "\n",
    collapse = "\n"
  )
}

# observed means
obs_means <- paste(structure$time, 
      "~1",
      sep = "",
      collapse = '\n')


# combine syntax
final_syntax <- paste("! regressions",
               lat_time,
               lat_structure,
               lat_errors,
               "! residuals, variances and covariances",
               time_resid,
               co_vars,
               co_err,
               latents_syntax,
               "! observed means",
               obs_means,
               sep = "\n",
               collapse = '\n'
               )



# syntax manipulation for multi group 

if(!is.null(groups)) {
  if(!is.numeric(groups) && !is.character(groups)) {stop("groups must be numeric or vector of character")}
  
  lat_list <- c(colnames(structure), e_label)
  
  if(is.numeric(groups)) {
    groups_list <- 1:groups
  }
  if(is.character(groups)) {
    groups_list <- groups
  }  
  
  for(i in lat_list) {
    
    final_syntax <- 
      stringr::str_replace_all(string = final_syntax,
                           pattern = paste(i,"\\*",
                                           sep = ""),
                           replacement = paste("c(",
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
  lower <- paste("\n",
        colnames(structure),
        " > 0.0001 ",
        sep = "",
        collapse = "")
  }
  
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
  variances <- c(colnames(structure), e_label)
  
  if(length(variances) != length(set_variances) ) {
    print("cannot run - set_variances is of incorrect length")
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
# 
# syntax <- iced_syntax(structure)
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
