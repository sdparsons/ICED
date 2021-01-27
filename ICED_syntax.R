dat1 <- data.frame(D1S1 = rnorm(100,100,100),
                   D1S2 = rnorm(100,100,100),
                   D2S1 = rnorm(100,100,100),
                   D2S2 = rnorm(100,100,100))

scans <- names(dat1)

structure <- data.frame(day = c("day1","day1","day2","day2"),
                        session = c("session1", "session1","session2", "session3"))


iced_syntax <- function(scans,
                        structure) {

structure[] <- lapply(structure, as.character)
  
## regressions =~

# time latent
lat_time <- paste("T =~ ", 
                  paste("1*", scans, sep = ""),
                  sep = "",
                  collapse = '\n')



lat_structure <- paste(unlist(structure),
      " =~ 1*",
      scans,
      sep = "",
      collapse = '\n')

# errors
lat_errors_list <- 1:length(scans)
lat_errors <- paste("E",
                    lat_errors_list,
                    " =~ 1*",
                    scans,
                    sep = "",
                    collapse = '\n')


## residuals, variances, covariances

time_resid <- "T ~~ t*T"

co_vars <- NULL
for(i in 1:(ncol(structure))){
  
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
  collapse = '\n'
)

}

co_err <- paste(
  paste("E",1:length(scans), sep = ""),
  " ~~ e*",
  paste("E",1:length(scans), sep = ""),
  sep = "",
  collapse = '\n')
  
  


# need to set loadings for each latent to each other to 0

latents <- as.vector(c("T",
                       as.character(unique(unlist(structure))),
                       paste("E",1:length(scans), sep = "")
                     ))



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

# obsered means

obs_means <- paste(scans, 
      "~1",
      sep = "",
      collapse = '\n')


final <- paste("! regressions",
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

cat(final)
}

iced_syntax(scans, structure)


# testing fig 5 

scans2 <- c("T1", "T2", "T3")

structure2 <- data.frame(p = c("P1", "P1","P2"))

iced_syntax(scans2, structure2)
