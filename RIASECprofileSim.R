#¯\_(ツ)_/¯

#Replicate n 3-Letter RIASEC Profiles

library(magrittr)
library(dplyr)
library(stringr)

#####

RIASEC_sim <- function(n_scales,n_people){
  profiles <- 
    replicate(n_people,
              replicate(
                n_scales,
                paste(sample(
                  c("R",
                    'I',
                    'A',
                    'S',
                    'E',
                    'C'),
                  size = 3),
                  sep='',
                  collapse = '')
              )
    )
  
  profile_data <- matrix(
    profiles,
    ncol = n_scales,
    nrow = n_people
  ) %>% 
    data.frame(stringsAsFactors = F) %>% 
    `colnames<-`(str_split(paste('SCALE',
                                 c(1:ncol(.)),
                                 sep = '_',
                                 collapse = " "),
                           pattern = ' ') 
                 %>% unlist)
  
  return(profile_data)
  
}

#####

adist_norm <- function(x){
  adist(x) %>% 
    .[upper.tri(.)] %>% 
    .^2 %>% 
    sum %>%
    sqrt
}
  
#####

RIASEC_sim(6,6)

#####


#####

apply(X = RIASEC_sim(
  n_scales = 3,
  n_people = 1000
),
MARGIN = 1,
FUN = adist_norm) %>% 
  hist(breaks=seq(0,15,l=1000))
