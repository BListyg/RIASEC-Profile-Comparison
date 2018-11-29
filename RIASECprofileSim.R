#¯\_(ツ)_/¯

#Replicate n 3-Letter RIASEC Profiles

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
  
return(
  matrix(
    profiles,
    ncol = n_scales,
    nrow = n_people
    ) %>% data.frame(stringsAsFactors = F)
  )
}

#####

RIASEC_sim(5,5)

#####

apply(X = RIASEC_sim(
  n_scales = 12,
  n_people = 1000
  ),
  MARGIN = 1,
  FUN = function(x){
    sqrt(sum(adist(x[upper.tri(x)])^2))
  }) %>% hist
