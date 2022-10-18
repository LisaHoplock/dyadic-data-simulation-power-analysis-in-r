#Simulate an indistinguishable dyads dataset and conduct power analyses to determine power for the interaction
#Code borrowed from Soderberg, Lane, & Hennes SIPS 2018 workshop (https://osf.io/mp2za); Randi Garcia’s R workshop (https://github.com/RandiLGarcia/2day-dyad-workshop/blob/master/Day 1/R Code/Day 1-Actor-Partner Interdependence Model.Rmd), and Ethan Young’s restructuring dyadic data tutorial (https://www.ethan-young.com/code/restructuring-dyadic-data/)
library(tidyr)
library(dplyr)
library(nlme)
library(purrr)
library(stringr)

apim.sim <- function (J){
  dyad <- rep(1:J)                #creates dyad IDs
  x_A <- rnorm(J,0,1)             #creates values of x for actors
  x_P <- rnorm(J,0,1)             #creates values of x for partners
  b0 <- 8.07                      #intercept; can change this value
  b1 <- .24                       #slope for actor x; can change this value
  b2 <- .09                       #slope for partner x; can change this value
  b3 <- .16                       #actor x partner interaction; can change this value
  y_A <- rnorm(J, b0 + (b1*x_A) + (b2*x_P) + (b3*x_A*x_P), (1-(b1^2 + b2^2 + b3^2))) #creates values of y for actors
  y_P <- rnorm(J, b0 + (b1*x_A) + (b2*x_P) + (b3*x_A*x_P), (1-(b1^2 + b2^2 + b3^2))) #creates values of y for partners
  data.frame(dyad, y_A, y_P, x_A, x_P) %>% #these next 5 lines changes the dataset from dyadic to individual (long)
    gather(key,value,-dyad) %>% 
    separate(key, c("key", "partnum"), sep = "_") %>%
    mutate(partnum = ifelse(partnum =="A", 1, 2)) %>% 
    spread(key,value) %>% 
    split(.$dyad) %>% #from here changes the dataset from long to pairwise (wide & long)
    map_df(function(MapD2P){
      
      person1 <- MapD2P %>% 
        mutate(act.par = ifelse(partnum == 1,"A","P")) %>% 
        gather(key,value,-dyad,-act.par) %>% 
        unite(new_key,key,act.par) %>% 
        spread(new_key,value)
      
      person2 <- MapD2P %>% 
        mutate(act.par = ifelse(partnum == 1,"P","A")) %>% #flipping the data so that partnum 1 is now the partner
        gather(key,value,-dyad,-act.par) %>% 
        unite(new_key,key,act.par) %>% 
        spread(new_key,value)
      
      bind_rows(person1,person2) #binds the rows to make them pairwise
    }) 
  
}


apim.power <- function (J,n.sims=10000){    #power analysis
  signif <- rep(NA,n.sims)                  
  estim <- rep(NA,n.sims)
  pval <- rep(NA,n.sims)
  for (s in 1:n.sims){
    sim <- apim.sim(J)                   
    gls.power <- gls(y_A ~ x_A + x_P + x_A*x_P, #APIM indistinguishable model
                     data = sim,
                     correlation = corCompSymm(form=~1|dyad),
                     na.action = na.omit)
    int_est <- coef(summary(gls.power))["x_A:x_P","Value"]   #takes the interaction estimates; can change "this value"x_A:x_P" to other fixed effects
    p <- coef(summary(gls.power))["x_A:x_P","p-value"]       #takes the interaction p-values; can change "this value"x_A:x_P" to other fixed effects
    se <- coef(summary(gls.power))["x_A:x_P","Std.Error"]    #takes the interaction standard errors; can change "this value"x_A:x_P" to other fixed effects
    signif[s] <- int_est>0 & p<=.05                          #returns how many of the interaction estimates are significant
  }
  power <- mean(signif) #takes the average power
  return(power) #tells you what the average power is
}

apim.power(J=100,n.sims=2) #can change the value of dyad sample size (J) and how many simulations to run
