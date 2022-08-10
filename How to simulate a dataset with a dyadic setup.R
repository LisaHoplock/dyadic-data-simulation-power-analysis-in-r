##How to simulate a dataset with a dyadic setup

#simulation code adapted from: https://osf.io/mp2za
apim.sim <- function (J){
  dyad <- rep(1:J)                #creates dyad IDs
  x_A <- rnorm(J,0,1)             #creates values of x for actors
  x_P <- rnorm(J,0,1)             #creates values of x for partners
  b0 <- 8.07                      #intercept; can change this value
  b1 <- .24                       #slope for actor x; can change this value
  b2 <- .09                       #slope for partner x; can change this value
  b3 <- .00                       #actor x partner interaction; can change this value
  y_A <- rnorm(J, b0 + (b1*x_A) + (b2*x_P) + (b3*x_A*x_P), (1-(b1^2 + b2^2 + b3^2))) #creates values of y for actors
  y_P <- rnorm(J, b0 + (b1*x_A) + (b2*x_P) + (b3*x_A*x_P), (1-(b1^2 + b2^2 + b3^2))) #creates values of y for partners
  return(data.frame(dyad, y_A, y_P,x_A,x_P))   #creates the dataframe and makes it appear in the console
}
apim.sim(J = 6) #Insert for J how many dyads you want it to have

#If want to turn into dataframe:
#dyad.data<- apim.sim(J = 6) #Insert for J how many dyads you want it to have
#see Turning a dyadic structured dataset into long then long & wide dataset for more 
