sampler <- function()
{ 
  trials = 100000
  y <- as.array(trials)
  y[1] <- rgamma(1, shape=1, rate = 1)
  u <- runif(trials)
  count <- 0
  for (i in 2:trials) 
  {
   y.p <- rgamma(n=1, shape=y[i-1], rate=1)
   f_ratio <- dist(y.p)/dist(y[i-1])
   q_ratio <- dgamma(x = y[i-1],shape = y.p, rate = 1)/dgamma(x = y.p,shape = y[i-1], rate = 1)
   acc_prob <- f_ratio*q_ratio
  
   if (u[i] <= acc_prob)
   {
    y[i] <- y.p
    count = count +1
    }
   else
   {
    y[i] <- y[i-1]
   }
  }
  cat(count)
  return(y)
}

dist <- function(x){
  return(x*x*exp(-(x^2)/fctr))
}
driver <- function(fctr)
  {
  a= (sqrt(fctr/2))
  heading = paste("Maxwell-Boltzmann distribution(a = ",a,") using\n Gamma as proposal density")
  result=sampler()
  plot(density(result),main = heading)
  abline(v = sqrt(fctr))
  plot(result[1:1000],type = 'l',main = "Corresponding Markov chain",xlab = '',ylab = '')
}



par(mfrow = c(3,2))


fctr <- 2*5*5
driver(fctr)

fctr <- 2*2*2
driver(fctr)

fctr <- 2
driver(fctr)
dev.off(0)
