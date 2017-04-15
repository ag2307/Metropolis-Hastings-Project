sampler<- function()
{
  trials <- 100000
  y<-as.array(trials)
  y[1]=0
  count = 0
  u <- runif(trials)
  for (i in 2:trials)
  {
    y.p <- prop(y[i-1])
    f_ratio <- dist(y.p)/dist(y[i-1])
    acc_prop <- f_ratio
    
    if(u[i] <= acc_prop)
    {
      y[i] = y.p
      count = count+1
    }
    else
    {
      y[i]=y[i-1]
    }
  }
  cat(count)
  return(y)
}

dist <- function(x)
{
  return (x*x*exp(-(x^2)/fctr))
}

prop <- function(x)
{
  return (rnorm(1,x,1))
}

driver <- function(fctr)
{
  a= (sqrt(fctr/2))
  heading = paste("Maxwell-Boltzmann distribution(a = ",a,") using\n Gamma as proposal density")
  result = sampler()
  plot(density(result),main = heading)
  abline(v = sqrt(fctr))
  plot(result[1:1000],type = 'l',main = "Corresponding Markov chain",xlab = '',ylab = '')
}


par(mfrow = c(3,2))

fctr = 2*5*5
driver(fctr)

fctr = 2*2*2
driver(fctr)

fctr = 2
driver(fctr)
dev.off(0)
