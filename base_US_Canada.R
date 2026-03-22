####################
# Arms Race Model
####################

#time values
t_vals <- seq(0, 250, by = 0.1)

#parameters
#values of e and f shift the equilibrium but do not change stability results - confirm
e <- 1.13357055 
f <- 1.10615370

#initial conditions determine the starting point of the simulation
#values below or above equilibrium affect whether spending initially increases or decreases
p0 <- 0
g0 <- 0

#create matrix to store model values: simulated spending levels of Purple and Green, respectively
arms <- matrix(NA, nrow = 2, ncol = length(t_vals))
arms[,1] <- c(p0, g0)

#Euler method to numerically simulate the arms race model
arms_model <- function(t_vals, arms, a, b, c, d, e, f){
  h <- t_vals[2] - t_vals[1]
  for(t in 2:length(t_vals)){
    
    p_prev <- arms[1, t-1]
    g_prev <- arms[2, t-1]
    
    dp <- a*g_prev - c*p_prev + e
    dg <- b*p_prev - d*g_prev + f
    
    p_new <- p_prev + h*dp
    g_new <- g_prev + h*dg
    
    arms[,t] <- c(p_new, g_new)
  }
  return(arms)
}

#find the predicted spending in both countries using parameters calibrated to US and Canada data 
model1 <- arms_model(t_vals, arms, a = 0.02070973, b = 0.84300722, c =0.83610545, d =0.01070327, e, f)
#Plot the results 
plot(t_vals, model1[2,], type="l", col="blue", lwd=2,
     xlab="Time", ylab="Military Spending (USD millions)",
     main="Arms Race Base Model: US and Canada",ylim=range(model1))
lines(t_vals, model1[1,], col="red", lwd=2)
legend("topleft",
       legend=c("Canada", "US"),
       col=c("red","blue"),
       lwd=2,
       cex=0.8)
