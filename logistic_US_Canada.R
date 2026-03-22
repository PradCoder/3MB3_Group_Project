#############################
# Logistic Arms Race Model
# With Logistic Constraints
############################

#time values
t_vals <- seq(0, 350, by = 0.1)

#parameters
#values of e and f shift the equilibrium but do not change stability results
e <- 0.733582888 
f <- -0.59771515 

#capacity limits for military spending (logistic constraint parameters)
Kp <- 40000
Kg <- 1041000

#initial conditions determine the starting point of the simulation
#values below or above equilibrium affect whether spending initially increases or decreases
p0 <- 29065.9 
g0 <- 968381.6

#create matrix to store model values: simulated spending levels of Purple and Green, respectively
arms <- matrix(NA, nrow = 2, ncol = length(t_vals))
arms[,1] <- c(p0, g0)

#Euler method to numerically simulate the Logistic arms race model
arms_model <- function(t_vals, arms, a, b, c, d, e, f, Kp, Kg){
  h <- t_vals[2] - t_vals[1]
  for(t in 2:length(t_vals)){
    
    p_prev <- arms[1, t-1]
    g_prev <- arms[2, t-1]
    
    #Logistic model equations with logistic constraint
    dp <- a*(1 - p_prev/Kp)*g_prev - c*p_prev + e
    dg <- b*(1 - g_prev/Kg)*p_prev - d*g_prev + f
    
    p_new <- p_prev + h*dp
    g_new <- g_prev + h*dg
    
    arms[,t] <- c(p_new, g_new)
  }
  return(arms)
}

model1 <- arms_model(t_vals, arms, a = 0.001025041, b = 3.06035844, c = 0.006444388, d = 0.01104621, e, f, Kp, Kg)
#Plot the results 
plot(t_vals, model1[1,], type="l", col="red", lwd=2,
     xlab="Time", ylab="Military Spending (USD Millions)",
     main="Arms Race Logistic Model: US and Canada",
     ylim = range(model1))
lines(t_vals, model1[2,], col="blue", lwd=2)
legend("right",
       legend=c("Canada", "US"),
       col=c("red","blue"),
       lwd=2,
       cex=0.8)


