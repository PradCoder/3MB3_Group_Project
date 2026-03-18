####################
# Arms Race Model
####################

#time values
t_vals <- seq(0, 50, by = 0.1)

#parameters
#values of e and f shift the equilibrium but do not change stability results - confirm
e <- 12
f <- 15

#initial conditions determine the starting point of the simulation
#values below or above equilibrium affect whether spending initially increases or decreases
#p0 <- 12
#g0 <- 15
p0 <- 35
g0 <- 20

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

#Case 1: Stable Equilibrium (spending converges to a stable long-term level)
model1 <- arms_model(t_vals, arms, a = 0.4, b = 0.3, c = 0.8, d = 0.9, e, f)
#Plot Case 1
plot(t_vals, model1[1,], type="l", col="purple", lwd=2,
     xlab="Time", ylab="Military Spending",
     main="Arms Race Model Simulation - Case 1: Stable Equilibrium",
     ylim = range(model1))
lines(t_vals, model1[2,], col="green", lwd=2)
legend("topright",
       legend=c("Purple country p(t)", "Green country g(t)"),
       col=c("purple","green"),
       lwd=2,
       cex=0.8)

#Case 2: Escalating Arms Race (over time, both nations continue to keep increasing spending)
model2 <- arms_model(t_vals, arms, a = 1.2, b = 1.1, c = 0.3, d = 0.3, e, f)
#Plot Case 2
plot(t_vals, model2[1,], type="l", col="purple", lwd=2,
     xlab="Time", ylab="Military Spending",
     main="Arms Race Model Simulation - Case 2: Escalating Arms Race",
     ylim = range(model2))
lines(t_vals, model2[2,], col="green", lwd=2, lty=2)
legend("topleft",
       legend=c("Purple country p(t)", "Green country g(t)"),
       col=c("purple","green"),
       lwd=2,
       lty=c(1,2),
       cex=0.8)


#Case 3: Asymmetric Arms Race (countries respond differently, leading to different long-term spending levels)
model3 <- arms_model(t_vals, arms, a = 1.5, b = 0.3, c = 0.8, d = 0.9, e, f)
#Plot Case 1
plot(t_vals, model3[1,], type="l", col="purple", lwd=2,
     xlab="Time", ylab="Military Spending",
     main="Arms Race Model Simulation - Case 3: Asymmetric Arms Race",
     ylim = range(model3))
lines(t_vals, model3[2,], col="green", lwd=2)
legend("topright",
       legend=c("Purple country p(t)", "Green country g(t)"),
       col=c("purple","green"),
       lwd=2,
       cex=0.6)

####################################
### Phase Diagrams for each Case ###
####################################

# Phase diagram for Case 1
plot(model1[1,], model1[2,],
     type="l",
     col="blue",
     lwd=2,
     xlab="Purple spending p(t)",
     ylab="Green spending g(t)",
     main="Phase Diagram: Case 1")
points(p0, g0, col="red", pch=19)  # starting point
text(p0, g0, "Start", pos=4)

# Phase diagram for Case 2
plot(model2[1,], model2[2,],
     type="l",
     col="blue",
     lwd=2,
     xlab="Purple spending p(t)",
     ylab="Green spending g(t)",
     main="Phase Diagram: Case 2")
points(p0, g0, col="red", pch=19)  # starting point
text(p0, g0, "Start", pos=4)

#Phase diagram for Case 3
plot(model3[1,], model3[2,],
     type="l",
     col="blue",
     lwd=2,
     xlab="Purple spending p(t)",
     ylab="Green spending g(t)",
     main="Phase Diagram: Case 3")
points(p0, g0, col="red", pch=19)  # starting point
text(p0, g0, "Start", pos=4)
