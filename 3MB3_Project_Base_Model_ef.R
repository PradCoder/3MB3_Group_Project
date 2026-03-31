####################
# Arms Race Model
# Richardson Model
####################

#time values
t_vals <- seq(0, 50, by = 0.1)

#parameters - calibrated from the data points
#values of e and f shift the equilibrium but do not change stability results - confirm
e <- 1.13357055
f <- 1.10615370

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
  
    dp <- a*g_prev - c*p_prev + e ## CANADA
    dg <- b*p_prev - d*g_prev + f ## USA
    
    p_new <- p_prev + h*dp
    g_new <- g_prev + h*dg
    
    arms[,t] <- c(p_new, g_new)
  }
  return(arms)
}

#Case 1: Stable Equilibrium (spending converges to a stable long-term level)
#Case 1a
model1 <- arms_model(t_vals, arms, a = 0.4, b = 0.3, c = 0.8, d = 0.9, e, f)
plot(t_vals, model1[1,], type="l", col="purple", lwd=2,
     xlab="Time", ylab="Military Spending",
     main="Arms Race Model Sim - Case 1a: Stable Equilibrium e>0 and f>0",
     ylim = range(model1))
lines(t_vals, model1[2,], col="green", lwd=2)
legend("topright",
       legend=c("Purple country p(t)", "Green country g(t)"),
       col=c("purple","green"),
       lwd=2,
       cex=0.8)

#Case 1: Stable Equilibrium (spending converges to a stable long-term level)
#Case 1aa - Extremely large e
model1 <- arms_model(t_vals, arms, a = 0.4, b = 0.3, c = 0.8, d = 0.9, e=e*100, f)
plot(t_vals, model1[1,], type="l", col="purple", lwd=2,
     xlab="Time", ylab="Military Spending",
     main="Arms Race Model Sim - Case 1aa: Stable Equilibrium e>0 and e*100 e >> f",
     ylim = range(model1))
lines(t_vals, model1[2,], col="green", lwd=2)
legend("topright",
       legend=c("Purple country p(t)", "Green country g(t)"),
       col=c("purple","green"),
       lwd=2,
       cex=0.8)

#Case 1: Stable Equilibrium (spending converges to a stable long-term level)
#Case 1ab - Extremely large e and f
model1 <- arms_model(t_vals, arms, a = 0.4, b = 0.3, c = 0.8, d = 0.9, e=e*100, f=f*100)
plot(t_vals, model1[1,], type="l", col="purple", lwd=2,
     xlab="Time", ylab="Military Spending",
     main="Arms Race Model Sim - Case 1ab: Stable Equilibrium e*100 and f*100",
     ylim = range(model1))
lines(t_vals, model1[2,], col="green", lwd=2)
legend("topright",
       legend=c("Purple country p(t)", "Green country g(t)"),
       col=c("purple","green"),
       lwd=2,
       cex=0.8)

#Case 1: Stable Equilibrium (spending converges to a stable long-term level)
#Case 1ac - Extremely large and negative e and f
model1 <- arms_model(t_vals, arms, a = 0.4, b = 0.3, c = 0.8, d = 0.9, e=e*100*-1, f=f*100*-1)
plot(t_vals, model1[1,], type="l", col="purple", lwd=2,
     xlab="Time", ylab="Military Spending",
     main="Arms Race Model Sim - Case 1ac: Stable Equilibrium e*-100 and f*-100",
     ylim = range(model1))
lines(t_vals, model1[2,], col="green", lwd=2)
legend("topright",
       legend=c("Purple country p(t)", "Green country g(t)"),
       col=c("purple","green"),
       lwd=2,
       cex=0.8)

# Case 1: Stable Equilibrium (spending converges to a stable long-term level)
# Case 1b, the green country eventually overtake purple even after 
model1 <- arms_model(t_vals, arms, a = 0.4, b = 0.3, c = 0.8, d = 0.9, e= -e, f)
plot(t_vals, model1[1,], type="l", col="purple", lwd=2,
     xlab="Time", ylab="Military Spending",
     main="Arms Race Model Sim - Case 1b: Stable Equilibrium e<0 f>0",
     ylim = range(model1))
lines(t_vals, model1[2,], col="green", lwd=2)
legend("topright",
       legend=c("Purple country p(t)", "Green country g(t)"),
       col=c("purple","green"),
       lwd=2,
       cex=0.8)

# Case 1: Stable Equilibrium (spending converges to a stable long-term level)
# Case 1c
model1 <- arms_model(t_vals, arms, a = 0.4, b = 0.3, c = 0.8, d = 0.9, e, f=-f)
plot(t_vals, model1[1,], type="l", col="purple", lwd=2,
     xlab="Time", ylab="Military Spending",
     main="Arms Race Model Sim - Case 1c: Stable Equilibrium e>0 f<0",
     ylim = range(model1))
lines(t_vals, model1[2,], col="green", lwd=2)
legend("topright",
       legend=c("Purple country p(t)", "Green country g(t)"),
       col=c("purple","green"),
       lwd=2,
       cex=0.8)

# Case 1: Stable Equilibrium (spending converges to a stable long-term level)
# Case 1d, Interesting Case
model1 <- arms_model(t_vals, arms, a = 0.4, b = 0.3, c = 0.8, d = 0.9, e=-e, f=-f)
plot(t_vals, model1[1,], type="l", col="purple", lwd=2,
     xlab="Time", ylab="Military Spending",
     main="Arms Race Model Sim - Case 1d: Stable Equilibrium e<0 and f<0",
     ylim = range(model1))
lines(t_vals, model1[2,], col="green", lwd=2)
legend("topright",
       legend=c("Purple country p(t)", "Green country g(t)"),
       col=c("purple","green"),
       lwd=2,
       cex=0.8)

#Case 2: Escalating Arms Race (over time, both nations continue to keep increasing spending)
model2 <- arms_model(t_vals, arms, a = 1.2, b = 1.1, c = 0.3, d = 0.3, e, f)
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
points(model1[1,length(tvals)], model1[2,length(tvals)],col="purple",pch=17)
text(p0, g0, "Start", pos=2)

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
text(p0, g0, "Start", pos=2)