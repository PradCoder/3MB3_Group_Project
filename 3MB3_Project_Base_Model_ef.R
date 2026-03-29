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
  
    dp <- a*g_prev - c*p_prev + e
    dg <- b*p_prev - d*g_prev + f
    
    p_new <- p_prev + h*dp
    g_new <- g_prev + h*dg
    
    arms[,t] <- c(p_new, g_new)
  }
  return(arms)
}

#Case 1: Stable Equilibrium (spending converges to a stable long-term level)
#Case 1a
model1 <- arms_model(t_vals, arms, a = 0.4, b = 0.3, c = 0.8, d = 0.9, e, f)
quartz(width=8, height=6)
plot(t_vals, model1[1,], type="l", col="purple", lwd=2,
     xlab="Time", ylab="Military Spending",
     main="Arms Race Model Sim - Case 1a: Stable Equilibrium",
     ylim = range(model1))
lines(t_vals, model1[2,], col="green", lwd=2)
legend("topright",
       legend=c("Purple country p(t)", "Green country g(t)"),
       col=c("purple","green"),
       lwd=2,
       cex=0.8)

# Case 1: Stable Equilibrium (spending converges to a stable long-term level)
# Case 1b
model1 <- arms_model(t_vals, arms, a = 0.4, b = 0.3, c = 0.8, d = 0.9, e= -e, f)
quartz(width=8, height=6)
plot(t_vals, model1[1,], type="l", col="purple", lwd=2,
     xlab="Time", ylab="Military Spending",
     main="Arms Race Model Sim - Case 1b: Stable Equilibrium e=-12",
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
quartz(width=8, height=6)
plot(t_vals, model1[1,], type="l", col="purple", lwd=2,
     xlab="Time", ylab="Military Spending",
     main="Arms Race Model Sim - Case 1c: Stable Equilibrium f=-15",
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
quartz(width=8, height=6)
plot(t_vals, model1[1,], type="l", col="purple", lwd=2,
     xlab="Time", ylab="Military Spending",
     main="Arms Race Model Sim - Case 1d: Stable Equilibrium e=-15 and f=-15",
     ylim = range(model1))
lines(t_vals, model1[2,], col="green", lwd=2)
legend("topright",
       legend=c("Purple country p(t)", "Green country g(t)"),
       col=c("purple","green"),
       lwd=2,
       cex=0.8)

#Case 2: Escalating Arms Race (over time, both nations continue to keep increasing spending)
model2 <- arms_model(t_vals, arms, a = 1.2, b = 1.1, c = 0.3, d = 0.3, e, f)
quartz(width=8, height=6)
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
quartz(width=8, height=6)
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

# Helper: draw a base R phase portrait (vector field + nullclines + trajectory)
draw_phase_portrait <- function(model_data, a, b, cc, d, e, f, p0, g0,
                                main_title, n_grid = 14) {

  # Plot limits with padding
  p_rng <- range(model_data[1,])
  g_rng <- range(model_data[2,])
  p_lim <- p_rng + c(-1, 1) * diff(p_rng) * 0.15
  g_lim <- g_rng + c(-1, 1) * diff(g_rng) * 0.15

  # Equilibrium: solve cc*p - a*g = e, -b*p + d*g = f  → det = cc*d - a*b
  det_val <- cc * d - a * b
  if (abs(det_val) > 1e-10) {
    p_eq <- (e * d + a * f) / det_val
    g_eq <- (cc * f + b * e) / det_val
    has_eq <- p_eq >= p_lim[1] && p_eq <= p_lim[2] &&
              g_eq >= g_lim[1] && g_eq <= g_lim[2]
  } else {
    has_eq <- FALSE
  }

  # Blank canvas
  plot(NA, xlim = p_lim, ylim = g_lim,
       xlab = "Purple spending p(t)", ylab = "Green spending g(t)",
       main = main_title)

  # Vector field
  p_grid <- seq(p_lim[1], p_lim[2], length.out = n_grid)
  g_grid <- seq(g_lim[1], g_lim[2], length.out = n_grid)
  scl    <- min(diff(p_lim), diff(g_lim)) / (n_grid * 1.8)
  for (pi in p_grid) {
    for (gi in g_grid) {
      dp  <- a * gi - cc * pi + e
      dg  <- b * pi -  d * gi + f
      mag <- sqrt(dp^2 + dg^2)
      if (mag > 0)
        arrows(pi, gi,
               pi + (dp / mag) * scl, gi + (dg / mag) * scl,
               length = 0.05, col = "gray70", lwd = 0.6)
    }
  }

  # Nullclines: dp/dt=0 → g = (cc*p - e)/a  |  dg/dt=0 → g = (b*p + f)/d
  p_seq <- seq(p_lim[1], p_lim[2], length.out = 300)
  lines(p_seq, (cc * p_seq - e) / a, col = "red",        lty = 2, lwd = 1.8)
  lines(p_seq, (b  * p_seq + f) / d, col = "darkorange", lty = 2, lwd = 1.8)

  # Trajectory
  lines(model_data[1,], model_data[2,], col = "blue", lwd = 2)

  # Direction arrow at ~40% along trajectory
  mid <- round(ncol(model_data) * 0.4)
  arrows(model_data[1, mid], model_data[2, mid],
         model_data[1, mid + 1], model_data[2, mid + 1],
         length = 0.12, col = "blue", lwd = 2)

  # Start / end points
  points(p0, g0, col = "red", pch = 19, cex = 1.3)
  text(p0, g0, "Start", pos = 4, cex = 0.8)
  n_t <- ncol(model_data)
  points(model_data[1, n_t], model_data[2, n_t], col = "navy", pch = 17, cex = 1.3)
  text(model_data[1, n_t], model_data[2, n_t], "End", pos = 4, cex = 0.8)

  # Equilibrium
  if (has_eq) {
    points(p_eq, g_eq, col = "black", pch = 8, cex = 1.8, lwd = 2)
    text(p_eq, g_eq, sprintf("Eq (%.1f, %.1f)", p_eq, g_eq), pos = 3, cex = 0.75)
  }

  legend("topleft", cex = 0.72, bg = "white",
         legend = c("Trajectory", "P-nullcline (dp/dt=0)", "G-nullcline (dg/dt=0)",
                    "Start", "End", "Equilibrium"),
         col    = c("blue", "red", "darkorange", "red", "navy", "black"),
         lty    = c(1, 2, 2, NA, NA, NA),
         pch    = c(NA, NA, NA, 19, 17, 8),
         lwd    = c(2, 1.8, 1.8, NA, NA, 2))
}

# Phase portrait for Case 1
quartz(width = 7, height = 6)
draw_phase_portrait(model1, a = 0.4, b = 0.3, cc = 0.8, d = 0.9, e = e, f = f,
                    p0 = p0, g0 = g0,
                    main_title = "Phase Portrait: Case 1 - Stable Equilibrium")

# Phase portrait for Case 2
quartz(width = 7, height = 6)
draw_phase_portrait(model2, a = 1.2, b = 1.1, cc = 0.3, d = 0.3, e = e, f = f,
                    p0 = p0, g0 = g0,
                    main_title = "Phase Portrait: Case 2 - Escalating Arms Race")

# Phase portrait for Case 3
quartz(width = 7, height = 6)
draw_phase_portrait(model3, a = 1.5, b = 0.3, cc = 0.8, d = 0.9, e = e, f = f,
                    p0 = p0, g0 = g0,
                    main_title = "Phase Portrait: Case 3 - Asymmetric Arms Race")
