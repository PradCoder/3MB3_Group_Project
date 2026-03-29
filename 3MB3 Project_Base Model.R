####################
# Arms Race Model
# Richardson Model
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
model1 <- arms_model(t_vals, arms, a = 0.4, b = 0.3, c = 0.8, d = 0.9, e= -12, f)
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
model1 <- arms_model(t_vals, arms, a = 0.4, b = 0.3, c = 0.8, d = 0.9, e, f=-15)
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
# Case 1d
model1 <- arms_model(t_vals, arms, a = 0.4, b = 0.3, c = 0.8, d = 0.9, e=-12, f=-15)
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

library(deSolve)
library(ggplot2)

# Richardson ODE for deSolve: state = c(p, g), parms = named vector (use cc for fatigue param)
richardson_ode <- function(t, state, parms) {
  dp <- parms["a"] * state[2] - parms["cc"] * state[1] + parms["e"]
  dg <- parms["b"] * state[1] - parms["d"]  * state[2] + parms["f"]
  list(c(dp, dg))
}

# Helper: build a ggplot2 phase portrait (vector field + nullclines + trajectory)
phase_portrait_gg <- function(parms, p0, g0, title, t_end = 50, n_grid = 15) {

  # Solve trajectory with deSolve (more accurate than Euler)
  out <- as.data.frame(ode(y = c(p = p0, g = g0),
                           times = seq(0, t_end, by = 0.1),
                           func  = richardson_ode,
                           parms = parms))

  # Auto plot limits from trajectory, with padding
  xlim <- range(out$p) + c(-1, 1) * diff(range(out$p)) * 0.15
  ylim <- range(out$g) + c(-1, 1) * diff(range(out$g)) * 0.15

  # Vector field on a regular grid
  vf <- expand.grid(p = seq(xlim[1], xlim[2], length.out = n_grid),
                    g = seq(ylim[1], ylim[2], length.out = n_grid))
  vf$dp  <- parms["a"] * vf$g - parms["cc"] * vf$p + parms["e"]
  vf$dg  <- parms["b"] * vf$p - parms["d"]  * vf$g + parms["f"]
  mag    <- sqrt(vf$dp^2 + vf$dg^2)
  scl    <- min(diff(xlim), diff(ylim)) / (n_grid * 2)
  vf$pend <- vf$p + (vf$dp / mag) * scl
  vf$gend <- vf$g + (vf$dg / mag) * scl

  # Nullclines: dp/dt = 0 → g = (cc*p - e)/a   |   dg/dt = 0 → g = (b*p + f)/d
  p_seq <- seq(xlim[1], xlim[2], length.out = 300)
  nc <- rbind(
    data.frame(p = p_seq,
               g = (parms["cc"] * p_seq - parms["e"]) / parms["a"],
               nullcline = "dp/dt = 0 (P-nullcline)"),
    data.frame(p = p_seq,
               g = (parms["b"]  * p_seq + parms["f"]) / parms["d"],
               nullcline = "dg/dt = 0 (G-nullcline)")
  )

  ggplot() +
    # Vector field
    geom_segment(data = vf,
                 aes(x = p, y = g, xend = pend, yend = gend),
                 arrow = arrow(length = unit(0.12, "cm"), type = "closed"),
                 color = "gray70", linewidth = 0.4) +
    # Nullclines
    geom_line(data = nc, aes(x = p, y = g, color = nullcline),
              linetype = "dashed", linewidth = 1.2) +
    # Trajectory
    geom_path(data = out, aes(x = p, y = g), color = "blue", linewidth = 1) +
    # Start point
    geom_point(data = out[1, ],       aes(x = p, y = g), color = "red",  shape = 19, size = 3) +
    geom_text(aes(x = out$p[1],       y = out$g[1],       label = "Start"), hjust = -0.3, size = 3) +
    # End point
    geom_point(data = out[nrow(out), ], aes(x = p, y = g), color = "navy", shape = 17, size = 3) +
    geom_text(aes(x = out$p[nrow(out)], y = out$g[nrow(out)], label = "End"), hjust = -0.3, size = 3) +
    scale_color_manual(values = c("dp/dt = 0 (P-nullcline)" = "red",
                                  "dg/dt = 0 (G-nullcline)" = "darkorange")) +
    coord_cartesian(xlim = xlim, ylim = ylim) +
    labs(x = "Purple spending p(t)", y = "Green spending g(t)",
         title = title, color = NULL) +
    theme_bw(base_size = 12) +
    theme(legend.position = "bottom", plot.title = element_text(size = 11))
}

# Phase portrait for Case 1 (stable, e=12, f=15)
quartz(width = 7, height = 6)
print(phase_portrait_gg(
  parms = c(a = 0.4, b = 0.3, cc = 0.8, d = 0.9, e = 12, f = 15),
  p0 = p0, g0 = g0,
  title = "Phase Portrait: Case 1 - Stable Equilibrium"
))

# Phase portrait for Case 2 (escalating)
quartz(width = 7, height = 6)
print(phase_portrait_gg(
  parms = c(a = 1.2, b = 1.1, cc = 0.3, d = 0.3, e = 12, f = 15),
  p0 = p0, g0 = g0,
  title = "Phase Portrait: Case 2 - Escalating Arms Race"
))

# Phase portrait for Case 3 (asymmetric)
quartz(width = 7, height = 6)
print(phase_portrait_gg(
  parms = c(a = 1.5, b = 0.3, cc = 0.8, d = 0.9, e = 12, f = 15),
  p0 = p0, g0 = g0,
  title = "Phase Portrait: Case 3 - Asymmetric Arms Race"
))
