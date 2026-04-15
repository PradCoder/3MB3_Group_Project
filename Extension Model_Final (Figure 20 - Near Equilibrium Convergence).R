####################################
# Extension Model
# With Logistic Terms
# Changed initial spending conditions for 
# near-Equilibrium Convergence.
#####################################

#install.packages("nleqslv")

#load libraries
library(ggplot2)
library(nleqslv)

#time values
t_vals <- seq(0, 20, by = 0.1)

#parameters
e <- 12
f <- 15

#military spending limits of each nation
Kp <- 80
Kg <- 70

#initial spending conditions of each nation
p0 <- 68
g0 <- 64

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

#Numerically solve for equilibrium
#g expressed in terms of p from the first equilibrium equation
g_from_p <- function(p, a, c, e, Kp) {
  Kp * (c * p - e) / (a * (Kp - p))
}

#single equilibrium equation in p
eq_p <- function(p, a, b, c, d, e, f, Kp, Kg) {
  g <- g_from_p(p, a, c, e, Kp)
  p * (b - (b / Kg) * g) - d * g + f
}

#general function to find equilibrium
find_equilibrium <- function(start_p, a, b, c, d, e, f, Kp, Kg) {
  sol <- nleqslv(
    x = start_p,
    fn = eq_p,
    a = a, b = b, c = c, d = d, e = e, f = f, Kp = Kp, Kg = Kg
  )
  
  p_star <- sol$x
  g_star <- g_from_p(p_star, a, c, e, Kp)
  
  list(
    p_star = p_star,
    g_star = g_star,
    termcd = sol$termcd,
    message = sol$message
  )
}

#Figure 20: Near-Equilibrium Convergence
model2 <- arms_model(t_vals, arms, a = 1.2, b = 1.1, c = 0.3, d = 0.3, e, f, Kp, Kg)
#find equilibrium numerically
eq_model2 <- find_equilibrium(
  start_p = 70,
  a = 1.2, b = 1.1, c = 0.3, d = 0.3, e = e, f = f, Kp = Kp, Kg = Kg
)
eq_model2$p_star
eq_model2$g_star
eq_model2$termcd
eq_model2$message
#Plot Case 2
soln2 <- data.frame(
  time = t_vals,
  purple = model2[1,],
  green = model2[2,]
)
ggplot(soln2) +
  geom_line(aes(x = time, y = purple, colour = "Purple"), linewidth = 0.9) +
  geom_line(aes(x = time, y = green, colour = "Green"), linewidth = 0.9) +
  # equilibrium labels
  annotate("text",
           x = max(t_vals) * 0.7,
           y = (eq_model2$p_star + eq_model2$g_star)/1.9,
           label = paste0("(p*, g*) = (",
                          round(eq_model2$p_star, 2), ", ",
                          round(eq_model2$g_star, 2), ")"),
           size = 4,
           colour = "firebrick",
           fontface = "italic") +
  labs(
    title = "Extension Model: Near-Equilibrium Growth",
    subtitle = "Initial conditions: p(0) = 68, g(0) = 64",
    x = "Time",
    y = "Military Spending",
    colour = "Country",
    caption = paste("a = 1.2, b = 1.1, c = 0.3, d = 0.3, e =", e, ", f =", f,
                    "| Kp =", Kp, ", Kg =", Kg)
  ) +
  scale_colour_manual(values = c("Purple" = "purple", "Green" = "green")) +
  theme_gray(base_size = 14)
