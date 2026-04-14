#############################
# Extension Model
# With Logistic Terms
############################

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
p0 <- 35
g0 <- 20

#create matrix to store model values: simulated spending levels of Purple and Green, respectively
arms <- matrix(NA, nrow = 2, ncol = length(t_vals))
arms[,1] <- c(p0, g0)

#Euler method to numerically simulate the Logistic arms race model
arms_model <- function(t_vals, arms, a, b, c, d, e, f, Kp, Kg){
  h <- t_vals[2] - t_vals[1]
  for(t in 2:length(t_vals)){
    
    p_prev <- arms[1, t-1]
    g_prev <- arms[2, t-1]
    
    #Extension model with logistic terms
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

#function to find equilibrium
find_equilibrium <- function(start_p, a, b, c, d, e, f, Kp, Kg) {
  sol <- nleqslv(
    x = start_p,
    fn = eq_p,
    a = a, b = b, c = c, d = d, e = e, f = f, Kp = Kp, Kg = Kg)
  
  p_star <- sol$x
  g_star <- g_from_p(p_star, a, c, e, Kp)
  
  list(
    p_star = p_star,
    g_star = g_star
  )
}

#Fiugre 15 - case: cd > ab
model3 <- arms_model(t_vals, arms, a = 0.4, b = 0.3, c = 0.8, d = 0.9, e, f, Kp, Kg)
#find equilibrium numerically
eq_model3 <- find_equilibrium(
  start_p = 70,
  a = 0.4, b = 0.3, c = 0.8, d = 0.9, e = e, f = f, Kp = Kp, Kg = Kg
)
eq_model3$p_star
eq_model3$g_star
eq_model3$termcd
eq_model3$message
#Plot Case 
soln3 <- data.frame(
  time = t_vals,
  purple = model3[1,],
  green = model3[2,])
ggplot(soln3) +
  geom_line(aes(x = time, y = purple, colour = "Purple"), linewidth = 0.9) +
  geom_line(aes(x = time, y = green, colour = "Green"), linewidth = 0.9) +
  # equilibrium labels
  annotate("text",
           x = max(t_vals) * 0.7,
           y = (eq_model3$p_star + eq_model3$g_star)/2.15,
           label = paste0("(p*, g*) = (",
                          round(eq_model3$p_star, 2), ", ",
                          round(eq_model3$g_star, 2), ")"),
           size = 4,
           colour = "firebrick",
           fontface = "italic") +
  labs(
    title = "Extension Model: cd > ab",
    subtitle = "Initial conditions: p(0) = 35, g(0) = 20",
    x = "Time",
    y = "Military Spending",
    colour = "Country",
    caption = paste("a = 0.4, b = 0.3, c = 0.8, d = 0.9, e =", e, ", f =", f,
                    "| Kp =", Kp, ", Kg =", Kg)
  ) +
  scale_colour_manual(values = c("Purple" = "purple", "Green" = "green")) +
  theme_gray(base_size = 14)

#Figure 16 - case: cd = ab
model4 <- arms_model(t_vals, arms, a = 0.4, b = 0.3, c = 0.6, d = 0.2, e, f, Kp, Kg)
#find equilibrium numerically
eq_model4 <- find_equilibrium(
  start_p = 70,
  a = 0.4, b = 0.3, c = 0.6, d = 0.2, e = e, f = f, Kp = Kp, Kg = Kg
)
eq_model4$p_star
eq_model4$g_star
eq_model4$termcd
eq_model4$message
#Plot Case
soln4 <- data.frame(
  time = t_vals,
  purple = model4[1,],
  green = model4[2,])
ggplot(soln4) +
  geom_line(aes(x = time, y = purple, colour = "Purple"), linewidth = 0.9) +
  geom_line(aes(x = time, y = green, colour = "Green"), linewidth = 0.9) +
  annotate("text",
           x = max(t_vals) * 0.7,
           y = (eq_model4$p_star + eq_model4$g_star)/1.5,
           label = paste0("(p*, g*) = (",
                          round(eq_model4$p_star, 2), ", ",
                          round(eq_model4$g_star, 2), ")"),
           size = 4,
           colour = "firebrick",
           fontface = "italic") +
  labs(
    title = "Extension Model: cd = ab",
    subtitle = "Initial conditions: p(0) = 35, g(0) = 20",
    x = "Time",
    y = "Military Spending",
    colour = "Country",
    caption = paste("a = 0.4, b = 0.3, c = 0.6, d = 0.2, e =", e, ", f =", f,
                    "| Kp =", Kp, ", Kg =", Kg)
  ) +
  scale_colour_manual(values = c("Purple" = "purple", "Green" = "green")) +
  theme_gray(base_size = 14)

#Figure 17 - case: cd < ab
model5 <- arms_model(t_vals, arms, a = 1.2, b = 1.1, c = 0.3, d = 0.3, e, f, Kp, Kg)
#find equilibrium numerically
eq_model5 <- find_equilibrium(
  start_p = 70,
  a = 1.2, b = 1.1, c = 0.3, d = 0.3, e = e, f = f, Kp = Kp, Kg = Kg
)
eq_model5$p_star
eq_model5$g_star
eq_model5$termcd
eq_model5$message
#Plot Case
soln5 <- data.frame(
  time = t_vals,
  purple = model5[1,],
  green = model5[2,])
ggplot(soln5) +
  geom_line(aes(x = time, y = purple, colour = "Purple"), linewidth = 0.9) +
  geom_line(aes(x = time, y = green, colour = "Green"), linewidth = 0.9) +
  # equilibrium labels
  annotate("text",
           x = max(t_vals) * 0.7,
           y = (eq_model5$p_star + eq_model5$g_star)/2.15,
           label = paste0("(p*, g*) = (",
                          round(eq_model5$p_star, 2), ", ",
                          round(eq_model5$g_star, 2), ")"),
           size = 4,
           colour = "firebrick",
           fontface = "italic") +
  labs(
    title = "Extension Model: cd < ab",
    subtitle = "Initial conditions: p(0) = 35, g(0) = 20",
    x = "Time",
    y = "Military Spending",
    colour = "Country",
    caption = paste("a = 1.2, b = 1.1, c = 0.3, d = 0.3, e =", e, ", f =", f,
                    "| Kp =", Kp, ", Kg =", Kg)
  ) +
  scale_colour_manual(values = c("Purple" = "purple", "Green" = "green")) +
  theme_gray(base_size = 14)

#Figure 18: Growth to Equilibrium
model2 <- arms_model(t_vals, arms, a = 1.2, b = 1.1, c = 0.3, d = 0.3, e, f, Kp, Kg)
#find equilibrium numerically
eq_model2 <- find_equilibrium(
  start_p = 70,
  a = 1.2, b = 1.1, c = 0.3, d = 0.3, e = e, f = f, Kp = Kp, Kg = Kg
)
#Plot Case
soln2 <- data.frame(
  time = t_vals,
  purple = model2[1,],
  green = model2[2,])
ggplot(soln2) +
  geom_line(aes(x = time, y = purple, colour = "Purple"), linewidth = 0.9) +
  geom_line(aes(x = time, y = green, colour = "Green"), linewidth = 0.9) +
  # equilibrium labels
  annotate("text",
           x = max(t_vals) * 0.7,
           y = (eq_model2$p_star + eq_model2$g_star)/2.2,
           label = paste0("(p*, g*) = (",
                          round(eq_model2$p_star, 2), ", ",
                          round(eq_model2$g_star, 2), ")"),
           size = 4,
           colour = "firebrick",
           fontface = "italic") +
  labs(
    title = "Extension Model: Growth to Equilibrium",
    subtitle = "Initial conditions: p(0) = 35, g(0) = 20",
    x = "Time",
    y = "Military Spending",
    colour = "Country",
    caption = paste("a = 1.2, b = 1.1, c = 0.3, d = 0.3, e =", e, ", f =", f,
                    "| Kp =", Kp, ", Kg =", Kg)
  ) +
  scale_colour_manual(values = c("Purple" = "purple", "Green" = "green")) +
  theme_gray(base_size = 14)