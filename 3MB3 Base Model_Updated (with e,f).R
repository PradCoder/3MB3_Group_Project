####################
# Arms Race Model
####################

#load libraries
library(ggplot2)

#time values
t_vals <- seq(0, 20, by = 0.1)

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

#Euler's method to numerically simulate the arms race model
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


#equilibirum
equilibrium <- function(a, b, c, d, e, f) {
  denom <- c*d - a*b
  if (abs(denom) < 1e-10) return(NULL)
  
  p_star <- (d*e + a*f) / denom
  g_star <- (b*e + c*f) / denom
  
  return(c(p_star = p_star, g_star = g_star))
}

#Model 1 - Condition: cd = ab, (e,f) >0
model1 <- arms_model(t_vals, arms, a = 0.4, b = 0.3, c = 0.6, d = 0.2, e, f)

soln1 <- data.frame(
  time = t_vals,
  purple = model1[1,],
  green = model1[2,]
)

ggplot(soln1) +
  geom_line(aes(x = time, y = purple, colour = "Purple"), linewidth = 0.9) +
  geom_line(aes(x = time, y = green, colour = "Green"), linewidth = 0.9) +
  
  labs(
    title = "Base Model: No Equilibrium",
    subtitle = "Condition: cd = ab",
    x = "Time",
    y = "Military Spending",
    colour = "Country",
    caption = paste0(
      "a = 0.4, b = 0.3, c = 0.6, d = 0.2, e = ", e, ", f = ", f,
      "   |   cd = ", round(0.6*0.2,2),
      ", ab = ", round(0.4*0.3,2)
    )
  ) +
  
  scale_colour_manual(values = c("Purple" = "purple", "Green" = "green")) +
  
  theme_gray(base_size = 14)

#Model 2 - Condition: cd < ab , +ve e,f
model2 <- arms_model(t_vals, arms, a = 1.2, b = 1.1, c = 0.3, d = 0.3, e, f)
#Plot Model 2
soln2 <- data.frame(
  time = t_vals,
  purple = model2[1,],
  green = model2[2,]
)

eq2 <- equilibrium(a = 1.2, b = 1.1, c = 0.3, d = 0.3, e = e, f = f)

ggplot(soln2) +
  geom_line(aes(x = time, y = purple, colour = "Purple"), linewidth = 0.9) +
  geom_line(aes(x = time, y = green, colour = "Green"), linewidth = 0.9) +
  
  #equilibrium label
  annotate("text",
           x = max(t_vals)*0.75,
           y = eq2["p_star"] + 1,
           label = paste0("(p*, g*) = (",
                          round(eq2["p_star"],0), ", ",
                          round(eq2["g_star"],0), ")"),
           hjust = 0,
           size = 4,
           colour = "firebrick",
           fontface = "italic") +
  
  labs(
    title = "Base Model: Unstable Saddle Point",
    subtitle = "Condition: cd < ab",
    x = "Time",
    y = "Military Spending",
    colour = "Country",
    caption = paste("a = 1.2, b = 1.1, c = 0.3, d = 0.3, e =", e, ", f =", f,
                    "| cd =", round(0.3*0.3,2), ", ab =", round(1.2*1.1,2))
  ) +
  scale_colour_manual(values = c("Purple" = "purple", "Green" = "green")) +
  theme_gray(base_size = 14)

#Model 3: Condition: cd > ab
model3 <- arms_model(t_vals, arms, a = 0.4, b = 0.3, c = 0.8, d = 0.9, e, f)

soln3 <- data.frame(
  time = t_vals,
  purple = model3[1,],
  green = model3[2,]
)

eq3 <- equilibrium(a = 0.4, b = 0.3, c = 0.8, d = 0.9, e = e, f = f)

ggplot(soln3) +
  geom_line(aes(x = time, y = purple, colour = "Purple"), linewidth = 0.9) +
  geom_line(aes(x = time, y = green, colour = "Green"), linewidth = 0.9) +
  
  #equilibrium label
  annotate("text",
           x = max(t_vals)*0.75,
           y = eq3["p_star"] + 1,
           label = paste0("(p*, g*) = (",
                          round(eq3["p_star"],0), ", ",
                          round(eq3["g_star"],0), ")"),
           hjust = 0,
           size = 4,
           colour = "firebrick",
           fontface = "italic") +
  
  labs(
    title = "Base Model: Stable Node",
    subtitle = "Condition: cd > ab",
    x = "Time",
    y = "Military Spending",
    colour = "Country",
    caption = paste("a = 0.4, b = 0.3, c = 0.8, d = 0.9, e =", e, ", f =", f,
                    "| cd =", round(0.8*0.9,2), ", ab =", round(0.4*0.3,2))
  ) +
  
  scale_colour_manual(values = c("Purple" = "purple", "Green" = "green")) +
  
  theme_gray(base_size = 14) +
  theme(
    plot.title = element_text(face = "plain"),
    plot.subtitle = element_text(size = 12),
    legend.position = "right",
    plot.caption = element_text(size = 10)
  )


#Model 1 - Condition: cd = ab, (e,f) < 0
model1_neg <- arms_model(t_vals, arms, a = 0.4, b = 0.3, c = 0.6, d = 0.2, e=-e, f=-f)

eq1 <- equilibrium(a = 0.4, b = 0.3, c = 0.6, d = 0.2, e = -e, f = -f) 
## Returns null so I'm going to delete logic for
## plotting equilibriums

soln1_neg <- data.frame(
  time = t_vals,
  purple = model1_neg[1,],
  green = model1_neg[2,]
)

ggplot(soln1_neg) +
  geom_line(aes(x = time, y = purple, colour = "Purple"), linewidth = 0.9) +
  geom_line(aes(x = time, y = green, colour = "Green"), linewidth = 0.9) +

  labs(
    title = "Model 2: No Equilibrium",
    subtitle = "Condition: cd = ab",
    x = "Time",
    y = "Military Spending",
    colour = "Country",
    caption = paste0(
      "a = 0.4, b = 0.3, c = 0.6, d = 0.2, e = ", -e, ", f = ", -f,
      "   |   cd = ", round(0.6*0.2,2),
      ", ab = ", round(0.4*0.3,2)
    )
  ) +
  
  scale_colour_manual(values = c("Purple" = "purple", "Green" = "green")) +
  
  theme_gray(base_size = 14)


#Model 2 - Condition: cd < ab , (e,f) < 0
model2_neg <- arms_model(t_vals, arms, a = 1.2, b = 1.1, c = 0.3, d = 0.3, e, f)
#Plot Model 2
soln2 <- data.frame(
  time = t_vals,
  purple = model2_neg[1,],
  green = model2_neg[2,]
)

eq2 <- equilibrium(a = 1.2, b = 1.1, c = 0.3, d = 0.3, e = -e, f = -f)

ggplot(soln2) +
  geom_line(aes(x = time, y = purple, colour = "Purple"), linewidth = 0.9) +
  geom_line(aes(x = time, y = green, colour = "Green"), linewidth = 0.9) +
  
  #equilibrium label
  abline()
  annotate("text",
           x = max(t_vals)*0.75,
           y = eq2["p_star"] + 1,
           label = paste0("(p*, g*) = (",
                          round(eq2["p_star"],0), ", ",
                          round(eq2["g_star"],0), ")"),
           hjust = 0,
           size = 4,
           colour = "firebrick",
           fontface = "italic") +
  
  labs(
    title = "Model 2: Unstable Saddle Point",
    subtitle = "Condition: cd < ab",
    x = "Time",
    y = "Military Spending",
    colour = "Country",
    caption = paste("a = 1.2, b = 1.1, c = 0.3, d = 0.3, e =", -e, ", f =", -f,
                    "| cd =", round(0.3*0.3,2), ", ab =", round(1.2*1.1,2))
  ) +
  scale_colour_manual(values = c("Purple" = "purple", "Green" = "green")) +
  theme_gray(base_size = 14)

#Model 3: Condition: cd > ab, (e,f) < 0
model3_neg <- arms_model(t_vals, arms, a = 0.4, b = 0.3, c = 0.8, d = 0.9, -e, -f)

soln3_neg <- data.frame(
  time = t_vals,
  purple = model3_neg[1,],
  green = model3_neg[2,]
)

eq3 <- equilibrium(a = 0.4, b = 0.3, c = 0.8, d = 0.9, e = -e, f = -f)

ggplot(soln3) +
  geom_line(aes(x = time, y = purple, colour = "Purple"), linewidth = 0.9) +
  geom_line(aes(x = time, y = green, colour = "Green"), linewidth = 0.9) +
  
  #equilibrium label
  annotate("text",
           x = max(t_vals)*0.75,
           y = eq3["p_star"] + 1,
           label = paste0("(p*, g*) = (",
                          round(eq3["p_star"],0), ", ",
                          round(eq3["g_star"],0), ")"),
           hjust = 0,
           size = 4,
           colour = "firebrick",
           fontface = "italic") +
  
  labs(
    title = "Model 2: Stable Node",
    subtitle = "Condition: cd > ab",
    x = "Time",
    y = "Military Spending",
    colour = "Country",
    caption = paste("a = 0.4, b = 0.3, c = 0.8, d = 0.9, e =", -e, ", f =", -f,
                    "| cd =", round(0.8*0.9,2), ", ab =", round(0.4*0.3,2))
  ) +
  
  scale_colour_manual(values = c("Purple" = "purple", "Green" = "green")) +
  
  theme_gray(base_size = 14) +
  theme(
    plot.title = element_text(face = "plain"),
    plot.subtitle = element_text(size = 12),
    legend.position = "right",
    plot.caption = element_text(size = 10)
  )
  
####################################
### Phase Diagrams for each Case ###
####################################

# Phase diagram for Case 1 e,f >0
plot(model1[1,], model1[2,],
     type="l",
     col="blue",
     lwd=2,
     xlab="Purple spending p(t)",
     ylab="Green spending g(t)",
     main="Phase Diagram: Case 1")
points(p0, g0, col="red", pch=19)  # starting point
text(p0, g0, "Start", pos=4)

# Phase diagram for Case 1 e,f < 0
plot(model1_neg[1,], model1_neg[2,],
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
