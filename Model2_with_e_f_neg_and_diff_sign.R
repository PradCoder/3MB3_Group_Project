####################
# Arms Race Model 2, extending base model with e,f parameters
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
# To vary initial conditions
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

#equilibirum point calculation
equilibrium <- function(a, b, c, d, e, f) {
  denom <- c*d - a*b
  if (abs(denom) < 1e-10) return(NULL)
  
  p_star <- (d*e + a*f) / denom
  g_star <- (b*e + c*f) / denom
  
  return(c(p_star = p_star, g_star = g_star))
}

# Note : I couldn't find a good way to modularize this in-time since I needed
# to move the equilibrium label based on conditions so I thought
# I'd just keep everything and duplicate it according to conditions
# and adjust, since I plot the equilibrium label 
# relative to the fitted data if it exists. However the code is 
# descriptive on it's own since and the same functionality is used

# Figure 5
#Condition: cd > ab, (e,f) < 0
model_neg <- arms_model(t_vals, arms, a = 0.4, b = 0.3, c = 0.8, d = 0.9, -e, -f)

# build data frame
soln_neg <- data.frame(
  time = t_vals,
  purple = model_neg[1,],
  green = model_neg[2,]
)

# calculate equilibrium if it exists
eq <- equilibrium(a = 0.4, b = 0.3, c = 0.8, d = 0.9, e = -e, f = -f)

# plotting logic with ggplot
ggplot(soln_neg) +
  geom_line(aes(x = time, y = purple, colour = "Purple"), linewidth = 0.9) +
  geom_line(aes(x = time, y = green, colour = "Green"), linewidth = 0.9) +
  
  #equilibrium label, if equilibrium exists
  annotate("text",
           x = max(t_vals)*0.75,
           y = max(model_neg[1,])*0.75,
           label = paste0("(p*, g*) = (",
                          round(eq["p_star"],0), ", ",
                          round(eq["g_star"],0), ")"),
           hjust = 0,
           size = 4,
           colour = "firebrick",
           fontface = "italic") +
  
  #plot labels
  labs(
    title = "Model 2: Stable Node",
    subtitle = "Condition: cd > ab, e < 0, f< 0",
    x = "Time",
    y = "Military Spending",
    colour = "Country",
    caption = paste("a = 0.4, b = 0.3, c = 0.8, d = 0.9, e =", -e, ", f =", -f,
                    "| cd =", round(0.8*0.9,2), ", ab =", round(0.4*0.3,2))
  ) +
  
  scale_colour_manual(values = c("Purple" = "purple", "Green" = "green")) +
  
  #plot settings
  theme_gray(base_size = 14) +
  theme(
    plot.title = element_text(face = "plain"),
    plot.subtitle = element_text(size = 12),
    legend.position = "right",
    plot.caption = element_text(size = 10)
  )

#Figure 6 with e < 0 and f > 0
#Condition: cd > ab, (e < 0, f > 0)
model_diff <- arms_model(t_vals, arms, a = 0.4, b = 0.3, c = 0.8, d = 0.9, -e, f)

soln_diff <- data.frame(
  time = t_vals,
  purple = model_diff[1,],
  green = model_diff[2,]
)

eq <- equilibrium(a = 0.4, b = 0.3, c = 0.8, d = 0.9, e = -e, f = f)

ggplot(soln_diff) +
  geom_line(aes(x = time, y = purple, colour = "Purple"), linewidth = 0.9) +
  geom_line(aes(x = time, y = green, colour = "Green"), linewidth = 0.9) +
  
  #equilibrium label
  annotate("text",
           x = max(t_vals)*0.75,
           y = max(model_diff[1,])*0.75,
           label = paste0("(p*, g*) = (",
                          round(eq["p_star"],0), ", ",
                          round(eq["g_star"],0), ")"),
           hjust = 0,
           size = 4,
           colour = "firebrick",
           fontface = "italic") +
  
  labs(
    title = "Model 2: Stable Node",
    subtitle = "Condition: cd > ab, e<0, f>0",
    x = "Time",
    y = "Military Spending",
    colour = "Country",
    caption = paste("a = 0.4, b = 0.3, c = 0.8, d = 0.9, e =", -e, ", f =", f,
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

#Figure 6 with e > 0 and f < 0
#Condition: cd > ab, (e > 0, f < 0)
model_diff_b <- arms_model(t_vals, arms, a = 0.4, b = 0.3, c = 0.8, d = 0.9, e, -f)

soln_diff_b <- data.frame(
  time = t_vals,
  purple = model_diff_b[1,],
  green = model_diff_b[2,]
)

eq <- equilibrium(a = 0.4, b = 0.3, c = 0.8, d = 0.9, e = e, f = -f)

ggplot(soln_diff_b) +
  geom_line(aes(x = time, y = purple, colour = "Purple"), linewidth = 0.9) +
  geom_line(aes(x = time, y = green, colour = "Green"), linewidth = 0.9) +
  
  #equilibrium label
  annotate("text",
           x = max(t_vals)*0.75,
           y = max(model_diff_b[1,])*0.75,
           label = paste0("(p*, g*) = (",
                          round(eq["p_star"],0), ", ",
                          round(eq["g_star"],0), ")"),
           hjust = 0,
           size = 4,
           colour = "firebrick",
           fontface = "italic") +
  
  labs(
    title = "Model 2: Stable Node",
    subtitle = "Condition: cd > ab, e>0, f<0",
    x = "Time",
    y = "Military Spending",
    colour = "Country",
    caption = paste("a = 0.4, b = 0.3, c = 0.8, d = 0.9, e =", e, ", f =", -f,
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

# Figure 8
#Condition: cd < ab , (e,f) < 0
model_neg <- arms_model(t_vals, arms, a = 1.2, b = 1.1, c = 0.3, d = 0.3, -e, -f)

#dataframe for condition
soln_neg <- data.frame(
  time = t_vals,
  purple = model_neg[1,],
  green = model_neg[2,]
)

eq <- equilibrium(a = 1.2, b = 1.1, c = 0.3, d = 0.3, e = -e, f = -f)

ggplot(soln_neg) +
  geom_line(aes(x = time, y = purple, colour = "Purple"), linewidth = 0.9) +
  geom_line(aes(x = time, y = green, colour = "Green"), linewidth = 0.9) +
  
  #equilibrium label
  annotate("text",
           x = max(t_vals)*0.65,
           y = max(model_neg[1,])*0.75,
           label = paste0("(p*, g*) = (",
                          round(eq["p_star"],0), ", ",
                          round(eq["g_star"],0), ")"),
           hjust = 0,
           size = 4,
           colour = "firebrick",
           fontface = "italic") +
  
  labs(
    title = "Model 2: Unstable Saddle Point",
    subtitle = "Condition: cd < ab e < 0,f <0",
    x = "Time",
    y = "Military Spending",
    colour = "Country",
    caption = paste("a = 1.2, b = 1.1, c = 0.3, d = 0.3, e =", -e, ", f =", -f,
                    "| cd =", round(0.3*0.3,2), ", ab =", round(1.2*1.1,2))
  ) +
  scale_colour_manual(values = c("Purple" = "purple", "Green" = "green")) +
  theme_gray(base_size = 14)

# Figure 9 with  e > 0 and f < 0
#Condition: cd < ab , (e > 0, f < 0)
model_diff <- arms_model(t_vals, arms, a = 1.2, b = 1.1, c = 0.3, d = 0.3, e, -f)
#Plot Model 2
soln <- data.frame(
  time = t_vals,
  purple = model_diff[1,],
  green = model_diff[2,]
)

eq <- equilibrium(a = 1.2, b = 1.1, c = 0.3, d = 0.3, e = e, f = -f)

ggplot(soln) +
  geom_line(aes(x = time, y = purple, colour = "Purple"), linewidth = 0.9) +
  geom_line(aes(x = time, y = green, colour = "Green"), linewidth = 0.9) +
  
#equilibrium label
annotate("text",
         x = max(t_vals)*0.75,
         y = eq["p_star"] + 1,
         label = paste0("(p*, g*) = (",
                        round(eq["p_star"],0), ", ",
                        round(eq["g_star"],0), ")"),
         hjust = 0,
         size = 4,
         colour = "firebrick",
         fontface = "italic") +
  
  labs(
    title = "Model 2: Unstable Saddle Point",
    subtitle = "Condition: cd < ab, e>0 f<0",
    x = "Time",
    y = "Military Spending",
    colour = "Country",
    caption = paste("a = 1.2, b = 1.1, c = 0.3, d = 0.3, e =", e, ", f =", -f,
                    "| cd =", round(0.3*0.3,2), ", ab =", round(1.2*1.1,2))
  ) +
  scale_colour_manual(values = c("Purple" = "purple", "Green" = "green")) +
  theme_gray(base_size = 14)

# Figure 9 with  e <0 and f>0
#Condition: cd < ab , (e < 0, f > 0)
model_diff_b <- arms_model(t_vals, arms, a = 1.2, b = 1.1, c = 0.3, d = 0.3, -e, f)
#Plot Model
soln <- data.frame(
  time = t_vals,
  purple = model_diff_b[1,],
  green = model_diff_b[2,]
)

eq <- equilibrium(a = 1.2, b = 1.1, c = 0.3, d = 0.3, e = -e, f = f)

ggplot(soln) +
  geom_line(aes(x = time, y = purple, colour = "Purple"), linewidth = 0.9) +
  geom_line(aes(x = time, y = green, colour = "Green"), linewidth = 0.9) +
  
  #equilibrium label
  annotate("text",
           x = max(t_vals)*0.75,
           y = eq["p_star"] + 1,
           label = paste0("(p*, g*) = (",
                          round(eq["p_star"],0), ", ",
                          round(eq["g_star"],0), ")"),
           hjust = 0,
           size = 4,
           colour = "firebrick",
           fontface = "italic") +
  
  labs(
    title = "Model 2: Unstable Saddle Point",
    subtitle = "Condition: cd < ab, e<0 f>0",
    x = "Time",
    y = "Military Spending",
    colour = "Country",
    caption = paste("a = 1.2, b = 1.1, c = 0.3, d = 0.3, e =", -e, ", f =", f,
                    "| cd =", round(0.3*0.3,2), ", ab =", round(1.2*1.1,2))
  ) +
  scale_colour_manual(values = c("Purple" = "purple", "Green" = "green")) +
  theme_gray(base_size = 14)

#Figure 11
#Condition: cd = ab, (e,f) < 0
model_neg <- arms_model(t_vals, arms, a = 0.4, b = 0.3, c = 0.6, d = 0.2, e=-e, f=-f)

eq <- equilibrium(a = 0.4, b = 0.3, c = 0.6, d = 0.2, e = -e, f = -f) 
## Returns null so I'm going to delete logic for
## plotting equilibriums

soln_neg <- data.frame(
  time = t_vals,
  purple = model_neg[1,],
  green = model_neg[2,]
)

ggplot(soln_neg) +
  geom_line(aes(x = time, y = purple, colour = "Purple"), linewidth = 0.9) +
  geom_line(aes(x = time, y = green, colour = "Green"), linewidth = 0.9) +
  
  labs(
    title = "Model 2: No Equilibrium",
    subtitle = "Condition: cd = ab, e < 0 f < 0",
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

# Figure 12 with e < 0 and f >0
# Condition: cd = ab, (e< 0 f> 0)
model_diff <- arms_model(t_vals, arms, a = 0.4, b = 0.3, c = 0.6, d = 0.2, e=-e, f=f)

eq <- equilibrium(a = 0.4, b = 0.3, c = 0.6, d = 0.2, e = -e, f = f) 
## Returns null so I'm going to delete logic for
## plotting equilibriums

soln_diff <- data.frame(
  time = t_vals,
  purple = model_diff[1,],
  green = model_diff[2,]
)

ggplot(soln_diff) +
  geom_line(aes(x = time, y = purple, colour = "Purple"), linewidth = 0.9) +
  geom_line(aes(x = time, y = green, colour = "Green"), linewidth = 0.9) +
  
  labs(
    title = "Model 2: No Equilibrium",
    subtitle = "Condition: cd = ab e<0, f>0",
    x = "Time",
    y = "Military Spending",
    colour = "Country",
    caption = paste0(
      "a = 0.4, b = 0.3, c = 0.6, d = 0.2, e = ", -e, ", f = ", f,
      "   |   cd = ", round(0.6*0.2,2),
      ", ab = ", round(0.4*0.3,2)
    )
  ) +
  
  scale_colour_manual(values = c("Purple" = "purple", "Green" = "green")) +
  
  theme_gray(base_size = 14)

# Figure 13 with e > 0 and f <0
# Condition: cd = ab, (e> 0 f< 0)
model_diff_b <- arms_model(t_vals, arms, a = 0.4, b = 0.3, c = 0.6, d = 0.2, e=e, f=-f)

eq <- equilibrium(a = 0.4, b = 0.3, c = 0.6, d = 0.2, e = e, f = -f) 
## Returns null so I'm going to delete logic for
## plotting equilibriums

soln_diff_b <- data.frame(
  time = t_vals,
  purple = model_diff_b[1,],
  green = model_diff_b[2,]
)

ggplot(soln_diff_b) +
  geom_line(aes(x = time, y = purple, colour = "Purple"), linewidth = 0.9) +
  geom_line(aes(x = time, y = green, colour = "Green"), linewidth = 0.9) +
  
  labs(
    title = "Model 2: No Equilibrium",
    subtitle = "Condition: cd = ab e>0, f<0",
    x = "Time",
    y = "Military Spending",
    colour = "Country",
    caption = paste0(
      "a = 0.4, b = 0.3, c = 0.6, d = 0.2, e = ", e, ", f = ", -f,
      "   |   cd = ", round(0.6*0.2,2),
      ", ab = ", round(0.4*0.3,2)
    )
  ) +
  
  scale_colour_manual(values = c("Purple" = "purple", "Green" = "green")) +
  
  theme_gray(base_size = 14)

#Figure 14
#Condition: cd = ab, infinite equilibria case
#pick good values for e and derive f using the relation established
e <- 12
b <-0.3
c <- 0.6
f <- (-b*e)/c

# build model with with the specified parameters
model_inf <- arms_model(t_vals, arms, a = 0.4, b = 0.3, c = 0.6, d = 0.2, e, f)

# find equilibrium
eq_inf <- equilibrium(a = 0.4, b = 0.3, c = 0.6, d = 0.2, e, f)

# setup dataframe for plotting
soln_inf <- data.frame(
  time = t_vals,
  purple = model_inf[1,],
  green = model_inf[2,]
)

#draw with ggplot
ggplot(soln_inf) +
  geom_line(aes(x = time, y = purple, colour = "Purple"), linewidth = 0.9) +
  geom_line(aes(x = time, y = green, colour = "Green"), linewidth = 0.9) +
  
  #equilibrium label
  annotate("text",
           x = max(t_vals)*0.75,
           y = max(model_inf[1,])*0.75,
           label = paste0("(p*, g*) = (",
                          "(ag+e)/c", ", ",
                          "g", ")"),
           hjust = 0,
           size = 4,
           colour = "firebrick",
           fontface = "italic") +
  
  # labels for titles and axis
  labs(
    title = "Base Model: Infinite Equilibria",
    subtitle = "Condition: cd = ab",
    x = "Time",
    y = "Military Spending",
    colour = "Country",
    caption = paste0(
      "a = 0.4, b = 0.3, c = 0.6, d = 0.2, e = ",  round(e,2), ", f = ", round(f,2),
      "   |   cd = ", round(0.6*0.2,2),
      ", ab = ", round(0.4*0.3,2)
    )
  ) +
  
  scale_colour_manual(values = c("Purple" = "purple", "Green" = "green")) +
  
  theme_gray(base_size = 14)

#parameters reinitialized since I need to go back to the default case and run
#models again
e <- 12
f <- 15
