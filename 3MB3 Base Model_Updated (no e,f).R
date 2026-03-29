#load libraries
library(expm)
library(ggplot2)

#t-values
t_vals <- seq(0, 50, by = 0.1)

#inital conditions
p0 <- 35
g0 <- 20
x0 <- c(p0, g0)

#Model solution
model <- function(t_vals, x0, a, b, c, d) {
  A <- matrix(c(-c, a,
                b, -d), nrow = 2, byrow = TRUE)
    arms <- matrix(NA, nrow = 2, ncol = length(t_vals))
  for (i in 1:length(t_vals)) {
    t <- t_vals[i]
    arms[, i] <- expm(A * t) %*% x0
  }
  return(arms)
}

colours <- c("Purple" = "purple", "Green" = "green")


#Model 1 - Case 1(d): Line of Stable Nodes
a1 <- 0.4
b1 <- 0.3
c1 <- 0.6
d1 <- 0.2

model1 <- model(t_vals, x0, a1, b1, c1, d1)

soln1 <- data.frame(
  time = t_vals,
  purple = model1[1,],
  green = model1[2,]
)

ggplot(soln1) +
  geom_line(aes(x = time, y = purple, colour = "Purple"), linewidth = 0.9) +
  geom_line(aes(x = time, y = green, colour = "Green"), linewidth = 0.9) +
  labs(
    title = "Base Model: Line of Stable Nodes",
    subtitle = "Condition: cd = ab",
    x = "Time",
    y = "Military Spending",
    colour = "Country",
    caption = paste("a =", a1, ", b =", b1, ", c =", c1, ", d =", d1,
                    "| cd =", round(c1*d1,2), ", ab =", round(a1*b1,2))
  ) +
  scale_colour_manual(values = colours) +
  theme_gray(base_size = 14)

#Model 2 - Case 1(c): Unstable Saddle Point
a2 <- 1.2
b2 <- 1.1
c2 <- 0.3
d2 <- 0.3

model2 <- model(t_vals, x0, a2, b2, c2, d2)

soln2 <- data.frame(
  time = t_vals,
  purple = model2[1,],
  green = model2[2,]
)

ggplot(soln2) +
  geom_line(aes(x = time, y = purple, colour = "Purple"), linewidth = 0.9) +
  geom_line(aes(x = time, y = green, colour = "Green"), linewidth = 0.9) +
  labs(
    title = "Base Model: Unstable Saddle Point",
    subtitle = "Condition: cd < ab",
    x = "Time",
    y = "Military Spending",
    colour = "Country",
    caption = paste("a =", a2, ", b =", b2, ", c =", c2, ", d =", d2,
                    "| cd =", round(c2*d2,2), ", ab =", round(a2*b2,2))
  ) +
  scale_colour_manual(values = colours) +
  theme_gray(base_size = 14)


#Model 3: Case 1(e): Stable Nodes
a3 <- 0.4
b3 <- 0.3
c3 <- 0.8
d3 <- 0.9

model3 <- model(t_vals, x0, a3, b3, c3, d3)

soln3 <- data.frame(
  time = t_vals,
  purple = model3[1,],
  green = model3[2,]
)

ggplot(soln3) +
  geom_line(aes(x = time, y = purple, colour = "Purple"), linewidth = 0.9) +
  geom_line(aes(x = time, y = green, colour = "Green"), linewidth = 0.9) +
  labs(
    title = "Base Model: Stable Nodes",
    subtitle = "Condition: cd > ab",
    x = "Time",
    y = "Military Spending",
    colour = "Country",
    caption = paste("a =", a3, ", b =", b3, ", c =", c3, ", d =", d3,
                    "| cd =", round(c3*d3,2), ", ab =", round(a3*b3,2))
  ) +
  scale_colour_manual(values = colours) +
  theme_gray(base_size = 14)
