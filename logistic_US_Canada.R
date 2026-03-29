#############################
# Logistic Arms Race Model
# With Logistic Constraints
############################

#time values
t_vals <- seq(0,350, by = 0.1)

#parameters
#values of e and f shift the equilibrium but do not change stability results
e <- 0.733582888 
f <- -0.59771515 

#capacity limits for military spending (logistic constraint parameters)
Kp <- 40000
Kg <- 1041000

#initial conditions determine the starting point of the simulation
#values below or above equilibrium affect whether spending initially increases or decreases
p0 <- 0
g0 <- 0

#create matrix to store model values: simulated spending levels of Purple and Green, respectively
arms <- matrix(NA, nrow = length(t_vals), ncol = 3)
arms[1,] <- c(p0, g0,1)

#Euler method to numerically simulate the logistic arms race model
arms_model <- function(t_vals, arms, a, b, c, d, e, f, Kp, Kg){
  h <- t_vals[2] - t_vals[1]
  for(t in 2:length(t_vals)){
    
    p_prev <- arms[t-1, 1]
    g_prev <- arms[t-1, 2]
    t_prev<-arms[t-1,3]
    
    #Logistic model equations with logistic constraint
    dp <- a*(1 - p_prev/Kp)*g_prev - c*p_prev + e
    dg <- b*(1 - g_prev/Kg)*p_prev - d*g_prev + f
    
    p_new <- p_prev + h*dp
    g_new <- g_prev + h*dg
    t_new<-t_prev+1
    
    arms[t,] <- c(p_new, g_new,t_new)
  }
  return(arms)
}

#plot the model 
model1 <- arms_model(t_vals, arms, a = 0.001025041, b = 3.06035844, c = 0.006444388, d = 0.01104621, e, f, Kp, Kg)
model1<-data.frame(model1)

colours <- c( "U.S" = "royalblue","Canada" = "red")

library(ggplot2)
ggplot(model1) +
  geom_line(mapping = aes(x=X3, y = X1, colour = "Canada"), lwd = 1)  +
  geom_line(mapping = aes(x=X3, y = X2, colour = "U.S"), lwd = 1) +
  labs(x = "Time", y = "Military Spending (Constant 2023 USD millions)", title = "Military Spending in Canada and U.S",
       colour = "Legend") + scale_colour_manual(values = colours)  + 
  theme(axis.title.x = element_text(family = "sans", size = 12, margin=margin(10,0,0,0)), 
        axis.title.y = element_text(family = "sans", size = 12, margin=margin(0,10,0,0)), 
        plot.title = element_text(family = "sans", size = 14, margin=margin(0,0,10,0)))
        
