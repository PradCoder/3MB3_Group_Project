#############################################
# Logistic Arms Race Model for Canada and U.S 
##############################################

#time values
t_vals <- seq(0,350, by = 0.1)

#parameters e and f 
e <- 0.733582888 
f <- -0.59771515 

#capacity limits for military spending 
#Kp for Canada and Kg for U.S
Kp <- 40000
Kg <- 1041000

#initial conditions
#p is Canada and g is U.S
p0 <- 0
g0 <- 0

#create matrix to store model values
#columns are: model for Canada, model for US, time
arms <- matrix(NA, nrow = length(t_vals), ncol = 3)
arms[1,] <- c(p0, g0,1)

#use Euler's method to numerically simulate the model
arms_model <- function(t_vals, arms, a, b, c, d, e, f, Kp, Kg){
  h <- t_vals[2] - t_vals[1]
  for(t in 2:length(t_vals)){
    
    p_prev <- arms[t-1, 1]
    g_prev <- arms[t-1, 2]
    t_prev<-arms[t-1,3]
    
    #Logistic model equations
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

#to find the second numerical simulation for Canada and U.S  
#we change the initial conditions and time values and run the code again 
p0 <- 29065.9
g0 <-968381.6
t_vals <- seq(0,30, by = 0.1)
