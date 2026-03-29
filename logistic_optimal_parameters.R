#######
Canada
#######

#enter observed values for military spending 
canada<-SIPRI$Canada
usa<-SIPRI$USA

#create a function to calculate the sum of squared errors for given a,c,e values
ss <- function(par = c(a,c,e)) {
  with(as.list(par), {
    t_vals <- seq(0, 74, by = 1) # all time periods (75 years total)
    C_vals <- rep(NA, length(t_vals)) # empty vector for prediction
    C_vals[1] <- 4324.4 # initial spending (in 1950)
    #using Euler's method to predict spending in t+1
    #C(t+1)≈C(t)+h*(dC/dt)
    #h=1 for one year increases, dC = a*(1-(canada[i]/40000))*usa[i] - c*canada[i] + e
    for (i in 1:(length(C_vals) - 1)) {
      C_vals[i + 1] <- canada[i]+ a*(1-(canada[i]/40000))*usa[i] - c*canada[i] + e
    }
    res <- rep(NA, length(canada)) # create empty vector to store residuals 
    # calculate the square of each residual
    for (i in 1:length(res)) {
      res[i] <- (canada[i] - C_vals[i])^2
    }
    ss <- sum(res) # sum of all squared errors 
    return(ss) # return result as output
  })
}
# find a,c,e that minimizes the sum of squared errors 
fit <- optim(par = c(a = 0.5, c=0.5, e=1), fn = ss)

# Best fitted value of a,c,e
print(fit$par)
a_fit <- fit$par[1]
c_fit <- fit$par[2]
e_fit<- fit$par[3]


#######
USA
#######
#create a function to calculate the sum of squared errors for given b,d,f values
ss <- function(par = c(b,d,f)) {
  with(as.list(par), {
    t_vals <- seq(0, 74, by = 1) # all time periods (75 years total)
    U_vals <- rep(NA, length(t_vals)) # empty vector for prediction
    U_vals[1] <- 189049 # initial spending (in 1950)
    #using Euler's method to predict spending in t+1
    #U(t+1)=U(t)+h*(dU/dt)
    #h=1 for one year increases, dU = b*(1-(usa[i]/1041000))*canada[i]-d*usa[i]+f
    for (i in 1:(length(U_vals) - 1)) {
      U_vals[i + 1] <- usa[i]+ b*(1-(usa[i]/1041000))*canada[i]-d*usa[i]+f
    }
    res <- rep(NA, length(usa)) # create empty vector to store residuals 
    # calculate the square of each residual
    for (i in 1:length(res)) {
      res[i] <- (usa[i] - U_vals[i])^2
    }
    ss <- sum(res) # sum of all squared errors 
    return(ss) # return result as output
  })
}
# find b,d,f that minimizes the sum of squared errors 
fit <- optim(par = c(b = 0.5, d=0.5, f=1), fn = ss)

# Best fitted value of b,d,f
print(fit$par)
b_fit <- fit$par[1]
d_fit <- fit$par[2]
f_fit<- fit$par[3]

###############
Simulate the Model 
###############

#define time periods
#model 74 years, starting in 1951
t_vals <- seq(0, 73, by = 1)

#create empty matrix for storing values 
arms <- matrix(NA, nrow = length(t_vals), ncol = 5)

#fill in initial year is 1951 and initial spending for Canada and U.S
arms[1,] <- c(1951,8763.2,404493.1,canada[2],usa[2])

#run the model with calibrated parameters 

arms_model <- function(t_vals, arms, a, b, c, d, e, f,canada,usa){
  h <- t_vals[2] - t_vals[1]
  for(t in 2:length(t_vals)){
    
    t_prev<-arms[t-1,1]
    C_prev <- arms[t-1, 2]
    U_prev <- arms[t-1, 3]
    
    #Logistic model equations with logistic constraint
    dC <- a*(1 - C_prev/40000)*U_prev - c*C_prev+ e
    dU <- b*(1 - U_prev/1041000)*C_prev - d*U_prev + f
    
    C_new <- C_prev + h*dC
    U_new <- U_prev + h*dU
    t_new<-t_prev+1
    arms[t,] <- c(t_new, C_new,U_new,canada[t+1],usa[t+1])
  }
  return(arms)
}

model1 <- arms_model(t_vals, arms, a=a_fit,b=b_fit,c=c_fit,d=d_fit,e=e_fit,f=f_fit,canada=canada,usa=usa)

#plot the model 
model1<-data.frame(model1)

colours <- c("Observed" = "black", "Predicted" = "red")

library(ggplot2)
ggplot(model1) +
  geom_line(mapping = aes(x = X1, y = X2, colour = "Predicted"), lwd = 1) +
  geom_line(mapping = aes(x = X1, y = X4, colour = "Observed"), lwd = 1) +
  labs(x = "Year", y = "Military Spending (Constant 2023 USD millions)", title = "Predicted vs. Observed Military Spending in Canada",
       colour = "Legend") + scale_colour_manual(values = colours) + 
  theme(panel.background = element_rect(fill = "#FFE3C0"), axis.title.x = element_text(family = "sans", size = 12, margin=margin(10,0,0,0)), 
        axis.title.y = element_text(family = "sans", size = 12, margin=margin(0,10,0,0)), 
        plot.title = element_text(family = "sans", size = 14, margin=margin(0,0,10,0))) 

colours<-c("Observed"="black",Predicted="royalblue")

ggplot(model1) +
  geom_line(mapping = aes(x = X1, y = X3, colour = "Predicted"), lwd = 1) +
  geom_line(mapping = aes(x = X1, y = X5, colour = "Observed"), lwd = 1) +
  labs(x = "Year", y = "Military Spending (Constant 2023 USD millions)", title = "Predicted vs. Observed Military Spending in U.S",
       colour = "Legend") + scale_colour_manual(values = colours) + 
  theme(panel.background = element_rect(fill = "#bfcfda"), axis.title.x = element_text(family = "sans", size = 12, margin=margin(10,0,0,0)), 
        axis.title.y = element_text(family = "sans", size = 12, margin=margin(0,10,0,0)), 
        plot.title = element_text(family = "sans", size = 14, margin=margin(0,0,10,0))) 


