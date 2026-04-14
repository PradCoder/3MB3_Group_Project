# install.packages("readxl")
library(readxl)
SIPRI <- as.data.frame(read_excel("SIPRI.xlsx"))

#######
# Canada
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
    #h=1 for one year increases, dp = a*usa[i] - c*canada[i] + e
    for (i in 1:(length(C_vals) - 1)) {
      C_vals[i + 1] <- canada[i]+ a*usa[i] - c*canada[i] + e
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

#define time periods 
t_vals <- seq(0, 74, by = 1)

#create empty vector for storing predicted Canadian military spending 
C_vals <- rep(NA, length(t_vals))

#initial spending (year is 1950)
C_vals[1] <- 4324.4
  
#run the model with calibrated parameters 
for (i in 1:(length(C_vals) - 1)) {
C_vals[i + 1] <- canada[i]+ a_fit*usa[i] -c_fit*canada[i] + e_fit
}

#plot the model 
plot(t_vals, C_vals, type = "l", xlab = "Time (years)", ylab = "Military spending (USD millions)",
     main="Predicted vs. Observed Military Spending in Canada",ylim=range(canada))
    
#add observed data to the model 
lines(t_vals,canada, pch = 16, col = "red")

#add legend 
legend("topleft",
       legend=c("Observed", "Predicted"),
       col=c("red","black"),
       lwd=2,
       cex=0.8)

#######
# USA
#######
#create a function to calculate the sum of squared errors for given b,d,f values
ss <- function(par = c(b,d,f)) {
  with(as.list(par), {
    t_vals <- seq(0, 74, by = 1) # all time periods (75 years total)
    U_vals <- rep(NA, length(t_vals)) # empty vector for prediction
    U_vals[1] <- 189049 # initial spending (in 1950)
    #using Euler's method to predict spending in t+1
    #h=1 for one year increases, dg = b*canada[i] - d*usa[i] + f
    for (i in 1:(length(U_vals) - 1)) {
      U_vals[i + 1] <- usa[i]+ b*canada[i] - d*usa[i] + f
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
# find a,c,e that minimizes the sum of squared errors 
fit <- optim(par = c(b = 0.5, d=0.5, f=1), fn = ss)

# Best fitted value of b,d,f
print(fit$par)
b_fit <- fit$par[1]
d_fit <- fit$par[2]
f_fit<- fit$par[3]

#define time periods 
t_vals <- seq(0, 74, by = 1)

#create empty vector for storing predicted Canadian military spending 
U_vals <- rep(NA, length(t_vals))

#initial spending (year is 1950)
U_vals[1] <- 189049

#run the model with calibrated parameters 
for (i in 1:(length(U_vals) - 1)) {
  U_vals[i + 1] <- usa[i]+ b_fit*canada[i] -d_fit*usa[i] + f_fit
}

#plot the model 
plot(t_vals, U_vals, type = "l", xlab = "Time (years)", ylab = "Military spending (USD millions)",
     main="Predicted vs. Observed Military Spending in the US",ylim=range(usa))

#add observed data to the model 
lines(t_vals,usa, pch = 16, col = "blue")

#add legend 
legend("topleft",
       legend=c("Observed", "Predicted"),
       col=c("blue","black"),
       lwd=2,
       cex=0.8)