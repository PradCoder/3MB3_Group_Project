####################
# Arms Race Model
# Richardson Model
####################

## Sourcer Grind R script from https://tbb.bio.uu.nl/rdb/grind.html
## for plotting nullclines and other phase diagrams
source("grind.R")

#time values
t_vals <- seq(0, 50, by = 0.1)

#parameters - calibrated from the data points
#values of e and f shift the equilibrium but do not change stability results - confirm
e <- 1.13357055
f <- 1.10615370

#initial conditions determine the starting point of the simulation
#values below or above equilibrium affect whether spending initially increases or decreases
#p0 <- 12
#g0 <- 15
p0 <- 35
g0 <- 20

#create matrix to store model values: simulated spending levels of Purple and Green, respectively
arms <- matrix(NA, nrow = 2, ncol = length(t_vals))
arms[,1] <- c(p0, g0)


model <- function(t, state, parms){
  with(as.list(c(state,parms)), {
    dp <- a*g - c*p + e ## CANADA - Purple
    dg <- b*p - d*g + f ## USA - Green
    return(list(c(dp, dg)))
  })
}

run(50)

# Define parameters
p <- c(a = 0.4, b = 0.3, c = 0.8, d = 0.9,e=1.13357055,f=1.10615370)
s <- c(p=35, g=20)
plane(main = "Phase Portrait of Base Model For Stable Equilibrium", xlab = "Purple", ylab="Green", xmin = 0, xmax = 100, ymin = 0, ymax = 100,tstep=0.1,vector=TRUE,portrait = TRUE)
run(traject=TRUE)
newton(plot=TRUE)
points(p0, g0, col="red", pch=19)  # starting point
text(p0, g0, "Start", pos=4)

run(50)
run(traject=TRUE,col = c("purple", "green"), add = TRUE)
# Define parameters
p <- c(a = 1.2, b = 1.1, c = 0.3, d = 0.3,e=1.13357055,f=1.10615370)
s <- c(p=35, g=20)
plane(main = "Phase Portrait of Base Model For Escalating Race", xlab = "Purple", ylab="Green",xmin = 0, xmax = 1.7e+19, ymin = 0, ymax = 1.4e+19,tstep=0.1,vector=TRUE,portrait = TRUE)
newton(plot=TRUE)
points(p0, g0, col="red", pch=19)  # starting point
text(p0, g0, "Start", pos=4)

run(50)
# Define parameters
p <- c(a = 1.5, b = 0.3, c = 0.8, d = 0.9,e=1.13357055,f=1.10615370)
s <- c(p=35, g=20)
plane(main = "Phase Portrait of Asymmetric Arms Race", xlab = "Purple", ylab="Green",xmin = 0, xmax = 100, ymin = 0, ymax = 100,tstep=0.1,vector=TRUE, portrait = TRUE)
run(traject=TRUE)
newton(plot=TRUE)
points(p0, g0, col="red", pch=19)  # starting point
text(p0, g0, "Start", pos=4)