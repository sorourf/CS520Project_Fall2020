# Simulation for the CS project
library(plm)
library(stargazer)
set.seed(5)
rm(list=ls())


#Defining function for 

#Assumptions
#1) t=1 and t=2 pretest periods, t=3 posttest period; A1 and A2 are pretest treatment effects and A3 is the posttest treatment effect
#Error term= Epsilon, U=fixed effect for a unit (individual's unobservable characteristics)
# n is the number if units in panel data

# Passing coefficients to the funtion for 
PanelData_Sml <- function (n = 100, b = 0.5,c = 0.5,
                           A1 = 0, A2= 0, alpha = 0.5,
                           T1 = 0, T2 =1, T3 = 2 )
{
#1-1)t=1; Assme that b = 0.5 , A1=0
Epsilon1 <- rnorm(n, 0, 1)
U <- rnorm(n, 0, 1)
i <- rep(1:n)
Y1 <- A1 + b*U+ Epsilon1

Time1 <- data.frame(A1, U, Epsilon1, Y1, T1, i)

#1-2) t=2; Assume that c=0.5, A3=0
Epsilon2 <- rnorm(n, 0, 1)
Y2 <- A2 + b*U+ Epsilon2 + c*Y1

Time2 <- data.frame(A2, U, Epsilon2, Y2, T2, i)

#1-3) t=3; Assume that alpha (the causal effect of interest) is 0.5
Epsilon3 <- rnorm(n, 0, 1)
#Unobserved characteristics affect A3(treatment)
p <- 1/(1 + exp(-U))
A3 <- rbinom(n, 1, prob = p)

Y3 <- alpha*A3 + b*U+ Epsilon3 + c*Y2

Time3 <- data.frame(A3, U, Epsilon3, Y3, T3, i)

#2) Data aggregaion
colnames(Time1) <- c("A", "U", "E", "Y", "t", "i")
colnames(Time2) <- c("A", "U", "E", "Y", "t", "i")
colnames(Time3) <- c("A", "U", "E", "Y", "t", "i")
mydata <- do.call("rbind", list(Time1,Time2,Time3))


#3) Perform Regression Analysis for 3 approach
#3-1)Pooled OLS
pooledOLS <- lm(Y ~ A + U , data = mydata)
ols_coef = summary(pooledOLS)$coef[2,1]

#3-2)Within Transformation approach
within <- plm(Y ~ A + U, data=mydata, model="within" , index =c("i","t"))
within_coef = summary(within)$coef[1,1]

#3-3)First differencing approach
fd <- plm(Y ~ A + U, data=mydata, model="fd" , index =c("i","t"))
fd_coef = summary(fd)$coef[2,1]

# Ceating a list of Dataframe to get the coefficient of 
out = cbind(data.frame(ols_coef,within_coef,fd_coef))

return (out)
}

n_replication= 1000
#4)Replicating the simulaiton to get the expectation of estimations
result <- replicate(n_replication, PanelData_Sml())

#5)Calculating the expectation of estimators after replicating the simulation
a=0
b=0
c=0
i=1
for(i in 1:n_replication)
{
  a = a+(as.numeric(result[,,i][1]))
  b = b+(as.numeric(result[,,i][2]))
  c = c+(as.numeric(result[,,i][3]))
}
print("E(Pooled OLS)")
E_OLS=a/n_replication
print("E(within differencing)")
E_within=b/n_replication
print("E(First differencing)")
E_FD=c/n_replication

#Results for one interation of the simulation
stargazer( pooledOLS , fd , within ,column.labels = c("OLS", "First Differencing", "Within Transformation"),type = "html" ,out = "regression.html",  title="Results", align=TRUE)

ss