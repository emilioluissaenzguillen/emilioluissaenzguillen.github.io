# ==============================================================================
# AS3209 Probabilistic Modelling - R Lab Session 2: Copulas                        
# Author:       Emilio Luis Sáenz Guillén                                   
# Professor:    Russell Gerrard                                            
# Institution:  Bayes Business School - City St George's, University of London 
# Date:         19th March 2026                                              
# Description:  This script introduces Copulas and       
#               demonstrates practical applications using R.                
# Dependencies: copula, Deriv, data.table, numDeriv, plotly, plot3D
#                                                    
# ==============================================================================
# First, clear all
rm(list = ls())
graphics.off()
# Set directory
setwd("C:\\Users\\addj700\\Dropbox\\Bayes Business School\\Teaching\\Bayes Business School\\2025-26\\AS3209 - Probabilistic Modelling\\Copulas")
# Install/load packages
pacman::p_load(copula, data.table, numDeriv, plotly, plot3D)

# Define color palette
col_prime <- colorRampPalette(c("blue", "gray", "yellow", "orange", "red"))(100)

## C(u1, u2) = C(F(x1), F(x2)) = F(x1, x2) = P( X1 ≤ x1, X2 ≤ x2) (by Sklar's Theorem)
# Why is this amazing? Because it means we can model the marginals and the dependence separately.

# Define copula dimension
d = 2

# C(u1, u2) in [0,1]; u1 and u2 in [0,1]
u1 <- u2 <- seq(0, 1, length.out = 50) 

# build a grid
u1u2 <- expand.grid("u1" = u1, "u2" = u2)
u1u2

## Fundamental copulas:
# 1) The independence copula
P <- u1u2[,1]*u1u2[,2] # this means that the joint cumulative probability is just
# the product of the individual probabilities, which is the defining property of
# independence.
# 2) The co-monotonicity copula
M <- pmin(u1u2[,1], u1u2[,2]) 
# 3) The counter-monotonicity copula
W <- pmax(u1u2[,1] + u1u2[,2] - 1, 0) 

## Fréchet–Hoeffding (FH) bounds:
# For any d-dimensional copula, C,
# W(u) ≤ C(u) ≤ M(u)


## Copula densities (see 4.4 in the lecture notes)
# Define the copulas
indep_copula <- function(u) u[1] * u[2]
comonotonicity_copula <- function(u) pmin(u[1], u[2])
countermonotonicity_copula <- function(u) pmax(u[1] + u[2] - 1, 0)

# 1) Compute second derivative d^2C/du1du2 for the independence copula
u1u2$d2C_du1du2 <- mapply(function(u1, u2) {
  H <- hessian(indep_copula, c(u1, u2))  # Compute Hessian
  return(H[1,2])  # Extract d^2C/du1du2
}, u1u2$u1, u1u2$u2)
# Store the results for the independence copula
Pd <- round(u1u2$d2C_du1du2, 6) # to avoid numerical precision issues

# 2) Compute second derivative for the comonotonicity copula
u1u2$d2C_du1du2 <- mapply(function(u1, u2) {
  H <- hessian(comonotonicity_copula, c(u1, u2))  # Compute Hessian
  return(H[1,2])  # Extract d^2C/du1du2
}, u1u2$u1, u1u2$u2)
# Store the results for the comonotonicity copula
Md <- round(u1u2$d2C_du1du2, 6) # to avoid numerical precision issues
Md[Md < 0] <- 0

# 3) Compute second derivative d^2C/du1du2 for the counter-monotonicity copula
u1u2$d2C_du1du2 <- mapply(function(u1, u2) {
  H <- hessian(countermonotonicity_copula, c(u1, u2))  # Compute Hessian
  return(H[1,2])  # Extract d^2C/du1du2
}, u1u2$u1, u1u2$u2)
# Store the results for the counter-monotonicity copula
Wd <- round(u1u2$d2C_du1du2, 6) # to avoid numerical precision issues
Wd[Wd < 0] <- 0

u1u2$d2C_du1du2 <- NULL

####################
## 1. Copula plot ##
####################
# Plot 3d figure with plotly
# converting to matrix (needed for plot_ly)
P_m <- matrix(P, nrow = length(u1), ncol = length(u2))
M_m <- matrix(M, nrow = length(u1), ncol = length(u2))
W_m <- matrix(W, nrow = length(u1), ncol = length(u2))

Pd_m <- matrix(Pd, nrow = length(u1), ncol = length(u2))
Md_m <- matrix(Md, nrow = length(u1), ncol = length(u2))
Wd_m <- matrix(Wd, nrow = length(u1), ncol = length(u2))


# 1) Independence Copula
plot_ly(x = u1, y = u2, z = P_m, type = "surface") %>%
  layout(title = "$\\Pi(u_1, u_2) = u_1 \\cdot u_2$",
         scene = list(xaxis = list(title = "u1"),
                      yaxis = list(title = "u2"),
                      zaxis = list(title = "C(u1, u2)"))) %>%
  config(mathjax = 'cdn')
# 2) Co-monotonicity Copula
plot_ly(x = u1, y = u2, z = M_m, type = "surface") %>%
  layout(title = "$M(u_1, u_2) = min(u_1, u_2)$",
         scene = list(xaxis = list(title = "u1"),
                      yaxis = list(title = "u2"),
                      zaxis = list(title = "C(u1, u2)"))) %>%
  config(mathjax = 'cdn')
# 3) Counter-monotonicity Copula
# The copula surface is zero whenever u1+u2 ≤ 1
plot_ly(x = u1, y = u2, z = W_m, type = "surface") %>%
  layout(title = "$W(u_1, u_2) = max(u_1 + u_2 - 1, 0)$",
         scene = list(xaxis = list(title = "u1"),
                      yaxis = list(title = "u2"),
                      zaxis = list(title = "C(u1, u2)"))) %>%
  config(mathjax = 'cdn')

# Alternatively, using plot3D

# 1) Independence Copula
persp3D(
  x = u1, y = u2, z = P_m, col = col_prime, theta = 30, phi = 30, expand = 0.618,
  main = bquote(Pi * "(" * u[1] * "," * u[2] * ")" ~ "=" ~ u[1] * u[2]),
  xlab = "u1", ylab = "u2", zlab = "C(u1, y2)", ticktype = "detailed", colkey = FALSE,
  border = "black", cex.main = 1.5, cex.axis = 1.25, cex.lab = 1.25
)
# 2) Co-monotonicity Copula
persp3D(
  x = u1, y = u2, z = M_m, col = col_prime, theta = 30, phi = 30, expand = 0.618,
  main = bquote(M * "(" * u[1] * "," * u[2] * ")" ~ "=" ~ min * "(" * u[1] * "," * u[2] * ")"),
  xlab = "u1", ylab = "u2", zlab = "C(u1, y2)", ticktype = "detailed", colkey = FALSE,
  border = "black", cex.main = 1.5, cex.axis = 1.25, cex.lab = 1.25
)
# 3) Counter-monotonicity Copula
persp3D(
  x = u1, y = u2, z = W_m, col = col_prime, theta = 30, phi = 30, expand = 0.618,
  main = bquote(W * "(" * u[1] * "," * u[2] * ")" ~ "=" ~ max * "(" * u[1] + u[2] - 1 * "," * 0 * ")"),
  xlab = "u1", ylab = "u2", zlab = "C(u1, y2)", ticktype = "detailed", colkey = FALSE,
  border = "black", cex.main = 1.25, cex.axis = 1.25, cex.lab = 1.25
)

############################
## 2. Copula density plot ##
############################
# 1) Independence Copula
persp3D(
  x = u1, y = u2, z = Pd_m, col = col_prime, theta = 30, phi = 30, expand = 0.618,
  main = bquote(Pi * "(" * u[1] * "," * u[2] * ")" ~ "=" ~ u[1] * u[2]),
  xlab = "u1", ylab = "u2", zlab = "C(u1, y2)", ticktype = "detailed", colkey = FALSE,
  border = "black", cex.main = 1.5, cex.axis = 1.25, cex.lab = 1.25
)
# 2) Co-monotonicity Copula
persp3D(
  x = u1, y = u2, z = log(1+Md_m), col = col_prime, theta = 30, phi = 30, expand = 0.618,
  main = bquote(M * "(" * u[1] * "," * u[2] * ")" ~ "=" ~ min * "(" * u[1] * "," * u[2] * ")"),
  xlab = "u1", ylab = "u2", zlab = "C(u1, y2)", ticktype = "detailed", colkey = FALSE,
  border = "black", cex.main = 1.5, cex.axis = 1.25, cex.lab = 1.25
)
# 3) Counter-monotonicity Copula
persp3D(
  x = u1, y = u2, z = log(1+Wd_m), col = col_prime, theta = 30, phi = 30, expand = 0.618,
  main = bquote(W * "(" * u[1] * "," * u[2] * ")" ~ "=" ~ max * "(" * u[1] + u[2] - 1 * "," * 0 * ")"),
  xlab = "u1", ylab = "u2", zlab = "C(u1, y2)", ticktype = "detailed", colkey = FALSE,
  border = "black", cex.main = 1.25, cex.axis = 1.25, cex.lab = 1.25
)


####################
## 3. Contourplot ##
####################
# Plot contour figures with contourplot2 (in Copula library)
# A contour plot represents a 3-D surface by plotting lines that connect points
# with common z-values along a slice.
# append grid (needed for contourplot2)
val.P <- cbind(u1u2, "P(u[1],u[2])" = P)
val.M <- cbind(u1u2, "M(u[1],u[2])" = M)
val.W <- cbind(u1u2, "W(u[1],u[2])" = W)

# 1) Independence Copula
contourplot2(val.P, col.regions = col_prime,
             main = bquote(Pi * "(" * u[1] * "," * u[2] * ")" ~ "=" ~ u[1] * u[2]))
# The same copula value can be achieved with many different combinations of u1 and u2

# 2) Co-monotonicity Copula
contourplot2(val.M, col.regions = col_prime,
             main = bquote(M * "(" * u[1] * "," * u[2] * ")" ~ "=" ~ min * "(" * u[1] * "," * u[2] * ")"))
# Only when both u1 and u2 increase, the copula value (which represents the joint
# probability) increases, reflecting perfect positive dependence

# 3) Counter-monotonicity Copula
contourplot2(val.W, col.regions = col_prime,
             main = bquote(W * "(" * u[1] * "," * u[2] * ")" ~ "=" ~ max * "(" * u[1] + u[2] - 1 * "," * 0 * ")"))
# The same copula value is achieved with high u1 and low u2 OR
# with low u1 and high u2



################################################################################
################################# Exercise 0 ###################################
################################################################################
# Simulate data from this copulas and produce a scatterplot of the marginals:
# Set seed for reproducibility
set.seed(123)

n = 1000
# Generate samples
u1 <- runif(n)  # independent u1
u2_indep <- runif(n)  # independent u2 (for independence copula)
u2_comon <- u1  # co-monotonicity: u1 = u2
u2_counter <- pmax(1 - u1, 0)  # counter-monotonicity: u2 = 1 - u1

plot(u1, u2_indep)
plot(u1, u2_comon)
plot(u1, u2_counter)


################################################################################
################## Exercise 1 (see p.80 of the lecture notes) ##################
################################################################################
# Let C_theta denote the bivariate Clayton copula with parameter theta > 0:
#   C_theta(u1, u2) = (u1^{-theta} + u2^{-theta} - 1)^(-1/theta).
#
# Using Sklar’s theorem, define the joint cumulative distribution function (cdf):
#   F(x1, x2) = C_theta(F1(x1), F2(x2)),
# where F1 and F2 are the marginal cdfs.
# Derive an explicit expression for the joint cdf F(x1, x2) and specify its domain
# in the following cases:
#
# (a) Assume X1 and X2 both follow an exponential distribution with rate alpha > 0:
#       F1(x1) = 1 - exp(-alpha * x1),   F2(x2) = 1 - exp(-alpha * x2),   x1, x2 >= 0.
#
# (b) For the setting in part (a), choose specific parameter values (e.g., theta = 2,
#     alpha = 0.5), evaluate:
#       F(x1, x2) = C_theta(F1(x1), F2(x2))
#     on a grid of (x1, x2)-values, and produce:
#       - a 3D surface plot of F,
#       - a contour plot of F.
#     You may use the R package 'copula' or any other appropriate method.
# ------------------------------------------------------------

# Clayton Copula
# Lower tail dependence, i.e., stronger dependence in negative extreme events
# (e.g., financial downturns).
# It is often used in credit risk modeling and insurance applications where joint
# defaults or large claims are correlated.

theta = 2

# subdivision points in each dimension
x1 = x2 = seq(0, 10, length.out = 50)
x1x2 = as.matrix(expand.grid(x1, x2))

# exponential distribution
alpha = 0.5
F1 = pexp(x1, alpha)
F2 = pexp(x2, alpha)

# build a grid 
F1F2 <- as.matrix(expand.grid("F1" = F1, "F2" = F1))

# Define Clayton Copula
cc = claytonCopula(theta, dim = d)

# Get Values of Copula at grid points
C = pCopula(F1F2, copula = cc)
#?pCopula

# Plot 3d figure with persp3D
# converting to matrix (needed for persp3D)
C_m <- matrix(C, ncol = length(F1), nrow= length(F2)) 

# Plot
persp3D(
  x = x1, y = x2, z = C_m, col = col_prime, theta = 30, phi = 30, expand = 0.618,
  main = bquote(F[X] * "(" * x[1] * "," ~ x[2] * ")" ~ ", " ~ theta == .(theta)),
  xlab = "X1", ylab = "X2", zlab = "", ticktype = "detailed", colkey = FALSE,
  border = "black", cex.main = 1.5, cex.axis = 1.25, cex.lab = 1.25
)

# Plot contour figures with contourplot2 (in Copula library)
# append grid (needed for contourplot2)
val.C <- cbind(F1F2, "C(u[1],u[2])" = C) 
# Plot, note: col.regions can be chosen arbitrarily. If not chosen it will be gray.
contourplot2(val.C, col.regions = col_prime,
             main = bquote(F[X] * "(" * x[1] * "," ~ x[2] * ")" ~ ", " ~ theta == .(theta)))


#####################
## Survival copula ##
#####################
# The survival copula flips cc, showing strong upper tail dependence
u1 = u2 = seq(0,1, length.out = 50)
u1u2 = as.matrix(expand.grid(u1, u2))

# Copula (see eq. (37) in the lecture notes)
C = pCopula(u1u2, copula = cc)
C_s <- u1 + u2 - 1 + pCopula(1-u1u2, copula = cc)
# or alternatively
cc_s = rotCopula(cc)
C_s = pCopula(u1u2, copula = cc_s)

# Copula density
Cd = dCopula(u1u2, copula = cc)

Cd_s = dCopula(1-u1u2, copula = cc)
# or alternatively
Cd_s = dCopula(u1u2, copula = cc_s)

# Plot 3d figure with persp3D
# converting to matrix (needed for persp3D)
C_s_m <- matrix(C_s, ncol = length(u1), nrow= length(u2))
Cd_m <- matrix(Cd, ncol = length(u1), nrow= length(u2)) 
Cd_s_m <- matrix(Cd_s, ncol = length(u1), nrow= length(u2)) 

# Plots

# 1) Copula
persp3D(
  x = u1, y = u2, z = C_m, col = col_prime, theta = 30, phi = 30, expand = 0.618,
  main = bquote(C[theta]^Gl * "(" * u[1] * "," ~ u[2] * ")" ~ ", " ~ theta == .(theta)),
  xlab = "u1", ylab = "u2", zlab = "C(u1, y2)", ticktype = "detailed", colkey = FALSE,
  border = "black", cex.main = 1.5, cex.axis = 1.25, cex.lab = 1.25
)
persp3D(
  x = u1, y = u2, z = C_s_m, col = col_prime, theta = 30, phi = 30, expand = 0.618,
  main = bquote(C[theta]^Gl * "(" * u[1] * "," ~ u[2] * ")" ~ ", " ~ theta == .(theta)),
  xlab = "u1", ylab = "u2", zlab = "C(u1, y2)", ticktype = "detailed", colkey = FALSE,
  border = "black", cex.main = 1.5, cex.axis = 1.25, cex.lab = 1.25
)
# 2) Copula density
persp3D(
  x = u1, y = u2, z = Cd_m, col = col_prime, theta = 30, phi = 30, expand = 0.618,
  main = bquote(C[theta]^Cl * "(" * u[1] * "," ~ u[2] * ")" ~ ", " ~ theta == .(theta)),
  xlab = "u1", ylab = "u2", zlab = "C(u1, y2)", ticktype = "detailed", colkey = FALSE,
  border = "black", cex.main = 1.5, cex.axis = 1.25, cex.lab = 1.25
)
persp3D(
  x = u1, y = u2, z = Cd_s_m, col = col_prime, theta = 30, phi = 30, expand = 0.618,
  main = bquote(C[theta]^Cl * "(" * u[1] * "," ~ u[2] * ")" ~ ", " ~ theta == .(theta)),
  xlab = "u1", ylab = "u2", zlab = "C(u1, y2)", ticktype = "detailed", colkey = FALSE,
  border = "black", cex.main = 1.5, cex.axis = 1.25, cex.lab = 1.25
)

# 3) Contour plots
val.Cp <- cbind(u1u2, "Cp" = C) 
val.Cp_s <- cbind(u1u2, "Cp_s" = C_s) 

contourplot2(val.Cp, col.regions = col_prime)
contourplot2(val.Cp_s, col.regions = col_prime)



################################################################################

# Fast and easy plotting of Copulas 

# Fig 4.04.
rho = 0.7071
nc = normalCopula(rho, dim = d)

par(mfrow = c(2,2))
persp(nc, dCopula, main = bquote(c[rho]^Ga * "(" * u[1] * "," ~ u[2] * ")" ~ ", " ~ rho == .(rho)))
contour(nc, pCopula, main = bquote(C[rho]^Ga * "(" * u[1] * "," ~ u[2] * ")" ~ ", " ~ rho == .(rho)))
contour(nc, dCopula, main = bquote(c[rho]^Ga * "(" * u[1] * "," ~ u[2] * ")" ~ ", " ~ rho == .(rho)))

c = rCopula(1000, nc)
plot(c, xlab = '', ylab = '', main = bquote(c[rho]^Ga * "(" * u[1] * "," ~ u[2] * ")" ~ ", " ~ rho == .(rho)))
par(mfrow = c(1,1))

# Fig. Fig 4.05
rho = 0.7071
v = 1
tc = tCopula(rho, dim = d, df = v)

par(mfrow = c(2,2))
persp(tc, dCopula, main = bquote(c[v * "," ~ rho]^.(t) * "(" * u[1] * "," ~ u[2] * ")" ~ ", " ~ v == .(v) ~ ", " ~ rho == .(rho)))
contour(tc, pCopula, main = bquote(C[v * "," ~ rho]^.(t) * "(" * u[1] * "," ~ u[2] * ")" ~ ", " ~ v == .(v) ~ ", " ~ rho == .(rho)))
contour(tc, dCopula, main = bquote(c[v * "," ~ rho]^.(t) * "(" * u[1] * "," ~ u[2] * ")" ~ ", " ~ v == .(v) ~ ", " ~ rho == .(rho)))

c = rCopula(1000, nc)
plot(c, xlab = '', ylab = '', main = bquote(c[v * "," ~ rho]^.(t) * "(" * u[1] * "," ~ u[2] * ")" ~ ", " ~ v == .(v) ~ ", " ~ rho == .(rho)))
par(mfrow = c(1,1))


################################################################################
################## Exercise 2 (see p.87 of the lecture notes) ##################
################################################################################
# For the meta distribution with cdf, F, constructed in Example 1, (Part 1 (a)):
# (a) derive an expression of its density f, assuming that θ ≥ 0;
# Using the R package copula, or otherwise:
# (b) produce 3D and contour plots of f.


md = mvdc(claytonCopula(theta, dim = d), margins = c('exp', 'exp'), paramMargins = list(alpha, alpha))

# quick plots
persp(md, dMvdc, xlim = c(0,8), ylim = c(0,8), zlab = 'Density')
contour(md, dMvdc, xlim = c(0, 1), ylim = c(0, 1))

# fancier plots
C_m <- matrix(dMvdc(x1x2, md), nrow = length(u1), ncol = length(u2))
persp3D(
  x = x1, y = x2, z = C_m, col = col_prime, theta = 30, phi = 30, expand = 0.618,
  main = bquote(f[X] * "(" * x[1] * "," ~ x[2] * ")" ~ ", " ~ theta == .(theta)),
  xlab = "X1", ylab = "X2", zlab = "", ticktype = "detailed", colkey = FALSE,
  border = "black", cex.main = 1.5, cex.axis = 1.25, cex.lab = 1.25
)
contourplot2(md, dMvdc, xlim = c(0,0.1), ylim = c(0,0.1), col.regions = col_prime)


################################################################################
################## Exercise 6 (see p.99 in the lecture notes) ##################
################################################################################
# For the meta distribution with cdf, F, constructed in Example 1, (Part 1 (a)):
# (a) describe an algorithm for simulating a random variate (X1, X2) ∼ F.
# Using the R package copula, or otherwise:
# (b) produce a contour plot of f(x1, x2), scatter plot of (X1,i, X2,i) ∼ f(x1, x2), i = 1,…, n,
# for a fixed sample size, n.

# Set seed for reproducibility
set.seed(123)

contourplot2(md, dMvdc, xlim = c(0,5), ylim = c(0,5), col.regions = col_prime)

r_md = rMvdc(10000, md)

plot(r_md, col = "steelblue", pch = 16, xlab = "", ylab = "", bty = "n",
     main = bquote(f[X] * "(" * x[1] * "," ~ x[2] * ")" ~ ", " ~ theta == .(theta)))


################################################################################
#                                                                              #   
#   Copulas: Fitting                                                           #
#                                                                              #  
################################################################################

# Simulate some data for the marginals
alpha = 3; lambda_gamma = 1 
x1 = rgamma(10000, shape = alpha, rate = lambda_gamma)
lambda_exp = 3 
x2 = rexp(10000, rate = lambda_exp)

# Now assume that we are provided the data
# and that we don't know the generating process that underlies it:

# Smartly define a start value for the Gaussian copula parameter, to allow for fast
# and proper convergence
kendall_tau <- cor(x1, x2, method = "kendall")
rho_gaussian <- sin(pi*kendall_tau/2)
rho_gaussian 

# Set the ML estimates of the parameters of the marginals as initial values in MLE
# estimation and also for using them in the IFM method:
library(MASS)
x1_ml <- fitdistr(x1, "gamma")
alpha_ml <- x1_ml$estimate[["shape"]]
lambda_gamma_ml <- x1_ml$estimate[["rate"]]

x2_ml <- fitdistr(x2, "exponential")
lambda_exp_ml <- x2_ml$estimate[["rate"]]

# 1) MLE
nc = normalCopula(dim = 2)

tMvdn <- mvdc(nc, c("gamma", "exp"),
              param = list(list(2, 0.5),
                           list(2)))


fit_n <- fitMvdc(cbind(x1,x2), tMvdn,
                 start = c(alpha_ml, lambda_gamma_ml, lambda_exp_ml, rho_gaussian))
fit_n


# 2) IFM

# Prepare marginal cdfs
F1 = pgamma(x1, alpha_ml, lambda_gamma)
F2 = pexp(x2, lambda_exp_ml)
F12 = cbind(F1,F2)
F12

# Fit meta distribution based on IFM
IFM_fit_n = fitCopula(normalCopula(), F12, method = "ml")
IFM_fit_n
IFM_fit_c = fitCopula(claytonCopula(), F12, method="ml")
IFM_fit_c


# 3) OM

# Compute pseudo-observations for X1 and X2
po1 = pobs(x1)  
po2 = pobs(x2)
po12 = cbind(po1, po2)

# Fit meta distribution based on OM
OMfitn = fitCopula(normalCopula(), po12, method = "ml")
OMfitn
OMfitc = fitCopula(claytonCopula(), po12, method = "ml")
OMfitc


################################################################################
#                                                                              #
# Rank correlation measures: Kendall's tau                                     #
#                                                                              # 
################################################################################
# Measures the ordinal association between two variables, i.e., the similarity of
# the orderings of the data when ranked by each of the variables
# Based on the number of concordant and discordant pairs, providing a nonparametric 
# alternative to Pearson's correlation.

pearson_corr <- cor(x1, x2, method = "pearson")
cat("Pearson's Correlation: ", pearson_corr, "\n")

tau_emp <- cor(x1, x2, method = "kendall")
tau_n <- tau(normalCopula(param=coef(OMfitn),dim = 2))
tau_c <- tau(claytonCopula(param=coef(OMfitc),dim = 2))

# Print results
cat("Kendall's Tau (Empirical): ", tau_emp, "\n",
    "Kendall's Tau (Gaussian): ", tau_n, "\n",
    "Kendall's Tau (Clayton): ", tau_c, "\n")


rho_emp <- cor(x1, x2, method = "spearman")
rho_n <- rho(normalCopula(param=coef(OMfitn),dim = 2))
rho_c <- rho(claytonCopula(param=coef(OMfitc),dim = 2))


# Print results
cat("Spearman's rho (Empirical): ", rho_emp, "\n",
    "Spearman's rho (Gaussian): ", rho_n, "\n",
    "Spearman's rho (Clayton): ", rho_c, "\n")



# Difference Kendall's Tau and Pearson Correlation
# In contrast to the Pearson correlation, the Kendall's rank correlation is a
# non-parametric test procedure. For the calculation of Kendall's tau, the data
# must not be normally distributed and the two variables must only have an ordinal
# scale level.
# Kendall's Tau should be preferred over Spearman's correlation when there is very
# little data and many rank ties (i.e., many values are the same, causing repeated ranks).





