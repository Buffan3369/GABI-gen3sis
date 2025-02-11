################################################################################
# Playaround parameter space for selecting plausible range of values
################################################################################

## Logistic species abundance determined by environmental suitability ----------
a <- 0.23
omega_T <- seq(0.001, 1, 0.01)
N_t <- sapply(omega_T, FUN = function(x){exp( -a**2/(2*x**2) )})

a_t <- seq(0, 1, 0.001)
oT <- 0.05
N_ta <- sapply(a_t, FUN = function(x){exp( -x**2/(2*oT**2) )})

par(mfrow = c(1,2))
plot(omega_T, N_t, type = "l", main = "As a function of strength of filtering")
plot(a_t, N_ta, type = "l", main = "As a function of temperature differnce")

## Probability of extirpation --------------------------------------------------
N_sp <- 5 # species abundance
  # inflection fixed
delta <- seq(-1.5, 1.5, 0.01) # decay param, can be whatever real number
alpha <- 10 # inflection param
p_ext_decay <- sapply(delta, FUN = function(x){1 / (1 + exp(-(x*(alpha - N_sp))))})
  # decay fixed
delta1 <- 0.05
alpha1 <- seq(N_sp, 100, 1) # alpha can't be below N_sp (otherwise p_ext > 1)
p_ext_inflection <- sapply(alpha1, FUN = function(x){1 / (1 + exp(-(delta1*(x - N_sp)))) })
  # Plots
par(mfrow = c(1,2))
plot(delta, p_ext_decay, type = "l", main = "Prob extirpation as a function of decay")
plot(alpha1, p_ext_inflection, type = "l", main = "Prob extirpation as a function of inflection")
abline(v = N_sp, col = "red", lty = 2)
text(x = N_sp+8.5, y = 0.9, "Abundance", col = "red")

x <- 0.2

## Sobol pseudo-random sequence with low discrepancy ---------------------------
# library(randtoolbox)
# 
# a2 <- sobol(n=500, dim = 2)
# omega <- data.frame(om_t = runif(n = 500, min = 0.01, max = 0.076),
#                     om_p = runif(n = 500, min = 0.05, max = 0.125))
# par(mfrow = c(1,2))
# plot(x = omega[,1], y = omega[,2], 
#      xlab = "temp", ylab = "prec", main = "Uniform")
# plot(a2[,1]*(0.076-0.01)+0.01,
#      a2[,2]*(0.126-0.05)+0.05,
#      xlab = "temp", ylab = "prec", main = "Sobol")
