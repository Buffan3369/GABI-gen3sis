# Header -------------------------------------------------------------
# Fixed parameters across species for the M0 model (Precipitations and
# Temperature constraints)
# Value range from Skeels et al. (2023)

# Parameters ---------------------------------------------------------
params <- list(
  # Random seed
  seed = 123,
  # Max total nb. of species
  max_species = 12000,
  # Max nb. of species per cell
  max_coex_sp = 300,
  # Number of species to start with
  start_species = 1,
  # Divergence threshold above which speciation occurs
  S = runif(1, min = 0.75, max = 1.25),
  # Temperature and precipitation niche breadth
  omega_T = runif(1, 0.01, 0.076), omega_P = runif(1, 0.05, 0.125),
  # Rate of environment niche evolution
  sigma_e = runif(1, 0.0075, 0.0225),
  # Shape and scale of the dispersal kernel (Weibull)
  disp_shape = runif(1, 1, 3), disp_scale = runif(1, 0.25, 2))



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
