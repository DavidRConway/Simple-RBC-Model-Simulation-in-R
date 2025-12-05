# Generated on 2025-12-05 13:46:04 by gEcon ver. 1.2.3 (2025-04-13)
# http://gecon.r-forge.r-project.org/

# Model name: rbc_setup

# info
info__ <- c("rbc_setup", "/Users/dcsmac/Desktop/rbc_writing_sample/rbc_setup.gcn", "2025-12-05 13:46:04", "false")

# index sets
index_sets__ <- list()

# variables
variables__ <- c("r",
                 "A",
                 "C",
                 "I",
                 "K_s",
                 "L_s",
                 "U",
                 "W",
                 "Y")

variables_tex__ <- c("r",
                     "A",
                     "C",
                     "I",
                     "K^{\\mathrm{s}}",
                     "L^{\\mathrm{s}}",
                     "U",
                     "W",
                     "Y")

# shocks
shocks__ <- c("epsilon_A")

shocks_tex__ <- c("\\epsilon^{\\mathrm{A}}")

# parameters
parameters__ <- c("alpha",
                  "beta",
                  "delta",
                  "eta",
                  "mu",
                  "rho")

parameters_tex__ <- c("\\alpha",
                     "\\beta",
                     "\\delta",
                     "\\eta",
                     "\\mu",
                     "\\rho")

# free parameters
parameters_free__ <- c("alpha",
                       "beta",
                       "delta",
                       "eta",
                       "mu",
                       "rho")

# free parameters' values
parameters_free_val__ <- c(0.36,
                           0.99,
                           0.025,
                           1.5,
                           0.3,
                           0.979)

# equations
equations__ <- c("-r[] + alpha * A[] * K_s[-1]^(-1 + alpha) * L_s[]^(1 - alpha) = 0",
                 "-A[] + exp(epsilon_A[] + rho * log(A[-1])) = 0",
                 "-W[] + A[] * (1 - alpha) * K_s[-1]^alpha * L_s[]^(-alpha) = 0",
                 "-Y[] + A[] * K_s[-1]^alpha * L_s[]^(1 - alpha) = 0",
                 "beta * (mu * E[][r[1] * C[1]^(-1 + mu) * (1 - L_s[1])^(1 - mu) * (C[1]^mu * (1 - L_s[1])^(1 - mu))^(-eta)] + mu * (1 - delta) * E[][C[1]^(-1 + mu) * (1 - L_s[1])^(1 - mu) * (C[1]^mu * (1 - L_s[1])^(1 - mu))^(-eta)]) - mu * C[]^(-1 + mu) * (1 - L_s[])^(1 - mu) * (C[]^mu * (1 - L_s[])^(1 - mu))^(-eta) = 0",
                 "(-1 + mu) * C[]^mu * (1 - L_s[])^(-mu) * (C[]^mu * (1 - L_s[])^(1 - mu))^(-eta) + mu * W[] * C[]^(-1 + mu) * (1 - L_s[])^(1 - mu) * (C[]^mu * (1 - L_s[])^(1 - mu))^(-eta) = 0",
                 "-C[] - I[] + Y[] = 0",
                 "I[] - K_s[] + K_s[-1] * (1 - delta) = 0",
                 "U[] - beta * E[][U[1]] - (1 - eta)^-1 * (C[]^mu * (1 - L_s[])^(1 - mu))^(1 - eta) = 0")

# calibrating equations
calibr_equations__ <- character(0)

# variables / equations map
vareqmap__ <- sparseMatrix(i = c(1, 1, 1, 1, 2, 3, 3, 3, 3, 4,
                                 4, 4, 4, 5, 5, 5, 6, 6, 6, 7,
                                 7, 7, 8, 8, 9, 9, 9),
                           j = c(1, 2, 5, 6, 2, 2, 5, 6, 8, 2,
                                 5, 6, 9, 1, 3, 6, 3, 6, 8, 3,
                                 4, 9, 4, 5, 3, 6, 7),
                           x = c(2, 2, 1, 2, 3, 2, 1, 2, 2, 2,
                                 1, 2, 2, 4, 6, 6, 2, 2, 2, 2,
                                 2, 2, 2, 3, 2, 2, 6),
                           dims = c(9, 9))

# variables / calibrating equations map
varcalibreqmap__ <- sparseMatrix(i = NULL, j = NULL, dims = c(0, 9))

# calibrated parameters / equations map
calibrpareqmap__ <- sparseMatrix(i = NULL, j = NULL, dims = c(9, 0))

# calibrated parameters / calibrating equations map
calibrparcalibreqmap__ <- sparseMatrix(i = NULL, j = NULL, dims = c(0, 0))

# free parameters / equations map
freepareqmap__ <- sparseMatrix(i = c(1, 2, 3, 4, 5, 5, 5, 5, 6, 6,
                                     8, 9, 9, 9),
                               j = c(1, 6, 1, 1, 2, 3, 4, 5, 4, 5,
                                     3, 2, 4, 5),
                               x = rep(1, 14), dims = c(9, 6))

# free parameters / calibrating equations map
freeparcalibreqmap__ <- sparseMatrix(i = NULL, j = NULL, dims = c(0, 6))

# shocks / equations map
shockeqmap__ <- sparseMatrix(i = c(2),
                             j = c(1),
                             x = rep(1, 1), dims = c(9, 1))

# steady state equations
ss_eq__ <- function(v, pc, pf)
{
    r <- numeric(9)
    r[1] = -v[1] + pf[1] * v[2] * v[5]^(-1 + pf[1]) * v[6]^(1 - pf[1])
    r[2] = -v[2] + exp(pf[6] * log(v[2]))
    r[3] = -v[8] + v[2] * (1 - pf[1]) * v[5]^pf[1] * v[6]^(-pf[1])
    r[4] = -v[9] + v[2] * v[5]^pf[1] * v[6]^(1 - pf[1])
    r[5] = pf[2] * (pf[5] * v[1] * v[3]^(-1 + pf[5]) * (1 - v[6])^(1 - pf[5]) * (v[3]^pf[5] * (1 - v[6])^(1 - pf[5]))^(-pf[4]) + pf[5] * (1 - pf[3]) * v[3]^(-1 + pf[5]) * (1 - v[6])^(1 - pf[5]) * (v[3]^pf[5] * (1 - v[6])^(1 - pf[5]))^(-pf[4])) - pf[5] * v[3]^(-1 + pf[5]) * (1 - v[6])^(1 - pf[5]) * (v[3]^pf[5] * (1 - v[6])^(1 - pf[5]))^(-pf[4])
    r[6] = (-1 + pf[5]) * v[3]^pf[5] * (1 - v[6])^(-pf[5]) * (v[3]^pf[5] * (1 - v[6])^(1 - pf[5]))^(-pf[4]) + pf[5] * v[8] * v[3]^(-1 + pf[5]) * (1 - v[6])^(1 - pf[5]) * (v[3]^pf[5] * (1 - v[6])^(1 - pf[5]))^(-pf[4])
    r[7] = -v[3] - v[4] + v[9]
    r[8] = v[4] - v[5] + v[5] * (1 - pf[3])
    r[9] = v[7] - pf[2] * v[7] - (1 - pf[4])^-1 * (v[3]^pf[5] * (1 - v[6])^(1 - pf[5]))^(1 - pf[4])

    return(r)
}

# calibrating equations
calibr_eq__ <- function(v, pc, pf)
{
    r <- numeric(0)

    return(r)
}

# steady state and calibrating equations Jacobian
ss_calibr_eq_jacob__ <- function(v, pc, pf)
{
    r <- numeric(0)
    jac <- numeric(27)
    jac[1] = -1
    jac[2] = pf[1] * v[5]^(-1 + pf[1]) * v[6]^(1 - pf[1])
    jac[3] = pf[1] * v[2] * (-1 + pf[1]) * v[5]^(-2 + pf[1]) * v[6]^(1 - pf[1])
    jac[4] = pf[1] * v[2] * (1 - pf[1]) * v[5]^(-1 + pf[1]) * v[6]^(-pf[1])
    jac[5] = -1 + pf[6] * v[2]^-1 * exp(pf[6] * log(v[2]))
    jac[6] = (1 - pf[1]) * v[5]^pf[1] * v[6]^(-pf[1])
    jac[7] = pf[1] * v[2] * (1 - pf[1]) * v[5]^(-1 + pf[1]) * v[6]^(-pf[1])
    jac[8] = -pf[1] * v[2] * (1 - pf[1]) * v[5]^pf[1] * v[6]^(-1 - pf[1])
    jac[9] = -1
    jac[10] = v[5]^pf[1] * v[6]^(1 - pf[1])
    jac[11] = pf[1] * v[2] * v[5]^(-1 + pf[1]) * v[6]^(1 - pf[1])
    jac[12] = v[2] * (1 - pf[1]) * v[5]^pf[1] * v[6]^(-pf[1])
    jac[13] = -1
    jac[14] = pf[2] * pf[5] * v[3]^(-1 + pf[5]) * (1 - v[6])^(1 - pf[5]) * (v[3]^pf[5] * (1 - v[6])^(1 - pf[5]))^(-pf[4])
    jac[15] = pf[2] * (-pf[4] * pf[5]^2 * v[1] * (v[3]^(-1 + pf[5]))^2 * ((1 - v[6])^(1 - pf[5]))^2 * (v[3]^pf[5] * (1 - v[6])^(1 - pf[5]))^(-1 - pf[4]) - pf[4] * pf[5]^2 * (1 - pf[3]) * (v[3]^(-1 + pf[5]))^2 * ((1 - v[6])^(1 - pf[5]))^2 * (v[3]^pf[5] * (1 - v[6])^(1 - pf[5]))^(-1 - pf[4]) + pf[5] * v[1] * (-1 + pf[5]) * v[3]^(-2 + pf[5]) * (1 - v[6])^(1 - pf[5]) * (v[3]^pf[5] * (1 - v[6])^(1 - pf[5]))^(-pf[4]) + pf[5] * (-1 + pf[5]) * (1 - pf[3]) * v[3]^(-2 + pf[5]) * (1 - v[6])^(1 - pf[5]) * (v[3]^pf[5] * (1 - v[6])^(1 - pf[5]))^(-pf[4])) + pf[4] * pf[5]^2 * (v[3]^(-1 + pf[5]))^2 * ((1 - v[6])^(1 - pf[5]))^2 * (v[3]^pf[5] * (1 - v[6])^(1 - pf[5]))^(-1 - pf[4]) - pf[5] * (-1 + pf[5]) * v[3]^(-2 + pf[5]) * (1 - v[6])^(1 - pf[5]) * (v[3]^pf[5] * (1 - v[6])^(1 - pf[5]))^(-pf[4])
    jac[16] = pf[2] * (pf[5] * v[1] * (-1 + pf[5]) * v[3]^(-1 + pf[5]) * (1 - v[6])^(-pf[5]) * (v[3]^pf[5] * (1 - v[6])^(1 - pf[5]))^(-pf[4]) + pf[5] * (-1 + pf[5]) * (1 - pf[3]) * v[3]^(-1 + pf[5]) * (1 - v[6])^(-pf[5]) * (v[3]^pf[5] * (1 - v[6])^(1 - pf[5]))^(-pf[4]) - pf[4] * pf[5] * v[1] * (-1 + pf[5]) * v[3]^(-1 + 2 * pf[5]) * (1 - v[6])^(-pf[5]) * (1 - v[6])^(1 - pf[5]) * (v[3]^pf[5] * (1 - v[6])^(1 - pf[5]))^(-1 - pf[4]) - pf[4] * pf[5] * (-1 + pf[5]) * (1 - pf[3]) * v[3]^(-1 + 2 * pf[5]) * (1 - v[6])^(-pf[5]) * (1 - v[6])^(1 - pf[5]) * (v[3]^pf[5] * (1 - v[6])^(1 - pf[5]))^(-1 - pf[4])) - pf[5] * (-1 + pf[5]) * v[3]^(-1 + pf[5]) * (1 - v[6])^(-pf[5]) * (v[3]^pf[5] * (1 - v[6])^(1 - pf[5]))^(-pf[4]) + pf[4] * pf[5] * (-1 + pf[5]) * v[3]^(-1 + 2 * pf[5]) * (1 - v[6])^(-pf[5]) * (1 - v[6])^(1 - pf[5]) * (v[3]^pf[5] * (1 - v[6])^(1 - pf[5]))^(-1 - pf[4])
    jac[17] = pf[5] * (-1 + pf[5]) * v[3]^(-1 + pf[5]) * (1 - v[6])^(-pf[5]) * (v[3]^pf[5] * (1 - v[6])^(1 - pf[5]))^(-pf[4]) - pf[4] * pf[5]^2 * v[8] * (v[3]^(-1 + pf[5]))^2 * ((1 - v[6])^(1 - pf[5]))^2 * (v[3]^pf[5] * (1 - v[6])^(1 - pf[5]))^(-1 - pf[4]) + pf[5] * v[8] * (-1 + pf[5]) * v[3]^(-2 + pf[5]) * (1 - v[6])^(1 - pf[5]) * (v[3]^pf[5] * (1 - v[6])^(1 - pf[5]))^(-pf[4]) - pf[4] * pf[5] * (-1 + pf[5]) * v[3]^(-1 + 2 * pf[5]) * (1 - v[6])^(-pf[5]) * (1 - v[6])^(1 - pf[5]) * (v[3]^pf[5] * (1 - v[6])^(1 - pf[5]))^(-1 - pf[4])
    jac[18] = -pf[4] * (-1 + pf[5])^2 * (v[3]^pf[5])^2 * ((1 - v[6])^(-pf[5]))^2 * (v[3]^pf[5] * (1 - v[6])^(1 - pf[5]))^(-1 - pf[4]) + pf[5] * (-1 + pf[5]) * v[3]^pf[5] * (1 - v[6])^(-1 - pf[5]) * (v[3]^pf[5] * (1 - v[6])^(1 - pf[5]))^(-pf[4]) + pf[5] * v[8] * (-1 + pf[5]) * v[3]^(-1 + pf[5]) * (1 - v[6])^(-pf[5]) * (v[3]^pf[5] * (1 - v[6])^(1 - pf[5]))^(-pf[4]) - pf[4] * pf[5] * v[8] * (-1 + pf[5]) * v[3]^(-1 + 2 * pf[5]) * (1 - v[6])^(-pf[5]) * (1 - v[6])^(1 - pf[5]) * (v[3]^pf[5] * (1 - v[6])^(1 - pf[5]))^(-1 - pf[4])
    jac[19] = pf[5] * v[3]^(-1 + pf[5]) * (1 - v[6])^(1 - pf[5]) * (v[3]^pf[5] * (1 - v[6])^(1 - pf[5]))^(-pf[4])
    jac[20] = -1
    jac[21] = -1
    jac[22] = 1
    jac[23] = 1
    jac[24] = -pf[3]
    jac[25] = -pf[5] * v[3]^(-1 + pf[5]) * (1 - v[6])^(1 - pf[5]) * (v[3]^pf[5] * (1 - v[6])^(1 - pf[5]))^(-pf[4])
    jac[26] = -(-1 + pf[5]) * v[3]^pf[5] * (1 - v[6])^(-pf[5]) * (v[3]^pf[5] * (1 - v[6])^(1 - pf[5]))^(-pf[4])
    jac[27] = 1 - pf[2]
    jacob <- sparseMatrix(i = c(1, 1, 1, 1, 2, 3, 3, 3, 3, 4,
                                4, 4, 4, 5, 5, 5, 6, 6, 6, 7,
                                7, 7, 8, 8, 9, 9, 9),
                          j = c(1, 2, 5, 6, 2, 2, 5, 6, 8, 2,
                                5, 6, 9, 1, 3, 6, 3, 6, 8, 3,
                                4, 9, 4, 5, 3, 6, 7),
                          x = jac, dims = c(9, 9))

    return(jacob)
}

# 1st order perturbation
pert1__ <- function(v, pc, pf)
{
    Atm1x <- numeric(5)
    Atm1x[1] = pf[1] * v[2] * (-1 + pf[1]) * v[5]^(-2 + pf[1]) * v[6]^(1 - pf[1])
    Atm1x[2] = pf[6] * v[2]^-1 * exp(pf[6] * log(v[2]))
    Atm1x[3] = pf[1] * v[2] * (1 - pf[1]) * v[5]^(-1 + pf[1]) * v[6]^(-pf[1])
    Atm1x[4] = pf[1] * v[2] * v[5]^(-1 + pf[1]) * v[6]^(1 - pf[1])
    Atm1x[5] = 1 - pf[3]
    Atm1 <- sparseMatrix(i = c(1, 2, 3, 4, 8),
                         j = c(5, 2, 5, 5, 5),
                         x = Atm1x, dims = c(9, 9))

    Atx <- numeric(23)
    Atx[1] = -1
    Atx[2] = pf[1] * v[5]^(-1 + pf[1]) * v[6]^(1 - pf[1])
    Atx[3] = pf[1] * v[2] * (1 - pf[1]) * v[5]^(-1 + pf[1]) * v[6]^(-pf[1])
    Atx[4] = -1
    Atx[5] = (1 - pf[1]) * v[5]^pf[1] * v[6]^(-pf[1])
    Atx[6] = -pf[1] * v[2] * (1 - pf[1]) * v[5]^pf[1] * v[6]^(-1 - pf[1])
    Atx[7] = -1
    Atx[8] = v[5]^pf[1] * v[6]^(1 - pf[1])
    Atx[9] = v[2] * (1 - pf[1]) * v[5]^pf[1] * v[6]^(-pf[1])
    Atx[10] = -1
    Atx[11] = pf[4] * pf[5]^2 * v[3]^(-2 + 2 * pf[5]) * (1 - v[6])^(2 - 2 * pf[5]) * (v[3]^pf[5] * (1 - v[6])^(1 - pf[5]))^(-1 - pf[4]) - pf[5] * (-1 + pf[5]) * v[3]^(-2 + pf[5]) * (1 - v[6])^(1 - pf[5]) * (v[3]^pf[5] * (1 - v[6])^(1 - pf[5]))^(-pf[4])
    Atx[12] = -pf[5] * (-1 + pf[5]) * v[3]^(-1 + pf[5]) * (1 - v[6])^(-pf[5]) * (v[3]^pf[5] * (1 - v[6])^(1 - pf[5]))^(-pf[4]) + pf[4] * pf[5] * (-1 + pf[5]) * v[3]^(-1 + 2 * pf[5]) * (1 - v[6])^(1 - 2 * pf[5]) * (v[3]^pf[5] * (1 - v[6])^(1 - pf[5]))^(-1 - pf[4])
    Atx[13] = pf[5] * (-1 + pf[5]) * v[3]^(-1 + pf[5]) * (1 - v[6])^(-pf[5]) * (v[3]^pf[5] * (1 - v[6])^(1 - pf[5]))^(-pf[4]) - pf[4] * pf[5] * (-1 + pf[5]) * v[3]^(-1 + 2 * pf[5]) * (1 - v[6])^(1 - 2 * pf[5]) * (v[3]^pf[5] * (1 - v[6])^(1 - pf[5]))^(-1 - pf[4]) - pf[4] * pf[5]^2 * v[8] * v[3]^(-2 + 2 * pf[5]) * (1 - v[6])^(2 - 2 * pf[5]) * (v[3]^pf[5] * (1 - v[6])^(1 - pf[5]))^(-1 - pf[4]) + pf[5] * v[8] * (-1 + pf[5]) * v[3]^(-2 + pf[5]) * (1 - v[6])^(1 - pf[5]) * (v[3]^pf[5] * (1 - v[6])^(1 - pf[5]))^(-pf[4])
    Atx[14] = -pf[4] * (-1 + pf[5])^2 * v[3]^(2 * pf[5]) * (1 - v[6])^(-2 * pf[5]) * (v[3]^pf[5] * (1 - v[6])^(1 - pf[5]))^(-1 - pf[4]) + pf[5] * (-1 + pf[5]) * v[3]^pf[5] * (1 - v[6])^(-1 - pf[5]) * (v[3]^pf[5] * (1 - v[6])^(1 - pf[5]))^(-pf[4]) + pf[5] * v[8] * (-1 + pf[5]) * v[3]^(-1 + pf[5]) * (1 - v[6])^(-pf[5]) * (v[3]^pf[5] * (1 - v[6])^(1 - pf[5]))^(-pf[4]) - pf[4] * pf[5] * v[8] * (-1 + pf[5]) * v[3]^(-1 + 2 * pf[5]) * (1 - v[6])^(1 - 2 * pf[5]) * (v[3]^pf[5] * (1 - v[6])^(1 - pf[5]))^(-1 - pf[4])
    Atx[15] = pf[5] * v[3]^(-1 + pf[5]) * (1 - v[6])^(1 - pf[5]) * (v[3]^pf[5] * (1 - v[6])^(1 - pf[5]))^(-pf[4])
    Atx[16] = -1
    Atx[17] = -1
    Atx[18] = 1
    Atx[19] = 1
    Atx[20] = -1
    Atx[21] = -pf[5] * v[3]^(-1 + pf[5]) * (1 - v[6])^(1 - pf[5]) * (v[3]^pf[5] * (1 - v[6])^(1 - pf[5]))^(-pf[4])
    Atx[22] = (1 - pf[5]) * v[3]^pf[5] * (1 - v[6])^(-pf[5]) * (v[3]^pf[5] * (1 - v[6])^(1 - pf[5]))^(-pf[4])
    Atx[23] = 1
    At <- sparseMatrix(i = c(1, 1, 1, 2, 3, 3, 3, 4, 4, 4,
                             5, 5, 6, 6, 6, 7, 7, 7, 8, 8,
                             9, 9, 9),
                       j = c(1, 2, 6, 2, 2, 6, 8, 2, 6, 9,
                             3, 6, 3, 6, 8, 3, 4, 9, 4, 5,
                             3, 6, 7),
                         x = Atx, dims = c(9, 9))

    Atp1x <- numeric(4)
    Atp1x[1] = pf[2] * pf[5] * v[3]^(-1 + pf[5]) * (1 - v[6])^(1 - pf[5]) * (v[3]^pf[5] * (1 - v[6])^(1 - pf[5]))^(-pf[4])
    Atp1x[2] = pf[2] * (pf[5] * (v[1] * (-1 + pf[5]) * v[3]^(-2 + pf[5]) * (1 - v[6])^(1 - pf[5]) * (v[3]^pf[5] * (1 - v[6])^(1 - pf[5]))^(-pf[4]) - pf[4] * pf[5] * v[1] * v[3]^(-2 + 2 * pf[5]) * (1 - v[6])^(2 - 2 * pf[5]) * (v[3]^pf[5] * (1 - v[6])^(1 - pf[5]))^(-1 - pf[4])) + pf[5] * (1 - pf[3]) * ((-1 + pf[5]) * v[3]^(-2 + pf[5]) * (1 - v[6])^(1 - pf[5]) * (v[3]^pf[5] * (1 - v[6])^(1 - pf[5]))^(-pf[4]) - pf[4] * pf[5] * v[3]^(-2 + 2 * pf[5]) * (1 - v[6])^(2 - 2 * pf[5]) * (v[3]^pf[5] * (1 - v[6])^(1 - pf[5]))^(-1 - pf[4])))
    Atp1x[3] = pf[2] * (pf[5] * (v[1] * (-1 + pf[5]) * v[3]^(-1 + pf[5]) * (1 - v[6])^(-pf[5]) * (v[3]^pf[5] * (1 - v[6])^(1 - pf[5]))^(-pf[4]) - pf[4] * v[1] * (-1 + pf[5]) * v[3]^(-1 + 2 * pf[5]) * (1 - v[6])^(1 - 2 * pf[5]) * (v[3]^pf[5] * (1 - v[6])^(1 - pf[5]))^(-1 - pf[4])) + pf[5] * (1 - pf[3]) * ((-1 + pf[5]) * v[3]^(-1 + pf[5]) * (1 - v[6])^(-pf[5]) * (v[3]^pf[5] * (1 - v[6])^(1 - pf[5]))^(-pf[4]) - pf[4] * (-1 + pf[5]) * v[3]^(-1 + 2 * pf[5]) * (1 - v[6])^(1 - 2 * pf[5]) * (v[3]^pf[5] * (1 - v[6])^(1 - pf[5]))^(-1 - pf[4])))
    Atp1x[4] = -pf[2]
    Atp1 <- sparseMatrix(i = c(5, 5, 5, 9),
                         j = c(1, 3, 6, 7),
                         x = Atp1x, dims = c(9, 9))

    Aepsx <- numeric(1)
    Aepsx[1] = exp(pf[6] * log(v[2]))
    Aeps <- sparseMatrix(i = c(2),
                         j = c(1),
                         x = Aepsx, dims = c(9, 1))

    return(list(Atm1, At, Atp1, Aeps))
}

ext__ <- list()

# create model object
gecon_model(model_info = info__,
            index_sets = index_sets__,
            variables = variables__,
            variables_tex = variables_tex__,
            shocks = shocks__,
            shocks_tex = shocks_tex__,
            parameters = parameters__,
            parameters_tex = parameters_tex__,
            parameters_free = parameters_free__,
            parameters_free_val = parameters_free_val__,
            equations = equations__,
            calibr_equations = calibr_equations__,
            var_eq_map = vareqmap__,
            shock_eq_map = shockeqmap__,
            var_ceq_map = varcalibreqmap__,
            cpar_eq_map = calibrpareqmap__,
            cpar_ceq_map = calibrparcalibreqmap__,
            fpar_eq_map = freepareqmap__,
            fpar_ceq_map = freeparcalibreqmap__,
            ss_function = ss_eq__,
            calibr_function = calibr_eq__,
            ss_calibr_jac_function = ss_calibr_eq_jacob__,
            pert = pert1__,
            ext = ext__)
