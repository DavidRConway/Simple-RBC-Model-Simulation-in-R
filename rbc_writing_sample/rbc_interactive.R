library(tidyverse)
library(MASS)
library(Matrix)
library(Rcpp)
library(nleqslv)
library(gEcon)


#NB: WHEN THIS CODE IS RUN, IT WILL GENERATE NUMEROUS PLOTS SAVED TO the 
#'figures' FOLDER.


#Calling model from rbc_model.gcn & solving it-----------------------------
rbc_object <- make_model('rbc_setup.gcn')

rbc_object <- steady_state(rbc_object)
rbc_object <- solve_pert(rbc_object)


#Verifying model has been solved------------------------------------------
get_par_values(rbc_object)
get_ss_values(rbc_object)
re_solved(rbc_object)


#Generating figure 1-------------------------------------------------------

#Creating shock path with zero exogenous shocks occurring
no_exog_path <- matrix(
  c(0),
  nrow = 1,
  ncol = 1
)

#Simulating the simple RBC model in the absence of exogenous shocks
rbc_no_exog_sim <- simulate_model(
  rbc_object,
  variables  = c('K_s', 'W', 'r', 'L_s', 'A'),
  shocks     = 'epsilon_A',
  shock_path = no_exog_path,
  sim_length = 100
)

#Generating & saving plot
pdf("figures/figure_1.pdf")
plot_simulation(rbc_no_exog_sim)
dev.off()


#Generating figure 2------------------------------------------------------

#Simulating simple RBC model with stochastic exogenous productivity shocks
rbc_exog_sim <- random_path(
  rbc_object,
  variables = c('Y','W', 'r', 'C', 'L_s'),
  sim_length = 100
)

#Generating & saving plot
pdf("figures/figure_2.pdf")
plot_simulation(rbc_exog_sim)
dev.off()

#NB: figure 3 was generated via LaTeX----------------------------------------


#Generating figure 4-------------------------------------------------------

#Setting parameter value of rho to zero for zero shock persistence 
rbc_object_rho_zero <- set_free_par(
  model = rbc_object,
  list(
    rho = 0
  )
)

#Solving rbc model with rho = 0
rbc_object_rho_zero <- steady_state(rbc_object_rho_zero)
rbc_object_rho_zero <- solve_pert(rbc_object_rho_zero)


#Creating POSITIVE shock path with +1% productivity shock occuring in 10th period 
path_pos <- matrix(
  c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0.01), 
  nrow = 1,
  ncol = 10
)

#Simulating the simple RBC model with POSITIVE zero persistence shock for K, W, 
#r, L, A
rbc_zero_persist_pos_sim_K_W_r_L_a <- simulate_model(
  rbc_object_rho_zero,
  variables  = c('K_s', 'W', 'r', 'L_s', 'A'),
  shocks     = 'epsilon_A',
  shock_path = path_pos,
  sim_length = 25 
)

#Simulating the simple RBC model with POSITIVE zero persistence shock for C, 
#I, Y
rbc_zero_persist_pos_sim_C_I_Y <- simulate_model(
  rbc_object_rho_zero,
  variables  = c('C', 'I', 'Y'),
  shocks     = 'epsilon_A',
  shock_path = path_pos,
  sim_length = 25 
)


#Creating POSITIVE shock path with -1% producitivity shock occuring in 10th period 
path_neg <- matrix(
  c(0, 0, 0, 0, 0, 0, 0, 0, 0, -0.01), 
  nrow = 1,
  ncol = 10
)

#Simulating the simple RBC model with NEGATIVE zero persistence shock for K, W, 
#r, L, A
rbc_zero_persist__neg_sim_K_W_r_L_a <- simulate_model(
  rbc_object_rho_zero,
  variables  = c('K_s', 'W', 'r', 'L_s', 'A'),
  shocks     = 'epsilon_A',
  shock_path = path_neg,
  sim_length = 25 
)

#Simulating the simple RBC model with NEGATIVE zero persistence shock for C, 
#I,Y
rbc_zero_persist_neg_sim_C_I_Y <- simulate_model(
  rbc_object_rho_zero,
  variables  = c('C', 'I', 'Y'),
  shocks     = 'epsilon_A',
  shock_path = path_neg,
  sim_length = 25 
)

#Generating & saving plots for panel presented in figure 4
pdf("figures/figure_4_a.pdf")
plot_simulation(rbc_zero_persist_pos_sim_K_W_r_L_a)
dev.off()

pdf("figures/figure_4_b.pdf")
plot_simulation(rbc_zero_persist_pos_sim_C_I_Y)
dev.off()

pdf("figures/figure_4_c.pdf")
plot_simulation(rbc_zero_persist__neg_sim_K_W_r_L_a)
dev.off()

pdf("figures/figure_4_d.pdf")
plot_simulation(rbc_zero_persist_neg_sim_C_I_Y)
dev.off()


#Generating figure 5-------------------------------------------------------

#Simulating the simple RBC model with POSITIVE high persistence shock for K, W, 
#r, L, A
rbc_persist_pos_sim_K_W_r_L_a <- simulate_model(
  rbc_object,
  variables  = c('K_s', 'W', 'r', 'L_s', 'A'),
  shocks     = 'epsilon_A',
  shock_path = path_pos,
  sim_length = 100 
)

#Simulating the simple RBC model with POSITIVE high persistence shock for C, 
#I, Y
rbc_persist_pos_sim_C_I_Y <- simulate_model(
  rbc_object,
  variables  = c('C', 'I', 'Y'),
  shocks     = 'epsilon_A',
  shock_path = path_pos,
  sim_length = 100 
)

#Simulating the simple RBC model with NEGATIVE high persistence shock for K, W, 
#r, L, A
rbc_persist__neg_sim_K_W_r_L_a <- simulate_model(
  rbc_object,
  variables  = c('K_s', 'W', 'r', 'L_s', 'A'),
  shocks     = 'epsilon_A',
  shock_path = path_neg,
  sim_length = 100
)

#Simulating the simple RBC model with NEGATIVE high persistence shock for C, 
#I, Y
rbc_persist_neg_sim_C_I_Y <- simulate_model(
  rbc_object,
  variables  = c('C', 'I', 'Y'),
  shocks     = 'epsilon_A',
  shock_path = path_neg,
  sim_length = 100 
)

#Generating & saving plots for panel presented in figure 5
pdf("figures/figure_5_a.pdf")
plot_simulation(rbc_persist_pos_sim_K_W_r_L_a)
dev.off()

pdf("figures/figure_5_b.pdf")
plot_simulation(rbc_persist_pos_sim_C_I_Y)
dev.off()

pdf("figures/figure_5_c.pdf")
plot_simulation(rbc_persist__neg_sim_K_W_r_L_a)
dev.off()

pdf("figures/figure_5_d.pdf")
plot_simulation(rbc_persist_neg_sim_C_I_Y)
dev.off()

