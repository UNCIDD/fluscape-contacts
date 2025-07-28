
require(rstan)
require(tidyverse)

###################### First-order analysis ##############################
## Hierarchical quasi-poisson regression

# Fully adjusted
full_1_res <- run_stan_model(stan_file = "code/stan_models/first_order_hier_full_quasipois.stan",
                             save_name = "model_results/new_runs/full_1",
                             run_type = "both",
                             data = tot_contacts %>% drop_na(),
                             analysis_type = "first_order",
                             hier = -TRUE, full = TRUE,
                             chains = 4, iter = 2000, cores = n_cores)
# Unadjusted - age
vars = c("age_cat0_15", "age_cat16_25", "age_cat26_35",
         "age_cat46_55", "age_cat56_65", "age_cat66_75",
         "age_cat76_")
age_1_res <- run_stan_model(stan_file = "code/stan_models/first_order_hier_unadj_quasipois.stan",
                            save_name = "model_results/new_runs/age_1",
                            run_type = "both",
                            data = tot_contacts %>% drop_na(),
                            var = vars, var_level = "obs",
                            analysis_type = "first_order",
                            hier = TRUE, full = FALSE,
                            chains = 4, iter = 2000, cores = n_cores)

# Unadjusted - occupation status
vars =  c("occ_cat_student", "occ_cat_other")
occ_1_res <- run_stan_model(stan_file = "code/stan_models/first_order_hier_unadj_quasipois.stan",
                            save_name = "model_results/new_runs/occ_1",
                            run_type = "both",
                            data = tot_contacts %>% drop_na(),
                            var = vars, var_level = "obs",
                            analysis_type = "first_order",
                            hier = TRUE, full = FALSE,
                            chains = 4, iter = 2000, cores = n_cores)

# Unadjusted - number of household members
vars =  c("HH_Num_Memb_1_3", "HH_Num_Memb_7_9", "HH_Num_Memb_10")
HH_num_1_res <- run_stan_model(stan_file = "code/stan_models/first_order_hier_unadj_quasipois.stan",
                               save_name = "model_results/new_runs/HH_num_1",
                               run_type = "both",
                               data = tot_contacts %>% drop_na(),
                               var = vars, var_level = "obs",
                               analysis_type = "first_order",
                               hier = TRUE, full = FALSE,
                               chains = 4, iter = 2000, cores = n_cores)

# Unadjusted - gender
vars <- c("PART_MALE")
gender_1_res <- run_stan_model(stan_file = "code/stan_models/first_order_hier_unadj_part_quasipois.stan",
                               save_name = "model_results/new_runs/gender_1",
                               run_type = "both",
                               data = tot_contacts %>% drop_na(),
                               var = vars, var_level = "part",
                               analysis_type = "first_order",
                               hier = TRUE, full = FALSE,
                               chains = 4, iter = 2000, cores = n_cores)

# Unadjusted - population density
vars <- c("log_ninetile_hh")
density_1_res <- run_stan_model(stan_file = "code/stan_models/first_order_hier_unadj_hh_quasipois.stan",
                               save_name = "model_results/new_runs/density_1",
                               run_type = "both",
                               data = tot_contacts %>% drop_na(),
                               var = vars, var_level = "hh",
                               analysis_type = "first_order",
                               hier = TRUE, full = FALSE,
                               chains = 4, iter = 2000, cores = n_cores)


###################### Second-order analysis ##############################
## Unadjusted non-hierarchical logistic regression

# Age
vars = c("age_cat0_15", "age_cat16_25", "age_cat26_35",
         "age_cat46_55", "age_cat56_65", "age_cat66_75",
         "age_cat76_")
age_nh_2_res <- run_stan_model(stan_file = "code/stan_models/second_order_nohier_unadj_group.stan",
                               save_name = "model_results/new_runs/age_nh_2",
                               run_type = "both",
                               data = contact_tri,
                               var = vars, var_level = "tri",
                               analysis_type = "second_order",
                               full = FALSE, grp_lik = TRUE, hier = FALSE,
                               group_ind = FALSE, group_int = FALSE,
                               chains = 4, iter = 2000, cores = n_cores)

# Occupation status
vars <- c("occ_cat_student", "occ_cat_other")
occ_nh_2_res <- run_stan_model(stan_file = "code/stan_models/second_order_nohier_unadj_group.stan",
                               save_name = "model_results/new_runs/occ_nh_2",
                               run_type = "both",
                               data = contact_tri,
                               var = vars, var_level = "tri",
                               analysis_type = "second_order",
                               full = FALSE, grp_lik = TRUE, hier = FALSE,
                               group_ind = FALSE, group_int = FALSE,
                               chains = 4, iter = 2000, cores = n_cores)

# Number of household members
vars <- c("HH_Num_Memb_1_3", "HH_Num_Memb_7_9","HH_Num_Memb_10")
HH_num_nh_2_res <- run_stan_model(stan_file = "code/stan_models/second_order_nohier_unadj_group.stan",
                                  save_name = "model_results/new_runs/HH_num_nh_2",
                                  run_type = "both",
                                  data = contact_tri,
                                  var = vars, var_level = "tri",
                                  analysis_type = "second_order",
                                  full = FALSE, grp_lik = TRUE, hier = FALSE,
                                  group_ind = FALSE, group_int = FALSE,
                                  chains = 4, iter = 2000, cores = n_cores)

# Gender
vars = c("PART_MALE")
gender_nh_2_res <- run_stan_model(stan_file = "code/stan_models/second_order_nohier_unadj_group.stan",
                                  save_name = "model_results/new_runs/gender_nh_2",
                                  run_type = "both",
                                  data = contact_tri,
                                  var = vars, var_level = "tri",
                                  analysis_type = "second_order",
                                  full = FALSE, grp_lik = TRUE, hier = FALSE,
                                  group_ind = FALSE, group_int = FALSE,
                                  chains = 4, iter = 2000, cores = n_cores)

# Density quartile
vars = c("dens_q1", "dens_q4")
dens_nh_2_res <- run_stan_model(stan_file = "code/stan_models/second_order_nohier_unadj_group.stan",
                                save_name = "model_results/new_runs/dens_nh_2",
                                run_type = "both",
                                data = contact_tri,
                                var = vars, var_level = "tri",
                                analysis_type = "second_order",
                                full = FALSE, grp_lik = TRUE, hier = FALSE,
                                group_ind = FALSE, group_int = FALSE,
                                chains = 4, iter = 2000, cores = n_cores)


## Hierarchical logistic regression

# Adjusted - participant characteristics
part_2_res <- run_stan_model(stan_file = "code/stan_models/second_order_hier_full_group.stan",
                             save_name = "model_results/new_runs/part_2",
                             run_type = "both",
                             save_chains = TRUE,
                             data = contact_tri,
                             analysis_type = "second_order",
                             full = TRUE, grp_lik = TRUE, hier = TRUE,
                             part_only = TRUE, group_ind = FALSE, group_int = FALSE,
                             chains = 4, iter = 2000, cores = n_cores)

# Adjusted - single contact characteristics
vars_ce <- c("loc_cat_school", "loc_cat_work", "loc_cat_other", "loc_cat_home",
             "dur_cat_10", "dur_cat_10_59","freq_cat_1", "freq_cat_1_3",
             "CONTACT_Touch", "HH_Num_Memb_1_3", "HH_Num_Memb_7_9","HH_Num_Memb_10",
             "occ_cat_student", "occ_cat_other", "age_cat0_15", "age_cat16_25",
             "age_cat26_35", "age_cat46_55", "age_cat56_65",
             "age_cat66_75", "age_cat76_")
ce_grpind_2_res <- run_stan_model(stan_file = "code/stan_models/second_order_hier_full_group.stan",
                                  save_name = "model_results/new_runs/ce_grpind_2",
                                  run_type = "both",
                                  save_chains = TRUE,
                                  data = contact_tri,
                                  var = vars_ce,
                                  analysis_type = "second_order",
                                  full = TRUE, grp_lik = TRUE, hier = TRUE,
                                  part_only = FALSE, group_ind = TRUE, group_int = TRUE,
                                  chains = 4, iter = 2000, cores = n_cores)

# Adjusted - single + shared contact characteristics
vars_all <- c(vars_ce,
              "shared_location", "shared_freq", "shared_dur", "both_touch")
all_grpind_2_res <- run_stan_model(stan_file = "code/stan_models/second_order_hier_full_group.stan",
                                   save_name = "model_results/new_runs/all_grpind_2",
                                   run_type = "both",
                                   save_chains = TRUE,
                                   data = contact_tri,
                                   var = vars_all,
                                   analysis_type = "second_order",
                                   full = TRUE, grp_lik = TRUE,
                                   part_only = FALSE, group_ind = TRUE, group_int = TRUE,
                                   chains = 4, iter = 2000, cores = n_cores)

# Unadjusted - age
vars = c("age_cat0_15", "age_cat16_25", "age_cat26_35",
         "age_cat46_55", "age_cat56_65", "age_cat66_75",
         "age_cat76_")
age_2_res <- run_stan_model(stan_file = "code/stan_models/second_order_hier_unadj_group.stan",
                            save_name = "model_results/new_runs/age_2",
                            run_type = "both",
                            data = contact_tri, var = vars, var_level = "tri",
                            analysis_type = "second_order",
                            full = FALSE, hier = TRUE, grp_lik = TRUE,
                            group_ind = FALSE, group_int = FALSE,
                            chains = 4, iter = 2000, cores = n_cores)

# Unadjusted - occupation status
vars <- c("occ_cat_student", "occ_cat_other")
occ_2_res <- run_stan_model(stan_file = "code/stan_models/second_order_hier_unadj_group.stan",
                            save_name = "model_results/new_runs/occ_2",
                            run_type = "both",
                            data = contact_tri, var = vars, var_level = "tri",
                            analysis_type = "second_order",
                            full = FALSE, hier = TRUE, grp_lik = TRUE,
                            group_ind = FALSE, group_int = FALSE,
                            chains = 4, iter = 2000, cores = n_cores)

# Unadjusted - number of household members
vars <- c("HH_Num_Memb_1_3", "HH_Num_Memb_7_9","HH_Num_Memb_10")
HH_num_2_res <- run_stan_model(stan_file = "code/stan_models/second_order_hier_unadj_group.stan",
                               save_name = "model_results/new_runs/HH_num_2",
                               run_type = "both",
                               data = contact_tri, var = vars, var_level = "tri",
                               analysis_type = "second_order",
                               full = FALSE, hier = TRUE, grp_lik = TRUE,
                               group_ind = FALSE, group_int = FALSE,
                               chains = 4, iter = 2000, cores = n_cores)

# Unadjusted - gender
vars <- c("PART_MALE")
gender_2_res <- run_stan_model(stan_file = "code/stan_models/second_order_hier_unadj_part_group.stan",
                               save_name = "model_results/new_runs/gender_2",
                               run_type = "both",
                               data = contact_tri,
                               var = vars, var_level = "part",
                               analysis_type = "second_order",
                               full = FALSE, grp_lik = TRUE, hier = TRUE,
                               group_ind = FALSE, group_int = FALSE,
                               chains = 4, iter = 2000, cores = n_cores)

# Unadjusted - population density
vars <- c("log_ninetile_hh")
density_2_res <- run_stan_model(stan_file = "code/stan_models/second_order_hier_unadj_hh_group.stan",
                               save_name = "model_results/new_runs/density_2",
                               run_type = "both",
                               data = contact_tri,
                               var = vars, var_level = "hh",
                               analysis_type = "second_order",
                               full = FALSE, grp_lik = TRUE, hier = TRUE,
                               group_ind = FALSE, group_int = FALSE,
                               chains = 4, iter = 2000, cores = n_cores)

# Unadjusted - contact setting
vars <- c("loc_cat_school", "loc_cat_work", "loc_cat_other", "loc_cat_home")
loc_2_res <- run_stan_model(stan_file = "code/stan_models/second_order_hier_unadj_group.stan",
                            save_name = "model_results/new_runs/loc_2",
                            run_type = "both",
                            data = contact_tri,
                            var = vars, var_level = "tri",
                            analysis_type = "second_order",
                            full = FALSE, grp_lik = TRUE, hier = TRUE,
                            group_ind = FALSE, group_int = FALSE,
                            chains = 4, iter = 2000, cores = n_cores)

# Unadjusted - contact duration
vars <- c("dur_cat_10", "dur_cat_10_59")
dur_2_res <- run_stan_model(stan_file = "code/stan_models/second_order_hier_unadj_group.stan",
                            save_name = "model_results/new_runs/dur_2",
                            run_type = "both",
                            data = contact_tri,
                            var = vars, var_level = "tri",
                            analysis_type = "second_order",
                            full = FALSE, grp_lik = TRUE, hier = TRUE,
                            group_ind = FALSE, group_int = FALSE,
                            chains = 4, iter = 2000, cores = n_cores)

# Unadjusted - contact frequency
vars <- c("freq_cat_1", "freq_cat_1_3")
freq_2_res <- run_stan_model(stan_file = "code/stan_models/second_order_hier_unadj_group.stan",
                            save_name = "model_results/new_runs/freq_2",
                            run_type = "both",
                            data = contact_tri,
                            var = vars, var_level = "tri",
                            analysis_type = "second_order",
                            full = FALSE, grp_lik = TRUE, hier = TRUE,
                            group_ind = FALSE, group_int = FALSE,
                            chains = 4, iter = 2000, cores = n_cores)

# Unadjusted - whether contact involved touch
vars <- c("CONTACT_Touch")
touch_2_res <- run_stan_model(stan_file = "code/stan_models/second_order_hier_unadj_group.stan",
                             save_name = "model_results/new_runs/touch_2",
                             run_type = "both",
                             data = contact_tri,
                             var = vars, var_level = "tri",
                             analysis_type = "second_order",
                             full = FALSE, grp_lik = TRUE, hier = TRUE,
                             group_ind = FALSE, group_int = FALSE,
                             chains = 4, iter = 2000, cores = n_cores)

# Unadjusted - both touch
vars <- c("both_touch")
both_touch_2_res <- run_stan_model(stan_file = "code/stan_models/second_order_hier_unadj_group.stan",
                                   save_name = "model_results/new_runs/both_touch_2",
                                   run_type = "both",
                                   data = contact_tri,
                                   var = vars, var_level = "tri",
                                   analysis_type = "second_order",
                                   full = FALSE, grp_lik = TRUE, hier = TRUE,
                                   group_ind = FALSE, group_int = FALSE,
                                   chains = 4, iter = 2000, cores = n_cores)

# Unadjusted - shared contact setting
vars <- c("shared_location")
sh_loc_2_res <- run_stan_model(stan_file = "code/stan_models/second_order_hier_unadj_group.stan",
                                   save_name = "model_results/new_runs/sh_loc_2",
                                   run_type = "both",
                                   data = contact_tri,
                                   var = vars, var_level = "tri",
                                   analysis_type = "second_order",
                                   full = FALSE, grp_lik = TRUE, hier = TRUE,
                                   group_ind = FALSE, group_int = FALSE,
                                   chains = 4, iter = 2000, cores = n_cores)

# Unadjusted - shared contact frequency
vars <- c("shared_freq")
sh_freq_2_res <- run_stan_model(stan_file = "code/stan_models/second_order_hier_unadj_group.stan",
                               save_name = "model_results/new_runs/sh_freq_2",
                               run_type = "both",
                               data = contact_tri,
                               var = vars, var_level = "tri",
                               analysis_type = "second_order",
                               full = FALSE, grp_lik = TRUE, hier = TRUE,
                               group_ind = FALSE, group_int = FALSE,
                               chains = 4, iter = 2000, cores = n_cores)

# Unadjusted - shared contact duration
vars <- c("shared_dur")
sh_dur_2_res <- run_stan_model(stan_file = "code/stan_models/second_order_hier_unadj_group.stan",
                                save_name = "model_results/new_runs/sh_dur_2",
                                run_type = "both",
                                data = contact_tri,
                                var = vars, var_level = "tri",
                                analysis_type = "second_order",
                                full = FALSE, grp_lik = TRUE, hier = TRUE,
                                group_ind = FALSE, group_int = FALSE,
                                chains = 4, iter = 2000, cores = n_cores)

