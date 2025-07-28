
# Load utility functions
source("code/utility_functions.R")

# Models are very computationally intensive
# Set to FALSE to use pre-saved model results
# Running on a computing cluster is highly recommended
run_models <- FALSE
n_cores <- 4 # can change to 4 for parallel computing

# Load data
tot_contacts <- read.csv("data/total_contacts.csv") # number of daily contacts for each participant-visit
ind_contacts <- read.csv("data/individual_contacts.csv")
contact_tri <- read.csv("data/contact_triangles.csv") # potential contact triangles


if(run_models) {
  source("code/run_models.R")
} else {
  # Load first-order contact regression model results
  # From hierarchical quasi-poisson regression models
  full_1_res <- read.csv("model_results/pre_saved/full_1_res.csv")
  age_1_res <- read.csv("model_results/pre_saved/age_1_res.csv") 
  occ_1_res <- read.csv("model_results/pre_saved/occ_1_res.csv") 
  HH_num_1_res <- read.csv("model_results/pre_saved/HH_num_1_res.csv") 
  gender_1_res <- read.csv("model_results/pre_saved/gender_1_res.csv") 
  density_1_res <- read.csv("model_results/pre_saved/density_1_res.csv") 
  
  # Second-order descriptive results for probability of triangle connection
  # From un-adjusted, non-hierarchical logistic regression models
  age_nh_2_res <- read.csv("model_results/pre_saved/age_nh_2_res.csv") 
  occ_nh_2_res <- read.csv("model_results/pre_saved/occ_nh_2_res.csv") 
  HH_num_nh_2_res <- read.csv("model_results/pre_saved/HH_num_nh_2_res.csv") 
  gender_nh_2_res <- read.csv("model_results/pre_saved/gender_nh_2_res.csv") 
  density_nh_2_res <- read.csv("model_results/pre_saved/dens_nh_2_res.csv") 
  
  # Load second-order contact regression model results
  # From hierarchical logistic regression models
  # all_2_res <- read.csv("model_results/pre_saved/all_grpind_soc_2_res.csv")
  all_2_res <- read.csv("model_results/pre_saved/all_grpind_soc_2_res_update.csv") # UPDATE
  # ce_2_res <- read.csv("model_results/pre_saved/ce_grpind_soc_2_res.csv")
  ce_2_res <- read.csv("model_results/pre_saved/ce_grpind_soc_2_res_update.csv") # UPDATE
  # part_2_res <- read.csv("model_results/pre_saved/part_2_res.csv")
  part_2_res <- read.csv("model_results/pre_saved/part_2_res.csv") # UPDATE
  age_2_res <- read.csv("model_results/pre_saved/age_2_res.csv") 
  occ_2_res <- read.csv("model_results/pre_saved/occ_2_res.csv") 
  HH_num_2_res <- read.csv("model_results/pre_saved/HH_num_2_res.csv") 
  gender_2_res <- read.csv("model_results/pre_saved/gender_2_res.csv") 
  density_2_res <- read.csv("model_results/pre_saved/density_2_res.csv")
  dur_2_res <- read.csv("model_results/pre_saved/dur_2_res.csv")
  freq_2_res <- read.csv("model_results/pre_saved/freq_2_res.csv")
  loc_2_res <- read.csv("model_results/pre_saved/loc_cat_soc_2_res.csv")
  touch_2_res <- read.csv("model_results/pre_saved/touch_2_res.csv")
  sh_loc_2_res <- read.csv("model_results/pre_saved/sh_loc_2_res.csv")
  sh_dur_2_res <- read.csv("model_results/pre_saved/sh_dur_2_res.csv")
  sh_freq_2_res <- read.csv("model_results/pre_saved/sh_freq_2_res.csv")
  # sh_touch_2_res <- read.csv("model_results/pre_saved/both_touch_2_res.csv")
  sh_touch_2_res <- read.csv("model_results/pre_saved/both_touch_2_res_update.csv") # UPDATE
}


models_1 <- bind_rows(age_1_res %>% mutate(Adj = "Unadjusted"),
                      occ_1_res %>% mutate(Adj = "Unadjusted"),
                      HH_num_1_res %>% mutate(Adj = "Unadjusted"),
                      gender_1_res %>% mutate(Adj = "Unadjusted"),
                      density_1_res %>% mutate(Adj = "Unadjusted"),
                      full_1_res %>% mutate(Adj = "Fully adjusted"))

models_2_prob <- bind_rows(age_nh_2_res,
                           occ_nh_2_res,
                           HH_num_nh_2_res,
                           gender_nh_2_res,
                           density_nh_2_res)

models_2 <- bind_rows(all_2_res %>% mutate(Adj = "Shared contact model"),
                      ce_2_res %>% mutate(Adj = "Individual contact model"),
                      part_2_res %>% mutate(Adj = "Participant-only model"),
                      age_2_res %>% mutate(Adj = "Unadjusted"),
                      gender_2_res %>% mutate(Adj = "Unadjusted"),
                      HH_num_2_res %>% mutate(Adj = "Unadjusted"),
                      occ_2_res %>% mutate(Adj = "Unadjusted"),
                      density_2_res %>% mutate(Adj = "Unadjusted"),
                      loc_2_res %>% mutate(Adj = "Unadjusted"),
                      dur_2_res %>% mutate(Adj = "Unadjusted"),
                      freq_2_res %>% mutate(Adj = "Unadjusted"),
                      touch_2_res %>% mutate(Adj = "Unadjusted"),
                      sh_loc_2_res %>% mutate(Adj = "Unadjusted"),
                      sh_dur_2_res %>% mutate(Adj = "Unadjusted"),
                      sh_freq_2_res %>% mutate(Adj = "Unadjusted"),
                      sh_touch_2_res %>% mutate(Adj = "Unadjusted")) %>%
  mutate(Adj = factor(Adj, levels = c("Unadjusted", "Participant-only model", "Individual contact model", "Shared contact model")))

############ Main text figures ############

# Figure 1
make_fig1(dat = tot_contacts,
          models = models_1 %>% filter(Contact_type == "All contacts"),
          type = "all")

# Figure 2
make_fig2(probs = models_2_prob %>% filter(Contact_type == "All contacts"),
          models = models_2 %>%
            filter(Contact_type == "All contacts",
                   Adj != "Individual contact model"),
          type = "all")

# Figure 3
make_fig3(models = models_2 %>%
              filter(Contact_type == "All contacts",
                     Adj != "Participant-only model"),
          type = "all",
          upper_limit = 10)

# Figure 4
make_fig4(ind_contacts)

############ Main text tables ############

# Table 1
make_table1(tot_contacts)

# Table 2
make_table2(ind_contacts)

############ Supplemental figures ############

# Figure S1
make_fig1(dat = tot_contacts,
          models = models_1 %>% filter(Contact_type == "Extra-household contacts"),
          type = "exhome")

# Figure S2
make_fig2(probs = models_2_prob %>% filter(Contact_type == "Contacts occuring\noutside the home"),
          models = models_2 %>%
            filter(Contact_type == "Extra-household contacts",
                   Adj != "Individual contact model"),
          type = "exhome")

# Figure S3
make_fig3(models = models_2 %>%
            filter(Adj != "Participant-only model") %>%
            bind_rows(sh_loc_2_res %>% mutate(Adj = "Unadjusted"),
                      sh_dur_2_res %>% mutate(Adj = "Unadjusted"),
                      sh_freq_2_res %>% mutate(Adj = "Unadjusted"),
                      sh_touch_2_res %>% mutate(Adj = "Unadjusted")) %>%
            filter(Contact_type == "Extra-household contacts"),
          type = "exhome",
          upper_limit = 10)

# Figure S4
make_figS4(ind_contacts)


############## Supplemental tables #######################

# Table S1
models_1 %>% 
  process_model_dat() %>%
  dplyr::select(var_type, var_name, Estimate, CI_low, CI_high, Contact_type, Adj) %>%
  arrange(Contact_type, Adj)

# Tables S3 and S4
models_2 %>%
  process_model_dat(vars = c("occ_cat_student", "occ_cat_other", "age_cat_comb0_15",
                             "age_cat_comb16_25", "age_cat_comb26_35", "age_cat_comb46_55",
                             "age_cat_comb56_65", "age_cat_comb66_75", "age_cat_comb76_",
                             "HH_Num_Memb_1_3", "HH_Num_Memb_7_9", "HH_Num_Memb_10",
                             "PART_MALE", "log_ninetile_hh", "loc_cat_school",
                             "loc_cat_work", "loc_cat_home", "loc_cat_other",
                             "freq_cat_1", "freq_cat_1_3",
                             "dur_cat_10", "dur_cat_10_59",
                             "CONTACT_Touch",
                             "shared_location", "shared_dur", "shared_freq", "both_touch",
                             "group_ind", "group_int")) %>%
  dplyr::select(var_type, var_name, Estimate, CI_low, CI_high, Contact_type, Adj) %>%
  mutate(Adj = factor(Adj, levels = c("Unadjusted", "Participant-only model", "Individual contact model", "Shared contact model"))) %>%
  arrange(Contact_type, Adj)

# Table S5
# Note, this will only run if run_models = TRUE as it requires having all the chains
models <- list(readRDS("model_results/new_runs/part_2.rds"),
               readRDS("model_results/new_runs/ce_grpind_2.rds"),
               readRDS("model_results/new_runs/all_grpind_2.rds"),
               readRDS("model_results/new_runs/part_2_ex.rds"),
               readRDS("model_results/new_runs/ce_grpind_2_ex.rds"),
               readRDS("model_results/new_runs/all_grpind_2_ex.rds"))

model_dat <- list(readRDS("model_results/new_runs/part_2_dat.rds"),
                  readRDS("model_results/new_runs/ce_grpind_2_dat.rds"),
                  readRDS("model_results/new_runs/all_grpind_2_dat.rds"),
                  readRDS("model_results/new_runs/part_2_dat_ex.rds"),
                  readRDS("model_results/new_runs/ce_grpind_2_dat_ex.rds"),
                  readRDS("model_results/new_runs/all_grpind_2_dat_ex.rds"))


get_waic_table(models, model_dat)

# Table S6 and S7
temp <- list.files(path = "model_results/pre_saved/sensitivity/", pattern="\\.csv$", full.names = TRUE)
myfiles <- lapply(temp, read.csv)
for(i in 1:length(myfiles)) {
  if(i %in% grep("all", temp)) {
    myfiles[[i]]$Adj <- "Shared contact model"
  } else {
    myfiles[[i]]$Adj <- "Unadjusted"
  }
  
  if(i %in% grep("_no_", temp)) {
    myfiles[[i]]$affirmative_response <- "Yes; Probably yes; Probably no"
  } else {
    myfiles[[i]]$affirmative_response <- "Yes"
  }
}

bind_rows(myfiles) %>%
  process_model_dat(vars = c("occ_cat_student", "occ_cat_other", "age_cat_comb0_15",
                             "age_cat_comb16_25", "age_cat_comb26_35", "age_cat_comb46_55",
                             "age_cat_comb56_65", "age_cat_comb66_75", "age_cat_comb76_",
                             "HH_Num_Memb_1_3", "HH_Num_Memb_7_9", "HH_Num_Memb_10",
                             "PART_MALE", "log_ninetile_hh", "loc_cat_school",
                             "loc_cat_work", "loc_cat_home", "loc_cat_other",
                             "freq_cat_1", "freq_cat_1_3",
                             "dur_cat_10", "dur_cat_10_59",
                             "CONTACT_Touch",
                             "shared_location", "shared_dur", "shared_freq", "both_touch",
                             "group_ind", "group_int")) %>%
  dplyr::select(var_type, var_name, Estimate, CI_low, CI_high, Contact_type, Adj, affirmative_response) %>%
  arrange(Contact_type, affirmative_response, Adj)
