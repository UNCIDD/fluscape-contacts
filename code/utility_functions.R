require(tidyverse)
require(grid)
require(cowplot)
require(ggpubr)
require(scales)
require(loo)
require(rstan)
require(scico)

recode_vars_for_plots <- function(x) {
  mutate(x, var_type = recode(var_type,
                              "age_cat" = "Age",
                              "PART_GENDER" = "Gender",
                              "dens_quantile" = "Population\ndensity",
                              "occ_cat" = "Occupation\nstatus",
                              "HH_Num_Member_Cat" = "Number of\nhousehold members"),
         var_type = factor(var_type, levels = c("Age", "Gender", "Number of\nhousehold members", "Occupation\nstatus", "Population\ndensity")))
}




process_raw_dat <- function(dat,
                            var_levels = c("0-15", "16-25", "26-35", "36-45", "46-55", "56-65", "66-75", "76+",
                                           "Male", "Female", "1st quartile", "Middle 50%", "4th quartile",
                                           "Employed", "Student", "Other", "1-3", "4-6", "7-9", "10+")) {
  out <- dat %>%
    pivot_longer(cols = c(age_cat, PART_GENDER, dens_quantile, occ_cat, HH_Num_Member_Cat),
                 names_to = "var_type", values_to = "var_name") %>%
    drop_na() %>%
    group_by(var_name) %>%
    mutate(avg = mean(num_contacts, na.rm = TRUE),
           var_name = str_replace(var_name, "_", "-"),
           var_name = factor(var_name, levels = var_levels)) %>%
    ungroup() %>%
    recode_vars_for_plots()
  
  return(out)
}

process_model_dat <- function(models,
                              vars = c("occ_cat_student", "occ_cat_other", "age_cat_comb0_15",
                                       "age_cat_comb16_25", "age_cat_comb26_35", "age_cat_comb46_55",
                                       "age_cat_comb56_65", "age_cat_comb66_75", "age_cat_comb76_",
                                       "HH_Num_Memb_1_3", "HH_Num_Memb_7_9", "HH_Num_Memb_10",
                                       "PART_MALE", "log_ninetile_hh"),
                              var_levels = c("0-15", "16-25", "26-35", "36-45", "46-55", "56-65", "66-75", "76+",
                                             "Male", "Female", "Ten-fold increase\nin pop. density",
                                             "Employed", "Student", "Other", "1-3", "4-6", "7-9", "10+")) {
  
  out <- models %>%
    dplyr::filter(name_long %in% vars) %>%
    mutate(var_type = ifelse(str_detect(name_long, "age_cat"), "Age",
                             ifelse(str_detect(name_long, "MALE"), "Gender",
                                    ifelse(str_detect(name_long, "ninetile"), "Population\ndensity",
                                           ifelse(str_detect(name_long, "HH_Num"), "Number of\nhousehold members",
                                                  ifelse(str_detect(name_long, "occ_"), "Occupation\nstatus",
                                                         ifelse(str_detect(name_long, "loc_cat"), "Contact setting",
                                                                ifelse(str_detect(name_long, "dur_cat"), "Contact duration",
                                                                       ifelse(str_detect(name_long, "freq_cat"), "Contact frequency",
                                                                              ifelse(str_detect(name_long, "CONTACT_Touch"), "Contact\ninvolves\ntouch", "Shared contact\ncharacteristics"))))))))),
           var_name = recode(name_long,
                             "occ_cat_student" = "Student",
                             "occ_cat_other" = "Other",
                             "age_cat_comb0_15" = "0-15",
                             "age_cat_comb16_25" = "16-25",
                             "age_cat_comb26_35" = "26-35",
                             "age_cat_comb46_55" = "46-55",
                             "age_cat_comb56_65" = "56-65",
                             "age_cat_comb66_75" = "66-75",
                             "age_cat_comb76_" = "76+",
                             "HH_Num_Memb_1_3" = "1-3",
                             "HH_Num_Memb_7_9" = "7-9",
                             "HH_Num_Memb_10" = "10+",
                             "PART_MALE" = "Male",
                             "log_ninetile_hh" = "Ten-fold increase\nin pop. density",
                             "loc_cat_school" = "School",
                             "loc_cat_work" = "Work",
                             "loc_cat_home" = "Home",
                             "loc_cat_other" = "Other",
                             "freq_cat_1" = "<1 per week",
                             "freq_cat_1_3" = "1-3 per week",
                             "dur_cat_10" = "<10 minutes",
                             "dur_cat_10_59" = "10-59 minutes",
                             "CONTACT_Touch" = "Contact involves\ntouch",
                             "shared_location" = "Shared location",
                             "shared_dur" = "Shared duration",
                             "shared_freq" = "Shared frequency",
                             "both_touch" = "Both contacts\ninvolve touch")) %>%
    dplyr::select(-X, -name, -name_long, -mean, -se_mean, -sd, -Rhat)
  
  return(out)
  
}

add_ref <- function(var_type, ref_value, model_names, contact_type = "All contacts") {
  
  data.frame(var_type = var_type,
             var_name = rep(ref_value, length(model_names)),
             Estimate = rep(1,length(model_names)),
             Adj = model_names,
             CI_low = rep(NA, length(model_names)),
             CI_high = rep(NA, length(model_names)),
             Contact_type = rep(contact_type, length(model_names)))
}

plot_reg_results <- function(model_dat,
                             vars = c("occ_cat_student", "occ_cat_other", "age_cat_comb0_15",
                                      "age_cat_comb16_25", "age_cat_comb26_35", "age_cat_comb46_55",
                                      "age_cat_comb56_65", "age_cat_comb66_75", "age_cat_comb76_",
                                      "HH_Num_Memb_1_3", "HH_Num_Memb_7_9", "HH_Num_Memb_10",
                                      "PART_MALE", "log_ninetile_hh"),
                             var_levels = c("0-15", "16-25", "26-35", "36-45", "46-55", "56-65", "66-75", "76+",
                                            "Male", "Female", "Ten-fold increase\nin pop. density",
                                            "Employed", "Student", "Other", "1-3", "4-6", "7-9", "10+"),
                             recolor = TRUE,
                             upper_limit = NULL) {
  
  
  model_names <- unique(model_dat$Adj) # get unique model names 
  
  part_res <- process_model_dat(model_dat, vars = vars, var_levels = var_levels) 
  
  var_types <- unique(part_res$var_type)
  
  part_res <- part_res %>%
    bind_rows(add_ref(var_type = "Age", ref_value = "36-45", model_names = model_names)) %>%
    bind_rows(add_ref(var_type = "Gender", ref_value = "Female", model_names = model_names)) %>%
    bind_rows(add_ref(var_type = "Number of\nhousehold members", ref_value = "4-6", model_names = model_names)) %>%
    bind_rows(add_ref(var_type = "Occupation\nstatus", ref_value = "Employed", model_names = model_names)) %>%
    bind_rows(add_ref(var_type = "Contact setting", ref_value = "Social", model_names = model_names)) %>%
    bind_rows(add_ref(var_type = "Contact duration", ref_value = "60+ minutes", model_names = model_names)) %>%
    bind_rows(add_ref(var_type = "Contact frequency", ref_value = "4+ per week", model_names = model_names)) %>%
    mutate(var_type = factor(var_type, levels = c("Age", "Gender", "Number of\nhousehold members", "Occupation\nstatus", "Population\ndensity",
                                                  "Contact setting", "Contact frequency", "Contact duration", "Contact\ninvolves\ntouch", "Shared contact\ncharacteristics")),
           var_name = factor(var_name, levels = var_levels)) %>%
    mutate(Adj = as.factor(Adj),
           upper_lim = 0) %>%
    filter(var_type %in% var_types)
  
  if(!is.null(upper_limit)) {
    part_res <- part_res %>%
      mutate(CI_low = ifelse(Estimate > upper_limit, NA, CI_low),
             CI_high = ifelse(Estimate > upper_limit, NA, CI_high),
             upper_lim = ifelse(Estimate > upper_limit, 1, 0),
             Estimate = ifelse(Estimate > upper_limit, upper_limit, Estimate))
  }
  
  g <- ggplot(part_res) +
    geom_point(aes(x = var_name,
                   y = Estimate,
                   color = Adj,
                   shape = factor(upper_lim)),
               position=position_dodge(width=0.5)) +
    geom_errorbar(aes(x = var_name,
                      ymin = CI_low, ymax = CI_high,
                      color = Adj),
                  position=position_dodge(width=0.5), width = 0.3) +
    geom_hline(yintercept = 1, linetype = "dashed") +
    facet_grid(cols = vars(var_type), scale = "free", space = "free_x") +
    theme_bw() +
    scale_y_sqrt() +
    guides(color = guide_legend(title="Analysis type"), shape = "none") +
    labs(y = "Relative number of contacts",
         x = "") +
    theme(axis.text.x = element_text(angle = 60, vjust = 0.5, hjust=0.5),
          legend.position = "bottom")
  
  if("Fully adjusted" %in% model_dat$Adj) {
    g <- g + scale_color_manual(values = c("Unadjusted" ="#E41A1C", "Fully adjusted" = "#377EB8"),
                                breaks = c("Unadjusted", "Fully adjusted"))
  }
  if("Shared contact model" %in% model_dat$Adj) {
    g <- g + scale_color_manual(values = c("Unadjusted" ="#E41A1C",
                                           "Participant-only model" = "#377EB8",
                                           "Individual contact model" = "#C77CFF",
                                           "Shared contact model" = "#00BA38"),
                                breaks = c("Unadjusted",
                                           "Participant-only model",
                                           "Individual contact model",
                                           "Shared contact model"))
  }
  
  leg <- ggpubr::get_legend(g)
  g <- g + theme(legend.position = "none")
  
  if(recolor) {
    reg_results <- recolor_headers(g)
  } else {
    reg_results <- g
  }
  
  return(list(reg_results, leg))
  
}

#' @param dat raw data on total number of contacts per participant
#' @param models output from regression models
#' @param type type of contact, either "all" or "exhome"
make_fig1 <- function(dat, models, type = c("all", "exhome")) {
  
  if(type == "all") {
    contact_type <- "All contacts"
  } else if(type == "exhome") {
    dat <- dat %>%
      dplyr::select(-num_contacts) %>%
      rename(num_contacts = num_contacts_exhome)
    contact_type <- "Contacts occuring\noutside the home"
  } else{
    stop("Please select a valid 'type' argument.")
  }
  
  n_contacts_long <- process_raw_dat(dat)
  
  plt1 <- ggplot(n_contacts_long) +
    geom_boxplot(aes(x = var_name, y = num_contacts, fill = var_type),
                 outlier.shape = NA) +              
    geom_point(aes(x = var_name, y = avg),
               color = "darkorange2", size = 2) +
    geom_point(aes(x = var_name, y = avg),
               color = "white", size = 2, shape = 1) +
    theme_bw() +
    facet_grid(cols = vars(var_type), scale = "free_x", space = "free_x") +
    guides(fill = FALSE, scale = "none") +
    scale_fill_brewer(palette = "Dark2") +
    scale_y_continuous(trans = "sqrt", limits = c(0,80)) +
    labs(x = "",
         y = "Number of contacts") +
    theme(plot.title = element_text(hjust = 0.5),
          axis.text.x = element_text(angle = 45, vjust = 0.8, hjust=1))
  
  
  boxplots <- recolor_headers(plt1)
  reg_results_full <- plot_reg_results(model_dat = models)
  reg_results <- reg_results_full[[1]]
  leg <- reg_results_full[[2]]
  
  
  plot_grid(boxplots, reg_results, leg, nrow = 3, rel_heights = c(1,1,.1))
  
}

#' @param probs results of non-hierarchical regression giving probability of triangle connection by participant characteristic
#' @param models output from regression models
#' @param type type of contact, either "all" or "exhome"
make_fig2 <- function(probs, models, type = c("all", "exhome")) {
  
  plt_nh <- ggplot(probs %>%
                     mutate(var_name = factor(var_name, levels = c("0-15", "16-25", "26-35", "36-45", "46-55", "56-65", "66-75", "76+",
                                                                   "Male", "Female", "1st quartile", "Middle 50%", "4th quartile",
                                                                   "Employed", "Student", "Other", "1-3", "4-6", "7-9", "10+")))) +
    geom_point(aes(x = var_name, y = Estimate, color = var_type)) +
    geom_errorbar(aes(x = var_name, ymin = CI_low, ymax = CI_high, color = var_type),
                  width = 0.3) +
    facet_grid(cols = vars(var_type), scale = "free_x", space = "free_x") +
    theme_bw() +
    guides(color = FALSE) +
    scale_color_brewer(palette = "Dark2") +
    scale_y_continuous(labels = scales::percent) +
    labs(x = "",
         y = "Probability of a connected triange") +
    theme(plot.title = element_text(hjust = 0.5),
          axis.text.x = element_text(angle = 45, vjust = 0.8, hjust=1))
  
  prob_plot <- recolor_headers(plt_nh)
  
  reg_results_full <- plot_reg_results(model_dat = models)
  reg_results <- reg_results_full[[1]] 
  leg <- reg_results_full[[2]]
  
  plot_grid(prob_plot, reg_results, leg, nrow = 3, rel_heights = c(1,1,.1))
  
}

make_fig3 <- function(models, type = c("all", "exhome"), upper_limit = NULL) {
  
  fig <- plot_reg_results(models,
                          vars = c("loc_cat_school", "loc_cat_work", "loc_cat_home", "loc_cat_other",
                                   "freq_cat_1", "freq_cat_1_3",
                                   "dur_cat_10", "dur_cat_10_59", "CONTACT_Touch",
                                   "shared_location", "shared_dur", "shared_freq", "both_touch"),
                          var_levels = c("Home", "Work", "School", "Social", "Other",
                                         "<1 per week", "1-3 per week", "4+ per week",
                                         "<10 minutes", "10-59 minutes", "60+ minutes", "Contact involves\ntouch",
                                         "Shared location", "Shared duration", "Shared frequency", "Both contacts\ninvolve touch"),
                          recolor = FALSE,
                          upper_limit = upper_limit)
  
  plot_grid(fig[[1]], fig[[2]], nrow = 2, rel_heights = c(1,.1))
  
}

make_fig4 <- function(dat) {
  
  dat <- dat %>%mutate(age_cat_pretty = gsub("_", "-", age_cat),
                       grp_ind = ifelse(CONTACT_Num == 1, "Individual",
                                        ifelse(CONTACT_Num <= 10, "2-10", "11+")),
                       freq_num = recode(CONTACT_HowOften,
                                         `1` = 5.5,
                                         `2` = 2.5,
                                         `3` = 1,
                                         `4` = 0.5,
                                         `5` = 0.5),
                       dur_num = recode(CONTACT_HowLong,
                                        `1` = 5,
                                        `2` = 19.5,
                                        `3` = 44.5,
                                        `4` = 89.5,
                                        `5` = 179.5,
                                        `6` = 359.5),
                       contact_time = freq_num*dur_num,
                       time_cat = ifelse(contact_time < 60, "<1 hr",
                                         ifelse(contact_time < 60*5, "1-5 hrs", "5+ hrs")),
                       loc_cat_new = ifelse(loc_cat %in% c("Work", "School"), "Wrk/Sch",
                                            ifelse(loc_cat == "Home", "Home", "Other")))
  
  dat_age <- dat %>%
    filter(!is.na(age_cat_pretty)) %>%
    group_by(age_cat_pretty) %>%
    mutate(n_ctct_by_age = sum(CONTACT_Num, na.rm = TRUE)) %>%
    ungroup() 
  
  donut_loc <- dat_age %>%
    filter(!is.na(loc_cat_new)) %>%
    group_by(age_cat_pretty, loc_cat_new) %>%
    summarize(prop = sum(CONTACT_Num, na.rm = TRUE)/n_ctct_by_age[1]) %>%
    mutate(ymax = cumsum(prop),
           ymin = c(0, head(ymax, -1)),
           char_type = "Contact\nlocation") %>%
    rename(char_level = loc_cat_new)
  
  donut_grp <- dat_age %>%
    filter(!is.na(grp_ind)) %>%
    group_by(age_cat_pretty, grp_ind) %>%
    summarize(prop = sum(CONTACT_Num, na.rm = TRUE)/n_ctct_by_age[1]) %>%
    mutate(ymax = cumsum(prop),
           ymin = c(0, head(ymax, -1)),
           char_type = "Group\ntype") %>%
    rename(char_level = grp_ind)
  
  donut_time <- dat_age %>%
    filter(!is.na(time_cat)) %>%
    group_by(age_cat_pretty, time_cat) %>%
    summarize(prop = sum(CONTACT_Num, na.rm = TRUE)/n_ctct_by_age[1]) %>%
    mutate(ymax = cumsum(prop),
           ymin = c(0, head(ymax, -1)),
           char_type = "Time spent\nwith conctact\nper week") %>%
    rename(char_level = time_cat)
  
  donut_dat <- bind_rows(donut_loc, donut_grp, donut_time) %>%
    mutate(char_level = factor(char_level, levels = c("Home", "Wrk/Sch", "Other", "Individual", "2-10", "11+", "<1 hr", "1-5 hrs", "5+ hrs"))) %>%
    filter(!is.na(age_cat_pretty))
  
  colors <- c("#C7E9C0","#41AB5D", "#006D2C",
              "#9ECAE1", "#4292C6","#08519C",
              "#BCBDDC", "#807DBA", "#54278F")
  
  age_donut <- ggplot(donut_dat,
                      aes(ymax = ymax, ymin = ymin, xmax = 4, xmin = 2, fill = char_level)) +
    geom_rect() +
    coord_polar(theta = "y") +
    scale_fill_manual(values = colors) +
    xlim(c(0, 4))+
    facet_grid(cols = vars(age_cat_pretty), rows = vars(char_type), switch = "y") +
    theme_void() +
    theme(legend.position = "none") +
    labs(title = "Contact characteristics by age group") +
    theme(plot.title = element_text(hjust = 0.5))
  
  dat_dens <- dat %>%
    filter(!is.na(density_quintile)) %>%
    group_by(density_quintile) %>%
    mutate(n_ctct_by_dens = sum(CONTACT_Num, na.rm = TRUE)) %>%
    ungroup()
  
  donut_loc <- dat_dens %>%
    filter(!is.na(loc_cat_new)) %>%
    group_by(density_quintile, loc_cat_new) %>%
    summarize(prop = sum(CONTACT_Num, na.rm = TRUE)/n_ctct_by_dens[1]) %>%
    mutate(ymax = cumsum(prop),
           ymin = c(0, head(ymax, -1)),
           char_type = "Contact\nlocation") %>%
    rename(char_level = loc_cat_new)
  
  donut_grp <- dat_dens %>%
    filter(!is.na(grp_ind)) %>%
    group_by(density_quintile, grp_ind) %>%
    summarize(prop = sum(CONTACT_Num, na.rm = TRUE)/n_ctct_by_dens[1]) %>%
    mutate(ymax = cumsum(prop),
           ymin = c(0, head(ymax, -1)),
           char_type = "Group\ntype") %>%
    rename(char_level = grp_ind)
  
  donut_time <- dat_dens %>%
    filter(!is.na(time_cat)) %>%
    group_by(density_quintile, time_cat) %>%
    summarize(prop = sum(CONTACT_Num, na.rm = TRUE)/n_ctct_by_dens[1]) %>%
    mutate(ymax = cumsum(prop),
           ymin = c(0, head(ymax, -1)),
           char_type = "Time spent\nwith conctact\nper week") %>%
    rename(char_level = time_cat)
  
  donut_dat <- bind_rows(donut_loc, donut_grp, donut_time)%>%
    mutate(char_level = factor(char_level, levels = c("Home", "Wrk/Sch", "Other", "Individual", "2-10", "11+", "<1 hr", "1-5 hrs", "5+ hrs"))) %>%
    filter(!is.na(density_quintile))
  
  dens_donut <- ggplot(donut_dat,
                       aes(ymax = ymax, ymin = ymin, xmax = 4, xmin = 2, fill = char_level)) +
    geom_rect() +
    coord_polar(theta = "y") +
    scale_fill_manual(values = colors, name = "") +
    xlim(c(0, 4))+
    facet_grid(cols = vars(density_quintile), rows = vars(char_type), switch = "y") +
    theme_void() +
    labs(title = "Contact characteristics by density quintile") +
    guides(fill=guide_legend(nrow=3, byrow=FALSE),) +
    theme(plot.title = element_text(hjust = 0.5))
  
  leg <- get_legend(dens_donut)
  dens_donut <- dens_donut +   theme(legend.position = "none")
  
  dens_grid <- plot_grid(dens_donut, leg, nrow = 1)
  
  plot_grid(age_donut, dens_grid, nrow = 2)
  
}

make_figS4 <- function(dat) {
  
  plt <- dat %>%
    mutate(freq_cat = factor(freq_cat, levels = c("<1/wk", "1-3/wk", "4+/wk")),
           dur_cat = factor(dur_cat, levels = c("<10 min", "10-59 min", "60+ min")),
           grp_cat_plt = base::cut(CONTACT_Num,
                                   breaks = c(0,1, 5, 10, 15, 20, 30, 40, 50, 10000),
                                   labels = c("1", "2-5", "6-10", "11-15", "16-20", "21-30", "31-40", "41-50", "51+")),
           age_cat = str_replace(age_cat, "_", "-"),
           freq_dur = factor(as.numeric(freq_cat)+as.numeric(dur_cat)))
  
  ggplot(plt %>% group_by(freq_dur, grp_cat_plt, age_cat) %>%
           summarize(n = sum(CONTACT_Num, na.rm = TRUE)) %>%
           group_by(age_cat) %>%
           mutate(n = n/sum(n)) %>%
           ungroup() %>%
           drop_na) +
    geom_tile(aes(x = freq_dur, y = grp_cat_plt, fill = n)) +
    #coord_fixed(ratio = 0.5) +
    facet_wrap(~age_cat) +
    scale_fill_scico(palette = "devon", name = "Proportion of\ncontacts", trans = "reverse", guide = guide_colorbar(reverse = TRUE)) +
    theme_classic() +
    labs(y = "Group size",
         x = "Contact intimacy (frequency + duration)") +
    theme(legend.position = "bottom")
  
}

make_table1  <- function(data) {
  
  out <- data.frame(var_type = c(rep("Age", 9),
                                 rep("Sex", 3),
                                 rep("Population density", 5),
                                 rep("Occupation status", 4)),
                    var_level = c("0_15", "16_25", "26_35", "36_45", "46_55", "56_65", "66_75", "76+", "Unknown",
                                  "Male", "Female", "Unknown",
                                  "0-100", "101-500", "501-5000", ">5000", "Unknown",
                                  "Employed", "Student", "Unemployed or retired", "Unknown"))
  
  data <- data %>%
    mutate(age_cat = factor(age_cat, levels = c("0_15", "16_25", "26_35", "36_45", "46_55", "56_65", "66_75", "76+")),
           PART_GENDER = factor(PART_GENDER, levels = c("Male", "Female")),
           occ_cat = factor(occ_cat, levels = c("Employed", "Student", "Other")),
           dens_cat = factor(dens_cat, levels = c("0-100", "101-500", "501-5000", ">5000")))
  
  part_char <- data %>%
    group_by(Full.ID) %>%
    summarize(age_cat = age_cat[1], PART_GENDER = PART_GENDER[1], occ_cat = occ_cat[1],
              dens_quantile = dens_quantile[1], dens_cat = dens_cat[1]) %>%
    ungroup()
    
  out$unique_participants <- c(table(part_char$age_cat, useNA = "always"),
                               table(part_char$PART_GENDER, useNA = "always"),
                               table(part_char$dens_cat, useNA = "always"),
                               table(part_char$occ_cat, useNA = "always"))
  
  out$participant_visits <- c(table(data$age_cat, useNA = "always"),
                              table(data$PART_GENDER, useNA = "always"),
                              table(data$dens_cat, useNA = "always"),
                              table(data$occ_cat, useNA = "always"))
  
  temp <- data %>% filter(num_contacts <= 5)
  out$part_visits_1_5 <- c(table(temp$age_cat, useNA = "always"),
                           table(temp$PART_GENDER, useNA = "always"),
                           table(temp$dens_cat, useNA = "always"),
                           table(temp$occ_cat, useNA = "always"))
  
  temp <- data %>% filter(num_contacts <= 10, num_contacts > 5)
  out$part_visits_6_10 <- c(table(temp$age_cat, useNA = "always"),
                           table(temp$PART_GENDER, useNA = "always"),
                           table(temp$dens_cat, useNA = "always"),
                           table(temp$occ_cat, useNA = "always"))
  
  temp <- data %>% filter(num_contacts <= 15, num_contacts > 10)
  out$part_visits_11_15 <- c(table(temp$age_cat, useNA = "always"),
                             table(temp$PART_GENDER, useNA = "always"),
                             table(temp$dens_cat, useNA = "always"),
                             table(temp$occ_cat, useNA = "always"))
  
  temp <- data %>% filter(num_contacts >= 16)
  out$part_visits_16plus <- c(table(temp$age_cat, useNA = "always"),
                              table(temp$PART_GENDER, useNA = "always"),
                              table(temp$dens_cat, useNA = "always"),
                              table(temp$occ_cat, useNA = "always"))
  
  return(out)
  
}

contact_sums <- function(x) {
  out <- bind_rows(x %>% group_by(group_size_cat) %>%
                     summarize(sum = sum(CONTACT_Num, na.rm = T)) %>%
                     rename(var_level = group_size_cat) %>%
                     mutate(var_type = "Group size"),
                   x %>% group_by(contact_age_cat) %>%
                     summarize(sum = sum(CONTACT_Num, na.rm = T)) %>%
                     rename(var_level = contact_age_cat) %>%
                     mutate(var_level = str_replace(var_level, "_", "-"),
                            var_type = "Contact age"),
                   x %>% group_by(loc_cat) %>%
                     summarize(sum = sum(CONTACT_Num, na.rm = T)) %>%
                     rename(var_level = loc_cat) %>%
                     mutate(var_type = "Contact setting"),
                   x %>% group_by(CONTACT_Touch) %>%
                     summarize(sum = sum(CONTACT_Num, na.rm = T)) %>%
                     rename(var_level = CONTACT_Touch) %>%
                     mutate(var_type = "Contact involved touch?"),
                   x %>% group_by(freq_cat) %>%
                     summarize(sum = sum(CONTACT_Num, na.rm = T)) %>%
                     rename(var_level = freq_cat) %>%
                     mutate(var_level = dplyr::recode(var_level,
                                                      `<1/wk` = "<1 time per week",
                                                      `1-3/wk` = "1-3 times per week",
                                                      `4+/wk` = "4+ times per week"),
                            var_type = "Contact frequency"),
                   x %>% group_by(dur_cat) %>%
                     summarize(sum = sum(CONTACT_Num, na.rm = T)) %>%
                     rename(var_level = dur_cat) %>%
                     mutate(var_level = str_replace(var_level, "min", "minutes"),
                            var_type = "Contact duration")) %>%
    replace_na(list(var_level = "Unknown"))
  
  return(out)
}

make_table2 <- function(data) {
  
  out <- data.frame(var_type = c(rep("Group size", 5),
                                 rep("Contact age", 7),
                                 rep("Contact setting", 6),
                                 rep("Contact involved touch?", 3),
                                 rep("Contact frequency", 4),
                                 rep("Contact duration", 4)),
                    var_level = c("1", "2-5", "6-10", "11-20", "21+",
                                  "0-4", "5-19", "20-39", "40-64", "65+", "Multiple", "Unknown",
                                  "Home", "Work", "School", "Social", "Other", "Unknown",
                                  "No", "Yes", "Unknown",
                                  "<1 time per week", "1-3 times per week", "4+ times per week", "Unknown",
                                  "<10 minutes", "10-59 minutes", "60+ minutes", "Unknown"))
  
  out <- out %>%
    left_join(contact_sums(data) %>%
                rename(Total = sum)) %>%
    left_join(contact_sums(data %>% filter(age_cat_2 == "0_4")) %>%
                rename(age_0_4 = sum)) %>%
    left_join(contact_sums(data %>% filter(age_cat_2 == "5_19")) %>%
                rename(age_5_19 = sum)) %>%
    left_join(contact_sums(data %>% filter(age_cat_2 == "20_39")) %>%
                rename(age_20_39 = sum)) %>%
    left_join(contact_sums(data %>% filter(age_cat_2 == "40_64")) %>%
                rename(age_40_64 = sum)) %>%
    left_join(contact_sums(data %>% filter(age_cat_2 == "65+")) %>%
                rename(age_65plus = sum)) %>%
    left_join(contact_sums(data %>% filter(is.na(age_cat_2))) %>%
                rename(age_unknown = sum)) %>%
    replace(is.na(.), 0)
  
  return(out)
  
}

############# Standardized plotting tools ##################

# Recolor panel header strips with theme colors
#' @param plt ggplot object with column headers to be filled with theme colors
recolor_headers <- function(plt) {
  # Change colors of panel labels
  gt <- ggplot_gtable(ggplot_build(plt))
  panel5 <- gt$layout$l[grep('panel-1-5', gt$layout$name)]
  stript <- which(grepl('strip-t', gt$layout$name))
  fills <- c("#1B9E77", "#D95F02", "#7570B3", "#E7298A", "#66A61E")
  k <- 1
  for (i in stript) {
    j <- which(grepl('rect', gt$grobs[[i]]$grobs[[1]]$childrenOrder))
    gt$grobs[[i]]$grobs[[1]]$children[[j]]$gp$fill <- fills[k]
    k <- k+1
  }
  
  return(as_ggplot(gt))
}


###################### Stan model utilities #######################

##' Get data into proper format for stan
##' @param data raw input data
##' @analysis_type "first_order" or "second_order"
##' @tri_outcome 1 = cutoff at no, 2 = cutoff at probably, 3 = cutoff at yes
get_stan_data <- function(data, analysis_type, full = TRUE, contact_type = "all",
                          part_only = FALSE, var = c(), var_level = c(), group_ind = FALSE,
                          group_int = FALSE, grp_lik = FALSE, hier = TRUE,
                          tri_outcome = 2) {
  
  if(hier) { # hierarchical
    if(analysis_type == "first_order") {
      
      if(contact_type == "exhome") {
        data$num_contacts <- data$num_contacts_exhome 
      }
      
      d <- data %>%
        dplyr::mutate(full_hh_id = paste(LOC_ID, HH_ID, sep = "."),
                      full_part_id = paste(LOC_ID, HH_ID, PARTICIPANT_ID, sep = "."))
      
      # Location data
      d_loc <- d %>%
        dplyr::group_by(LOC_ID) %>%
        dplyr::summarize(LOC_ID = LOC_ID[1])
      
      loc_key <- data.frame(loc_ID_fluscape = d_loc$LOC_ID,
                            loc_ID_new = 1:nrow(d_loc))
      
      N_loc <- nrow(d_loc)
      
      # Household data
      d_hh <- d %>%
        dplyr::group_by(full_hh_id) %>%
        dplyr::summarize(ninetile_hh = ninetile_hh[1],
                         LOC_ID = LOC_ID[1]) %>%
        dplyr::mutate(log_ninetile_hh = log10(ninetile_hh))
      
      hh_key <- d_hh %>%
        dplyr::select(full_hh_id, LOC_ID) %>%
        dplyr::rename(loc_ID_fluscape = LOC_ID,
                      hh_ID_fluscape = full_hh_id) %>%
        dplyr::mutate(hh_ID_new = 1:nrow(d_hh)) %>%
        dplyr::left_join(loc_key)
      
      d_hh <- d_hh %>%
        dplyr::select(-full_hh_id, -LOC_ID, -ninetile_hh)
      
      N_hh <- nrow(d_hh)
      K_hh <- ncol(d_hh)
      
      # Participant data
      d_part <- d %>%
        dplyr::select(PART_GENDER, full_hh_id, full_part_id) %>%
        dplyr::group_by(full_part_id) %>%
        dplyr::summarize(PART_GENDER = PART_GENDER[1],
                         full_hh_id = full_hh_id[1]) %>%
        dplyr::mutate(PART_MALE = ifelse(PART_GENDER == "Male", 1, 0))
      
      part_key <- d_part %>%
        dplyr::select(full_part_id, full_hh_id) %>%
        dplyr::rename(part_ID_fluscape = full_part_id,
                      hh_ID_fluscape = full_hh_id) %>%
        dplyr::mutate(part_ID_new = 1:nrow(d_part)) %>%
        dplyr::left_join(hh_key)
      
      d_part <- d_part %>%
        dplyr::select(-full_hh_id, -full_part_id, -PART_GENDER)
      
      N_part <- nrow(d_part)
      K_part <- ncol(d_part)
      
      
      # Observation-level data
      d_obs <- d %>%
        dplyr::select(HH_Num_Member_Cat, age_cat, occ_cat, VISIT, full_part_id) %>%
        dplyr::mutate(occ_cat_student = ifelse(occ_cat == "Student", 1, 0),
                      occ_cat_other = ifelse(occ_cat == "Other", 1, 0),
                      age_cat0_15 = ifelse(age_cat == "0_15", 1, 0),
                      age_cat16_25 = ifelse(age_cat == "16_25", 1, 0),
                      age_cat26_35 = ifelse(age_cat == "26_35", 1, 0),
                      age_cat46_55 = ifelse(age_cat == "46_55", 1, 0),
                      age_cat56_65 = ifelse(age_cat == "56_65", 1, 0),
                      age_cat66_75 = ifelse(age_cat == "66_75", 1, 0),
                      age_cat76_ = ifelse(age_cat == "76+", 1, 0),
                      HH_Num_Memb_1_3 = ifelse(HH_Num_Member_Cat == "1-3", 1, 0),
                      HH_Num_Memb_7_9 = ifelse(HH_Num_Member_Cat == "7-9", 1, 0),
                      HH_Num_Memb_10 = ifelse(HH_Num_Member_Cat == "10+", 1, 0))
      
      round_key <- d_obs$VISIT - 1
      
      obs_key <- d_obs %>%
        dplyr::select(full_part_id) %>%
        dplyr::rename(part_ID_fluscape = full_part_id) %>%
        dplyr::left_join(part_key)
      
      d_obs <- d_obs %>%
        dplyr::select(-full_part_id, -occ_cat, -age_cat, -VISIT, -HH_Num_Member_Cat)
      
      N_obs <- nrow(d_obs)
      K_obs <- ncol(d_obs)  
      
      if(!full) {
        if(var_level == "obs") {
          d_obs <- d_obs %>% dplyr::select(all_of(var))
          K_obs <- ncol(d_obs)
          d_part <- NULL
          K_part <- NULL
          d_hh <- NULL
          K_hh <- NULL
        }
        if(var_level == "part") {
          d_obs <- NULL
          K_obs <- NULL
          d_part <- d_part %>% dplyr::select(all_of(var))
          K_part <- ncol(d_part)
          d_hh <- NULL
          K_hh <- NULL
        }
        if(var_level == "hh") {
          d_obs <- NULL
          K_obs <- NULL
          d_part <- NULL
          K_part <- NULL
          d_hh <- d_hh %>% dplyr::select(all_of(var))
          K_hh <- ncol(d_hh)
        }
      }
      
      out <- list(N_obs = N_obs,
                  N_part=N_part,
                  N_hh=N_hh,
                  N_loc=N_loc,
                  N_rd = 4,
                  K_obs = K_obs,
                  K_part = K_part,
                  K_hh = K_hh,
                  y = d$num_contacts,
                  x_obs=d_obs,
                  x_part=d_part,
                  x_hh=d_hh,
                  loc_lookup=hh_key$loc_ID_new,
                  hh_lookup=part_key$hh_ID_new,
                  part_lookup=obs_key$part_ID_new,
                  rd_lookup = round_key)
      
      out <- Filter(Negate(is.null), out)
      
    }
    
    if(analysis_type == "second_order") {
      
      if(contact_type == "exhome") {
        data <- filter(data, both_exhome == "Yes")
      }
      
      d <- data %>%
        dplyr::mutate(full_hh_id = paste(LOC_ID, HH_ID, sep = "."),
                      full_part_id = paste(LOC_ID, HH_ID, PARTICIPANT_ID, sep = "."))
      
      # Location data
      d_loc <- d %>%
        dplyr::group_by(LOC_ID) %>%
        dplyr::summarize(LOC_ID = LOC_ID[1])
      
      loc_key <- data.frame(loc_ID_fluscape = d_loc$LOC_ID,
                            loc_ID_new = 1:nrow(d_loc))
      
      d_loc <- d_loc %>%
        dplyr::select(-LOC_ID)
      
      N_loc <- nrow(d_loc)
      K_loc <- ncol(d_loc)
      
      # Household data
      d_hh <- d %>%
        dplyr::group_by(full_hh_id) %>%
        dplyr::summarize(LOC_ID = LOC_ID[1],
                         log_ninetile_hh = log10(ninetile_hh[1]))
      
      hh_key <- d_hh %>%
        dplyr::select(full_hh_id, LOC_ID) %>%
        dplyr::rename(loc_ID_fluscape = LOC_ID,
                      hh_ID_fluscape = full_hh_id) %>%
        dplyr::mutate(hh_ID_new = 1:nrow(d_hh)) %>%
        dplyr::left_join(loc_key)
      
      d_hh <- d_hh %>%
        dplyr::select(-full_hh_id, -LOC_ID)
      
      N_hh <- nrow(d_hh)
      K_hh <- ncol(d_hh)
      
      # Participant data
      d_part <- d %>%
        dplyr::group_by(full_part_id) %>%
        dplyr::summarize(PART_GENDER = PART_GENDER[1],
                         full_hh_id = full_hh_id[1]) %>%
        dplyr::mutate(PART_MALE = ifelse(PART_GENDER == "Male", 1, 0))
      
      part_key <- d_part %>%
        dplyr::select(full_part_id, full_hh_id) %>%
        dplyr::rename(part_ID_fluscape = full_part_id,
                      hh_ID_fluscape = full_hh_id) %>%
        dplyr::mutate(part_ID_new = 1:nrow(d_part)) %>%
        dplyr::left_join(hh_key)
      
      d_part <- d_part %>%
        dplyr::select(-full_hh_id, -full_part_id, -PART_GENDER)
      
      N_part <- nrow(d_part)
      K_part <- ncol(d_part)
      
      # Triangle data
      d_tri <- d %>%
        dplyr::select(full_part_id, age_cat, occ_cat, CONTACT_Touch, loc_cat, dur_cat, freq_cat,
                      touch_rc, loc_cat_rc, dur_cat_rc, freq_cat_rc,
                      both_touch, shared_location, shared_freq, shared_dur, VISIT, HH_Num_Member_Cat) %>%
        dplyr::mutate(occ_cat_student = ifelse(occ_cat == "Student", 1, 0),
                      occ_cat_other = ifelse(occ_cat == "Other", 1, 0),
                      age_cat0_15 = ifelse(age_cat == "0_15", 1, 0),
                      age_cat16_25 = ifelse(age_cat == "16_25", 1, 0),
                      age_cat26_35 = ifelse(age_cat == "26_35", 1, 0),
                      age_cat46_55 = ifelse(age_cat == "46_55", 1, 0),
                      age_cat56_65 = ifelse(age_cat == "56_65", 1, 0),
                      age_cat66_75 = ifelse(age_cat == "66_75", 1, 0),
                      age_cat76_ = ifelse(age_cat == "76+", 1, 0),
                      loc_cat_school = ifelse(loc_cat == "School" | loc_cat_rc == "School", 1, 0),
                      loc_cat_work = ifelse(loc_cat == "Work" | loc_cat_rc == "Work", 1, 0),
                      loc_cat_social = ifelse(loc_cat == "Social" | loc_cat_rc == "Social", 1, 0),
                      loc_cat_home = ifelse(loc_cat == "Home" | loc_cat_rc == "Home", 1, 0),
                      loc_cat_other = ifelse(loc_cat == "Other" | loc_cat_rc == "Other", 1, 0),
                      dur_cat_10 = ifelse(dur_cat == "<10 min" | dur_cat_rc == "<10 min", 1, 0),
                      dur_cat_10_59 = ifelse(dur_cat == "10-59 min" | dur_cat_rc == "10-59 min", 1, 0),
                      freq_cat_1 = ifelse(freq_cat == "<1/wk" | freq_cat_rc == "<1/wk", 1, 0),
                      freq_cat_1_3 = ifelse(freq_cat == "1-3/wk" | freq_cat_rc == "1-3/wk", 1, 0),
                      HH_Num_Memb_1_3 = ifelse(HH_Num_Member_Cat == "1-3", 1, 0),
                      HH_Num_Memb_7_9 = ifelse(HH_Num_Member_Cat == "7-9", 1, 0),
                      HH_Num_Memb_10 = ifelse(HH_Num_Member_Cat == "10+", 1, 0),
                      both_touch = ifelse(both_touch == "Yes", 1, 0),
                      shared_location = ifelse(shared_location == "Yes", 1, 0),
                      shared_freq = ifelse(shared_freq == "Yes", 1, 0),
                      shared_dur = ifelse(shared_dur == "Yes", 1, 0))
      
      tri_key <- d_tri %>%
        dplyr::select(full_part_id) %>%
        dplyr::rename(part_ID_fluscape = full_part_id) %>%
        dplyr::left_join(part_key)
      
      round_key <- d_tri$VISIT - 1
      
      d_tri <- d_tri %>%
        dplyr::select(-full_part_id, -loc_cat, -dur_cat,
                      -touch_rc, -loc_cat_rc, -dur_cat_rc, -freq_cat_rc,
                      -freq_cat, -VISIT, -age_cat, -occ_cat,
                      -HH_Num_Member_Cat)
      
      if(part_only) {
        d_tri <- d_tri %>%
          dplyr::select(occ_cat_student, occ_cat_other, age_cat0_15, age_cat16_25,
                        age_cat26_35, age_cat46_55, age_cat56_65,
                        age_cat66_75, age_cat76_,
                        HH_Num_Memb_1_3, HH_Num_Memb_7_9, HH_Num_Memb_10)
      }
      
      N_tri <- nrow(d_tri)
      K_tri <- ncol(d_tri) 
      
      if(full & length(var) > 0) {
        d_tri <- d_tri %>% dplyr::select(all_of(var))
      }
      
      if(!full) {
        if(var_level == "tri") {
          d_tri <- d_tri %>% dplyr::select(all_of(var))
          K_tri <- ncol(d_tri)
          d_part <- NULL
          K_part <- NULL
          d_hh <- NULL
          K_hh <- NULL
        }
        if(var_level == "part") {
          d_tri <- NULL
          K_tri <- NULL
          d_part <- d_part %>% dplyr::select(all_of(var))
          K_part <- ncol(d_part)
          d_hh <- NULL
          K_hh <- NULL
        }
        if(var_level == "hh") {
          d_tri <- NULL
          K_tri <- NULL
          d_part <- NULL
          K_part <- NULL
          d_hh <- d_hh %>% dplyr::select(all_of(var))
          K_hh <- ncol(d_hh)
        }
      } 
      
      # Set apropriate outcomoe variable
      if(tri_outcome == 1) {
        y <- d$met_rc_no
      }
      if(tri_outcome == 2) {
        y <- d$met_rc_yn
      }
      if(tri_outcome == 3) {
        y <- d$met_rc_yes
      }
      
      out <- list(N_tri=N_tri,
                  N_hh=N_hh,
                  N_part=N_part,
                  N_loc=N_loc,
                  N_rd=4,
                  K_tri = K_tri,
                  K_hh = K_hh,
                  K_part = K_part,
                  K_loc = K_loc,
                  y = y,
                  x_tri=d_tri,
                  x_part=d_part,
                  x_hh=d_hh,
                  loc_lookup=hh_key$loc_ID_new,
                  hh_lookup=part_key$hh_ID_new,
                  part_lookup=tri_key$part_ID_new,
                  rd_lookup = round_key)
      
      if(grp_lik) {
        out <- append(out, list(g1 = d$CONTACT_Num,
                                g2 = d$CONTACT_Num_rc))
        
        if(group_ind) {
          # Create indicator for whether a group is in the potential traingle
          d_tri$group_ind <- as.numeric(!(out$g1 + out$g2 == 2))
          if(group_int) {
            # Create interaction term between group indicator and combinatoric group size
            d_tri$group_int <- log(out$g1 * out$g2)  
          }
          out$x_tri <- d_tri
          out$K_tri <- ncol(d_tri)
        }
      }
      
      out <- Filter(Negate(is.null), out)
      
    }
  } else { #non-hierarchical
    
    if(analysis_type == "second_order") {
      if(contact_type == "exhome") {
        data <- filter(data, both_exhome == "Yes")
      }
      
      d <- data %>%
        dplyr::mutate(full_hh_id = paste(LOC_ID, HH_ID, sep = "."),
                      full_part_id = paste(LOC_ID, HH_ID, PARTICIPANT_ID, sep = "."))
      
      # Triangle data
      d_tri <- d %>%
        dplyr::select(full_part_id, age_cat, occ_cat, CONTACT_Touch, loc_cat, dur_cat, freq_cat,
                      touch_rc, loc_cat_rc, dur_cat_rc, freq_cat_rc,
                      both_touch, shared_location, shared_freq, shared_dur, VISIT, HH_Num_Member_Cat,
                      ninetile_hh, PART_GENDER, dens_quantile) %>%
        dplyr::mutate(occ_cat_student = ifelse(occ_cat == "Student", 1, 0),
                      occ_cat_other = ifelse(occ_cat == "Other", 1, 0),
                      age_cat0_15 = ifelse(age_cat == "0_15", 1, 0),
                      age_cat16_25 = ifelse(age_cat == "16_25", 1, 0),
                      age_cat26_35 = ifelse(age_cat == "26_35", 1, 0),
                      age_cat46_55 = ifelse(age_cat == "46_55", 1, 0),
                      age_cat56_65 = ifelse(age_cat == "56_65", 1, 0),
                      age_cat66_75 = ifelse(age_cat == "66_75", 1, 0),
                      age_cat76_ = ifelse(age_cat == "76+", 1, 0),
                      loc_cat_school = ifelse(loc_cat == "School" | loc_cat_rc == "School", 1, 0),
                      loc_cat_work = ifelse(loc_cat == "Work" | loc_cat_rc == "Work", 1, 0),
                      loc_cat_social = ifelse(loc_cat == "Social" | loc_cat_rc == "Social", 1, 0),
                      loc_cat_home = ifelse(loc_cat == "Home" | loc_cat_rc == "Home", 1, 0),
                      loc_cat_other = ifelse(loc_cat == "Other" | loc_cat_rc == "Other", 1, 0),
                      dur_cat_10 = ifelse(dur_cat == "<10 min" | dur_cat_rc == "<10 min", 1, 0),
                      dur_cat_10_59 = ifelse(dur_cat == "10-59 min" | dur_cat_rc == "10-59 min", 1, 0),
                      freq_cat_1 = ifelse(freq_cat == "<1/wk" | freq_cat_rc == "<1/wk", 1, 0),
                      freq_cat_1_3 = ifelse(freq_cat == "1-3/wk" | freq_cat_rc == "1-3/wk", 1, 0),
                      HH_Num_Memb_1_3 = ifelse(HH_Num_Member_Cat == "1-3", 1, 0),
                      HH_Num_Memb_7_9 = ifelse(HH_Num_Member_Cat == "7-9", 1, 0),
                      HH_Num_Memb_10 = ifelse(HH_Num_Member_Cat == "10+", 1, 0),
                      both_touch = ifelse(both_touch == "Yes", 1, 0),
                      shared_location = ifelse(shared_location == "Yes", 1, 0),
                      shared_freq = ifelse(shared_freq == "Yes", 1, 0),
                      shared_dur = ifelse(shared_dur == "Yes", 1, 0),
                      log_ninetile_hh = log10(ninetile_hh),
                      PART_MALE = ifelse(PART_GENDER == "Male", 1, 0),
                      dens_q1 = ifelse(dens_quantile == "1st quartile", 1, 0),
                      dens_q4 = ifelse(dens_quantile == "4th quartile", 1, 0),
                      dens_qmid = ifelse(dens_quantile == "Middle 50%", 1, 0))
      
      d_tri <- d_tri %>%
        dplyr::select(-full_part_id, -loc_cat, -dur_cat,
                      -touch_rc, -loc_cat_rc, -dur_cat_rc, -freq_cat_rc,
                      -freq_cat, -VISIT, -age_cat, -occ_cat,
                      -HH_Num_Member_Cat, -ninetile_hh,
                      -PART_GENDER, -dens_quantile)
      
      if(part_only) {
        d_tri <- d_tri %>%
          dplyr::select(occ_cat_student, occ_cat_other, age_cat0_15, age_cat16_25,
                        age_cat26_35, age_cat46_55, age_cat56_65,
                        age_cat66_75, age_cat76_,
                        HH_Num_Memb_1_3, HH_Num_Memb_7_9, HH_Num_Memb_10,
                        log_ninetile_hh, PART_MALE)
      }
      
      if(length(var) > 0) {
        d_tri <- d_tri %>% dplyr::select(all_of(var))
      }
      
      N_tri <- nrow(d_tri)
      K_tri <- ncol(d_tri) 
      
      # Set apropriate outcomoe variable
      if(tri_outcome == 1) {
        y <- d$met_rc_no
      }
      if(tri_outcome == 2) {
        y <- d$met_rc_yn
      }
      if(tri_outcome == 3) {
        y <- d$met_rc_yes
      }
      
      out <- list(N_tri=N_tri,
                  K_tri = K_tri,
                  y = y,
                  x_tri=d_tri)
      
      if(grp_lik) {
        out <- append(out, list(g1 = d$CONTACT_Num,
                                g2 = d$CONTACT_Num_rc))
        
        if(group_ind) {
          # Create indicator for whether a group is in the potential traingle
          d_tri$group_ind <- as.numeric(!(out$g1 + out$g2 == 2))
          if(group_int) {
            # Create interaction term between group indicator and combinatoric group size
            d_tri$group_int <- log(out$g1 * out$g2)  
          }
          out$x_tri <- d_tri
          out$K_tri <- ncol(d_tri)
        }
      }
      
      out <- Filter(Negate(is.null), out)
    }
  }
  
  return(out)
  
}

# Wrapper function that creates data, runs model, and processes results
##' @stan_file file path to stan model to run
##' @save_name name to save the the stanfit rds object as 
##' @run_type "all", "exhome", or "both" - indiciates whether to run for all contacts or contacts outside the home
run_stan_model <- function(stan_file, save_name, run_type, save_chains = FALSE,
                           data, analysis_type, full = TRUE, part_only = FALSE,
                           var = c(), var_level = c(), grp_lik = FALSE,
                           group_ind = FALSE, group_int = FALSE, hier = TRUE,
                           chains = 4, iter = 2000, cores = 1, tri_outcome = 2) {
  
  all <- run_type %in% c("all", "both")
  exhome <- run_type %in% c("exhome", "both")
  
  if(exhome & length(var) > 0) {
    var_ex <- var[var != "loc_cat_home"]
    var_ex <- var_ex[var_ex != "num_contacts"]
  } else {
    var_ex <- var
  }
  
  if(all & length(var) > 0) {
    var <- var[var != "num_contacts_exhome"]
  }
  
  # Create data
  if(all) {
    dat_all <- get_stan_data(data = data, analysis_type = analysis_type,
                             full = full, contact_type = "all",
                             grp_lik = grp_lik, part_only = part_only, hier = hier,
                             group_ind = group_ind, group_int = group_int,
                             var = var, var_level = var_level, tri_outcome = tri_outcome)
    if(save_chains) {
      saveRDS(dat_all, paste0(save_name, "_dat.rds"))
    }
    
  } 
  if(exhome) {
    dat_ex <- get_stan_data(data = data, analysis_type = analysis_type,
                            full = full, contact_type = "exhome",
                            grp_lik = grp_lik, part_only = part_only, hier = hier,
                            group_ind = group_ind, group_int = group_int,
                            var = var_ex, var_level = var_level, tri_outcome = tri_outcome)
    
    if(save_chains) {
      saveRDS(dat_ex, paste0(save_name, "_dat_ex.rds"))
    }
  }
  
  # Run models
  if(all) {
    run_all <- stan(file = stan_file,
                    data = dat_all,
                    chains = chains, iter = iter, cores = cores)
      
    if(save_chains) {
      saveRDS(run_all, paste0(save_name, ".rds"))
    }
    
  }
  if(exhome) {
    run_ex <- stan(file = stan_file,
                   data = dat_ex,
                   chains = chains, iter = iter, cores = cores)
      
    if(save_chains) {
      saveRDS(run_ex, paste0(save_name, "_ex.rds"))
    }
  }  
  
  
  # Process results
  res <- data.frame()
  if(all) {
    res_temp <- get_coefficients(fit = run_all,
                                 data = dat_all,
                                 analysis_type = analysis_type) %>%
      mutate(Contact_type = "All contacts")
    
    res <- bind_rows(res, res_temp)
  }
  if(exhome) {
    res_temp <- get_coefficients(fit = run_ex,
                                 data = dat_ex,
                                 analysis_type = analysis_type) %>%
      mutate(Contact_type = "Extra-household contacts")
    
    res <- bind_rows(res, res_temp)
  }
  
  saveRDS(res, paste0(save_name, "_res.rds"))
  
  return(res)
  
}

##' Process stan model fit into interpretable coefficients
##' Currently only full hierarchical models supported
##' @param fit stan model fit to process
##' @param data data fed into the stan model
##' @param analysis_type "first_order" or "second_order"
##' ##' @param hier TRUE or FALSE indicating whether hierarchical model or non-hierarchical model was used
get_coefficients <- function(fit, data, analysis_type) {
  if(analysis_type == "first_order") {
    
    # Pull variable names from input data
    coef_obs <- colnames(data$x_obs)
    coef_part <- colnames(data$x_part)
    coef_hh <- colnames(data$x_hh)
    coef_loc <- colnames(data$x_loc)
    
    fit_sum <- as.data.frame(summary(fit)$summary)
    fit_sum$name <- rownames(fit_sum)
    
    # get coefficients at each level
    fit_obs <- fit_sum %>%
      filter(str_detect(name, "B_obs")) %>%
      mutate(name_long = coef_obs)
    
    fit_part <- fit_sum %>%
      filter(str_detect(name, "B_part")) %>%
      mutate(name_long = coef_part)
    
    fit_hh <- fit_sum %>%
      filter(str_detect(name, "B_hh")) %>%
      mutate(name_long = coef_hh)
    
    fit_loc <- fit_sum %>%
      filter(str_detect(name, "B_loc")) %>%
      mutate(name_long = coef_loc)
    
    fit_sigma <- fit_sum %>%
      filter(str_detect(name, "sigma")) %>%
      mutate(name_long = name)
    
    # bind together and exponentiate
    out <- bind_rows(fit_obs, fit_part, fit_hh, fit_loc, fit_sigma) %>%
      mutate(Estimate = exp(mean),
             CI_low = exp(`2.5%`),
             CI_high = exp(`97.5%`)) %>%
      dplyr::select(name, name_long, Estimate, CI_low, CI_high, mean, se_mean, sd, Rhat)
    rownames(out) <- NULL
    
    return(out)
    
  } else { # second-order
    # Pull variable names from input data
    coef_obs <- colnames(data$x_tri)
    coef_part <- colnames(data$x_part)
    coef_hh <- colnames(data$x_hh)
    coef_loc <- colnames(data$x_loc)
    
    fit_sum <- as.data.frame(rstan::summary(fit)$summary)
    fit_sum$name <- rownames(fit_sum)
    
    # get coefficients at each level
    fit_tri <- fit_sum %>%
      filter(str_detect(name, "B_tri")) %>%
      mutate(name_long = coef_obs)
    
    fit_part <- fit_sum %>%
      filter(str_detect(name, "B_part")) %>%
      mutate(name_long = coef_part)
    
    fit_hh <- fit_sum %>%
      filter(str_detect(name, "B_hh")) %>%
      mutate(name_long = coef_hh)
    
    fit_loc <- fit_sum %>%
      filter(str_detect(name, "B_loc")) %>%
      mutate(name_long = coef_loc)
    
    fit_sigma <- fit_sum %>%
      filter(str_detect(name, "sigma") | str_detect(name, "scale")) %>%
      mutate(name_long = name)
    
    # bind together and exponentiate
    out <- bind_rows(fit_tri, fit_part, fit_hh, fit_loc, fit_sigma) %>%
      mutate(Estimate = exp(mean),
             CI_low = exp(`2.5%`),
             CI_high = exp(`97.5%`)) %>%
      dplyr::select(name, name_long, Estimate, CI_low, CI_high, mean, se_mean, sd, Rhat)
    rownames(out) <- NULL
    
    return(out)
  }
}

##' Manually compute likelihood matrix to feed into loo package waic function
##' Works for second-order analyses only
##' @stanfit stanfit object for the model to compute WAIC for
##' @data Input data for the model
##' @iter Number of (non-warmup) iterations of the MCMC chain
##' @verbose Indicator for whether to print what iteration likelihood is being calculated for
##' @scale Indicator for whether scaling parameter is included in the likelihood
get_waic <- function(stanfit, data, iter = 4000, verbose = FALSE, scale = FALSE) {
  x_tri <- data$x_tri
  if("CONTACT_Touch" %in% colnames(x_tri)) {
    x_tri$CONTACT_Touch <- as.numeric(x_tri$CONTACT_Touch)
  }
  y <- data$y
  g1 <- data$g1
  g2 <- data$g2
  part_lookup <- data$part_lookup
  rd_lookup <- data$rd_lookup
  eff_part <- extract_log_lik(stanfit, "eff_part")
  B_tri <- rstan::extract(stanfit, "B_tri")[[1]]
  B0_rd <- rstan::extract(stanfit, "B0_rd")[[1]]
  if(scale) {
    s <- rstan::extract(stanfit, "scale")[[1]]
  }
  
  llik <- matrix(nrow = iter, ncol = nrow(x_tri))
  
  
  if(scale) {
    for(i in 1:iter) {
      if(verbose) {print(paste("Iteration", i, "of", iter))}
      log_odds <- as.matrix(x_tri)%*%B_tri[i,] + eff_part[i, part_lookup] +B0_rd[i,rd_lookup]
      pr <- exp(log_odds)/(1+exp(log_odds))
      for(p in 1:nrow(x_tri)) {
        llik[i, p] <- if(y[p] == 1) {
          log(1 - (1-pr[p])^((g1[p]*g2[p])^s[i]))
        } else {
          log((1-pr[p]))*(g1[p]*g2[p])^s[i]
        }
      }
    }
  } else {
    for(i in 1:iter) {
      if(verbose) {print(paste("Iteration", i, "of", iter))}
      log_odds <- as.matrix(x_tri)%*%B_tri[i,] + eff_part[i, part_lookup] +B0_rd[i,rd_lookup]
      pr <- exp(log_odds)/(1+exp(log_odds))
      for(p in 1:nrow(x_tri)) {
        llik[i, p] <- if(y[p] == 1) {
          log(1 - (1-pr[p])^(g1[p]*g2[p]))
        } else {
          log((1-pr[p]))*(g1[p]*g2[p])
        }
      }
    }
  }
  
  waic <- waic(llik)
  waic_out <- as.data.frame(waic[[1]]) %>%
    rownames_to_column(var = "Type")
  
  return(list("waic" = waic_out,
              "llik" = as.matrix(llik)))
  
}


get_waic_table <- function(models, model_dat) {
  num_models <- length(models)
  
  waic_grpind <- data.frame(waic = numeric(length = num_models),
                            p_waic = numeric(length = num_models),
                            elpd_waic = numeric(length = num_models),
                            n = numeric(length = num_models),
                            model = rep(c("Participant-only", "Individual contact", "Shared contact"),2),
                            contact_type = c(rep("All contacts", 3), rep("Extra-household contacts", 3)))
  
  llik_grpind <- list()
  
  for(i in 1:num_models) {
    print(paste("Model", i))
    temp <- get_waic(models[[i]], model_dat[[i]], iter = 4000, scale = FALSE)
    waic_temp <- temp$waic
    waic_grpind$waic[[i]] <- filter(waic_temp, Type == "waic")$Estimate
    waic_grpind$p_waic[[i]] <- filter(waic_temp, Type == "p_waic")$Estimate
    waic_grpind$elpd_waic[[i]] <- filter(waic_temp, Type == "elpd_waic")$Estimate
    waic_grpind$n[[i]] = nrow(model_dat[[i]]$x_tri)
    
    llik_grpind[[i]] <- temp$llik
  }
  
  waic_grpind <- waic_grpind %>%
    group_by(contact_type) %>%
    summarize(waic_std = waic - min(waic))
  
  return(waic_grpind)
  
}

