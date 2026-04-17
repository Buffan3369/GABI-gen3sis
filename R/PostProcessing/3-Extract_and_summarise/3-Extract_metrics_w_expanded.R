################################################################################
# Name: 3-Extract_metrics_w_expanded.R
# Author: Lucas Buffan (lucas.l.buffan@gmail.com)
# Aim: Same as 3-Extract_metrics.R but for outputs from analyses with expanded
#      thermal niche breadth.
#
#     Datasets ready to be used for plots.
################################################################################

library(tidyverse)
source("./R/useful/helper_functions.R")

for(w in c(0.6, 1)){
  ## Loop across models to create plot datasets --------------------------------
  for(mdl in c("M0", "M1")){
    NA_recap_tbl <- read.table(paste0("./Data/Gen3sis_parameter_tables/Niche_expanded/w_", w,
                                      "/", mdl, "/North_America_parameters_EXTENDED_EXCH_AREA_DIV_DIST.txt"),
                                 header = T, sep = "\t")
    SA_recap_tbl <- read.table(paste0("./Data/Gen3sis_parameter_tables/Niche_expanded/w_", w,
                                      "/", mdl, "/South_America_parameters_EXTENDED_EXCH_AREA_DIV_DIST.txt"),
                               header = T, sep = "\t")
    
    ## Filter out simulations that crashed -------------------------------------
    cat("Number of simulations that crashed for ", mdl, " with North American ancestor: ", length(which(NA_recap_tbl$exchanged == -1)), "\n")
    NA_recap_tbl <- NA_recap_tbl %>% filter(!(exchanged == -1))
    cat("Number of simulations that crashed for ", mdl, " with South American ancestor: ", length(which(SA_recap_tbl$exchanged == -1)), "\n")
    SA_recap_tbl <- SA_recap_tbl %>% filter(!(exchanged == -1))
    
    ## Success tables (= simulations resulting in a successful exchange) -------
    NA_success_tbl <- NA_recap_tbl %>% filter(exchanged == 1)
    SA_success_tbl <- SA_recap_tbl %>% filter(exchanged == 1)
    
    ## 1. Success proportions --------------------------------------------------
    na_success <- nrow(NA_success_tbl) / nrow(NA_recap_tbl)
    sa_success <- nrow(SA_success_tbl) / nrow(SA_recap_tbl)
    
    ## 1b. Success proportions accounting for simulations that crashed -----------
    na_success_raw <- nrow(NA_success_tbl) / 500
    sa_success_raw <- nrow(SA_success_tbl) / 500
    
    ## 2. Proportion of colonised area -----------------------------------------
    na_prop_col_area <- mean(NA_success_tbl$prop_col_area, na.rm = T)
    na_sd_area <- sd(NA_success_tbl$prop_col_area, na.rm = T)
    sa_prop_col_area <- mean(SA_success_tbl$prop_col_area, na.rm = T)
    sa_sd_area <- sd(SA_success_tbl$prop_col_area, na.rm = T)
    
    ## 3. Absolute colonised area ----------------------------------------------
    na_abs_col_area <- mean(NA_success_tbl$abs_col_area, na.rm = T)
    na_sd_abs_area <- sd(NA_success_tbl$abs_col_area, na.rm = T)
    sa_abs_col_area <- mean(SA_success_tbl$abs_col_area, na.rm = T)
    sa_sd_abs_area <- sd(SA_success_tbl$abs_col_area, na.rm = T)
    
    ## 4. Diversity in colonised area ------------------------------------------
    na_mean_div <- mean(NA_success_tbl$div_col, na.rm = T)
    na_sd_div <- sd(NA_success_tbl$div_col, na.rm = T)
    sa_mean_div <- mean(SA_success_tbl$div_col, na.rm = T)
    sa_sd_div <- sd(SA_success_tbl$div_col, na.rm = T)
    
    ## 5. Distance to the Isthmus ----------------------------------------------
    na_mean_dist <- mean(NA_success_tbl$dist_to_isthmus, na.rm = T)
    na_sd_dist <- sd(NA_success_tbl$dist_to_isthmus, na.rm = T)
    sa_mean_dist <- mean(SA_success_tbl$dist_to_isthmus, na.rm = T)
    sa_sd_dist <- sd(SA_success_tbl$dist_to_isthmus, na.rm = T)
    
    if(mdl == "M0"){
      ## 1. Proportion of success (associated CI from binomial) ----------------
      plot_df_prop_success <- data.frame(Ori = c("North America", "South America"),
                                         Prop_success = c(na_success, sa_success),
                                         Lower_CI = c(bino_CI(prop = na_success,
                                                              n = nrow(NA_recap_tbl),
                                                              alpha = 0.05,
                                                              what = "Lower"),
                                                      bino_CI(prop = sa_success,
                                                              n = nrow(SA_recap_tbl),
                                                              alpha = 0.05,
                                                              what = "Lower")),
                                         Upper_CI = c(bino_CI(prop = na_success,
                                                              n = nrow(NA_recap_tbl),
                                                              alpha = 0.05,
                                                              what = "Upper"),
                                                      bino_CI(prop = sa_success,
                                                              n = nrow(SA_recap_tbl),
                                                              alpha = 0.05,
                                                              what = "Upper")),
                                         Model = c(mdl, mdl))
      
      ## 1b. Raw proportion of success (associated CI from binomial) -------------
      plot_df_prop_success_raw <- data.frame(Ori = c("North America", "South America"),
                                             Prop_success = c(na_success_raw, sa_success_raw),
                                             Lower_CI = c(bino_CI(prop = na_success_raw,
                                                                  n = 500,
                                                                  alpha = 0.05,
                                                                  what = "Lower"),
                                                          bino_CI(prop = sa_success_raw,
                                                                  n = 500,
                                                                  alpha = 0.05,
                                                                  what = "Lower")),
                                             Upper_CI = c(bino_CI(prop = na_success_raw,
                                                                  n = 500,
                                                                  alpha = 0.05,
                                                                  what = "Upper"),
                                                          bino_CI(prop = sa_success_raw,
                                                                  n = 500,
                                                                  alpha = 0.05,
                                                                  what = "Upper")),
                                             Model = c(mdl, mdl))
      
      ## 2. Proportion of colonised area (and associated CI) -------------------
      plot_df_prop_col_area <- data.frame(Ori = c("North America", "South America"),
                                          Prop_col_area = c(na_prop_col_area, sa_prop_col_area),
                                          Lower_CI = c(Student_CI(x_bar = na_prop_col_area,
                                                                  n = nrow(NA_recap_tbl),
                                                                  sigma = na_sd_area,
                                                                  alpha = 0.05,
                                                                  what = "Lower"),
                                                       Student_CI(x_bar = sa_prop_col_area,
                                                                  n = nrow(SA_recap_tbl),
                                                                  sigma = sa_sd_area,
                                                                  alpha = 0.05,
                                                                  what = "Lower")),
                                          Upper_CI = c(Student_CI(x_bar = na_prop_col_area,
                                                                  n = nrow(NA_recap_tbl),
                                                                  sigma = na_sd_area,
                                                                  alpha = 0.05,
                                                                  what = "Upper"),
                                                       Student_CI(x_bar = sa_prop_col_area,
                                                                  n = nrow(SA_recap_tbl),
                                                                  sigma = sa_sd_area,
                                                                  alpha = 0.05,
                                                                  what = "Upper")),
                                          Model = c(mdl, mdl))
      
      ## 3. Absolute colonised area (and associated CI) --------------------------
      plot_df_abs_col_area <- data.frame(Ori = c("North America", "South America"),
                                         Abs_col_area = c(na_abs_col_area, sa_abs_col_area),
                                         Lower_CI = c(Student_CI(x_bar = na_abs_col_area,
                                                                 n = nrow(NA_recap_tbl),
                                                                 sigma = na_sd_abs_area,
                                                                 alpha = 0.05,
                                                                 what = "Lower"),
                                                      Student_CI(x_bar = sa_abs_col_area,
                                                                 n = nrow(SA_recap_tbl),
                                                                 sigma = sa_sd_abs_area,
                                                                 alpha = 0.05,
                                                                 what = "Lower")),
                                         Upper_CI = c(Student_CI(x_bar = na_abs_col_area,
                                                                 n = nrow(NA_recap_tbl),
                                                                 sigma = na_sd_abs_area,
                                                                 alpha = 0.05,
                                                                 what = "Upper"),
                                                      Student_CI(x_bar = sa_abs_col_area,
                                                                 n = nrow(SA_recap_tbl),
                                                                 sigma = sa_sd_abs_area,
                                                                 alpha = 0.05,
                                                                 what = "Upper")),
                                         Model = c(mdl, mdl))      
      ## 4. Diversity in colonised area (and associated CI) --------------------
      plot_df_div_col_area <- data.frame(Ori = c("North America", "South America"),
                                         Div_col_area = c(na_mean_div, sa_mean_div),
                                         Lower_CI = c(Student_CI(x_bar = na_mean_div,
                                                                 n = nrow(NA_recap_tbl),
                                                                 sigma = na_sd_div,
                                                                 alpha = 0.05,
                                                                 what = "Lower"),
                                                      Student_CI(x_bar = sa_mean_div,
                                                                 n = nrow(SA_recap_tbl),
                                                                 sigma = sa_sd_div,
                                                                 alpha = 0.05,
                                                                 what = "Lower")),
                                         Upper_CI = c(Student_CI(x_bar = na_mean_div,
                                                                 n = nrow(NA_recap_tbl),
                                                                 sigma = na_sd_div,
                                                                 alpha = 0.05,
                                                                 what = "Upper"),
                                                      Student_CI(x_bar = sa_mean_div,
                                                                 n = nrow(SA_recap_tbl),
                                                                 sigma = sa_sd_div,
                                                                 alpha = 0.05,
                                                                 what = "Upper")),
                                         Model = c(mdl, mdl))
      
      ## 5. Distance of the starting species to the Isthmus --------------------
      plot_df_dist_isthmus <- data.frame(Ori = c("North America", "South America"),
                                         Dist = c(na_mean_dist, sa_mean_dist),
                                         Lower_CI = c(Student_CI(x_bar = na_mean_dist,
                                                                 n = nrow(NA_recap_tbl),
                                                                 sigma = na_sd_dist,
                                                                 alpha = 0.05,
                                                                 what = "Lower"),
                                                      Student_CI(x_bar = sa_mean_dist,
                                                                 n = nrow(SA_recap_tbl),
                                                                 sigma = sa_sd_dist,
                                                                 alpha = 0.05,
                                                                 what = "Lower")),
                                         Upper_CI = c(Student_CI(x_bar = na_mean_dist,
                                                                 n = nrow(NA_recap_tbl),
                                                                 sigma = na_sd_dist,
                                                                 alpha = 0.05,
                                                                 what = "Upper"),
                                                      Student_CI(x_bar = sa_mean_dist,
                                                                 n = nrow(SA_recap_tbl),
                                                                 sigma = sa_sd_dist,
                                                                 alpha = 0.05,
                                                                 what = "Upper")),
                                         Model = c(mdl, mdl))
      
    }
    else{
      ## 1. Proportion of success (associated CI from binomial) ----------------
      plot_df_prop_success <- plot_df_prop_success %>%
        add_row(Ori = c("North America", "South America"),
                Prop_success = c(na_success, sa_success),
                Lower_CI = c(bino_CI(prop = na_success,
                                     n = nrow(NA_recap_tbl),
                                     alpha = 0.05,
                                     what = "Lower"),
                             bino_CI(prop = sa_success,
                                     n = nrow(SA_recap_tbl),
                                     alpha = 0.05,
                                     what = "Lower")),
                Upper_CI = c(bino_CI(prop = na_success,
                                     n = nrow(NA_recap_tbl),
                                     alpha = 0.05,
                                     what = "Upper"),
                             bino_CI(prop = sa_success,
                                     n = nrow(SA_recap_tbl),
                                     alpha = 0.05,
                                     what = "Upper")),
                Model = c(mdl, mdl))
      
      ## 1b. Raw proportion of success (associated CI from binomial) -------------
      plot_df_prop_success_raw <- plot_df_prop_success_raw %>% 
        add_row(Ori = c("North America", "South America"),
                Prop_success = c(na_success_raw, sa_success_raw),
                Lower_CI = c(bino_CI(prop = na_success_raw,
                                     n = 500,
                                     alpha = 0.05,
                                     what = "Lower"),
                             bino_CI(prop = sa_success_raw,
                                     n = 500,
                                     alpha = 0.05,
                                     what = "Lower")),
                Upper_CI = c(bino_CI(prop = na_success_raw,
                                     n = 500,
                                     alpha = 0.05,
                                     what = "Upper"),
                             bino_CI(prop = sa_success_raw,
                                     n = 500,
                                     alpha = 0.05,
                                     what = "Upper")),
                Model = c(mdl, mdl))
      
      ## 2. Proportion of colonised area ---------------------------------------
      plot_df_prop_col_area <- plot_df_prop_col_area %>% 
        add_row(Ori = c("North America", "South America"),
                Prop_col_area = c(na_prop_col_area, sa_prop_col_area),
                Lower_CI = c(Student_CI(x_bar = na_prop_col_area,
                                        n = nrow(NA_recap_tbl),
                                        sigma = na_sd_area,
                                        alpha = 0.05,
                                        what = "Lower"),
                             Student_CI(x_bar = sa_prop_col_area,
                                        n = nrow(SA_recap_tbl),
                                        sigma = sa_sd_area,
                                        alpha = 0.05,
                                        what = "Lower")),
                Upper_CI = c(Student_CI(x_bar = na_prop_col_area,
                                        n = nrow(NA_recap_tbl),
                                        sigma = na_sd_area,
                                        alpha = 0.05,
                                        what = "Upper"),
                             Student_CI(x_bar = sa_prop_col_area,
                                        n = nrow(SA_recap_tbl),
                                        sigma = sa_sd_area,
                                        alpha = 0.05,
                                        what = "Upper")),
                Model = c(mdl, mdl))
      
      ## 3. Absolute colonised area --------------------------------------------
      plot_df_abs_col_area <- plot_df_abs_col_area %>% 
        add_row(Ori = c("North America", "South America"),
                Abs_col_area = c(na_abs_col_area, sa_abs_col_area),
                Lower_CI = c(Student_CI(x_bar = na_abs_col_area,
                                        n = nrow(NA_recap_tbl),
                                        sigma = na_sd_abs_area,
                                        alpha = 0.05,
                                        what = "Lower"),
                             Student_CI(x_bar = sa_abs_col_area,
                                        n = nrow(SA_recap_tbl),
                                        sigma = sa_sd_abs_area,
                                        alpha = 0.05,
                                        what = "Lower")),
                Upper_CI = c(Student_CI(x_bar = na_abs_col_area,
                                        n = nrow(NA_recap_tbl),
                                        sigma = na_sd_abs_area,
                                        alpha = 0.05,
                                        what = "Upper"),
                             Student_CI(x_bar = sa_abs_col_area,
                                        n = nrow(SA_recap_tbl),
                                        sigma = sa_sd_abs_area,
                                        alpha = 0.05,
                                        what = "Upper")),
                Model = c(mdl, mdl))
      
      ## 4. Diversity in the colonised area ------------------------------------
      plot_df_div_col_area <- plot_df_div_col_area %>% 
        add_row(Ori = c("North America", "South America"),
                Div_col_area = c(na_mean_div, sa_mean_div),
                Lower_CI = c(Student_CI(x_bar = na_mean_div,
                                        n = nrow(NA_recap_tbl),
                                        sigma = na_sd_div,
                                        alpha = 0.05,
                                        what = "Lower"),
                             Student_CI(x_bar = sa_mean_div,
                                        n = nrow(SA_recap_tbl),
                                        sigma = sa_sd_div,
                                        alpha = 0.05,
                                        what = "Lower")),
                Upper_CI = c(Student_CI(x_bar = na_mean_div,
                                        n = nrow(NA_recap_tbl),
                                        sigma = na_sd_div,
                                        alpha = 0.05,
                                        what = "Upper"),
                             Student_CI(x_bar = sa_mean_div,
                                        n = nrow(SA_recap_tbl),
                                        sigma = sa_sd_div,
                                        alpha = 0.05,
                                        what = "Upper")),
                Model = c(mdl, mdl))
      
      ## 5. Distance to the Isthmus --------------------------------------------
      plot_df_dist_isthmus <- plot_df_dist_isthmus %>% 
        add_row(Ori = c("North America", "South America"),
                Dist = c(na_mean_dist, sa_mean_dist),
                Lower_CI = c(Student_CI(x_bar = na_mean_dist,
                                        n = nrow(NA_recap_tbl),
                                        sigma = na_sd_dist,
                                        alpha = 0.05,
                                        what = "Lower"),
                             Student_CI(x_bar = sa_mean_dist,
                                        n = nrow(SA_recap_tbl),
                                        sigma = sa_sd_dist,
                                        alpha = 0.05,
                                        what = "Lower")),
                Upper_CI = c(Student_CI(x_bar = na_mean_dist,
                                        n = nrow(NA_recap_tbl),
                                        sigma = na_sd_dist,
                                        alpha = 0.05,
                                        what = "Upper"),
                             Student_CI(x_bar = sa_mean_dist,
                                        n = nrow(SA_recap_tbl),
                                        sigma = sa_sd_dist,
                                        alpha = 0.05,
                                        what = "Upper")),
                Model = c(mdl, mdl))
    }
  }
  
  ## Save each dataset ---------------------------------------------------------

  # Proportion of succesful exchange
  saveRDS(plot_df_prop_success, paste0("./Results/Exchanged_metrics/Niche_expanded/w_",
                                       w, "/Prop_successful_exchange.RDS"))
  # Raw proportion of succesful exchange
  saveRDS(plot_df_prop_success_raw, paste0("./Results/Exchanged_metrics/Niche_expanded/w_", 
                                           w, "/RawProp_successful_exchange.RDS"))
  # Proportion of colonised area
  saveRDS(plot_df_prop_col_area, paste0("./Results/Exchanged_metrics/Niche_expanded/w_", 
                                        w, "/Prop_colonised_area.RDS"))
  # Absolute colonised area
  saveRDS(plot_df_abs_col_area, paste0("./Results/Exchanged_metrics/Niche_expanded/w_", 
                                       w, "/Absolute_colonised_area.RDS"))
  # Diversity in the colonised area
  saveRDS(plot_df_div_col_area, paste0("./Results/Exchanged_metrics/Niche_expanded/w_", 
                                       w, "/Div_col_area.RDS"))
  # Distance to the isthmus
  saveRDS(plot_df_dist_isthmus, paste0("./Results/Exchanged_metrics/Niche_expanded/w_", 
                                       w, "/Dist_isthmus.RDS"))
}

