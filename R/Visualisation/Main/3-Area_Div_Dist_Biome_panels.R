################################################################################
# Name: 3-Area_Div_Dist_panels.R
# Author: Lucas Buffan
# E-mail: lucas.l.buffan@gmail.com
# Goal: Violin plots for 
#         1. Proportion of colonised area
#         2. Diversity in the colonised area
#         3. Distance between simulation starting point and isthmus
#
#       Additionally, barplot for starting biomes.
################################################################################

library(tidyverse)

## Assemble large dataset ------------------------------------------------------
P_tbl <- read.table(paste0("./Data/Gen3sis_parameter_tables/M0/North_America_parameters_EXTENDED_EXCH_AREA_DIV_DIST_biome.txt"),
                    header = T)
P_tbl <- P_tbl %>% 
  # convert to km
  mutate(exchanged = as.factor(exchanged),
         model = "M0",
         start_region = "North America")

for(mdl in c("M0", "M1", "M2", "M3")){
  for(start in c("North", "South")){
    if(mdl == "M0" & start == "North"){
      next
    }
    param_tbl <- read.table(paste0("./Data/Gen3sis_parameter_tables/", mdl,
                                   "/", start, "_America_parameters_EXTENDED_EXCH_AREA_DIV_DIST_biome.txt"),
                            header = T)
    
    param_tbl <- param_tbl %>% 
      mutate(exchanged = as.factor(exchanged),
             model = mdl,
             start_region = paste0(start, " America"))
    # Adjust in case one of the two climatic breadths are missing (M2 or M3)
    if(mdl == "M2"){
      param_tbl <- param_tbl %>% add_column(omega_P = NA, .after = "omega_T")
    }
    if(mdl == "M3"){
      param_tbl <- param_tbl %>% add_column(omega_T = NA, .before = "omega_P")
    }
    # Merge
    P_tbl <- rbind.data.frame(P_tbl, param_tbl)
    }
}

################################################################################
####################### 1. PROPORTION OF COLONISED AREA ########################
################################################################################

prop_plot <- P_tbl %>% 
  # Filter out unsuccessful colonisations
  filter(prop_col_area != -1) %>% 
  # Plot
  ggplot(aes(x = start_region, y = prop_col_area)) +
  geom_violin(adjust = .75, draw_quantiles = c(0.5), scale = "width", linewidth = 0.2, aes(fill = factor(start_region))) +
  geom_point(shape = ".") +
  scale_y_continuous(limits = c(0, 1)) +
  scale_fill_manual(values = c("#fcbba1", "#ccece6")) +
  labs(x = "Ancestral region", y = "Prop. area colonised") +
  theme(legend.position = "none",
        axis.title = element_text(size = 7.5),
        axis.text = element_text(size = 6),
        axis.line = element_line(linewidth = 0.3, color = "black"),
        strip.background = element_rect(fill = "#DDE6F5"),
        panel.background = element_rect(fill = "#F7F2E0"),
        panel.grid.major = element_line(linewidth = 0.25),
        panel.grid.minor = element_line(linewidth = 0.25),
        plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm")) +
  facet_grid(.~model)

ggsave("./Figures/prop_col_area/PANEL_prop_col_area.pdf", 
       plot = prop_plot, height = 80, width = 170, units = "mm")

ggsave("./Figures/prop_col_area/PANEL_prop_col_area.png", 
       plot = prop_plot, height = 80, width = 170, dpi = 600, units = "mm")


################################################################################
##################### 2. DIVERSITY IN THE COLONISED AREA #######################
################################################################################

div_plot <-  P_tbl %>% 
  # Filter out unsuccessful colonisations
  filter(div_col != -1) %>% 
  # Log-transform diversity in the colonised area
  mutate(logdiv = sapply(X = div_col, FUN = log10)) %>% 
  # Plot
  ggplot(aes(x = start_region, y = logdiv)) +
  geom_violin(adjust = .75, draw_quantiles = c(0.5), scale = "width", linewidth = 0.2, aes(fill = factor(start_region))) +
  geom_point(shape = ".") +
  scale_fill_manual(values = c("#fcbba1", "#ccece6")) +
  labs(x = "Ancestral region", y = "Log(Diversity in colonised region)") +
  theme(legend.position = "none",
        axis.title = element_text(size = 7.5),
        axis.text = element_text(size = 6),
        axis.line = element_line(linewidth = 0.3, color = "black"),
        strip.background = element_rect(fill = "#DDE6F5"),
        panel.background = element_rect(fill = "#EAF4FB"),
        panel.grid.major = element_line(linewidth = 0.25),
        panel.grid.minor = element_line(linewidth = 0.25),
        plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm")) +
  facet_grid(.~model)

ggsave("./Figures/div_col_area/PANEL_div_col_area.pdf", 
       plot = div_plot, height = 80, width = 170, units = "mm")

ggsave("./Figures/div_col_area/PANEL_div_col_area.png", 
       plot = div_plot, height = 80, width = 170, dpi = 600, units = "mm")


################################################################################
############################ 3. DISTANCE TO ISTHMUS ############################
################################################################################

## Plot with simulations that crashed ------------------------------------------
plot_crash <- P_tbl %>% 
  ggplot(aes(x = exchanged, y = dist_to_isthmus)) +
  geom_violin(adjust = .75, draw_quantiles = c(0.5), scale = "width", linewidth = 0.2, aes(fill = exchanged)) +
  scale_fill_manual(values = c("grey80", "#f7fcb9", "#78c679")) +
  geom_point(shape = ".") +
  labs(x = "Exchange success", y = "Distance to isthmus (km)") +
  facet_grid(start_region~model) +
  theme(legend.position = "none",
        axis.title = element_text(size = 7.5),
        axis.text = element_text(size = 6),
        axis.line = element_line(linewidth = 0.3, color = "black"),
        strip.background = element_rect(fill = "#DDE6F5"),
        panel.background = element_rect(fill = "grey85"),
        panel.grid.major = element_line(linewidth = 0.25),
        panel.grid.minor = element_line(linewidth = 0.25),
        plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"))

ggsave(paste0("./Figures/dist_to_isthm/distance_violin_panel_CRASH.pdf"), 
       plot = plot_crash, height = 120, width = 200, units = "mm")

ggsave(paste0("./Figures/dist_to_isthm/distance_violin_panel_CRASH.png"), 
       plot = plot_crash, height = 120, width = 200, dpi = 600, units = "mm")

## Plot without simulations that crashed ---------------------------------------
plot_NoCrash <- P_tbl %>% 
  filter(exchanged != -1) %>% 
  ggplot(aes(x = exchanged, y = dist_to_isthmus)) +
  geom_violin(adjust = .75, draw_quantiles = c(0.5), scale = "width", linewidth = 0.2, aes(fill = exchanged)) +
  scale_fill_manual(values = c( "#f7fcb9", "#78c679")) +
  geom_point(shape = ".") +
  labs(x = "Exchange success", y = "Distance to isthmus (km)") +
  facet_grid(start_region~model) +
  theme(legend.position = "none",
        axis.title = element_text(size = 7.5),
        axis.text = element_text(size = 6),
        axis.line = element_line(linewidth = 0.3, color = "black"),
        strip.background = element_rect(fill = "#DDE6F5"),
        panel.background = element_rect(fill = "grey85"),
        panel.grid.major = element_line(linewidth = 0.25),
        panel.grid.minor = element_line(linewidth = 0.25),
        plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"))

ggsave(paste0("./Figures/dist_to_isthm/distance_violin_panel_NoCRASH.pdf"), 
       plot = plot_NoCrash, height = 120, width = 170, units = "mm")

ggsave(paste0("./Figures/dist_to_isthm/distance_violin_panel_NoCRASH.png"), 
       plot = plot_NoCrash, dpi = 600, height = 120, width = 170, units = "mm")


## North-South Panel -----------------------------------------------------------
  # Successful exchange
comp_plot_successful <- P_tbl %>% 
  filter(exchanged == 1) %>% 
  mutate(model = sapply(model, function(x){paste0(x, " (successful)")})) %>% 
  ggplot(aes(x = start_region, y = dist_to_isthmus)) +
  geom_violin(adjust = .75, draw_quantiles = c(0.5), scale = "width", linewidth = 0.2, aes(fill = factor(start_region))) +
  geom_point(shape = ".") +
  scale_fill_manual(values = c("#fcbba1", "#ccece6")) +
  labs(x = "Ancestral region", y = "Distance to isthmus (km)") +
  theme(legend.position = "none",
        axis.title = element_text(size = 7.5),
        axis.text = element_text(size = 6),
        axis.line = element_line(linewidth = 0.3, color = "black"),
        strip.background = element_rect(fill = "#DDE6F5"),
        panel.background = element_rect(fill = "#F5F0FF"),
        panel.grid.major = element_line(linewidth = 0.25),
        panel.grid.minor = element_line(linewidth = 0.25),
        plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm")
  ) +
  facet_grid(.~model)

ggsave(paste0("./Figures/dist_to_isthm/comparative_distances_SUCCESSFUL.pdf"), 
       plot = comp_plot_successful, height = 80, width = 170, units = "mm")

ggsave(paste0("./Figures/dist_to_isthm/comparative_distances_SUCCESSFUL.png"), 
       plot = comp_plot_successful, height = 80, width = 170, dpi = 600, units = "mm")

  # Unsuccessful exchange
comp_plot_unsuccessful <- P_tbl %>% 
  filter(exchanged == 0) %>% 
  mutate(model = sapply(model, function(x){paste0(x, " (unsuccessful)")})) %>% 
  ggplot(aes(x = start_region, y = dist_to_isthmus)) +
  geom_violin(adjust = .75, draw_quantiles = c(0.5), scale = "width", linewidth = 0.2, aes(fill = factor(start_region))) +
  geom_point(shape = ".") +
  scale_fill_manual(values = c("#fcbba1", "#ccece6")) +
  labs(x = "Ancestral region", y = "Distance to isthmus (km)") +
  theme(legend.position = "none",
        axis.title = element_text(size = 7.5),
        axis.text = element_text(size = 6),
        axis.line = element_line(linewidth = 0.3, color = "black"),
        strip.background = element_rect(fill = "#DDE6F5"),
        panel.background = element_rect(fill = "#F5F0FF"),
        panel.grid.major = element_line(linewidth = 0.25),
        panel.grid.minor = element_line(linewidth = 0.25),
        plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm")
  ) +
  facet_grid(.~model)

ggsave(paste0("./Figures/dist_to_isthm/comparative_distances_UNSUCCESSFUL.pdf"), 
       plot = comp_plot_unsuccessful, height = 80, width = 170, units = "mm")

ggsave(paste0("./Figures/dist_to_isthm/comparative_distances_UNSUCCESSFUL.png"), 
       plot = comp_plot_unsuccessful, height = 80, width = 170, dpi = 600, units = "mm")


################################################################################
############################### 4. Starting biome ##############################
################################################################################

# All simulations

P_tbl <- P_tbl %>% 
  mutate(start_biome = as.factor(start_biome))

start_biome_all <- P_tbl %>% 
  filter(!(is.na(start_biome))) %>%
  ggplot(aes(x = start_biome)) +
  geom_bar(aes(fill = start_biome), colour = "black", linewidth = 0.3) +
  geom_text(stat = "count", aes(label=after_stat(count)), size = 2, vjust = -0.5) +
  scale_fill_manual(values = c("1" = "#f9d14a", "2" = "#ab3329", "3" = "#ed968c", "4" = "#7c4b73", "5" = "#88a0dc")) +
  labs(x = "Ancestral biome", y = "Nb. simulations") +
  scale_x_discrete(labels = c("1" = "Tropical", "2" = "Arid", "3" = "Temperate", "4" = "Cold", "5" = "Polar")) +
  ylim(0, 315) +
  facet_grid(model~start_region) +
  theme(axis.text = element_text(size = 7),
        axis.title = element_text(size = 10),
        axis.line = element_line(linewidth = 0.3, color = "black"),
        legend.position = "none",
        strip.background = element_rect(fill = "#DDE6F5"),
        strip.text = element_text(size = 10),
        panel.background = element_rect(fill = "grey85"),
        panel.grid.major = element_line(linewidth = 0.25),
        panel.grid.minor = element_line(linewidth = 0.25),
        plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"))

ggsave(paste0("./Figures/starting_biome/starting_biome_ALL.pdf"), 
       plot = start_biome_all, height = 170, width = 170, units = "mm")

ggsave(paste0("./Figures/starting_biome/starting_biome_ALL.png"), 
       plot = start_biome_all, height = 170, width = 170, units = "mm", dpi = 600)

start_biome_success <- P_tbl %>% 
  # all initial conditions are the same as each simulation batch share the same seed
  filter(!(is.na(start_biome)) & exchanged == 1) %>%
  ggplot(aes(x = start_biome)) +
  geom_bar(aes(fill = start_biome), colour = "black", linewidth = 0.3) +
  geom_text(stat = "count", aes(label=after_stat(count)), size = 2, vjust = -0.5) +
  scale_fill_manual(values = c("1" = "#f9d14a", "2" = "#ab3329", "3" = "#ed968c", "4" = "#7c4b73", "5" = "#88a0dc")) +
  labs(x = "Ancestral biome", y = "Nb. successful simulations") +
  scale_x_discrete(labels = c("1" = "Tropical", "2" = "Arid", "3" = "Temperate", "4" = "Cold", "5" = "Polar")) +
  ylim(0, 280) +
  facet_grid(model~start_region) +
  theme(axis.text = element_text(size = 7),
        axis.title = element_text(size = 10),
        axis.line = element_line(linewidth = 0.3, color = "black"),
        legend.position = "none",
        strip.background = element_rect(fill = "#DDE6F5"),
        strip.text = element_text(size = 10),
        panel.background = element_rect(fill = "grey85"),
        panel.grid.major = element_line(linewidth = 0.25),
        panel.grid.minor = element_line(linewidth = 0.25),
        plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"))

ggsave(paste0("./Figures/starting_biome/starting_biome_EXCH.pdf"), 
       plot = start_biome_success, height = 170, width = 170, units = "mm")

ggsave(paste0("./Figures/starting_biome/starting_biome_EXCH.png"), 
       plot = start_biome_success, height = 170, width = 170, dpi = 600, units = "mm")

# PROPORTION OF REALISED EXCHANGE PER BIOME (e.g. n_trop_exch / n_trop_init)
  # Nb of starting species per biome
per_biome_df <- P_tbl %>% 
  filter(!(is.na(start_biome))) %>% 
  count(start_biome, model, start_region)
  # Same but only for exchanged species
per_biome_exch_df <- P_tbl %>% 
  filter(!(is.na(start_biome)) & exchanged == 1) %>% 
  count(start_biome, model, start_region)
    ## No cold species exchanged from NA in M0, from SA in M0 & M1; polar species from NA in M0 : add them
per_biome_exch_df <- per_biome_exch_df %>% 
  add_row(start_biome = "5", model = "M0", start_region = "North America", n = 0, .before = 30) %>% 
  add_row(start_biome = "4", model = "M1", start_region = "South America", n = 0, .before = 26) %>% 
  add_row(start_biome = "4", model = "M0", start_region = "South America", n = 0, .before = 25) %>% 
  add_row(start_biome = "4", model = "M0", start_region = "North America", n = 0, .before = 25)
  # Assess proportion
per_biome_exch_df <- per_biome_exch_df %>% 
  mutate(n_init = per_biome_df$n) %>% 
  mutate(prop_exch = n / per_biome_df$n)
  # Plot
prop_biome_success_plot <- per_biome_exch_df %>% 
  ggplot(aes(x = start_biome, y = prop_exch)) +
  geom_col(aes(fill = start_biome), linewidth = 0.3, colour = "black") +
  geom_text(aes(label = sapply(prop_exch, round, digits = 2)), inherit.aes = TRUE, nudge_y = 0.05, size = 2.5) +
  scale_fill_manual(values = c("1" = "#f9d14a", "2" = "#ab3329", "3" = "#ed968c", "4" = "#7c4b73", "5" = "#88a0dc")) +
  scale_x_discrete(labels = c("1" = "Tropical", "2" = "Arid", "3" = "Temperate", "4" = "Cold", "5" = "Polar")) +
  labs(x = "Ancestral biome", y = "Prop. success") +
  facet_grid(model~start_region) +
  theme(axis.text = element_text(size = 7),
        axis.title = element_text(size = 10),
        axis.line = element_line(linewidth = 0.3, color = "black"),
        legend.position = "none",
        strip.background = element_rect(fill = "#DDE6F5"),
        strip.text = element_text(size = 10),
        panel.background = element_rect(fill = "grey85"),
        panel.grid.major = element_line(linewidth = 0.25),
        panel.grid.minor = element_line(linewidth = 0.25),
        plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"))

ggsave(paste0("./Figures/starting_biome/prop_biome_success.pdf"), 
       plot = prop_biome_success_plot, height = 170, width = 170, units = "mm")

ggsave(paste0("./Figures/starting_biome/prop_biome_success.png"), 
       plot = prop_biome_success_plot, height = 170, width = 170, dpi = 600, units = "mm")
