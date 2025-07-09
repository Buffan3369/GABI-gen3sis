################################################################################
# Name: 3-Area_Div_Dist_panels.R
# Author: Lucas Buffan
# E-mail: lucas.l.buffan@gmail.com
# Goal: Violin plots for 
#         1. Proportion of colonised area
#         2. Diversity in the colonised area
#         3. Distance between simulation starting point and isthmus
################################################################################

library(tidyverse)

## Assemble large dataset ------------------------------------------------------
P_tbl <- read.table(paste0("./Data/Gen3sis_parameter_tables/M0/North_America_parameters_EXTENDED_EXCH_AREA_DIV_DIST.txt"),
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
                                   "/", start, "_America_parameters_EXTENDED_EXCH_AREA_DIV_DIST.txt"),
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
  labs(x = "Ancestral regtion", y = "Prop. area colonised") +
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
  labs(x = "Ancestral regtion", y = "Log(Diversity in colonised region)") +
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
        panel.background = element_blank(),
        plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"))

ggsave(paste0("./Figures/dist_to_isthm/distance_violin_panel_CRASH.pdf"), 
       plot = plot_crash, height = 120, width = 200, units = "mm")

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
        panel.background = element_blank(),
        plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"))

ggsave(paste0("./Figures/dist_to_isthm/distance_violin_panel_NoCRASH.pdf"), 
       plot = plot_NoCrash, height = 120, width = 170, units = "mm")

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

