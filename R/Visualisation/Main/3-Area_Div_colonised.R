library(tidyverse)

for(model in c("M0", "M1", "M2", "M3")){
  ptbl_N <- read.table(paste0("./Data/Gen3sis_parameter_tables/", model, "/North_America_parameters_EXTENDED_EXCH_AREA_DIV.txt"),
                       header = T)
  ptbl_S <- read.table(paste0("./Data/Gen3sis_parameter_tables/", model, "/South_America_parameters_EXTENDED_EXCH_AREA_DIV.txt"),
                       header = T)
  
  ## Proportion of colonised area plot -----------------------------------------
    # Filter out unsuccessful colonisations
  ptbl_N_ar <- ptbl_N %>% filter(prop_col_area != -1)
  ptbl_S_ar <- ptbl_S %>% filter(prop_col_area != -1)
  
  plot_area_ds <- data.frame(area = c(ptbl_N_ar$prop_col_area, 
                                      ptbl_S_ar$prop_col_area),
                             start_reg = c(rep("North", nrow(ptbl_N_ar)),
                                           rep("South", nrow(ptbl_S_ar))))
  
  prop_plot <- plot_area_ds %>% 
    ggplot(aes(x = start_reg, y = area)) +
    geom_violin(adjust = .75, draw_quantiles = c(0.5), scale = "width", aes(fill = factor(start_reg))) +
    geom_point(size = 0.05) +
    scale_y_continuous(limits = c(0,1)) +
    scale_fill_manual(values = c("#fb6a4a", "#66c2a4")) +
    labs(x = "Ancestral regtion", y = "Prop. area colonised") +
    ggtitle(label = model) +
    theme(legend.position = "none",
          plot.title = element_text(hjust = 0.5),
          axis.title = element_text(size = 7.5),
          axis.text = element_text(size = 6),
          axis.line = element_line(linewidth = 0.3, color = "black"),
          panel.background = element_blank(),
          plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm")
    )
  ggsave(paste0("./Figures/prop_col_area/", model, "_prop_col_area.pdf"), 
         plot = prop_plot, height = 70, width = 60, units = "mm")  
  
  ## Diversity in the colonised area plot --------------------------------------
  # Filter out unsuccessful colonisations
  ptbl_N_div <- ptbl_N %>% filter(div_col != -1)
  ptbl_S_div <- ptbl_S %>% filter(div_col != -1)
  
  plot_div_ds <- data.frame(div = c(ptbl_N_div$div_col, 
                                    ptbl_S_div$div_col),
                             start_reg = c(rep("North", nrow(ptbl_N_div)),
                                           rep("South", nrow(ptbl_S_div))))
  # Log-transform
  plot_div_ds <- plot_div_ds %>% mutate(logdiv = sapply(X = div, FUN = log10))
  
  div_plot <- plot_div_ds %>% 
    ggplot(aes(x = start_reg, y = logdiv)) +
    geom_violin(adjust = .75, draw_quantiles = c(0.5), scale = "width", aes(fill = factor(start_reg))) +
    geom_point(size = 0.05) +
    scale_fill_manual(values = c("#fb6a4a", "#66c2a4")) +
    labs(x = "Ancestral regtion", y = "Log(Diversity in colonised region)") +
    ggtitle(label = model) +
    theme(legend.position = "none",
          plot.title = element_text(hjust = 0.5),
          axis.title = element_text(size = 7.5),
          axis.text = element_text(size = 6),
          axis.line = element_line(linewidth = 0.3, color = "black"),
          panel.background = element_blank(),
          plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm")
    )
  ggsave(paste0("./Figures/div_col_area/", model, "_div_col_area.pdf"), 
         plot = div_plot, height = 70, width = 60, units = "mm")  
}

