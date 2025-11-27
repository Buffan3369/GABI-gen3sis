library(tidyverse)

for(model in c("M0", "M1", "M2", "M3")){
  for(start in c("North", "South")){
    df <- readRDS(paste0("./Data/Relative_biome_areas/", model, "_", start, "America_start_rel_area.RDS"))
    df_wide <- df[,2:ncol(df)] %>% 
      pivot_longer(cols = colnames(df[,2:ncol(df)]),
                   names_to = "Final_biome",
                   values_to = "Occ_area")
    df_wide$model <- model
    df_wide$start <- paste0(start, " America")
    if(model == "M0" & start == "North"){
      plot_df <- df_wide
    }
    else{
      plot_df <- rbind.data.frame(plot_df, df_wide)
    }
  }
}
plot_df$Final_biome <- factor(plot_df$Final_biome, levels = c("Tropical", "Arid", "Temperate", "Cold", "Polar"))

occ_per_biome <- plot_df %>% 
  ggplot(aes(x = Final_biome, y = Occ_area)) +
  geom_boxplot(aes(fill = Final_biome), outliers = F) +
  geom_point(size = 0.1, alpha = 0.6) +
  scale_fill_manual(values = c("Tropical" = "#f9d14a", "Arid" = "#ab3329", "Temperate" = "#ed968c", "Cold" = "#7c4b73", "Polar" = "#88a0dc")) +
  facet_grid(model~start) +
  labs(x = "Biome", y = "Proportion of the final colonised area") +
  theme(axis.text = element_text(size = 6),
        axis.title = element_text(size = 10),
        axis.line = element_line(linewidth = 0.3, color = "black"),
        legend.position = "none",
        strip.background = element_rect(fill = "#DDE6F5"),
        strip.text = element_text(size = 10),
        panel.background = element_rect(fill = "grey85"),
        panel.grid.major = element_line(linewidth = 0.25),
        panel.grid.minor = element_line(linewidth = 0.25),
        plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"))

ggsave("./Figures/MS/Supp/biomes/final_area_occupied_per_biome.png", plot = occ_per_biome,
       height = 200, width = 200, dpi = 600, units = "mm")  
