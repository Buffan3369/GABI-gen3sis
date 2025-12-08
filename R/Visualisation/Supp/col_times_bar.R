library(tidyverse)
library(deeptime)

# Twickling GTS
epochs <- deeptime::epochs[1:3,]
epochs <- epochs %>% mutate(min_age = min_age*100, max_age = max_age*100)

# Loop across models and initial continents
for(mdl in c("M0", "M1")){
  for(start in c("North", "South")){
    if(mdl == "M0" & start == "North"){
      time_df <- readRDS(paste0("./Results/col_times/", mdl, "_", start, "Col_times_df.rds"))
      time_df$model <- mdl
      time_df$start <- paste0(start, " America")
    }
    else{
      tmp <- readRDS(paste0("./Results/col_times/", mdl, "_", start, "Col_times_df.rds"))
      tmp$model <- mdl
      tmp$start <- paste0(start, " America")
      time_df <- rbind.data.frame(time_df, tmp)
    }
  }
}
rownames(time_df) <- 1:nrow(time_df)

# Plot
time_hist <- time_df %>% 
  ggplot(aes(x = time)) +
  scale_x_reverse(breaks = seq(500,0,-100), labels = seq(5,0,-1)) +
  geom_histogram(binwidth = 10, aes(y = after_stat(density)), fill = "#FAFFF0", colour = "black", alpha = 0.5) +
#  geom_line(aes(y = Temperature), linewidth = 1) +
#  geom_density(bw = 10, linewidth = 1) +
  labs(x = "Time (Ma)", y = "Exchange density") +
#  scale_y_continuous(sec.axis = sec_axis(~ . * 100)) +
#  ylim(c(0, 0.011)) +
  facet_wrap(model~start, scales = "free_y") +
  theme(axis.title = element_text(size = 9),
        axis.text = element_text(size = 7),
        axis.line = element_line(linewidth = 0.3, color = "black"),
        legend.position = "none",
        panel.background = element_rect(fill = "#F5F0FF", colour = "black"),
        panel.grid.major = element_line(linewidth = 0.1, color = "grey50"),
        panel.grid.minor = element_line(linewidth = 0.1, color = "grey50"),
        plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"),
        strip.background = element_rect(fill = "#DDE6F5")) +
  coord_geo(dat = epochs, abbrv = F, height = unit(1, "line"), size = 2.5, center_end_labels = T)

ggsave("./Figures/MS/Supp/colonisation_timing/time_hist.pdf", plot = time_hist,
       height = 150, width = 200, units = "mm")

# Temperature only

av_clim <- readRDS("./Data/PALEO_PGEM-bioclim/average_clim_both_continents.RDS")
temp <- av_clim %>%
  filter(var == "Temperature")

temp1 <- temp
temp1$model <- "M1"
temp$model <- "M0"
temp <- rbind.data.frame(temp, temp1)
temp <- temp %>% 
  filter(time %in% seq(0, 5000, 10)) %>% 
  mutate(time = time / 1000)

temp_panel <- temp %>% 
  ggplot(aes(x = time)) +
  scale_x_reverse() +
  scale_y_continuous(position = "right") +
  geom_line(aes(y = clim_av), linewidth = 0.25, colour = "#993404") +
  labs(x = "Time (Ma)", y = "Temperature (°C)") +
  facet_wrap(model~continent, scales = "free_y") +
  theme(axis.title.x = element_text(size = 9),
        axis.title.y = element_text(size = 9, colour = "#993404"),
        axis.text.x = element_text(size = 7),
        axis.text.y = element_text(size = 7, colour = "#993404"),
        axis.line.x = element_line(linewidth = 0.3, color = "black"),
        axis.line.y = element_line(linewidth = 0.3, color = "#993404"),
        axis.ticks.y = element_line(colour = "#993404"), 
        legend.position = "none",
        panel.background = element_rect(fill = "#F5F0FF", colour = "black"),
        panel.grid.major = element_line(linewidth = 0.1, color = "grey50"),
        panel.grid.minor = element_line(linewidth = 0.1, color = "grey50"),
        plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"),
        strip.background = element_rect(fill = "#DDE6F5")) +
  coord_geo(dat = "epochs", abbrv = F, height = unit(1, "line"), size = 2.5, center_end_labels = T)

ggsave("./Figures/MS/Supp/colonisation_timing/temperature_panel.pdf", 
       plot = temp_panel, height = 150, width = 200, units = "mm")
