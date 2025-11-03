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
      time_df <- rbind.data.frame(tmp, time_df)
    }
  }
}
rownames(time_df) <- 1:nrow(time_df)
# Temperature data
av_clim <- readRDS("./Data/PALEO_PGEM-bioclim/average_clim_both_continents.RDS")
temp <- av_clim %>%
  filter(var == "Temperature")

# Create the temperature column
choose_temp <- function(i){
  t <- time_df$time[i]*10
  cont <- strsplit(time_df$start[i], split = " ")[[1]][1]
  corr_T <- temp$clim_av[which(temp$time == t & temp$continent == cont)]
  return(corr_T)
}

Temperature <- sapply(X = 1:nrow(time_df), FUN = choose_temp)
time_df$Temperature <- as.numeric(Temperature)

M <- mean(time_df$Temperature)
SD <- sd(time_df$Temperature)
time_df$std_temperature <- sapply(time_df$Temperature,
                                  FUN = function(x){
                                    return((x-M)/SD)
                                  })

# Plot
time_df %>% 
  ggplot(aes(x = time)) +
  scale_x_reverse(breaks = seq(500,0,-100), labels = seq(5,0,-1)) +
#  geom_histogram(binwidth = 10, aes(y = after_stat(density)), fill = "#FAFFF0", colour = "black") +
  geom_line(aes(y = Temperature), linewidth = 1) +
#  geom_density(bw = 10, linewidth = 1) +
  labs(x = "Time (Ma)") +
#  scale_y_continuous(sec.axis = sec_axis(~ . * 100)) +
#  ylim(c(0, 0.011)) +
  facet_grid(model~start) +
  theme(axis.title = element_text(size = 7.5),
        axis.text = element_text(size = 5),
        axis.line = element_line(linewidth = 0.3, color = "black"),
        legend.position = "none",
        panel.background = element_rect(fill = "#F5F0FF", colour = "black"),
        panel.grid.major = element_line(linewidth = 0.1, color = "grey50"),
        panel.grid.minor = element_line(linewidth = 0.1, color = "grey50"),
        plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"),
        strip.background = element_rect(fill = "#DDE6F5")) +
  coord_geo(dat = epochs, abbrv = F, height = unit(1, "line"))
  

