################################################################################
# Name: Biome_Counter.R
# Author: Lucas Buffan
# Contact: lucas.l.buffan@gmail.com
# Aim: Counting starting biomes for all simulations and successful ones, 
#      including those with standardised distances to isthmus 
################################################################################


source("./R/PostProcessing/4-Extract_distribs_with_moments.R")


## All simulations with  Standardised distances to isthmus ---------------------
P_tbl_all_std <- summarise_distrib(metric = "start_biome",
                                   what = "all",
                                   dist_std = T)
P_tbl_all_std <- P_tbl_all_std %>% 
  filter(start == "North" & !(is.na(start_biome))) %>% # South is just the same thing as above
  mutate(start = sapply(X = start, FUN = function(x){paste0(x, " America (standardised)")}))

start_biome_all <- P_tbl_all_std %>% 
  mutate(start_biome = as.character(start_biome)) %>% 
  ggplot(aes(x = start_biome)) +
  geom_bar(aes(fill = start_biome), colour = "black", linewidth = 0.3) +
  geom_text(stat = "count", aes(label=after_stat(count)), size = 2, vjust = -0.5) +
  scale_fill_manual(values = c("1" = "#f9d14a", "2" = "#ab3329", "3" = "#ed968c", "4" = "#7c4b73", "5" = "#88a0dc")) +
  labs(x = "Ancestral biome", y = "Nb. simulations") +
  scale_x_discrete(labels = c("1" = "Tropical", "2" = "Arid", "3" = "Temperate", "4" = "Cold", "5" = "Polar")) +
  facet_grid(.~model) +
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

ggsave(paste0("./Figures/MS/Supp/biomes/starting_biome_STD.png"),
       plot = start_biome_all, height = 75, width = 200, units = "mm", dpi = 600)


## Only successful simulations -------------------------------------------------

P_tbl_succ <- summarise_distrib(metric = "start_biome",
                               what = "exchanged")
P_tbl_succ <- P_tbl_succ %>% 
  mutate(start = sapply(X = start, FUN = function(x){paste0(x, " America")})) %>%
  filter(!is.na(start_biome))

# Standardised distances to isthmus
P_tbl_succ_std <- summarise_distrib(metric = "start_biome",
                                   what = "exchanged",
                                   dist_std = T)
P_tbl_succ_std <- P_tbl_succ_std %>% 
  filter(start == "North" & !(is.na(start_biome))) %>% # South is just the same thing as above
  mutate(start = sapply(X = start, FUN = function(x){paste0(x, " America (standardised)")}))

# Merge
P_tbl_win <- rbind.data.frame(P_tbl_succ, P_tbl_succ_std)

start_biome_success <- P_tbl_win %>%
  mutate(start_biome = as.character(start_biome)) %>%
  ggplot(aes(x = start_biome)) +
  geom_bar(aes(fill = start_biome), colour = "black", linewidth = 0.3) +
  geom_text(stat = "count", aes(label=after_stat(count)), size = 2, vjust = -0.5) +
  scale_fill_manual(values = c("1" = "#f9d14a", "2" = "#ab3329", "3" = "#ed968c", "4" = "#7c4b73", "5" = "#88a0dc")) +
  labs(x = "Ancestral biome", y = "Nb. successful simulations") +
  scale_x_discrete(labels = c("1" = "Tropical", "2" = "Arid", "3" = "Temperate", "4" = "Cold", "5" = "Polar")) +
  ylim(0, 280) +
  facet_grid(model~start) +
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

# ggsave(paste0("./Figures/starting_biome/starting_biome_EXCH.pdf"), 
#        plot = start_biome_success, height = 170, width = 170, units = "mm")
# 
# ggsave(paste0("./Figures/starting_biome/starting_biome_EXCH.png"), 
#        plot = start_biome_success, height = 170, width = 170, dpi = 600, units = "mm")

ggsave("./Figures/MS/Supp/biomes/starting_biome_EXCH.png", 
       plot = start_biome_success, height = 200, width = 200, dpi = 600, units = "mm")


## Proportion of successful exchanges for standardised distances to isthmus ----

P_tbl_all_count <- P_tbl %>% 
  mutate(start_biome = as.character(start_biome)) %>% 
  count(start_biome, model, start)

P_tbl_win_count <- P_tbl_win %>% 
  mutate(start_biome = as.character(start_biome)) %>%
  count(start_biome, model, start)

# Adding missing ones
P_tbl_win_count <- P_tbl_win_count %>%
  add_row(start_biome = "5", model = "M2", start = "North America (standardised)", n = 0, .before = 53) %>% 
  add_row(start_biome = "5", model = "M0", start = "North America (standardised)", n = 0, .before = 48) %>% 
  add_row(start_biome = "5", model = "M0", start = "North America", n = 0, .before = 48) %>% 
  add_row(start_biome = "4", model = "M0", start = "South America", n = 0, .before = 39)

# Compute proportion
P_tbl_win_count <- P_tbl_win_count %>% 
  mutate(n_init = P_tbl_all_count$n) %>% 
  mutate(prop_exch = n / P_tbl_all_count$n)

# Compute binomial 95% confidence interval
P_tbl_win_count <- P_tbl_win_count %>% 
  mutate(lower_ci = sapply(X = 1:nrow(P_tbl_win_count),
                           FUN = function(i){
                             p <- P_tbl_win_count$prop_exch[i]
                             N <- P_tbl_win_count$n_init[i]
                             lwr <- bino_CI(prop = p, n = N, what = "Lower")
                             return(lwr)
                           }),
         upper_ci = sapply(X = 1:nrow(P_tbl_win_count),
                           FUN = function(i){
                             p <- P_tbl_win_count$prop_exch[i]
                             N <- P_tbl_win_count$n_init[i]
                             upr <- bino_CI(prop = p, n = N, what = "Upper")
                             if(upr > 1){
                               upr <- 1
                             }
                             return(upr)
                           }))

# Plot
prop_biome_success_plot_std <- P_tbl_win_count %>% 
  mutate(start_biome = as.character(start_biome)) %>%
  # Only keep simulations from North America with standardised distances to isthmus
  filter(start == "North America (standardised)") %>% 
  ggplot(aes(x = start_biome, y = prop_exch)) +
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.5) +
  geom_point(aes(fill = start_biome), colour = "black", pch = 23, size = 2.5) +
  scale_fill_manual(values = c("#f9d14a","#ab3329", "#ed968c", "#7c4b73", "#88a0dc")) +
  scale_x_discrete(labels = c("1" = "Tropical", "2" = "Arid", "3" = "Temperate", "4" = "Cold", "5" = "Polar")) +
  labs(x = "Ancestral biome", y = "Prop. success") +
  facet_grid(.~model) +
  theme(axis.text = element_text(size = 5.5),
        axis.title = element_text(size = 10),
        axis.line = element_line(linewidth = 0.3, color = "black"),
        legend.position = "none",
        strip.background = element_rect(fill = "#DDE6F5"),
        strip.text = element_text(size = 10),
        panel.background = element_rect(fill = "grey85"),
        panel.grid.major = element_line(linewidth = 0.25),
        panel.grid.minor = element_line(linewidth = 0.25),
        plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"))


ggsave(paste0("./Figures/MS/Supp/biomes/starting_biome_EXCH_PROP_STD.png"), 
       plot = prop_biome_success_plot_std, height = 75, width = 200, dpi = 600, units = "mm")

