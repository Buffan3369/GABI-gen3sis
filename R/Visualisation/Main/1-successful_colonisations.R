################################################################################
# Author: Lucas Buffan
# E-mail: lucas.l.buffan@gmail.com
# Aim: Assess and represent the proportion of successful colonisations depending
#     on the ancestral area.
################################################################################

library(tidyverse)

#### M1 equal area ####
NA_recap_tbl <- read.table("./Data/Gen3sis_parameter_tables/M1_equal_area/North_America_parameters_EXTENDED_TABLE_EXCH.txt",
                           header = T, sep = "\t")
# Slight adjustments to fix a mistake
NA_recap_tbl <- NA_recap_tbl[9:nrow(NA_recap_tbl),]
NA_recap_tbl$exchanged <- sapply(X = NA_recap_tbl$exchanged, FUN = as.numeric)

SA_recap_tbl <- read.table("./Data/Gen3sis_parameter_tables/M1_equal_area/South_America_parameters_EXTENDED_TABLE_EXCH.txt",
                           header = T, sep = "\t")

## Filter out simulations that crashed -----------------------------------------
NA_recap_tbl <- NA_recap_tbl %>% filter(!(exchanged == -1))
SA_recap_tbl <- SA_recap_tbl %>% filter(!(exchanged == -1))

## Compute success proportions -------------------------------------------------
na_success <- sum(NA_recap_tbl$exchanged) / nrow(NA_recap_tbl)
sa_success <- sum(SA_recap_tbl$exchanged) / nrow(SA_recap_tbl)

## Plot ------------------------------------------------------------------------
plot_df <- data.frame(Ori = c("North America", "South America"),
                      Prop_success = c(na_success, sa_success))

prop_plot <- plot_df %>% ggplot(aes(x = Ori, y = Prop_success)) +
  geom_col(lwd = 0.2, colour = "black", fill = c("#fb6a4a", "#66c2a4")) +
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "red") +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1.02)) +
  labs(x = "Region of orgin", y = "Prop. successful exchange") +
  theme(axis.title = element_text(size = 7.5),
        axis.text = element_text(size = 6),
        axis.line = element_line(linewidth = 0.3, color = "black"),
        panel.background = element_blank())

ggsave("./Figures/prop_successful_exch/M1_equal_area_NoCrash.pdf", plot = prop_plot, height = 70, width = 50, units = "mm")
