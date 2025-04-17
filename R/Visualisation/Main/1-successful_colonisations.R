################################################################################
# Author: Lucas Buffan
# E-mail: lucas.l.buffan@gmail.com
# Aim: Assess and represent the proportion of successful colonisations depending
#     on the ancestral area.
################################################################################

library(tidyverse)

for(mdl in c("M0", "M1")){
  NA_recap_tbl <- read.table(paste0("./Data/Gen3sis_parameter_tables/", mdl, "/North_America_parameters_EXTENDED_EXCH.txt"),
                             header = T, sep = "\t")
  # # Slight adjustments to fix a mistake
  # NA_recap_tbl <- NA_recap_tbl[9:nrow(NA_recap_tbl),]
  # NA_recap_tbl$exchanged <- sapply(X = NA_recap_tbl$exchanged, FUN = as.numeric)
  
  SA_recap_tbl <- read.table(paste0("./Data/Gen3sis_parameter_tables/", mdl, "/South_America_parameters_EXTENDED_EXCH.txt"),
                             header = T, sep = "\t")
  
  ## Filter out simulations that crashed -----------------------------------------
  cat("Number of simulations that crashed for ", mdl, " with North American ancestor: ", length(which(NA_recap_tbl$exchanged == -1)), "\n")
  NA_recap_tbl <- NA_recap_tbl %>% filter(!(exchanged == -1))
  cat("Number of simulations that crashed for ", mdl, " with South American ancestor: ", length(which(SA_recap_tbl$exchanged == -1)), "\n")
  SA_recap_tbl <- SA_recap_tbl %>% filter(!(exchanged == -1))
  
  ## Compute success proportions -------------------------------------------------
  na_success <- sum(NA_recap_tbl$exchanged) / nrow(NA_recap_tbl)
  sa_success <- sum(SA_recap_tbl$exchanged) / nrow(SA_recap_tbl)
  
  ## Plot ------------------------------------------------------------------------
  plot_df <- data.frame(Ori = c(paste0("North America \n(", nrow(NA_recap_tbl), " simulations)"), 
                                paste0("South America \n(", nrow(SA_recap_tbl), " simulations)")),
                        Prop_success = c(na_success, sa_success))
  
  prop_plot <- plot_df %>% ggplot(aes(x = Ori, y = Prop_success)) +
    geom_col(lwd = 0.2, colour = "black", fill = c("#fb6a4a", "#66c2a4")) +
    geom_hline(yintercept = 0.5, linetype = "dashed", color = "red") +
    scale_y_continuous(expand = c(0, 0), limits = c(0, 1.02)) +
    labs(x = "Region of orgin", y = "Prop. successful exchange") +
    annotate(geom = "text", x = 1, y = na_success-0.03, label = round(na_success, digits = 2), size = 2.3, color = "white") +
    annotate(geom = "text", x = 2, y = sa_success-0.03, label = round(sa_success, digits = 2), size = 2.3, color = "white") +
    theme(axis.title = element_text(size = 7.5),
          axis.text = element_text(size = 6),
          axis.line = element_line(linewidth = 0.3, color = "black"),
          panel.background = element_blank(),
          plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"))
  
  ggsave(paste0("./Figures/prop_successful_exch/", mdl, "_NoCrash.pdf"), plot = prop_plot, height = 70, width = 60, units = "mm")
  
}
