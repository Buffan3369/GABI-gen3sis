################################################################################
# Author: Lucas Buffan
# E-mail: lucas.l.buffan@gmail.com
# Aim: Assess and represent the proportion of successful colonisations depending
#     on the ancestral area.
################################################################################

library(tidyverse)

for(mdl in c("M0", "M0_loco", "M1", "M2", "M3")){
  NA_recap_tbl <- read.table(paste0("./Data/Gen3sis_parameter_tables/", mdl, "/North_America_parameters_EXTENDED_EXCH.txt"),
                             header = T, sep = "\t")

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
    scale_y_continuous(expand = c(0, 0), limits = c(0, 1.02)) +
    labs(x = "Region of orgin", y = "Prop. successful exchange") +
    annotate(geom = "text", x = 1, y = na_success-0.03, label = round(na_success, digits = 2), size = 2.3, color = "white") +
    annotate(geom = "text", x = 2, y = sa_success-0.03, label = round(sa_success, digits = 2), size = 2.3, color = "white") +
    ggtitle(mdl) +
    theme(axis.title = element_text(size = 7.5),
          axis.text = element_text(size = 6),
          axis.line = element_line(linewidth = 0.3, color = "black"),
          plot.title = element_text(hjust = 0.5),
          panel.background = element_blank(),
          plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"))
  
  ggsave(paste0("./Figures/prop_successful_exch/", mdl, "_NoCrash.pdf"), plot = prop_plot, height = 70, width = 60, units = "mm")
  
}


################################################################################
# DIY Panel
################################################################################

library(ggpubr)
library(grid)

plotlist <- list()
models <- c("M0", "M1", "M2", "M3")

for(i in 1:4){
  mdl <- models[i]
  NA_recap_tbl <- read.table(paste0("./Data/Gen3sis_parameter_tables/", mdl, "/North_America_parameters_EXTENDED_EXCH.txt"),
                             header = T, sep = "\t")
  
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
  
  if(i > 2){
    ylab <- NULL
  }
  
  prop_plot <- plot_df %>% ggplot(aes(x = Ori, y = Prop_success)) +
    geom_col(lwd = 0.2, colour = "black", fill = c("#fcbba1", "#ccece6")) +
    scale_y_continuous(expand = c(0, 0), limits = c(0, 1.12), breaks = c(0, 0.25, 0.5, 0.75, 1)) +
    labs(x = NULL, y = NULL) +
    annotate(geom = "text", x = 1, y = na_success-0.03, label = round(na_success, digits = 2), size = 1.85, color = "black") +
    annotate(geom = "text", x = 2, y = sa_success-0.03, label = round(sa_success, digits = 2), size = 1.85, color = "black") +
    annotate(geom = "rect", ymin = 1.02, ymax = 1.12, xmin = -Inf, xmax = Inf, fill = "#DDE6F5") +
    annotate(geom = "text", y = 1.07, x = 1.5, label = mdl, size = 3, color = "black")
  if(i == 1){
    prop_plot <- prop_plot + 
      theme(axis.text = element_text(size = 4),
            axis.line = element_line(linewidth = 0.3, color = "black"),
            panel.background = element_rect(fill = "grey85"),
            panel.grid.major = element_line(linewidth = 0.25),
            panel.grid.minor = element_line(linewidth = 0.25),
            plot.margin = unit(c(1, 0.05, 0, 0.2), "cm")) # c(top, right, bottom, left)
  }
  else{
    prop_plot <- prop_plot + 
      theme(axis.text.x = element_text(size = 4.5),
            axis.line.x = element_line(linewidth = 0.3, color = "black"),
            axis.line.y = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            panel.background = element_rect(fill = "grey85"),
            panel.grid.major = element_line(linewidth = 0.25),
            panel.grid.minor = element_line(linewidth = 0.25),
            plot.margin = unit(c(1, 0.075, 0, 0.075), "cm"))
  }
  
  plotlist[[i]] <- prop_plot
}

p <- ggarrange(plotlist = plotlist, ncol = 4, widths = c(1, 0.95, 0.95, 0.95))

# Add x and y labels
p <- annotate_figure(p, 
                     left = text_grob("Prop. successful exchange", rot = 90, vjust = 1, size = 8),
                     bottom = text_grob("Ancestral region", size = 8))

# Save
ggsave("./Figures/prop_successful_exch/succ_exch_panel.pdf",
       plot = p, height = 70, width = 150, units = "mm")

ggsave("./Figures/prop_successful_exch/succ_exch_panel.png",
       plot = p, dpi = 600, height = 70, width = 150, units = "mm")
