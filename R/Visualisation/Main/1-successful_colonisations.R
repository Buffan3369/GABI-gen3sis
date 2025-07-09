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


## WORKAROUND FOR DIY PANEL

################################################################################
# Author: Lucas Buffan
# E-mail: lucas.l.buffan@gmail.com
# Aim: Assess and represent the proportion of successful colonisations depending
#     on the ancestral area.
################################################################################

library(tidyverse)
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
    geom_col(lwd = 0.2, colour = "black", fill = c("#fb6a4a", "#66c2a4")) +
    scale_y_continuous(expand = c(0, 0), limits = c(0, 1.06)) +
    labs(x = NULL, y = NULL) +
    annotate(geom = "text", x = 1, y = na_success-0.03, label = round(na_success, digits = 2), size = 2.3, color = "white") +
    annotate(geom = "text", x = 2, y = sa_success-0.03, label = round(sa_success, digits = 2), size = 2.3, color = "white") +
    annotate(geom = "rect", ymin = 1.02, ymax = 1.06, xmin = -Inf, xmax = Inf, colour = "red") +
    ggtitle(mdl)
  if(i == 1){
    prop_plot <- prop_plot + 
      theme(axis.title = element_text(size = 7.5),
            axis.text = element_text(size = 6),
            axis.line = element_line(linewidth = 0.3, color = "black"),
            plot.title = element_text(hjust = 0.5),
            panel.background = element_blank(),
            plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"))
  }
  else{
    prop_plot <- prop_plot + 
      theme(axis.text.x = element_text(size = 6),
            axis.line.x = element_line(linewidth = 0.3, color = "black"),
            axis.line.y = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            plot.title = element_text(hjust = 0.5),
            panel.background = element_blank(),
            plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"))
  }
  
  plotlist[[i]] <- prop_plot
}

p <- ggarrange(plotlist = plotlist, ncol = 4, label.x = "Region of orgin", label.y = "Prop. successful exchange")

p

annotate_figure(p, left = textGrob("Prop. successful exchange", rot = 90, vjust = 1, gp = gpar(cex = 1.3)),
                bottom = textGrob("Region of orgin", gp = gpar(cex = 1.3)))

