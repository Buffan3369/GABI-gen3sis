# Code associated with Buffan et al. *Palaeoclimate alone fails to explain the asymmetry of the Great American Biotic Interchange*

This repository contains the data and scripts associated with the article entitled *Palaeoclimate alone fails to explain the asymmetry of the Great American Biotic Interchange*.
This study uses the eco-evolutionary simulator [gen3sis](https://github.com/project-gen3sis/R-package) to explore the contribution of palaeoclimate to the asymmetry behind the Great American Biotic Interchange.
Below is a brief outlook of what you may find within this repository:

* `./Data/Gen3sis_parameter_tables/` contains summary data extracted from gen3sis experiments generated under our four models ($M0$, $M1$, $M2$, $M3$), either starting from North or South America
* `./R/PreProcessing/` stores all the scripts used to process palaeoclimate data and construct our simulation landscape based on them, and to generate our simulation config files
* `./R/PostProcessing/` contains all the scripts used to extract parameters of interest from simulation outputs. You may only need to execute the masterscript `2-PostProcessing_MASTER.R` masterscript to execute everything
* `./R/Visualisation/` contains all the scripts used to make the figures (further stored in `./Figures/`)

Additional data (simulation landscape and config files) are availalable here: <https://doi.org/10.6084/m9.figshare.31160704>.

For additional information, feel free to contact: lucas.l.buffan\@gmail.com