[![Build Status](https://travis-ci.org/resplab/prism.svg?branch=master)](https://travis-ci.org/resplab/prism)
[![CRAN Status](https://www.r-pkg.org/badges/version/prism)](https://cran.r-project.org/web/packages/prism/index.html)
[![Project Status: Active – The project has reached a stable, usable state and is being actively developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)

# prism
Client package for the Programmable Interface for Simulation/Statistical Models (PRISM)

This package provides a user-friendly API interface in R for clinical prediction and decision analytics models hosted on UBC PRISM cloud.

### Example
```
library(prism)
connect_to_model('epicR')
get_default_input()
get_output_structure()
input <- get_default_input()
input
names(input)
View(input)
input$global_parameters$time_horizon
input$global_parameters$time_horizon <- 10
model_run(input = input)
```
