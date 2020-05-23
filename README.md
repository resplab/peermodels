<!-- badges: start -->
[![R build status](https://github.com/resplab/prism/workflows/R-CMD-check/badge.svg)](https://github.com/resplab/prism/actions)
<!-- badges: end -->
[![Project Status: Active – The project has reached a stable, usable state and is being actively developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)

# prism
Client package for the Programmable Interface for Simulation/Statistical Models (PRISM)

This package provides a user-friendly API interface in R for clinical prediction and decision analytics models hosted on UBC PRISM cloud.

### Example
```
library(prism)
connect_to_model("epicPrism",api_key = "123456")
model_run()  #Run with default inputs
model_input<-get_default_input()  
model_input
names(model_input)
View(model_input)
model_input$global_parameters$time_horizon
model_input$global_parameters$time_horizon <- 10
model_run(input = model_input)  #Run with modified input
```
