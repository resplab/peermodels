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
