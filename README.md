<!-- badges: start -->
[![R build status](https://github.com/resplab/peermodels/workflows/R-CMD-check/badge.svg)](https://github.com/resplab/peermodels/actions)
[![CRAN status](https://www.r-pkg.org/badges/version/peermodels)](https://CRAN.R-project.org/package=peermodels)
<!-- badges: end -->[![Project Status: Active – The project has reached a stable, usable state and is being actively developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)

# peermodels
Client package for the Programmable Interface for Simulation/Statistical Models (PRISM)

This package provides a user-friendly API interface in R for clinical prediction and decision analytics models hosted on UBC PRISM cloud.

### Example
```
library(peermodels)
connect_to_model("accept", api_key = YOUR_API_KEY)
input <- get_default_input()
results <- model_run(input)
```

