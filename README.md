<!-- badges: start -->
[![R build status](https://github.com/resplab/prism/workflows/R-CMD-check/badge.svg)](https://github.com/resplab/prism/actions)
<!-- badges: end -->
[![CRAN status](https://www.r-pkg.org/badges/version/peermodels)](https://CRAN.R-project.org/package=peermodels)
[![Project Status: WIP – Initial development is in progress, but there has not yet been a stable, usable release suitable for the public.](https://www.repostatus.org/badges/latest/wip.svg)](https://www.repostatus.org/#wip)


# peermodels
The `peermodels` package allows easy access to models hosted on the [Peer Models Network (PMN)](https://www.peermodelsnetwork.com). The Peer Models Network is an initiative to make statistical models that inform healthcare decisions available online, along with educational videos and other resources to help understand how they work. Learn more about PMN by visiting [www.peermodelsnetwork.com](https://www.peermodelsnetwork.com) and the PMN Model Repository page at [models.peermodelsnetwork.com](https://models.peermodelsnetwork.com).

This package allows users to directly access models on the cloud and interact with them without worrying about setting up the required software environment and accessing high-performance computing power.


### Installing the package

```
install.packages('remotes')
remotes::install_github('resplab/peermodels')
```

### Workflow

The `peermodels` package allows you to connect to a model hosted on PMN cloud, fetch its input parameters, change them as needed, and submit queries to the model. You can find a list of currently available models at the [PMN Model Repository page](https://models.peermodelsnetwork.com). To access a model, you would need to request an API Key. Please [contact us](https://www.peermodelsnetwork.com/join) to request an API key.

 The `connect_to_model` function, checks access to the model. `get_default_input()` returns the default input of the model, which allows the user to become familiarized with the structure of the input. The parameters can then be changed as required and passed to the `model_run()` function, which runs the model on the PMN cloud and returns the response. If the model is producing graphical output, the `draw_plots()` function can be used to fetch the produced graphics from the PMN cloud.
 
The code snippet below provides an example of a how to call a prediction model, in this case [The Acute COPD Exacerbation Tool (ACCEPT)](https://www.thelancet.com/journals/lanres/article/PIIS2213-2600%2819%2930397-2/fulltext):

```
> library(peermodels)
> input <- get_default_input("accept", api_key = "[YOUR_API_KEY]")
Selected model is accept
Calling server at https://prism.peermodelsnetwork.com/route/accept/run
> 
> input
     ID male age smoker oxygen statin LAMA LABA ICS FEV1 BMI SGRQ LastYrExacCount LastYrSevExacCount randomized_azithromycin randomized_statin
1 10001    1  70      1      1      1    1    1   1   33  25   50               2                  1                       0                 0
  randomized_LAMA randomized_LABA randomized_ICS random_sampling_N random_distribution_iteration calculate_CIs
1               0               0              0              1000                         20000          TRUE
```
The input shows that the default COPD patient is a 70 years old male who is a current smoker, has received oxygen therapy in the past year, is currently on LAMAs, LABAs, ICS, has a lung function of 33% and an SGRQ score of 50. The patient has has 2 exacerbations in the past year, one of which has been severe. Now imagine we want to run the model for a female patient, who is 55 years old, has an FEV1 of 67%, and is only receiving LAMA and LABA, but is otherwise similar to the previous patient. 

```
> input$male <- 0
> input$age  <- 55
> input$FEV1 <- 67
> input$oxygen <- 0
> input$statin <- 0
> input$ICS <- 0
> 
> 
> results <- model_run(input, "accept", api_key = "[YOUR_API_KEY")
Selected model is accept
Calling server at https://prism.peermodelsnetwork.com/route/accept/run
```
Now we can explore the predictions of the model: 
```
> results$predicted_exac_probability
[1] 0.7656
> results$predicted_exac_rate
[1] 1.774
> results$predicted_severe_exac_probability
[1] 0.215
> results$predicted_severe_exac_rate
[1] 0.3638
```
More info and examples on how to access different models on R as well as other platforms can be found on the [PMN API User Guide page](https://resplab.github.io/prismguide/).
