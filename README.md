# Causal Inference R-Shiny
This RShiny application aims to simplify the process of conducting Causal Inference using BART (Bayesian Additive regression Trees).

## What it does
* Allows options of different methods on propensity scores, treatment variables, and response variables
* Estimates ATE (Average Treatment Effect), ATT (Average Treatment Effect on the Treated), and ATC (Average Treatment Effect for the Controls)
* Renders Diagnostic plots of convergence, individual effects, and overlap
* Conducts sensitivity analysis

## Steps
* Trim dataset so it only includes confounders (X), treatment (Y), response (Z), and ID column (optional)
* Convert and save dataset into .csv file
* Click Upload panel on top of the page and choose the file path to upload dataset
* Specify the column number of ID column (if applicable), treatment column (Y), and response column (Z)
* Use Option panel to select method of analysis

## Examples:
* [bartCause](https://github.com/vdorie/bartCause)  
* [treatSens](https://cran.r-project.org/web/packages/treatSens/treatSens.pdf)
