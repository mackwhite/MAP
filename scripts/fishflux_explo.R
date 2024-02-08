install.packages("fishflux")
library(fishflux)
devtools::install_github("nschiett/fishflux", dependencies=TRUE)
library(fishflux)
dat <- metabolic_parameters
library(fishflux)
metabolism(family = "Centrarchidae", temp = 27, troph_m = 2)

dat1<- weight_prop
data(weight_prop)

fishflux::growth_params("Centropomus undecimalis", otolith = FALSE)

## load the example parameters for Zebrasoma scopas, a list
param_zebsco <- fishflux::param_zebsco
## Run the model, specifying the target length(s) and the parameter list
model <- fishflux::cnp_model_mcmc(TL = 5:20, param = param_zebsco)