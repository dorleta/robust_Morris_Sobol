# robust_Morris_Sobol
Code and examples to implement the criteria defined in "Robust combination of the Morris and Sobol methods in complex multidimensional models" by Dorleta Garcia, Inmaculada Arostegui and Raul Prellezo.


## code folder
It contains the R functions with the implementation of the selection and convergence criteria described in the paper.

## example folder
It contains an example of the application of the selection and convergence criteria to a simplified version of the case study described in the paper.

The files:
* AEE.RData: A data frame with the output of the morris method with the absolute elementary effects (AEE) for each input factor and output variable. The dataframe must have at lest three columns 'inpFact', 'outVar' and 'AEE'. Each row corresponds with the aboslute elementary effect (AEE) of the input factor in column  'inpFact' for ouput variable in 'outVar' column.


| inpFact | outVar     | AEE |
------------------------------
| weight  | catch      | 3.14|
| maturity| catch      | 2.01|
| weight  | abundance  | 1.08|
| maturity| abundance  | 5.27|


* AEE_boot_25.RData, AEE_boot_50.RData, AEE_boot_100.RData, AEE_boot_150.RData, AEE_boot_200.RData, AEE_boot_250.RData, AEE_boot_300.RData: The output of a bootstrap of the morris method with the absolute elementary effects (AEE) for each input factor, output variable and bootstrap iteration.
* Selection_&_Convergence_Criteria.R


## shiny folder 
It contains the R scripts and the R data needed to generate a Shiny app with all the results from the application of Morris and Sobol method described in the paper. To run it locally you need to run the RunApp.R script in an R session.

