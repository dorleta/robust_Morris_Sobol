# robust_Morris_Sobol
Code and examples to implement the criteria defined in "Robust combination of the Morris and Sobol methods in complex multidimensional models" by Dorleta Garcia, Inmaculada Arostegui and Raul Prellezo and show the results of the Morris and Sobol methods described in the paper.

## code folder
It contains the R functions needes to implement the selection and convergence criteria described in the paper "Robust combination of the Morris and Sobol methods in complex multidimensional models".

## example folder
It contains an example of the application of the selection and convergence criteria to a simplified version of the case study described in the paper.

The files:

* AEE.RData: A data frame with the output of the morris method with the absolute elementary effects (AEE) for each input factor and output variable. The dataframe must have at lest three columns 'inpFact', 'outVar' and 'AEE'. Each row corresponds with the aboslute elementary effect (AEE) of the input factor in column  'inpFact' for ouput variable in 'outVar' column.


| inpFact | outVar     | AEE |
|---------|------------|-----|
| weight  | catch      | 3.14|
| maturity| catch      | 2.01|
| weight  | abundance  | 1.08|
| maturity| abundance  | 5.27|


* AEE_boot_25.RData, AEE_boot_50.RData, AEE_boot_100.RData, AEE_boot_150.RData, AEE_boot_200.RData, AEE_boot_250.RData, AEE_boot_300.RData: A data frame with the output of a bootstrap of the morris method with the absolute elementary effects (AEE) for each input factor, output variable and bootstrap iteration. The shape is the same as the data frame defined in the previous point but with one more column 'bootit' with the corresponding bootstrap iteration.

| inpFact | outVar     | AEE | booit |
|---------|------------|-----|-------|
| weight  | catch      | 3.14| 1 |
| maturity| catch      | 2.01| 1 |
| weight  | abundance  | 1.08| 1 |
| maturity| abundance  | 5.27| 1 |

* Selection_&_Convergence_Criteria.R: This R script uses the functions in the 'code' folder and the data frames described in the previous two points to implemented the selection and convergence criteria defined in the paper "Robust combination of the Morris and Sobol methods in complex multidimensional models". The script can work with any other data frames as long as they have the shape defined above. 


## shiny folder 
It contains the R scripts and the R data needed to generate a Shiny app with all the results from the application of Morris and Sobol method described in the paper "Robust combination of the Morris and Sobol methods in complex multidimensional models". To run it locally it is enough to run the RunApp.R script in an R session.

