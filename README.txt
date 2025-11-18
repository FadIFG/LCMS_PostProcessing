LC-MS untargeted analysis_Post-processing workflow in R
This code includes several steps of post-processing, from imputation and plotting PCAs, to calculatiing correlations, filtering, and statistical analysis.

Example_Dataset and the folder "Functions must be in the same directory of the R file "LCMS_PostProcessing_Workflow"
                      
The following libraries and functions are necessary for the code to run. 
Please notice that the code only calls the libraries, assuming they are already installed.
If a library is not already installed, use the command install.packages("libraryname") to install it. 

library("openxlsx")    # to write xslx files instead of csv
library("HiClimR")      # to calculate correlations fast
library("matrixTests")  # for more control over t.tests in matrices
library("ggplot2")      #for Plots
library("ggrepel")      #for Plots
library("readxl")       #to read xslx files
library("dplyr")        #many functions,but here especially necessary for filtering as in filter(RT > L & RT<U)
library("stats")        #to adjust for multiple tests


