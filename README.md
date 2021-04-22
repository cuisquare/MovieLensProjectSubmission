# MovieLens Project

R project for the MovieLens capstone project of HarvardX R professional certificate.

## Installation

You can download the whole repository ready to run in RStudio, by running the following from command line while in the destination folder of your choice.

```bash
git clone https://github.com/cuisquare/MovieLensProjectSubmission.git
```

You can then open the file **MovieLensProject.Rproj** in RStudio to browse through the content and run the R Script or Rmd file. Alternatively you can access the submission files directly. 

## Submission Content

### PDF report 

It is located at the root level, with name **MovieLensReport.pdf**. 

It was knitted from the Rmd file. 

### Rmd file 

It is located at the root level, with name **MovieLensReport.Rmd**. 

You can knit it to generate the pdf report. 

### R script

It is located at the root level, with name **RProf_Capstone_MovieLens.R**. 

You can run it through the Rmd file in the set up code chunk or directly by running the following code in R: 

```r
source("RProf_Capstone_MovieLens.R")
```

By running the R script once without modifying the loading logic section, you should then be able to re-run any specific line of code you might have interest in, for testing/grading purpose. This would be the advised method. 

For reasons of repository size, only the Output data has been included in the repository (namely files *Output_020.RData*, *Output_021.RData* and *Output_022.RData*). Therefore upon running the R script for the first time, the Input data will be generated and saved in the Data folder, which should take an indicative 5 minutes on a 16Gb machine. It will then be ready to be loaded in future runs.

The code has been organised in sections using **[code folding](https://support.rstudio.com/hc/en-us/articles/200484568-Code-Folding-and-Sections)** to make it easier to browse. Note that when completely folded the *# INPUT DATA CREATION* and *# OUTPUT DATA CREATION* sections will only show their root levels because they are included in conditional sections to allow for loading RData files instead of running the code.  

If you want to re-run the whole of the script from scratch you will have to hard set the parameters *load_input_data* and *load_output_data* to FALSE, which you will find in the section called *# RUNNING OR LOADING LOGIC ####*. Note however that running the whole script from scratch takes around 5-6 hours on a 16Gb machine so it is not advised to do so. 

## Disclaimer

This work was realised by the author without external help or plagiarism. Where 3rd party material was used it has been appropriately attributed in the report and it did not pertain to the actual analysis work. 