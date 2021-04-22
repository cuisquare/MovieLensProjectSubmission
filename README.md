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

It is located at the root level, with name **MovieLensReport.pdf**. It was knitted from the Rmd file. 

### Rmd file 

It is located at the root level, with name **MovieLensReport.Rmd**. You can run it to generate the pdf report. 

### R script

It is located at the root level, with name **RProf_Capstone_MovieLens.R**. You can run it directly or through the Rmd file in the set up code chunk. 

It has been organised in sections using code folding.

It contains running / loading logic whereby the input and output data will be loaded if it is available in the **Data** folder. Upon running that data will also be saved automatically for future loading. 

For reasons of repository size, only the Output data has been included (namely files *Output_020.RData*, *Output_021.RData* and *Output_022.RData*). Therefore upon running the R script the Input data will be generated and saved, which should take an indicated 5 minutes on a 16Gb machine. It will then be ready to be loaded in future runs.

By running the R script without modifying the loading logic section, you should be able to re-run any specific line of code you might have interest in, for testing/grading purpose. This would be the advised method. 

If you want to re-run the whole of the script from scratch you will have to hard set the parameters *load_input_data* and *load_output_data* to FALSE, which you will find in the section called *# RUNNING OR LOADING LOGIC ####*. Note however that running the script from scratch takes around 5-6 hours on a 16Gb machine. 

## Disclaimer

This work was realised by the author without external help or plagiarism. Where 3rd party material was used it has been appropriately attributed in the report and it did not pertain to the actual analysis work. 