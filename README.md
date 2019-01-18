# Example of Repository

[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.146.svg)](https://doi.org/10.5281/zenodo.146)

This repository contains code associated with the book, entitled, 'blah blah blah'. When run, it will replicate the results published in the book ([link to the book](https://www.google.com)). Please cite both the textbook and this repository if you make use of any part of this.

## Worked Examples

*Choose an option*:


### Option 1: Static version in HTML (website) 

 * Worked Example 5.1: [Flow Duration Curve](http://htmlpreview.github.io/?https://github.com/jstagge/example_droughts/blob/master/worked-examples/worked_example_fdc.nb.html)


### Option 2: Run code live in the cloud with no prerequisites

Click at this badge to execute the analysis and replicate results using R-Studio in the cloud without needing to install R or its dependencies on your local machine.    
RStudio: [![Binder](http://mybinder.org/badge.svg)](http://mybinder.org/v2/gh/jstagge/example_droughts/master?urlpath=rstudio)

If you want to share this live RStudio link with others, please share this URL: http://mybinder.org/v2/gh/jstagge/example_droughts/master?urlpath=rstudio

*Note*, URLs that contain (https://hub.mybinder.org.....) will become invalid after few minutes of inactivity.

Once RStudio launches online, it will come with all the prerequisites installed and ready to execute. So skip ```Rscript 00_prepare_file_system.R``` step in running the code.    
 

### Option 3: Run code on a local machine   
These instructions will allow you to process the reproducibility survey data on your local machine for testing purposes. All code is written in R. See Prerequisites and Running sections below for detailed instructions.  

### Prerequisites

In order to run this code, you must install:
* [R for Statistical Computing](https://www.r-project.org/)

All necesary R packages will be installed automatically in the first file.

## Running the Code

First, make sure to set the working directory to the downloaded and unzipped folder.  

### Running all scripts at once

Code is numbered based on the order of operations. For more detailed information about each file, see below:


### Chapter 7
The following file prepares the file system, installing any necesary packages and creating folders for model output. 

If you're using the cloud option, skip this step.  
```
Rscript 00_prepare_file_system.R
```
The next script processes all articles from 2017, plots their keywords, separates the keyword or non-keyword papers, and randomly assigns papers to reviewers.

```
Rscript 01_article_analysis.R
```
The following script performs all calculations on the results of the  reproducibility survey. It prepares the data to be plotted using code file number 3. All results will be saved into a large .RDS file. This allows for the data to be plotted immediately or to be loaded later for additional analysis.

```
Rscript 02_reproduc_data_handling.R
```
The following file plots all figures from the analysis, incuding many that are not provided in the published paper. All files will be saved to a folder located at /output/figures.
```
Rscript 03_reproduc_figs.R
```
The final code file creates a estimate for all articles published in these journals during 2017 (i.e. the population).
```
Rscript 04_pop_estimate.R
```
## Results       
After you run the scripts above, look into a new generated folder called "Output". Then open the sub-folder inside it for the relevant chapter which will contain the figures as shown in the book.   


## Reference and How to Cite

For any description of this methodology, please use the following citation (s):

* The book citation [The book citation chapter](https://github.com/jstagge/reproduc_hyd/blob/master/assets/stagge_et_al_reproducibility_preprint.pdf)

* The github citation (2018)  doi: 10.5281/zenodo.1467417


## Authors

* **James H. Stagge** - *Owner* - [jstagge](https://github.com/jstagge)

## License
This project is licensed under the MIT License - see the [LICENSE.md](LICENSE.md) file for details


## Acknowledgments   
This material is based upon work supported by ........... Any opinions, findings, and conclusions or recommendations expressed in this material are those of the author(s) and do not necessarily reflect the views of any of the funding organizations. 
