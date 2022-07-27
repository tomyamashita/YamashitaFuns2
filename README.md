
# YamashitaFuns2

<!-- badges: start -->
<!-- badges: end -->

The goal of YamashitaFuns2 is to provide easy access to functions that I commonly use for processing data. I'm lazy and want a centralized location for all of my commonly used and generalizable functions. I also forgot which scripts contain which commonly used functions so its easier to just have them all in one place. This also allows me to document my functions so I can remember what they do and how they work, then share them with others who may be able to modify them for their work. 

## Installation

You can install the latest version of YamashitaFuns2 from the available tar.bz file:

``` r
install.packages("D:/TAMUK/2Dissertation_Work/YamashitaFuns2_0.0.0.9.tar.gz", repos = NULL, type = "source")
```

Note that you will need to update the file path or just install using Rstudio. 

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(YamashitaFuns2)
## basic example code

# Please note that errors are likely to arise if your data is not formatted in the same way as mine. Again, I build these functions for my convenience and this assumes my formatting method. I can help you diagnose problems but most problems seem to be associated with differently formatted data. 
```

## Acknowledgements
Chloe Bates helped test and debug the removeGhosts, occFun, and APFun_env functions. 

Aidan Branney helped test and debug the occFun, APFun_env, and actfun and helped develop the timeConvert function. 

Duston Duffie helped test, debug, and expand on the cameraRename2 function. 

Kevin Ryer developed early versions of the APFun_env function. 


## Changelog
0.0.0.21 - 2022-07-27
  Modified the timelapseQC function to allow the user to set which species are excluded from the check on number of individuals
  Modified the APFun_Timelapse function to use column names instead of column IDs to subset data

0.0.0.20 - 2022-07-14
  Fixed links to functions in this package and other packages. 

0.0.0.19 - 2022-07-14
  Modified the imageeffort_fun function to require a list object input and provide an option to input a timelapse or dataorganize file. This was to make running the function smoother. Removed the foreach requirement.
  Modified the movePictures function to require an object input instead of a file path. This reduces the likelihood of errors in the functionality. Also made slight tweaks to the code to improve efficiency.

0.0.0.18 - 2022-07-07
  Modified the occFun function to better accommodate changes in months when using non-day intervals for aggregation and to better explain what it is doing as it does things.
  
0.0.0.17 - 2022-06-07 - 2022-07-01
  Added the cameraRename3 function and associated documentation and fixed a mistake that caused all images to receive the same name. 
  Deprecated the cameraRename2 function due to addition of cameraRename3
  Added the findCorruptImages function and associated documentation.
  Changed the foreach package to a dependency instead of an Import (in future versions, the foreach requirement will be removed and replaced with for or apply)

0.0.0.16 - 2022-05-11
  Modified the cameraRename2 function to add additional checks, the ability to input a vector for out.dir, and some clarifications for some of the terms used. 
  Added an Acknowledgements section to the Readme and removed individuals from the Authors section to Acknowledgements. 
  Removed the foreach package requirement for the actfun function. 
  Removed empty sections from documentation. 

0.0.0.15 - 2022-05-05
  Modified the cameraRename2 to add several checks on correct directories, sub-directory structure, and image paths. Added options to copy instead of move images and to move/copy images to a new directory instead of replacing in the same directory. 
  Added the movePictures function and associated documentation to use a timelapse csv to "sort" images into appropriate folders. 

0.0.0.14 - 2022-05-04 - 2022-05-05
  Modified the removeGhosts function to print the absolute paths for the "in" and "out" folders. Added a check for directories and a way to add new directories. 

0.0.0.13 - 2022-03-24
  Added the timeConvert function and associated documentation. 
  Added the timelapseQC function and associated documentation. 

0.0.0.12 - 2022-01-18
  Modified the cameraRename2 function to be able to check for different file types and accommodate video files. 

0.0.0.11 - 2022-01-14
  Updated the APFun_env to increase its processing speed using apply functions instead of for loops. 
  Added the occFun function and associated documentation to set up data for occupancy modelling. 
  Added the APFun_Timelapse function and associated documentation to use other functions in this package with a timelapse output. 

0.0.0.10 - 2021-11-19 - 2021-12-10
  Deleted the default hello() function. 
  Added the interactionsDataOrganize and interactionsTimelapse functions and associated documentation. 

0.0.0.9 - 2021-11-19
  Added the removeGhosts function and associated documentation to extract ghost images from the Microsoft Megadetector AI.
  Updated the description file with updated author information. 

0.0.0.8 - 2021-10-11
  Updated the cameraRename2 function to (hopefully) deal with the problem of images not being renamed. I think the issue had to do with defining relative vs. absolute paths and the paste function not realizing that it needed to operate on items in a data frame so I switched paste to the unite function from tidyr. 

0.0.0.7 - 2021-10-10
  Updated the cameraRename2 function to notify when images are not being renamed. 
  Updated the cameraRename2 documentation to discuss the issue where file.rename doesn't work on huge numbers of files.
  Added missing dataorganize function. 

0.0.0.6 - 2021-10-08
  Updated the cameraRename2 function to better handle issues with image order and name issues. See details in code. 

0.0.0.5 - 2021-10-05
  Added the cameraRename2 function and associated documentation. 
  Added the dataorganize function and associated documentation. 
  Updated ReadMe file to make it more readable. 

0.0.0.4 - 2021-09-24
  Added the cameraRename function and associated documentation. 

0.0.0.3 - 2021-09-10
  Modified the actfun function to work better with no splitting of data before species. 
  Removed hidden loading of the foreach package due to issues with calling foreach() %do% {}. 
  Added references to the activity documentation related to options and choices made. 
  Added qualifier to each function's documentation indicating that functions may not be fully generalizable. 

0.0.0.2 - 2021-09-06
  Added ctdates_fun and started documentation for this function. 
  Updated documentation for trapeffort, APFun_env, ctdates, imageeffort. 
  Added proper references to functions from other packages. 
  Updated package dependencies to function properly. 

0.0.0.1 - 2021-08-23
  Began populating help documentation. 

0.0.0.0 - 2021-08-19
  Package created
  Added APFun_env, trapeffort_fun, imageeffort_fun, and actfun functions. 
  Added readme file. 
  Added package dependencies. 
  Added help files for code (These were not populated though). 
