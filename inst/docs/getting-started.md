# Getting Started with aceR

## Installation

1. [Install R](https://cran.r-project.org/)

2. [Install RStudio](https://www.rstudio.com/products/rstudio/download/)

3. [Install Xcode from App Store](https://itunes.apple.com/us/app/xcode/id497799835?mt=12)
  
4. Install `devtools` R-package.
  
  Open RStudio, and run the following command in the console:
  ```{r}
  install.packages("devtools")
  ```
  
5. Finally, install `aceR` using `devtools`

  ```{r}
  devtools::install_github("josegallegos07/aceR")
  ```

## Example
The following section describes how to set up a simple script for loading and processing raw ACE data.

### 1. Load `aceR`

```{r}
library(aceR)
```

### 2. Set your working directory
_Working directory_ refers to the directory you want R to load data from (or write data to).

If we wanted to process RedBull data:

```{r}
setwd("~/Google Drive/ACE/All Raw ACE Data/ACE Studies_Raw Data/Redbull")
```

We can also set up our script to process "all" ACE data in one go:

```{r}
setwd("~/Google Drive/ACE/All Raw ACE Data/ACE Studies_Raw Data")
```

#### Protips:
- `getwd()` tells your current working directory.
- setting your working directory to a directory that doesn't exist on your machine will throw an error.

### 3. Load our Data

```{r}
dat = load_ace_bulk()
```

#### Protips:
- `acerR` will throw warnings in cases where you are trying to load invalid data (data formatted in a way `aceR` doesn't know how to handle). Please sanity check un-loadable files before reporting bugs.

### 4. Process our data

```{r}
proc = proc_by_module(dat, verbose = TRUE)
```

#### Protips:
- The more data you try to load and process, the longer it'll take. Processing "all" the data will take about an hour (or 2). Grab yourself a cup of coffee.

### 5. Change our working directory to output directory
To avoid running into issues, save our processed data into a different directory:

```{r}
setwd("~/Desktop/processed")
```

#### Protips:
- Make sure this directory exists before you try to save to it. 

### 6. Finally, export our processed Data

```{r}
export_csv(proc)
```

### All Together

```{r}
# load aceR
library(aceR)

# set working directory
setwd("~/Google Drive/ACE/All Raw ACE Data/ACE Studies_Raw Data/Redbull")

# load raw data
dat = load_ace_bulk()

# process data by module
proc = proc_by_module(dat, verbose = TRUE)

# export data
setwd("~/Desktop/processed")
export_csv(proc)
```

## More examples

See [example scripts](https://github.com/josegallegos07/aceR/tree/master/scripts)
