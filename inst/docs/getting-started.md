
# Getting Started with aceR

## Installation

1. [Install R](https://cran.r-project.org/)

2. [Install RStudio](https://www.rstudio.com/products/rstudio/download/)

3. [Install Xcode from App Store](https://itunes.apple.com/us/app/xcode/id497799835?mt=12)
  
4. Install devtools R-package.
  
  Open RStudio, and run the following command in the console:
  ```
  install.packages("devtools")
  ```
  
5. Finally, install aceR using devtools

  ```
  GITHUB_PAT = "64235145b808f152d84467ea1362dc0b7a0def6c"
  devtools::install_github("josegallegos07/aceR", auth_token = GITHUB_PAT)
  ```
  
  **NOTE**: Use this step to install the latest version of aceR.