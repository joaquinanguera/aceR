# aceR dev

## overview

`aceR` is an [R package](http://www.statmethods.net/interface/packages.html). [It is not a script nor is it collection of scripts](https://youtu.be/r1yYJBzf1VQ?t=18). Please don't treat this repository as a script dump. 

## prereqs

1. Install [R](http://cran.rstudio.com/)
2. Install [RStudio](http://www.rstudio.com/products/rstudio/download/)

#### R-dev package dependencies

```
dev_deps = c("devtools", "roxygen2", "testthat", "sciplot")
install.packages(dev_deps)
```

## development flow

In RStudio, 

File `->` Open Project `->` aceR.Rproj

Make your changes.

Build `->` Build & Reload

Test your changes.

Check changes 

Build `->` Check Package

## style guide

Please refer to [this guide](http://r-pkgs.had.co.nz/style.html) when writing new functions or refactoring old ones.

#### tl;dr

1. snake_case
2. spaces around all infix operators (`=`, `+`, `-`, `<-`, etc)
3. curly braces on same line as first statement  
4. use `=` within functions. otherwise use `<-`
5. 2 space indentation

#### function documentation

Use [roxygen2](http://r-pkgs.had.co.nz/man.html) for documenting functions.

## tests

We use the [testthat](https://github.com/hadley/testthat) package to run basic unit tests. Please make sure your code passes before building the package for distribution. Add [unit tests](tests/testthat/) when possible.