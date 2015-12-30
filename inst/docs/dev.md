# aceR dev

## prereqs

1. Install [R](http://cran.rstudio.com/)
2. Install [RStudio](http://www.rstudio.com/products/rstudio/download/)

#### R-dev package dependencies

```
dev_deps = c("devtools", "roxygen2", "testthat", "sciplot")
install.packages(dev_deps)
```

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