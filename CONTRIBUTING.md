# Contributing to aceR

Please take a moment to review this document in order to make the contribution process easy and effective for everyone involved.

## overview

`aceR` is an [R package](http://www.statmethods.net/interface/packages.html). [It is not a *script* nor is it collection of scripts](https://youtu.be/r1yYJBzf1VQ?t=18). Please don't treat this repository as a script dump. The scripts provided in [scripts](/scripts) are meant to be examples to build on (not copy/paste).

### pull-requests

New features are great. They should remain focused in scope and avoid containing unrelated commits.

Please ask first before embarking on any significant pull request (e.g. implementing features, refactoring code, porting to a different language), otherwise you risk spending a lot of time working on something that the project's developers might not want to merge into the project.

## development pre-reqs

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

1. Make your changes.

   Build `->` Build & Reload

2. Test your changes (add unit tests).

3. Check changes with R check: 

   Build `->` Check Package

4. Make a pull-request if your code passes tests and R checks.

## philosophy

1. Aside from some notable exceptions, functions should only do one thing. Break up larger functions into smaller general functions where possible.

2. Add new functions to existing files where possible. More files makes it harder to find things.

3. Use file name prefixes to organize files (no subdirectories). Use existing prefixes when possible.

4. function names should be general, concise, and descriptive. (ex. `make_box_plot` instead of `make_box_plot_for_seacrest_reaction_time`) - see 1.

5. variable names should be generic (ex. `x` instead of `age`)

A rule of thumb: make everything generic and re-usable. 

## syntax style guide

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

We use the [testthat](https://github.com/hadley/testthat) package to run basic unit tests. Please make sure your code passes before checking-in your changes. Please add [unit tests](tests/testthat/) when possible.

### Travis 

Tests are automatically deployed on Travis after every push. 

https://travis-ci.org/josegallegos07/aceR/builds