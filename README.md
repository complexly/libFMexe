<!-- README.md is generated from README.Rmd. Please edit that file -->
R Wrapper for the libFM Executable
==================================

This package provides a rough interface to [libFM](http://www.libfm.org/) using the executable with [R](https://www.r-project.org/). It does this in two ways:

-   The `libFM.model.frame()` function converts data into libFM (also LIBSVD) format
-   It calls the libFM executable with your data and returns the resulting prediction

Installing
----------

### Installing libFM executable

#### Windows

First, you will need to [download the libFM windows executable](http://www.libfm.org/#download) and save it on your computer, for example in `C:\libFM`.

Then, it is recommended that you add the directory that you saved libFM in to you system path. [This webpage](https://msdn.microsoft.com/en-us/library/office/ee537574(v=office.14).aspx) provides one way to do that. If you do not or cannot do that, you can enter the directory as `exe_loc` argument into the `libFM()` function, for example `libFM(..., exe_loc = "C:\\libFM")`.

#### Mac / Linux

First, you will need to [download the libFM C++ source code](http://www.libfm.org/#download) and install it on your computer, for example in `/usr/local/share/libFM/bin`. You can also download the development code from Steffen Rendle's [github repository](https://github.com/srendle/libfm).

Then, it is recommended that you add the directory that you saved libFM in to you system path. [This webpage](http://architectryan.com/2012/10/02/add-to-the-path-on-mac-os-x-mountain-lion/) worked for me. If you do not or cannot do that, you can enter the directory as `exe_loc` argument into the `libFM()` function, for example `libFM(..., exe_loc = "/usr/local/share/libFM/bin")`.

#### Debugging

You can verify that the path contains the libFM directory by running `Sys.getenv('PATH')`. Verify that the program works by running `system("libFM -help")`.

### Installing libFMexe R package

With the `devtools` package, you can install this package by running the following.

``` r
# install.package("devtools")
devtools::install_github("andland/libFMexe")
```

Using libFMexe
--------------

The support for `libFM` is still fairly limited, but this package allows you to quickly iterate on a number of models.

libFM does well with categorical variables with lots of levels. The canonical example is for collaborative filtering, where there is one categorical variable for the users and one for the items. For now, I have only included support for covariates that factor variables.

The main advantage of this package is being able to try many different models. For example, trying different combinations of variables, trying different `dim` of the two-way interactions, or trying different `init_stdev`.

### Collaborative Filtering Example

Using the Movie Lense 100k data, we can predict what ratings users will give to movies.

``` r
library(libFMexe)

data(movie_lense)

set.seed(1)
train_rows = sample.int(nrow(movie_lense), nrow(movie_lense) * 2 / 3)
train = movie_lense[train_rows, ]
test  = movie_lense[-train_rows, ]

predFM = libFM(train, test, Rating ~ User + Movie,
               task = "r", dim = 10, iter = 500)

mean((predFM - test$Rating)^2)
#> [1] 0.8188555
```

This gives a mean squared error of 0.8188555 with dimension 10.

We can compare to something simpler, such as ridge regression. Ridge regression cannot model interactions of users and movies because each interaction is observed at most once.

``` r
suppressPackageStartupMessages(library(glmnet))

spmat = sparse.model.matrix(Rating ~ User + Movie, data = movie_lense)
trainsp = spmat[train_rows, ]
testsp = spmat[-train_rows, ]

mod = cv.glmnet(x = trainsp, y = movie_lense$Rating[train_rows], alpha = 0)
predRR = predict(mod, testsp, s = "lambda.min")

mean((predRR - test$Rating)^2)
#> [1] 0.8954028
```

Ridge regression gives a mean squared error of 0.8954028.

For comparison, we can run libFM with `dim = 0`, which is basically the same as ridge regression.

``` r
predFM_RR = libFM(train, test, Rating ~ User + Movie,
                  task = "r", dim = 0, iter = 100)

mean((predFM_RR - test$Rating)^2)
#> [1] 0.8883521
```

This gives a mean squared error of 0.8883521, nearly the same as ridge regression.

Improving this package
----------------------

[c++ source code](https://github.com/srendle/libfm) is available for libFM. Let me know if you would like to help build an implementation that calls the source code.
