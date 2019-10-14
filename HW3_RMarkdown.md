HW 3
================
CC
10/14/2019

## Problem 1

Load
    libraries

``` r
library(tidyverse)
```

    ## ── Attaching packages ────────────────────────────────────────────────────────────── tidyverse 1.2.1 ──

    ## ✔ ggplot2 3.2.1     ✔ purrr   0.3.2
    ## ✔ tibble  2.1.3     ✔ dplyr   0.8.3
    ## ✔ tidyr   1.0.0     ✔ stringr 1.4.0
    ## ✔ readr   1.3.1     ✔ forcats 0.4.0

    ## ── Conflicts ───────────────────────────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()

``` r
library(p8105.datasets)

data(instacart)
```

# Describing the dataset

This dataset is quite large, with 1384617 1,384,617 observations from
131,209 unique users. There is a single order per user, and each row
represents a product from an order. For example, user 1 has an order id
of 2539329, which functions as their unique identifier, and they ordered
from instacart on Monday, the second day of the week at 8AM. The 1st
item they added to their cart was Soda, listed in the product name
column. There are a few other variables of interest, including whether
or not the user has ordered an item in the past (reordered) and days
since the user last ordered from instacart (days\_since\_prior\_order).
In total there are 15 15 variables.

``` r
nrow(instacart)
```

    ## [1] 1384617

``` r
ncol(instacart)
```

    ## [1] 15
