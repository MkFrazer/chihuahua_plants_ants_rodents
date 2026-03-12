---
title: "Manuscript_draft"
author: "Madeline Frazer"
bibliography: Reproducibility.bib
csl: chicago-fullnote-bibliography.csl 

output: pdf_document
---

# Ants, Plants, and Rats: Effects of Predator Exclusion on Chihuahua Plant Species Abundance

## Introduction

Plant species abundance is a result of many abiotic and biotic factors, including predation. Two main plant predators of Chihuahua plants include ants and rodents.

We aimed to use existing, publicly available data by [@ernest2009] to understand how each predator population (ants and rodents) influence plant species diversity. This data set collected longitudinal data plant abundance data, ant colony data, rodent data, and precipitation data from 1977-2002 across 24 experimental exclusion plots.

## Materials and Methods

The data for this project is publicly available. <!-- We ran a linear mixed effects model in order to understand the interactive impact of predators on plant species abundance, using ant colony presence/absence and rodent presence/absence as fixed effects, and precipitation as a random effect. -->

## Results

Here is a table of which plots had which treatment(s).

Changes in treatment assignments to the 24 experimental plots through time. To help highlight which plots were modified, only changes in treatment after 1985 are recorded below. Blank cells denote no changes in treatment from the previous time period. Pogonomyrmex rugosus was a dominant granivorous ant species that declined at the site. Banner-tailed kangaroo rats (Dipodomys spectabilis) were a dominant rodent granivore that also declined during the 1980s. (Table modified from Brown 1998). * plot 24 was not initiated until 1979 (Samson et al. 1992)


``` r
library(knitr)
plots_treatments <- read.csv("./../data/data_raw/raw_data/plot_treatments.csv")
kable(plots_treatments, align = "cccc", caption = "Plot Treatments")
```



Table: Plot Treatments

| Plot |                    X1977_1985                     |         X1985_1987          |              X1988_2002               |
|:----:|:-------------------------------------------------:|:---------------------------:|:-------------------------------------:|
|  1   |          Mixed-size seeds added in pulse          |     All annuals removed     |  Banner-tailed kangaroo rat removed   |
|  2   |                 Small seeds added                 |   Summer annuals removed    |         Unmanipulated control         |
|  3   | Kangaroo rats removed
Pogonomyrmex rugosus removed |                             | Kangaroo rats removed
All ants removed |
|  4   |                 All ants removed                  |                             |                                       |
|  5   |        Banner-tailed kangaroo rats removed        |                             |          All rodents removed          |
|  6   |                 Large seeds added                 |   Winter annuals removed    |         Kangaroo rats removed         |
|  7   |                All rodents removed                |                             |                                       |
|  8   |           Pogonomyrmex rugosus removed            |                             |           All ants removed            |
|  9   |              Mixed-size seeds added               | Bi-seasonal annuals removed |  Banner-tailed kangaroo rats removed  |
|  10  |        All rodents removed
All ants removed        |                             |                                       |
|  11  |               Unmanipulated control               |                             |                                       |
|  12  |           Pogonomyrmex rugosus removed            |                             |           All ants removed            |
|  13  |                 Large seeds added                 |   Winter annuals removed    | Kangaroo rats removed
All ants removed |
|      |                                                   |                             |                                       |
|  14  |               Unmanipulated control               |                             |                                       |
|  15  |               Kangaroo rats removed               |                             |                                       |
|  16  |                All rodents removed                |                             |                                       |
|  17  |                 All ants removed                  |                             |                                       |
|  18  |         Mixed-sized seeds added in pulse          |     All annuals removed     |         Kangaroo rats removed         |
|  19  | Kangaroo rats removed
Pogonomyrmex rugosus removed |                             | Kangaroo rats removed
All ants removed |
|  20  |              Mixed-sized seeds added              | Biseasonal annuals removed  | Kangaroo rats removed
All ants removed |
|  21  |               Kangaroo rats removed               |                             |                                       |
|  22  |                 Small seeds added                 |   Summer annuals removed    |         Unmanipulated control         |
|  23  |                All rodents removed                |                             |                                       |
|      |                                                   |                             |                                       |
|      |                 All ants removed                  |                             |                                       |
| 24*  |        Banner-tailed kangaroo rats removed        |                             |          All rodents removed          |

Here is a figure of the linear mixed effects model and how it predicts the data.


```
## 
## Attaching package: 'palmerpenguins'
```

```
## The following objects are masked from 'package:datasets':
## 
##     penguins, penguins_raw
```

```
## Loading required package: Matrix
```


```
## Warning in AIC.default(model1, model2, model3): models are not all fitted to
## the same number of observations
```

```
##        df      AIC
## model1  7 2036.988
## model2  6 2075.048
## model3  5 2242.698
```

![](Manuscript_files/figure-latex/unnamed-chunk-4-1.pdf)<!-- --> 

The residuals clearly have heteroskodasticity.

## Discussion

## Conclusion

## Bibliography
