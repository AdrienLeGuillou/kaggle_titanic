---
title: "Project name"
author: "Adrien Le Guillou"
output:
  html_document: 
    toc: yes
  html_notebook: default
bibliography: references.bib
---

# Setup


## Load libraries


```r
# Load all the project packages
library("knitr")
library("kableExtra")
library("bibtex")
library("tidyverse")
library("forcats")
library("lubridate")
library("stringr")
library("ggpubr")
```


## Process the data


```r
process_data <- function(update = FALSE){
  if (update == TRUE | file.exists("data/processed/datas.Rda") == FALSE) {
    # Put the data processing here
    train <- read_csv("data/raw/train.csv")
    test <- read_csv("data/raw/test.csv")
    gender_submission <- read_csv("data/raw/gender_submission.csv")
    
    save(train, test, gender_submission, file = "data/processed/datas.Rda")
  }

  # clean workspace before exiting
  rm(list = ls())
}
```






# Data description


```r
train <- train %>%
  mutate(Pclass = as_factor(Pclass))
```

```
## Error in mutate_impl(.data, dots): Evaluation error: no applicable method for 'as_factor' applied to an object of class "c('integer', 'numeric')".
```

```r
ggdensity(train, x = "Age",
          add = "mean", rug = "true",
          color = "Sex", fill = "Sex")
```

```
## Warning: Removed 177 rows containing non-finite values (stat_density).
```

![plot of chunk data_description](figure/data_description-1.png)


---
nocite: |
  @base, @bibtex, @bindrcpp, @dplyr, @forcats, @ggplot2, @ggpubr, @kableExtra, @knitr, @lubridate, @magrittr, @nvimcom, @purrr, @readr, @stringr, @tibble, @tidyr, @tidyverse
...


# References and R packages used
