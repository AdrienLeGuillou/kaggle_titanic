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
```

```
## Loading tidyverse: ggplot2
## Loading tidyverse: tibble
## Loading tidyverse: tidyr
## Loading tidyverse: readr
## Loading tidyverse: purrr
## Loading tidyverse: dplyr
```

```
## Conflicts with tidy packages ----------------------------------------------
```

```
## filter(): dplyr, stats
## lag():    dplyr, stats
```

```r
library("forcats")
library("lubridate")
```

```
## 
## Attaching package: 'lubridate'
```

```
## The following object is masked from 'package:base':
## 
##     date
```

```r
library("stringr")
library("ggpubr")
```

```
## Loading required package: magrittr
```

```
## 
## Attaching package: 'magrittr'
```

```
## The following object is masked from 'package:purrr':
## 
##     set_names
```

```
## The following object is masked from 'package:tidyr':
## 
##     extract
```


## Process the data


```r
process_data <- function(update = FALSE){
  if (update == TRUE | file.exists("data/processed/datas.Rda") == FALSE) {
    # store all data in one data frame for easing the feature engineering
    datas <- read_csv("data/raw/train.csv") %>%
      mutate(train = TRUE)
    
    datas <- read_csv("data/raw/test.csv") %>%
      mutate(train = FALSE,
             Survived = NA) %>%
      bind_rows(datas)

    save(datas, file = "data/processed/datas.Rda")
  }

  # clean workspace before exiting
  rm(list = ls())
}
```







# Dataset exploration

## General exploration

First let's take a look at the data with glimpse and summary


```r
glimpse(datas)
```

```
## Observations: 1,309
## Variables: 13
## $ PassengerId <int> 892, 893, 894, 895, 896, 897, 898, 899, 900, 901, ...
## $ Pclass      <int> 3, 3, 2, 3, 3, 3, 3, 2, 3, 3, 3, 1, 1, 2, 1, 2, 2,...
## $ Name        <chr> "Kelly, Mr. James", "Wilkes, Mrs. James (Ellen Nee...
## $ Sex         <chr> "male", "female", "male", "male", "female", "male"...
## $ Age         <dbl> 34.5, 47.0, 62.0, 27.0, 22.0, 14.0, 30.0, 26.0, 18...
## $ SibSp       <int> 0, 1, 0, 0, 1, 0, 0, 1, 0, 2, 0, 0, 1, 1, 1, 1, 0,...
## $ Parch       <int> 0, 0, 0, 0, 1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0,...
## $ Ticket      <chr> "330911", "363272", "240276", "315154", "3101298",...
## $ Fare        <dbl> 7.8292, 7.0000, 9.6875, 8.6625, 12.2875, 9.2250, 7...
## $ Cabin       <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, "B...
## $ Embarked    <chr> "Q", "S", "Q", "S", "S", "S", "Q", "S", "C", "S", ...
## $ train       <lgl> FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, F...
## $ Survived    <int> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA...
```

```r
summary(datas)
```

```
##   PassengerId       Pclass          Name               Sex           
##  Min.   :   1   Min.   :1.000   Length:1309        Length:1309       
##  1st Qu.: 328   1st Qu.:2.000   Class :character   Class :character  
##  Median : 655   Median :3.000   Mode  :character   Mode  :character  
##  Mean   : 655   Mean   :2.295                                        
##  3rd Qu.: 982   3rd Qu.:3.000                                        
##  Max.   :1309   Max.   :3.000                                        
##                                                                      
##       Age            SibSp            Parch          Ticket         
##  Min.   : 0.17   Min.   :0.0000   Min.   :0.000   Length:1309       
##  1st Qu.:21.00   1st Qu.:0.0000   1st Qu.:0.000   Class :character  
##  Median :28.00   Median :0.0000   Median :0.000   Mode  :character  
##  Mean   :29.88   Mean   :0.4989   Mean   :0.385                     
##  3rd Qu.:39.00   3rd Qu.:1.0000   3rd Qu.:0.000                     
##  Max.   :80.00   Max.   :8.0000   Max.   :9.000                     
##  NA's   :263                                                        
##       Fare            Cabin             Embarked           train        
##  Min.   :  0.000   Length:1309        Length:1309        Mode :logical  
##  1st Qu.:  7.896   Class :character   Class :character   FALSE:418      
##  Median : 14.454   Mode  :character   Mode  :character   TRUE :891      
##  Mean   : 33.295                                                        
##  3rd Qu.: 31.275                                                        
##  Max.   :512.329                                                        
##  NA's   :1                                                              
##     Survived     
##  Min.   :0.0000  
##  1st Qu.:0.0000  
##  Median :0.0000  
##  Mean   :0.3838  
##  3rd Qu.:1.0000  
##  Max.   :1.0000  
##  NA's   :418
```

## Renaming the columns

Now we will rename the columns following my personnal guidelines:

* snake_case
* somewhat logical names


```r
datas <- datas %>%
  select(passenger_id    = PassengerId,
         name            = Name,
         ticket_class    = Pclass,
         sex             = Sex,
         age             = Age,
         familly_horiz   = SibSp,
         familly_vert    = Parch,
         ticket_number   = Ticket,
         ticket_fare     = Fare,
         cabin_number    = Cabin,
         embark_port     = Embarked,
         survived        = Survived,
         train)
```

## Familly size

Compute the familly size from the horizontal and vertical familly members


```r
# The familly size is the number of sibbling / spouse (horizontal relationship)
# plus the number of children / parents (vertical relationship)
# plus the passenger
datas <- datas %>%
  mutate(familly_size = familly_horiz + familly_vert + 1)
```

## Cabin's deck

Using the full cabin number when available is meaningless. But from it I can extract the deck.
Warnings :  

* Some passengers have several cabin number but all on the same deck 
* Some cabin numbers begin with "F" then a regular number but it seems it correspond to cabin on the F deck

So for my purpose I will extract the first chararcter of the 'cabin_number'


```r
datas <- datas %>%
  mutate(deck = str_sub(cabin_number, 1, 1))
```

## Names

The name variable contains a lot of information :

* last name
* title
* first name (often the husband one if maried) 
* maiden name between parenthesis (the distinction between first name and maiden familly name is not obvious)
* surname between double quotes
* surname between parenthesis and double quotes (it seems to correspond to more wealthy people)
  
Because of the wide variety of information let's just keep those :

* last name
* title
* presence of a surname 
* presence of a "formal" surname


```r
datas <- datas %>%
  mutate(last_name    = str_extract(name, "^.+,") %>%
                        str_extract("[A-Z][A-Z 'a-z]*"),
         title        = str_extract(name, ", .+[.]") %>%
                        str_extract("[A-Z][A-Z 'a-z]*"),
         surname_rich = !is.na(str_extract(name, "\\(\".*\"\\)")),
         surname_poor = !is.na(str_extract(name, "\".*\"")) & !surname_rich)
```


<!-- biliography -->

---
nocite: |
  @base, @bibtex, @dplyr, @forcats, @ggplot2, @ggpubr, @kableExtra, @knitr, @lubridate, @magrittr, @nvimcom, @purrr, @readr, @stringr, @tibble, @tidyr, @tidyverse
...


# References and R packages used
