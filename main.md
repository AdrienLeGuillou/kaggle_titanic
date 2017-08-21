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



## Read the data



```r
dataset <- read_csv("data/raw/train.csv") %>%
  mutate(train = TRUE)
```

```
## Parsed with column specification:
## cols(
##   PassengerId = col_integer(),
##   Survived = col_integer(),
##   Pclass = col_integer(),
##   Name = col_character(),
##   Sex = col_character(),
##   Age = col_double(),
##   SibSp = col_integer(),
##   Parch = col_integer(),
##   Ticket = col_character(),
##   Fare = col_double(),
##   Cabin = col_character(),
##   Embarked = col_character()
## )
```

```r
dataset <- read_csv("data/raw/test.csv") %>%
  mutate(train = FALSE,
         Survived = NA) %>%
  bind_rows(datas)
```

```
## Parsed with column specification:
## cols(
##   PassengerId = col_integer(),
##   Pclass = col_integer(),
##   Name = col_character(),
##   Sex = col_character(),
##   Age = col_double(),
##   SibSp = col_integer(),
##   Parch = col_integer(),
##   Ticket = col_character(),
##   Fare = col_double(),
##   Cabin = col_character(),
##   Embarked = col_character()
## )
```

```
## Error in eval_bare(dot$expr, dot$env): object 'datas' not found
```
# Dataset exploration

## General exploration

First let's take a look at the data with glimpse and summary


```r
glimpse(datas)
```

```
## Error in glimpse(datas): object 'datas' not found
```

```r
summary(datas)
```

```
## Error in summary(datas): object 'datas' not found
```

## Renaming the columns

Now we will rename the columns following my personnal guidelines:

* snake_case
* somewhat logical names


```r
dataset <- dataset %>%
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

## One by one variable exploration

### Passenger_id


```r
# do not redo summary and glimpse. Just meaningfull explo with explaination
summary(dataset$passenger_id)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##     1.0   223.5   446.0   446.0   668.5   891.0
```

## Familly size

Compute the familly size from the horizontal and vertical familly members
The familly size is the number of sibbling / spouse (horizontal relationship) 
plus the number of children / parents (vertical relationship) 
plus the passenger himself


```r
dataset <- dataset %>%
  mutate(familly_size = familly_horiz + familly_vert + 1)
```

## Cabin's deck

Using the full cabin number when available is meaningless. But from it I can extract the deck.
Warnings :  

* Some passengers have several cabin number but all on the same deck 
* Some cabin numbers begin with "F" then a regular number but it seems it correspond to cabin on the F deck

So for my purpose I will extract the first chararcter of the 'cabin_number'


```r
dataset <- dataset %>%
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
dataset <- dataset %>%
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
