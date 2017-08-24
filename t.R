pck <- c("knitr",
"kableExtra",
"bibtex",
"tidyverse",
"forcats",
"lubridate",
"stringr",
"ggpubr",
"ggmosaic",
"broom",
"caret",
"ranger",
"gbm",
"doParallel",
"GGally")

lapply(pck, install.packages)


dataset %>% 
  #mutate_at(vars(sex, set, cabin_number, embark_port), funs(as.factor)) %>%
  mutate_if(is.character, funs(as.factor)) %>%
  mutate_if(is.factor, funs(as.integer)) %>% 
  ggcorr(label = TRUE, hjust = 0.75, method = c("pairwise", "spearman"))

dataset %>%
  filter(is.na(ticket_fare))

dataset %>%
  group_by(ticket_class) %>% 
  summarise(median(ticket_fare, na.rm = T))

dataset %>%
  mutate(ticket_fare = 
    replace(ticket_fare,
            which(is.na(ticket_fare)),


prop.table(table(dataset$survived, is.na(dataset$cabin_number)))

qplot(dataset$embark_port)
