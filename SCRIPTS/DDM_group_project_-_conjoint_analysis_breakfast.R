# Loading libraries ------
# load required libraries
library(conjoint)
library(dplyr)
library(purrr)
library(readxl)
library(usethis)
library(ggplot2)
library(knitr)
library(googlesheets4)
library(forcats)
library(tidyr)
library(gridExtra)
library(tidytext)
library(tidyr)
library(wordcloud)
library(reshape2)
library(syuzhet)
library(wordcloud)
library(topicmodels)
library(utf8)
library(readxl)
library(textdata)
library(lubridate)
library(plotly)
library(GGally)
library(factoextra)
library(cluster)

# source custom functions from Shingo-sensei
source("SCRIPTS/conjoint_tool_new.R")
source("SCRIPTS/cltool_new.R")


# Defining Atrributes ------
# define the attributes
attribute <- list(
  Price = c("200", "400", "600"),
  Form = c("Bread and Pastries","Onigiri","Protein sticks"),
  PurchaseLocation = c("On Route to work","Within 150m", "More than 150m"),
  FunPackaging = c("None", "Attack on Titan", "Doan Ritsu"),
  CognitiveSupport = c("None","Caffeine or Alertness boost",
                       "Natural Mood Stabilizer"),
  DigitalIntegration = c("None", "Loyalty points", "Nutrition Advice")
)


# Full factorial design
profiles <- expand.grid(attribute)

# Orthogonal design
design.o <- caFactorialDesign(data=profiles, type="fractional", cards=13)
print(design.o)

tibble::rowid_to_column(design.o, "ID")
seq.int(nrow(design.o))
brkfst_design <- design.o


# Sanitizing data -----
## Data input -----
## reading the survey data
#Read google sheets data into R
brkfst <- read_excel("DATA/DDM_MCU_survey.xlsx", 
                            sheet = "Form Responses 1")


# check country
brkfst %>% 
  group_by(country) %>% 
  summarize(n = n())

# sanitize county
brkfst <- 
  brkfst %>% 
  mutate(country2 = case_when(country == "TW" ~ "Taiwan",
                              country == "singapore" ~ "Singapore",
                              country == "PHILIPPINES" ~ "Philippines",
                              TRUE ~ country))

# check country2
brkfst %>% 
  group_by(country2) %>% 
  summarize(n = n())


# create region
brkfst <- brkfst %>%
  mutate(region = case_when(
    country2 %in% c("Taiwan", "China", "Japan", "Korea", 
                   "Mongolia", "South Korea") ~ "East Asia",
    country2 %in% c("Singapore", "Malaysia", "Thailand", "Vietnam",
                   "Myanmar", "Philippines" ) ~ "South-East Asia",
    country2 %in% c("India", "Bangladesh") ~ "Indian subcontinent",
    country2 %in% c("Bulgaria", "Romania", "Slovakia",
                   "United States") ~ "Western",
                              TRUE ~ country))
# check region
brkfst %>% 
  group_by(region) %>% 
  summarize(n = n())


# roll up working starting time
brkfst %>% 
  group_by(work_start) %>% 
  summarize(n = n())

brkfst <- brkfst %>% 
  mutate(work_start_cut = 
           case_when(
             work_start %in% c("Before 07:00", "07:00~08:00", "08:00~09:00") ~ "Before 9:00",
             work_start %in% c("09:00~10:00", "After 10:00") ~ "After 9:00"
         ))


brkfst %>% 
  group_by(work_start_cut) %>% 
  summarize(n = n())


# roll up exercise per week
brkfst <- brkfst %>% 
  mutate(exercise_per_week_cut = 
           case_when(
             exercise_per_week %in% c("< 1") ~ "Never",
             exercise_per_week %in% c("1-3", "4-5") ~ "1-5 times"
           ))


# ratings cut
brkfst_rating <- 
  brkfst %>% 
  select(starts_with("PROD"))


# Generating Partworths -----
brkfst_part <- ca.part.util(brkfst_rating ,brkfst_design, attribute)
head(brkfst_part, 1)


## Combine parthworth-demographics -----
### combine parth worth utilities with main data frame
brkfst_cmb <- 
  cbind(brkfst, brkfst_part) %>% 
  ### deal with duplicated 'none' column
  rename(L_10_None = 39,
         L_13_None = 42,
         L_16_None = 45,
  )

### rename with long ordered columns
brkfst_cmb <- 
  brkfst_cmb %>% 
  rename(L_1_200 = 30 ,
         L_2_400 = 31,
         L_3_600 = 32, 
         `L_4_Bread and Pastries` = 33,
         L_5_Onigiri = 34,
         `L_6_Protein sticks` = 35,
         `L_7_On Route to work` = 36,
         `L_8_Within 150m` = 37,
         `L_9_More than 150m` = 38,
         L_10_None = 39,
         `L_11_Attack on Titan` = 40 ,
         `L_12_Doan Ritsu` = 41,
         `L_13_None`= 42,
         `L_14_Caffeine or Alertness boost` = 43,
         `L_15_Natural Mood Stabilizer` = 44,
         L_16_None = 45 ,
         `L_17_Loyalty points` = 46,
         `L_18_Nutrition Advice` = 47
  )

## Compute utilities ----
#overall average utilities
average_utility <- colMeans(brkfst_part)
average_utility_df <- data.frame(level = names(average_utility),
           avg_utility = average_utility)

average_utility_df <- 
  average_utility_df %>% 
  filter(level != "intercept")

average_utility_df <- 
  average_utility_df %>% 
  mutate(group =
           c("Price", "Price", "Price",
             "Form", "Form", "Form",
             "PurchaseLocation", "PurchaseLocation", "PurchaseLocation",
             "FunPackaging", "FunPackaging", "FunPackaging",
             "CognitiveSupport", "CognitiveSupport","CognitiveSupport",
             "DigitalIntegration", "DigitalIntegration", "DigitalIntegration"
             )) %>% 
  mutate(level_fct = factor(level)) %>% 
  mutate(rank = seq.int(1:length(average_utility_df$level))) %>% 
  mutate(order_rnk = level, rank) %>% 
  mutate(space_char = rep(c("L"), length(average_utility_df$level))) %>% 
  unite("lng_level", c("space_char","rank","level"))
  
average_utility_df <- average_utility_df %>% 
  mutate(rank = seq.int(1:length(average_utility_df$level))) %>% 
  mutate(lng_level = as.factor(lng_level)) %>% 
  mutate(lng_level = fct_reorder(lng_level, rank )) %>% 
  mutate(group_fct = factor(group, levels = c("Price", "Form", "PurchaseLocation",
                            "FunPackaging", "CognitiveSupport", "DigitalIntegration"
                            )))


ggplot(average_utility_df, aes(x=lng_level, y = avg_utility
                               ,fill = group_fct
))+
  geom_col() +
  coord_flip() + 
  scale_x_discrete(limits = rev(levels(average_utility_df$lng_level))) +
  geom_text(aes(label = format(round(avg_utility, 2), nsmall = 2)), vjust = -0.5) +
  ggtitle(paste("All sample", length(brkfst$nickname), sep = ", n=")) +
  guides(fill="none")
  


## all sample parthworth utilities - sorted descending
plot_list <- list()
n_obs = length(brkfst_cmb$nickname)
iter = "All sample"

  brkfst_desc_slice <-  brkfst_cmb %>% 
    mutate(item_col = iter) %>% 
    group_by(item_col) %>% 
    summarise_at(vars(L_1_200:`L_18_Nutrition Advice`),
                 mean , na.rm = TRUE) %>% 
    pivot_longer(-item_col) %>% 
    mutate(group_aggr =
             c("Price", "Price", "Price",
               "Form", "Form", "Form",
               "PurchaseLocation", "PurchaseLocation", "PurchaseLocation",
               "FunPackaging", "FunPackaging", "FunPackaging",
               "CognitiveSupport", "CognitiveSupport","CognitiveSupport",
               "DigitalIntegration", "DigitalIntegration", "DigitalIntegration"
             )) %>%
    mutate(group_aggr = 
             factor(group_aggr, levels = c("Price", "Form", "PurchaseLocation",
                                           "FunPackaging", "CognitiveSupport", "DigitalIntegration"
             ))) %>% 
    arrange(desc(value)) %>% 
    ungroup

    ggplot(brkfst_desc_slice, 
                           aes(x= reorder(name,value), y = value
                               ,fill = group_aggr
                           ))+
    geom_col() +
    coord_flip() + 
    #scale_x_discrete(limits = rev(levels(average_utility_df$lng_level))) +
    geom_text(aes(label = format(round(value, 2), nsmall = 2)), vjust = -0.5) +
    ggtitle(paste(iter, n_obs, sep = ", n=")) +
    #guides(fill="none") +
    theme(legend.position="bottom")



# Importance -----
# Overall
ca.importance(brkfst_part, attribute)
# First respondent
ca.importance(brkfst_part[1,], attribute)
# First five respondents as a group
ca.importance(brkfst_part[brkfst$country=="Japan",], attribute)

## plot importance overall
sub_importance <- 
  ca.importance(brkfst_part, attribute)

sub_importance_df <- data.frame(
  sub_importance = sub_importance,
  attribute_txt = names(sub_importance)
)
sub_importance_df <- sub_importance_df %>% 
  mutate(group_fct = factor(attribute_txt, levels = c("Price", "Form", "PurchaseLocation",
                                                      "FunPackaging", "CognitiveSupport", "DigitalIntegration"
  )))

ggplot(sub_importance_df, aes(x=group_fct, y = sub_importance,
                              fill=group_fct)) +
  geom_col() +
  coord_flip() +
  scale_x_discrete(limits = rev(levels(sub_importance_df$group_fct))) +
  geom_text(aes(label = format(round(sub_importance, 2), nsmall = 2)), vjust = -0.5) +
  ggtitle(paste("All sample", length(brkfst$nickname), sep = ", n=")) +
  guides(fill="none")




# Exploratory Subgroups  ------
## gender ------
# gender - computing parthworth
plot_list <- list()
var_by = brkfst$gender
attr_levels <- unique(var_by)

for (i in seq_along(attr_levels)) {
  
  iter = attr_levels[i]
  n_obs = length(brkfst_part[var_by == iter,1])
  
  average_utility <- colMeans(brkfst_part[var_by==iter,,drop=FALSE])
  average_utility_df <- data.frame(level = names(average_utility),
                                   avg_utility = average_utility)
  
  average_utility_df <- 
    average_utility_df %>% 
    filter(level != "intercept")
  
  average_utility_df <- 
    average_utility_df %>% 
    mutate(group =
             c("Price", "Price", "Price",
               "Form", "Form", "Form",
               "PurchaseLocation", "PurchaseLocation", "PurchaseLocation",
               "FunPackaging", "FunPackaging", "FunPackaging",
               "CognitiveSupport", "CognitiveSupport","CognitiveSupport",
               "DigitalIntegration", "DigitalIntegration", "DigitalIntegration"
             )) %>% 
    mutate(level_fct = factor(level)) %>% 
    mutate(rank = seq.int(1:length(average_utility_df$level))) %>% 
    mutate(order_rnk = level, rank) %>% 
    mutate(space_char = rep(c("L"), length(average_utility_df$level))) %>% 
    unite("lng_level", c("space_char","rank","level"))
  
  average_utility_df <- average_utility_df %>% 
    mutate(rank = seq.int(1:length(average_utility_df$level))) %>% 
    mutate(lng_level = as.factor(lng_level)) %>% 
    mutate(lng_level = fct_reorder(lng_level, rank )) %>% 
    mutate(group_fct = factor(group, levels = c("Price", "Form", "PurchaseLocation",
                                                "FunPackaging", "CognitiveSupport", "DigitalIntegration"
    )))
  
  
  plot_list[[i]] <- ggplot(average_utility_df, aes(x=lng_level, y = avg_utility
                                                   ,fill = group_fct
  ))+
    geom_col() +
    coord_flip() + 
    scale_x_discrete(limits = rev(levels(average_utility_df$lng_level))) +
    geom_text(aes(label = format(round(avg_utility, 2), nsmall = 2)), vjust = -0.5) +
    ggtitle(paste(iter, n_obs, sep = ", n=")) +
    guides(fill="none")
}
grid.arrange(grobs=plot_list,ncol=2)

# gender- sorted descending
plot_list <- list()
var_by = brkfst_cmb$gender
attr_levels <- unique(var_by)


for (i in seq_along(attr_levels)) {
  
  iter = attr_levels[i]
  n_obs = length(brkfst_cmb[var_by == iter,1])
  
  brkfst_desc_slice <-  brkfst_cmb[var_by == iter,] %>% 
    mutate(item_col = iter) %>% 
    group_by(item_col) %>% 
    summarise_at(vars(L_1_200:`L_18_Nutrition Advice`),
                 mean , na.rm = TRUE) %>% 
    pivot_longer(-item_col) %>% 
    mutate(group_aggr =
             c("Price", "Price", "Price",
               "Form", "Form", "Form",
               "PurchaseLocation", "PurchaseLocation", "PurchaseLocation",
               "FunPackaging", "FunPackaging", "FunPackaging",
               "CognitiveSupport", "CognitiveSupport","CognitiveSupport",
               "DigitalIntegration", "DigitalIntegration", "DigitalIntegration"
             )) %>%
    mutate(group_aggr = 
             factor(group_aggr, levels = c("Price", "Form", "PurchaseLocation",
                                           "FunPackaging", "CognitiveSupport", "DigitalIntegration"
             ))) %>% 
    arrange(desc(value)) %>% 
    ungroup
  
  #kable(brkfst_desc_slice)
  
  
  plot_list[[i]] <- ggplot(brkfst_desc_slice, 
                           aes(x= reorder(name,value), y = value
                               ,fill = group_aggr
                           ))+
    geom_col() +
    coord_flip() + 
    #scale_x_discrete(limits = rev(levels(average_utility_df$lng_level))) +
    geom_text(aes(label = format(round(value, 2), nsmall = 2)), vjust = -0.5) +
    ggtitle(paste(iter, n_obs, sep = ", n=")) +
    guides(fill="none")
}
grid.arrange(grobs=plot_list,ncol=2)


#### gender - importance
plot_list <- list()
var_by = brkfst$gender
attr_levels <- unique(var_by)

for (i in seq_along(attr_levels)) {
  
  iter = attr_levels[i]
  n_obs = length(brkfst_part[var_by == iter,1])
  
  sub_importance <- 
    ca.importance(brkfst_part[var_by == iter,], attribute)
  
  sub_importance_df <- data.frame(
    sub_importance = sub_importance,
    attribute_txt = names(sub_importance)
  )
  sub_importance_df <- sub_importance_df %>% 
    mutate(group_fct = factor(attribute_txt, levels = c("Price", "Form", "PurchaseLocation",
                                                        "FunPackaging", "CognitiveSupport", "DigitalIntegration"
    )))
  
  plot_list[[i]] <- ggplot(sub_importance_df, aes(x=group_fct, y = sub_importance,
                                                  fill=group_fct)) +
    geom_col() +
    coord_flip() +
    scale_x_discrete(limits = rev(levels(sub_importance_df$group_fct))) +
    geom_text(aes(label = format(round(sub_importance, 2), nsmall = 2)), vjust = -0.5) +
    ggtitle(paste(iter, n_obs, sep = ", n=")) +
    guides(fill="none")
}

grid.arrange(grobs=plot_list,ncol=2)





## region ----- 
# region - segmenting part-worth utilities
plot_list <- list()
var_by = brkfst$region
attr_levels <- unique(var_by)

for (i in seq_along(attr_levels)) {
  
  iter = attr_levels[i]
  n_obs = length(brkfst_part[var_by == iter,1])
  
  average_utility <- colMeans(brkfst_part[var_by==iter,,drop=FALSE])
  average_utility_df <- data.frame(level = names(average_utility),
                                   avg_utility = average_utility)
  
  average_utility_df <- 
    average_utility_df %>% 
    filter(level != "intercept")
  
  average_utility_df <- 
    average_utility_df %>% 
    mutate(group =
             c("Price", "Price", "Price",
               "Form", "Form", "Form",
               "PurchaseLocation", "PurchaseLocation", "PurchaseLocation",
               "FunPackaging", "FunPackaging", "FunPackaging",
               "CognitiveSupport", "CognitiveSupport","CognitiveSupport",
               "DigitalIntegration", "DigitalIntegration", "DigitalIntegration"
             )) %>% 
    mutate(level_fct = factor(level)) %>% 
    mutate(rank = seq.int(1:length(average_utility_df$level))) %>% 
    mutate(order_rnk = level, rank) %>% 
    mutate(space_char = rep(c("L"), length(average_utility_df$level))) %>% 
    unite("lng_level", c("space_char","rank","level"))
  
  average_utility_df <- average_utility_df %>% 
    mutate(rank = seq.int(1:length(average_utility_df$level))) %>% 
    mutate(lng_level = as.factor(lng_level)) %>% 
    mutate(lng_level = fct_reorder(lng_level, rank )) %>% 
    mutate(group_fct = factor(group, levels = c("Price", "Form", "PurchaseLocation",
                                                "FunPackaging", "CognitiveSupport", "DigitalIntegration"
    )))
  
  
  plot_list[[i]] <- ggplot(average_utility_df, aes(x=lng_level, y = avg_utility
                                                   ,fill = group_fct
  ))+
    geom_col() +
    coord_flip() + 
    scale_x_discrete(limits = rev(levels(average_utility_df$lng_level))) +
    geom_text(aes(label = format(round(avg_utility, 2), nsmall = 2)), vjust = -0.5) +
    ggtitle(paste(iter, n_obs, sep = ", n=")) +
    guides(fill="none")
}
grid.arrange(grobs=plot_list,ncol=2)


# region - sorted descending
plot_list <- list()
var_by = brkfst_cmb$region
attr_levels <- unique(var_by)


for (i in seq_along(attr_levels)) {
  
  iter = attr_levels[i]
  n_obs = length(brkfst_cmb[var_by == iter,1])
  
  brkfst_desc_slice <-  brkfst_cmb[var_by == iter,] %>% 
    mutate(item_col = iter) %>% 
    group_by(item_col) %>% 
    summarise_at(vars(L_1_200:`L_18_Nutrition Advice`),
                 mean , na.rm = TRUE) %>% 
    pivot_longer(-item_col) %>% 
    mutate(group_aggr =
             c("Price", "Price", "Price",
               "Form", "Form", "Form",
               "PurchaseLocation", "PurchaseLocation", "PurchaseLocation",
               "FunPackaging", "FunPackaging", "FunPackaging",
               "CognitiveSupport", "CognitiveSupport","CognitiveSupport",
               "DigitalIntegration", "DigitalIntegration", "DigitalIntegration"
             )) %>%
    mutate(group_aggr = 
             factor(group_aggr, levels = c("Price", "Form", "PurchaseLocation",
                                           "FunPackaging", "CognitiveSupport", "DigitalIntegration"
             ))) %>% 
    arrange(desc(value)) %>% 
    ungroup
  
  #kable(brkfst_desc_slice)
  
  
  plot_list[[i]] <- ggplot(brkfst_desc_slice, 
                           aes(x= reorder(name,value), y = value
                               ,fill = group_aggr
                           ))+
    geom_col() +
    coord_flip() + 
    #scale_x_discrete(limits = rev(levels(average_utility_df$lng_level))) +
    geom_text(aes(label = format(round(value, 2), nsmall = 2)), vjust = -0.5) +
    ggtitle(paste(iter, n_obs, sep = ", n=")) +
    guides(fill="none")
}
grid.arrange(grobs=plot_list,ncol=2)


## region - importance
plot_list <- list()
var_by = brkfst$region
attr_levels <- unique(var_by)

for (i in seq_along(attr_levels)) {
  
  iter = attr_levels[i]
  n_obs = length(brkfst_part[var_by == iter,1])
  
  sub_importance <- 
    ca.importance(brkfst_part[var_by == iter,], attribute)
  
  sub_importance_df <- data.frame(
    sub_importance = sub_importance,
    attribute_txt = names(sub_importance)
  )
  sub_importance_df <- sub_importance_df %>% 
    mutate(group_fct = factor(attribute_txt, levels = c("Price", "Form", "PurchaseLocation",
                                                        "FunPackaging", "CognitiveSupport", "DigitalIntegration"
    )))
  
  plot_list[[i]] <- ggplot(sub_importance_df, aes(x=group_fct, y = sub_importance,
                                                  fill=group_fct)) +
    geom_col() +
    coord_flip() +
    scale_x_discrete(limits = rev(levels(sub_importance_df$group_fct))) +
    geom_text(aes(label = format(round(sub_importance, 2), nsmall = 2)), vjust = -0.5) +
    ggtitle(paste(iter, n_obs, sep = ", n=")) +
    guides(fill="none")
}

grid.arrange(grobs=plot_list,ncol=2)






## age ----- 
#segmenting part-worth utilities

plot_list <- list()
var_by = brkfst$age
attr_levels <- unique(var_by)

for (i in seq_along(attr_levels)) {
  
  iter = attr_levels[i]
  n_obs = length(brkfst_part[var_by == iter,1])
  
  average_utility <- colMeans(brkfst_part[var_by==iter,,drop=FALSE])
  average_utility_df <- data.frame(level = names(average_utility),
                                   avg_utility = average_utility)
  
  average_utility_df <- 
    average_utility_df %>% 
    filter(level != "intercept")
  
  average_utility_df <- 
    average_utility_df %>% 
    mutate(group =
             c("Price", "Price", "Price",
               "Form", "Form", "Form",
               "PurchaseLocation", "PurchaseLocation", "PurchaseLocation",
               "FunPackaging", "FunPackaging", "FunPackaging",
               "CognitiveSupport", "CognitiveSupport","CognitiveSupport",
               "DigitalIntegration", "DigitalIntegration", "DigitalIntegration"
             )) %>% 
    mutate(level_fct = factor(level)) %>% 
    mutate(rank = seq.int(1:length(average_utility_df$level))) %>% 
    mutate(order_rnk = level, rank) %>% 
    mutate(space_char = rep(c("L"), length(average_utility_df$level))) %>% 
    unite("lng_level", c("space_char","rank","level"))
  
  average_utility_df <- average_utility_df %>% 
    mutate(rank = seq.int(1:length(average_utility_df$level))) %>% 
    mutate(lng_level = as.factor(lng_level)) %>% 
    mutate(lng_level = fct_reorder(lng_level, rank )) %>% 
    mutate(group_fct = factor(group, levels = c("Price", "Form", "PurchaseLocation",
                                                "FunPackaging", "CognitiveSupport", "DigitalIntegration"
    )))
  
  
  plot_list[[i]] <- ggplot(average_utility_df, aes(x=lng_level, y = avg_utility
                                                   ,fill = group_fct
  ))+
    geom_col() +
    coord_flip() + 
    scale_x_discrete(limits = rev(levels(average_utility_df$lng_level))) +
    geom_text(aes(label = format(round(avg_utility, 2), nsmall = 2)), vjust = -0.5) +
    ggtitle(paste(iter, n_obs, sep = ", n=")) +
    guides(fill="none")
}
grid.arrange(grobs=plot_list,ncol=2)


# age - desceding order
plot_list <- list()
var_by = brkfst_cmb$age
attr_levels <- unique(var_by)


for (i in seq_along(attr_levels)) {
  
  iter = attr_levels[i]
  n_obs = length(brkfst_cmb[var_by == iter,1])
  
  brkfst_desc_slice <-  brkfst_cmb[var_by == iter,] %>% 
    mutate(item_col = iter) %>% 
    group_by(item_col) %>% 
    summarise_at(vars(L_1_200:`L_18_Nutrition Advice`),
                 mean , na.rm = TRUE) %>% 
    pivot_longer(-item_col) %>% 
    mutate(group_aggr =
             c("Price", "Price", "Price",
               "Form", "Form", "Form",
               "PurchaseLocation", "PurchaseLocation", "PurchaseLocation",
               "FunPackaging", "FunPackaging", "FunPackaging",
               "CognitiveSupport", "CognitiveSupport","CognitiveSupport",
               "DigitalIntegration", "DigitalIntegration", "DigitalIntegration"
             )) %>%
    mutate(group_aggr = 
             factor(group_aggr, levels = c("Price", "Form", "PurchaseLocation",
                                           "FunPackaging", "CognitiveSupport", "DigitalIntegration"
             ))) %>% 
    arrange(desc(value)) %>% 
    ungroup
  
  #kable(brkfst_desc_slice)
  
  
  plot_list[[i]] <- ggplot(brkfst_desc_slice, 
                           aes(x= reorder(name,value), y = value
                               ,fill = group_aggr
                           ))+
    geom_col() +
    coord_flip() + 
    #scale_x_discrete(limits = rev(levels(average_utility_df$lng_level))) +
    geom_text(aes(label = format(round(value, 2), nsmall = 2)), vjust = -0.5) +
    ggtitle(paste(iter, n_obs, sep = ", n=")) +
    guides(fill="none")
}
grid.arrange(grobs=plot_list,ncol=2)

## age - importance
plot_list <- list()
var_by = brkfst$age
attr_levels <- unique(var_by)

for (i in seq_along(attr_levels)) {
  
  iter = attr_levels[i]
  n_obs = length(brkfst_part[var_by == iter,1])
  
  sub_importance <- 
    ca.importance(brkfst_part[var_by == iter,], attribute)
  
  sub_importance_df <- data.frame(
    sub_importance = sub_importance,
    attribute_txt = names(sub_importance)
  )
  sub_importance_df <- sub_importance_df %>% 
    mutate(group_fct = factor(attribute_txt, levels = c("Price", "Form", "PurchaseLocation",
                                                        "FunPackaging", "CognitiveSupport", "DigitalIntegration"
    )))
  
  plot_list[[i]] <- ggplot(sub_importance_df, aes(x=group_fct, y = sub_importance,
                                                  fill=group_fct)) +
    geom_col() +
    coord_flip() +
    scale_x_discrete(limits = rev(levels(sub_importance_df$group_fct))) +
    geom_text(aes(label = format(round(sub_importance, 2), nsmall = 2)), vjust = -0.5) +
    ggtitle(paste(iter, n_obs, sep = ", n =")) +
    guides(fill="none")
}

grid.arrange(grobs=plot_list,ncol=2)



## living with partner -----
#segmenting part-worth utilities
plot_list <- list()
var_by = brkfst$living_with_partner
attr_levels <- unique(var_by)

for (i in seq_along(attr_levels)) {
  
  iter = attr_levels[i]
  n_obs = length(brkfst_part[var_by == iter,1])
  
  average_utility <- colMeans(brkfst_part[var_by==iter,,drop=FALSE])
  average_utility_df <- data.frame(level = names(average_utility),
                                   avg_utility = average_utility)
  
  average_utility_df <- 
    average_utility_df %>% 
    filter(level != "intercept")
  
  average_utility_df <- 
    average_utility_df %>% 
    mutate(group =
             c("Price", "Price", "Price",
               "Form", "Form", "Form",
               "PurchaseLocation", "PurchaseLocation", "PurchaseLocation",
               "FunPackaging", "FunPackaging", "FunPackaging",
               "CognitiveSupport", "CognitiveSupport","CognitiveSupport",
               "DigitalIntegration", "DigitalIntegration", "DigitalIntegration"
             )) %>% 
    mutate(level_fct = factor(level)) %>% 
    mutate(rank = seq.int(1:length(average_utility_df$level))) %>% 
    mutate(order_rnk = level, rank) %>% 
    mutate(space_char = rep(c("L"), length(average_utility_df$level))) %>% 
    unite("lng_level", c("space_char","rank","level"))
  
  average_utility_df <- average_utility_df %>% 
    mutate(rank = seq.int(1:length(average_utility_df$level))) %>% 
    mutate(lng_level = as.factor(lng_level)) %>% 
    mutate(lng_level = fct_reorder(lng_level, rank )) %>% 
    mutate(group_fct = factor(group, levels = c("Price", "Form", "PurchaseLocation",
                                                "FunPackaging", "CognitiveSupport", "DigitalIntegration"
    )))
  
  
  plot_list[[i]] <- ggplot(average_utility_df, aes(x=lng_level, y = avg_utility
                                                   ,fill = group_fct
  ))+
    geom_col() +
    coord_flip() + 
    scale_x_discrete(limits = rev(levels(average_utility_df$lng_level))) +
    geom_text(aes(label = format(round(avg_utility, 2), nsmall = 2)), vjust = -0.5) +
    ggtitle(paste(iter, n_obs, sep = ", n=")) +
    guides(fill="none")
}
grid.arrange(grobs=plot_list,ncol=2)


# living with partner - sorted descending
plot_list <- list()
var_by = brkfst_cmb$living_with_partner
attr_levels <- unique(var_by)


for (i in seq_along(attr_levels)) {
  
  iter = attr_levels[i]
  n_obs = length(brkfst_cmb[var_by == iter,1])
  
  brkfst_desc_slice <-  brkfst_cmb[var_by == iter,] %>% 
    mutate(item_col = iter) %>% 
    group_by(item_col) %>% 
    summarise_at(vars(L_1_200:`L_18_Nutrition Advice`),
                 mean , na.rm = TRUE) %>% 
    pivot_longer(-item_col) %>% 
    mutate(group_aggr =
             c("Price", "Price", "Price",
               "Form", "Form", "Form",
               "PurchaseLocation", "PurchaseLocation", "PurchaseLocation",
               "FunPackaging", "FunPackaging", "FunPackaging",
               "CognitiveSupport", "CognitiveSupport","CognitiveSupport",
               "DigitalIntegration", "DigitalIntegration", "DigitalIntegration"
             )) %>%
    mutate(group_aggr = 
             factor(group_aggr, levels = c("Price", "Form", "PurchaseLocation",
                                           "FunPackaging", "CognitiveSupport", "DigitalIntegration"
             ))) %>% 
    arrange(desc(value)) %>% 
    ungroup
  
  #kable(brkfst_desc_slice)
  
  
  plot_list[[i]] <- ggplot(brkfst_desc_slice, 
                           aes(x= reorder(name,value), y = value
                               ,fill = group_aggr
                           ))+
    geom_col() +
    coord_flip() + 
    #scale_x_discrete(limits = rev(levels(average_utility_df$lng_level))) +
    geom_text(aes(label = format(round(value, 2), nsmall = 2)), vjust = -0.5) +
    ggtitle(paste(iter, n_obs, sep = ", n=")) +
    guides(fill="none")
}
grid.arrange(grobs=plot_list,ncol=2)


## by living with partner - importance
plot_list <- list()
var_by = brkfst$living_with_partner
attr_levels <- unique(var_by)

for (i in seq_along(attr_levels)) {
  
  iter = attr_levels[i]
  n_obs = length(brkfst_part[var_by == iter,1])
  
  sub_importance <- 
    ca.importance(brkfst_part[var_by == iter,], attribute)
  
  sub_importance_df <- data.frame(
    sub_importance = sub_importance,
    attribute_txt = names(sub_importance)
  )
  sub_importance_df <- sub_importance_df %>% 
    mutate(group_fct = factor(attribute_txt, levels = c("Price", "Form", "PurchaseLocation",
                                                        "FunPackaging", "CognitiveSupport", "DigitalIntegration"
    )))
  
  plot_list[[i]] <- ggplot(sub_importance_df, aes(x=group_fct, y = sub_importance,
                                                  fill=group_fct)) +
    geom_col() +
    coord_flip() +
    scale_x_discrete(limits = rev(levels(sub_importance_df$group_fct))) +
    geom_text(aes(label = format(round(sub_importance, 2), nsmall = 2)), vjust = -0.5) +
    ggtitle(paste(iter, n_obs, sep = ", n =")) +
    guides(fill="none")
}

grid.arrange(grobs=plot_list,ncol=2)







## expected salary -----
#segmenting part-worth utilities
plot_list <- list()
var_by = brkfst$expected_salary
attr_levels <- unique(var_by)

for (i in seq_along(attr_levels)) {
  
  iter = attr_levels[i]
  n_obs = length(brkfst_part[var_by == iter,1])
  
  average_utility <- colMeans(brkfst_part[var_by==iter,,drop=FALSE])
  average_utility_df <- data.frame(level = names(average_utility),
                                   avg_utility = average_utility)
  
  average_utility_df <- 
    average_utility_df %>% 
    filter(level != "intercept")
  
  average_utility_df <- 
    average_utility_df %>% 
    mutate(group =
             c("Price", "Price", "Price",
               "Form", "Form", "Form",
               "PurchaseLocation", "PurchaseLocation", "PurchaseLocation",
               "FunPackaging", "FunPackaging", "FunPackaging",
               "CognitiveSupport", "CognitiveSupport","CognitiveSupport",
               "DigitalIntegration", "DigitalIntegration", "DigitalIntegration"
             )) %>% 
    mutate(level_fct = factor(level)) %>% 
    mutate(rank = seq.int(1:length(average_utility_df$level))) %>% 
    mutate(order_rnk = level, rank) %>% 
    mutate(space_char = rep(c("L"), length(average_utility_df$level))) %>% 
    unite("lng_level", c("space_char","rank","level"))
  
  average_utility_df <- average_utility_df %>% 
    mutate(rank = seq.int(1:length(average_utility_df$level))) %>% 
    mutate(lng_level = as.factor(lng_level)) %>% 
    mutate(lng_level = fct_reorder(lng_level, rank )) %>% 
    mutate(group_fct = factor(group, levels = c("Price", "Form", "PurchaseLocation",
                                                "FunPackaging", "CognitiveSupport", "DigitalIntegration"
    )))
  
  
  plot_list[[i]] <- ggplot(average_utility_df, aes(x=lng_level, y = avg_utility
                                                   ,fill = group_fct
  ))+
    geom_col() +
    coord_flip() + 
    scale_x_discrete(limits = rev(levels(average_utility_df$lng_level))) +
    geom_text(aes(label = format(round(avg_utility, 2), nsmall = 2)), vjust = -0.5) +
    ggtitle(paste(iter, n_obs, sep = ", n=")) +
    guides(fill="none")
}
grid.arrange(grobs=plot_list,ncol=2)


# expected salary - sorted descending
plot_list <- list()
var_by = brkfst_cmb$gender
attr_levels <- unique(var_by)


for (i in seq_along(attr_levels)) {
  
  iter = attr_levels[i]
  n_obs = length(brkfst_cmb[var_by == iter,1])
  
  brkfst_desc_slice <-  brkfst_cmb[var_by == iter,] %>% 
    mutate(item_col = iter) %>% 
    group_by(item_col) %>% 
    summarise_at(vars(L_1_200:`L_18_Nutrition Advice`),
                 mean , na.rm = TRUE) %>% 
    pivot_longer(-item_col) %>% 
    mutate(group_aggr =
             c("Price", "Price", "Price",
               "Form", "Form", "Form",
               "PurchaseLocation", "PurchaseLocation", "PurchaseLocation",
               "FunPackaging", "FunPackaging", "FunPackaging",
               "CognitiveSupport", "CognitiveSupport","CognitiveSupport",
               "DigitalIntegration", "DigitalIntegration", "DigitalIntegration"
             )) %>%
    mutate(group_aggr = 
             factor(group_aggr, levels = c("Price", "Form", "PurchaseLocation",
                                           "FunPackaging", "CognitiveSupport", "DigitalIntegration"
             ))) %>% 
    arrange(desc(value)) %>% 
    ungroup
  
  #kable(brkfst_desc_slice)
  
  
  plot_list[[i]] <- ggplot(brkfst_desc_slice, 
                           aes(x= reorder(name,value), y = value
                               ,fill = group_aggr
                           ))+
    geom_col() +
    coord_flip() + 
    #scale_x_discrete(limits = rev(levels(average_utility_df$lng_level))) +
    geom_text(aes(label = format(round(value, 2), nsmall = 2)), vjust = -0.5) +
    ggtitle(paste(iter, n_obs, sep = ", n=")) +
    guides(fill="none")
}
grid.arrange(grobs=plot_list,ncol=2)


## by expected salary - importance
plot_list <- list()
var_by = brkfst$expected_salary
attr_levels <- unique(var_by)

for (i in seq_along(attr_levels)) {
  
  iter = attr_levels[i]
  n_obs = length(brkfst_part[var_by == iter,1])
  
  sub_importance <- 
    ca.importance(brkfst_part[var_by == iter,], attribute)
  
  sub_importance_df <- data.frame(
    sub_importance = sub_importance,
    attribute_txt = names(sub_importance)
  )
  sub_importance_df <- sub_importance_df %>% 
    mutate(group_fct = factor(attribute_txt, levels = c("Price", "Form", "PurchaseLocation",
                                                        "FunPackaging", "CognitiveSupport", "DigitalIntegration"
    )))
  
  plot_list[[i]] <- ggplot(sub_importance_df, aes(x=group_fct, y = sub_importance,
                                                  fill=group_fct)) +
    geom_col() +
    coord_flip() +
    scale_x_discrete(limits = rev(levels(sub_importance_df$group_fct))) +
    geom_text(aes(label = format(round(sub_importance, 2), nsmall = 2)), vjust = -0.5) +
    ggtitle(paste(iter, n_obs, sep = ", n =")) +
    guides(fill="none")
}

grid.arrange(grobs=plot_list,ncol=2)





 
## dietary restriction ----
#segmenting part-worth utilities
plot_list <- list()
var_by = brkfst$dietary_restrictions
attr_levels <- unique(var_by)

for (i in seq_along(attr_levels)) {
  
  iter = attr_levels[i]
  n_obs = length(brkfst_part[var_by == iter,1])
  
  average_utility <- colMeans(brkfst_part[var_by==iter,,drop=FALSE])
  average_utility_df <- data.frame(level = names(average_utility),
                                   avg_utility = average_utility)
  
  average_utility_df <- 
    average_utility_df %>% 
    filter(level != "intercept")
  
  average_utility_df <- 
    average_utility_df %>% 
    mutate(group =
             c("Price", "Price", "Price",
               "Form", "Form", "Form",
               "PurchaseLocation", "PurchaseLocation", "PurchaseLocation",
               "FunPackaging", "FunPackaging", "FunPackaging",
               "CognitiveSupport", "CognitiveSupport","CognitiveSupport",
               "DigitalIntegration", "DigitalIntegration", "DigitalIntegration"
             )) %>% 
    mutate(level_fct = factor(level)) %>% 
    mutate(rank = seq.int(1:length(average_utility_df$level))) %>% 
    mutate(order_rnk = level, rank) %>% 
    mutate(space_char = rep(c("L"), length(average_utility_df$level))) %>% 
    unite("lng_level", c("space_char","rank","level"))
  
  average_utility_df <- average_utility_df %>% 
    mutate(rank = seq.int(1:length(average_utility_df$level))) %>% 
    mutate(lng_level = as.factor(lng_level)) %>% 
    mutate(lng_level = fct_reorder(lng_level, rank )) %>% 
    mutate(group_fct = factor(group, levels = c("Price", "Form", "PurchaseLocation",
                                                "FunPackaging", "CognitiveSupport", "DigitalIntegration"
    )))
  
  
  plot_list[[i]] <- ggplot(average_utility_df, aes(x=lng_level, y = avg_utility
                                                   ,fill = group_fct
  ))+
    geom_col() +
    coord_flip() + 
    scale_x_discrete(limits = rev(levels(average_utility_df$lng_level))) +
    geom_text(aes(label = format(round(avg_utility, 2), nsmall = 2)), vjust = -0.5) +
    ggtitle(paste(iter, n_obs, sep = ", n=")) +
    guides(fill="none")
}
grid.arrange(grobs=plot_list,ncol=2)


# dietary restrictions - sorted descending
plot_list <- list()
var_by = brkfst_cmb$dietary_restrictions
attr_levels <- unique(var_by)


for (i in seq_along(attr_levels)) {
  
  iter = attr_levels[i]
  n_obs = length(brkfst_cmb[var_by == iter,1])
  
  brkfst_desc_slice <-  brkfst_cmb[var_by == iter,] %>% 
    mutate(item_col = iter) %>% 
    group_by(item_col) %>% 
    summarise_at(vars(L_1_200:`L_18_Nutrition Advice`),
                 mean , na.rm = TRUE) %>% 
    pivot_longer(-item_col) %>% 
    mutate(group_aggr =
             c("Price", "Price", "Price",
               "Form", "Form", "Form",
               "PurchaseLocation", "PurchaseLocation", "PurchaseLocation",
               "FunPackaging", "FunPackaging", "FunPackaging",
               "CognitiveSupport", "CognitiveSupport","CognitiveSupport",
               "DigitalIntegration", "DigitalIntegration", "DigitalIntegration"
             )) %>%
    mutate(group_aggr = 
             factor(group_aggr, levels = c("Price", "Form", "PurchaseLocation",
                                           "FunPackaging", "CognitiveSupport", "DigitalIntegration"
             ))) %>% 
    arrange(desc(value)) %>% 
    ungroup
  
  #kable(brkfst_desc_slice)
  
  
  plot_list[[i]] <- ggplot(brkfst_desc_slice, 
                           aes(x= reorder(name,value), y = value
                               ,fill = group_aggr
                           ))+
    geom_col() +
    coord_flip() + 
    #scale_x_discrete(limits = rev(levels(average_utility_df$lng_level))) +
    geom_text(aes(label = format(round(value, 2), nsmall = 2)), vjust = -0.5) +
    ggtitle(paste(iter, n_obs, sep = ", n=")) +
    guides(fill="none")
}
grid.arrange(grobs=plot_list,ncol=2)

## by dietary restriction - importance
plot_list <- list()
var_by = brkfst$dietary_restrictions
attr_levels <- unique(var_by)

for (i in seq_along(attr_levels)) {
  
  iter = attr_levels[i]
  n_obs = length(brkfst_part[var_by == iter,1])
  
  sub_importance <- 
    ca.importance(brkfst_part[var_by == iter,], attribute)
  
  sub_importance_df <- data.frame(
    sub_importance = sub_importance,
    attribute_txt = names(sub_importance)
  )
  sub_importance_df <- sub_importance_df %>% 
    mutate(group_fct = factor(attribute_txt, levels = c("Price", "Form", "PurchaseLocation",
                                                        "FunPackaging", "CognitiveSupport", "DigitalIntegration"
    )))
  
  plot_list[[i]] <- ggplot(sub_importance_df, aes(x=group_fct, y = sub_importance,
                                                  fill=group_fct)) +
    geom_col() +
    coord_flip() +
    scale_x_discrete(limits = rev(levels(sub_importance_df$group_fct))) +
    geom_text(aes(label = format(round(sub_importance, 2), nsmall = 2)), vjust = -0.5) +
    ggtitle(paste(iter, n_obs, sep = ", n =")) +
    guides(fill="none")
}

grid.arrange(grobs=plot_list,ncol=2)







## work_start cut ----
#segmenting part-worth utilities
plot_list <- list()
var_by = brkfst$work_start_cut
attr_levels <- unique(var_by)

for (i in seq_along(attr_levels)) {
  
  iter = attr_levels[i]
  n_obs = length(brkfst_part[var_by == iter,1])
  
  average_utility <- colMeans(brkfst_part[var_by==iter,,drop=FALSE])
  average_utility_df <- data.frame(level = names(average_utility),
                                   avg_utility = average_utility)
  
  average_utility_df <- 
    average_utility_df %>% 
    filter(level != "intercept")
  
  average_utility_df <- 
    average_utility_df %>% 
    mutate(group =
             c("Price", "Price", "Price",
               "Form", "Form", "Form",
               "PurchaseLocation", "PurchaseLocation", "PurchaseLocation",
               "FunPackaging", "FunPackaging", "FunPackaging",
               "CognitiveSupport", "CognitiveSupport","CognitiveSupport",
               "DigitalIntegration", "DigitalIntegration", "DigitalIntegration"
             )) %>% 
    mutate(level_fct = factor(level)) %>% 
    mutate(rank = seq.int(1:length(average_utility_df$level))) %>% 
    mutate(order_rnk = level, rank) %>% 
    mutate(space_char = rep(c("L"), length(average_utility_df$level))) %>% 
    unite("lng_level", c("space_char","rank","level"))
  
  average_utility_df <- average_utility_df %>% 
    mutate(rank = seq.int(1:length(average_utility_df$level))) %>% 
    mutate(lng_level = as.factor(lng_level)) %>% 
    mutate(lng_level = fct_reorder(lng_level, rank )) %>% 
    mutate(group_fct = factor(group, levels = c("Price", "Form", "PurchaseLocation",
                                                "FunPackaging", "CognitiveSupport", "DigitalIntegration"
    )))
  
  
  plot_list[[i]] <- ggplot(average_utility_df, aes(x=lng_level, y = avg_utility
                                                   ,fill = group_fct
  ))+
    geom_col() +
    coord_flip() + 
    scale_x_discrete(limits = rev(levels(average_utility_df$lng_level))) +
    geom_text(aes(label = format(round(avg_utility, 2), nsmall = 2)), vjust = -0.5) +
    ggtitle(paste(iter, n_obs, sep = ", n=")) +
    guides(fill="none")
}
grid.arrange(grobs=plot_list,ncol=2)

## work start cut - sorted descending
plot_list <- list()
var_by = brkfst_cmb$work_start_cut
attr_levels <- unique(var_by)


for (i in seq_along(attr_levels)) {
  
  iter = attr_levels[i]
  n_obs = length(brkfst_cmb[var_by == iter,1])
  
  brkfst_desc_slice <-  brkfst_cmb[var_by == iter,] %>% 
    mutate(item_col = iter) %>% 
    group_by(item_col) %>% 
    summarise_at(vars(L_1_200:`L_18_Nutrition Advice`),
                 mean , na.rm = TRUE) %>% 
    pivot_longer(-item_col) %>% 
    mutate(group_aggr =
             c("Price", "Price", "Price",
               "Form", "Form", "Form",
               "PurchaseLocation", "PurchaseLocation", "PurchaseLocation",
               "FunPackaging", "FunPackaging", "FunPackaging",
               "CognitiveSupport", "CognitiveSupport","CognitiveSupport",
               "DigitalIntegration", "DigitalIntegration", "DigitalIntegration"
             )) %>%
    mutate(group_aggr = 
             factor(group_aggr, levels = c("Price", "Form", "PurchaseLocation",
                                           "FunPackaging", "CognitiveSupport", "DigitalIntegration"
             ))) %>% 
    arrange(desc(value)) %>% 
    ungroup
  
  #kable(brkfst_desc_slice)
  
  
  plot_list[[i]] <- ggplot(brkfst_desc_slice, 
                           aes(x= reorder(name,value), y = value
                               ,fill = group_aggr
                           ))+
    geom_col() +
    coord_flip() + 
    #scale_x_discrete(limits = rev(levels(average_utility_df$lng_level))) +
    geom_text(aes(label = format(round(value, 2), nsmall = 2)), vjust = -0.5) +
    ggtitle(paste(iter, n_obs, sep = ", n=")) +
    guides(fill="none")
}
grid.arrange(grobs=plot_list,ncol=2)


## by work start cut - importance
plot_list <- list()
var_by = brkfst$work_start_cut
attr_levels <- unique(var_by)

for (i in seq_along(attr_levels)) {
  
  iter = attr_levels[i]
  n_obs = length(brkfst_part[var_by == iter,1])
  
  sub_importance <- 
    ca.importance(brkfst_part[var_by == iter,], attribute)
  
  sub_importance_df <- data.frame(
    sub_importance = sub_importance,
    attribute_txt = names(sub_importance)
  )
  sub_importance_df <- sub_importance_df %>% 
    mutate(group_fct = factor(attribute_txt, levels = c("Price", "Form", "PurchaseLocation",
                                                        "FunPackaging", "CognitiveSupport", "DigitalIntegration"
    )))
  
  plot_list[[i]] <- ggplot(sub_importance_df, aes(x=group_fct, y = sub_importance,
                                                  fill=group_fct)) +
    geom_col() +
    coord_flip() +
    scale_x_discrete(limits = rev(levels(sub_importance_df$group_fct))) +
    geom_text(aes(label = format(round(sub_importance, 2), nsmall = 2)), vjust = -0.5) +
    ggtitle(paste(iter, n_obs, sep = ", n =")) +
    guides(fill="none")
}

grid.arrange(grobs=plot_list,ncol=2)


## importance
#### by work start cut (new smaller grouping)
plot_list <- list()
var_by = brkfst$work_start_cut
attr_levels <- unique(var_by)

for (i in seq_along(attr_levels)) {
  
  iter = attr_levels[i]
  n_obs = length(brkfst_part[var_by == iter,1])
  
  sub_importance <- 
    ca.importance(brkfst_part[var_by == iter,], attribute)
  
  sub_importance_df <- data.frame(
    sub_importance = sub_importance,
    attribute_txt = names(sub_importance)
  )
  sub_importance_df <- sub_importance_df %>% 
    mutate(group_fct = factor(attribute_txt, levels = c("Price", "Form", "PurchaseLocation",
                                                        "FunPackaging", "CognitiveSupport", "DigitalIntegration"
    )))
  
  plot_list[[i]] <- ggplot(sub_importance_df, aes(x=group_fct, y = sub_importance,
                                                  fill=group_fct)) +
    geom_col() +
    coord_flip() +
    scale_x_discrete(limits = rev(levels(sub_importance_df$group_fct))) +
    geom_text(aes(label = format(round(sub_importance, 2), nsmall = 2)), vjust = -0.5) +
    ggtitle(paste(iter, n_obs, sep = ", n =")) +
    guides(fill="none")
}

grid.arrange(grobs=plot_list,ncol=2)





## exercise per week cut -----
#segmenting part-worth utilities
plot_list <- list()
var_by = brkfst$exercise_per_week_cut
attr_levels <- unique(var_by)

for (i in seq_along(attr_levels)) {
  
  iter = attr_levels[i]
  n_obs = length(brkfst_part[var_by == iter,1])
  
  average_utility <- colMeans(brkfst_part[var_by==iter,,drop=FALSE])
  average_utility_df <- data.frame(level = names(average_utility),
                                   avg_utility = average_utility)
  
  average_utility_df <- 
    average_utility_df %>% 
    filter(level != "intercept")
  
  average_utility_df <- 
    average_utility_df %>% 
    mutate(group =
             c("Price", "Price", "Price",
               "Form", "Form", "Form",
               "PurchaseLocation", "PurchaseLocation", "PurchaseLocation",
               "FunPackaging", "FunPackaging", "FunPackaging",
               "CognitiveSupport", "CognitiveSupport","CognitiveSupport",
               "DigitalIntegration", "DigitalIntegration", "DigitalIntegration"
             )) %>% 
    mutate(level_fct = factor(level)) %>% 
    mutate(rank = seq.int(1:length(average_utility_df$level))) %>% 
    mutate(order_rnk = level, rank) %>% 
    mutate(space_char = rep(c("L"), length(average_utility_df$level))) %>% 
    unite("lng_level", c("space_char","rank","level"))
  
  average_utility_df <- average_utility_df %>% 
    mutate(rank = seq.int(1:length(average_utility_df$level))) %>% 
    mutate(lng_level = as.factor(lng_level)) %>% 
    mutate(lng_level = fct_reorder(lng_level, rank )) %>% 
    mutate(group_fct = factor(group, levels = c("Price", "Form", "PurchaseLocation",
                                                "FunPackaging", "CognitiveSupport", "DigitalIntegration"
    )))
  
  
  plot_list[[i]] <- ggplot(average_utility_df, aes(x=lng_level, y = avg_utility
                                                   ,fill = group_fct
  ))+
    geom_col() +
    coord_flip() + 
    scale_x_discrete(limits = rev(levels(average_utility_df$lng_level))) +
    geom_text(aes(label = format(round(avg_utility, 2), nsmall = 2)), vjust = -0.5) +
    ggtitle(paste(iter, n_obs, sep = ", n=")) +
    guides(fill="none")
}
grid.arrange(grobs=plot_list,ncol=2)

## exercise per week cut - sorted descending
plot_list <- list()
var_by = brkfst_cmb$exercise_per_week_cut
attr_levels <- unique(var_by)


for (i in seq_along(attr_levels)) {
  
  iter = attr_levels[i]
  n_obs = length(brkfst_cmb[var_by == iter,1])
  
  brkfst_desc_slice <-  brkfst_cmb[var_by == iter,] %>% 
    mutate(item_col = iter) %>% 
    group_by(item_col) %>% 
    summarise_at(vars(L_1_200:`L_18_Nutrition Advice`),
                 mean , na.rm = TRUE) %>% 
    pivot_longer(-item_col) %>% 
    mutate(group_aggr =
             c("Price", "Price", "Price",
               "Form", "Form", "Form",
               "PurchaseLocation", "PurchaseLocation", "PurchaseLocation",
               "FunPackaging", "FunPackaging", "FunPackaging",
               "CognitiveSupport", "CognitiveSupport","CognitiveSupport",
               "DigitalIntegration", "DigitalIntegration", "DigitalIntegration"
             )) %>%
    mutate(group_aggr = 
             factor(group_aggr, levels = c("Price", "Form", "PurchaseLocation",
                                           "FunPackaging", "CognitiveSupport", "DigitalIntegration"
             ))) %>% 
    arrange(desc(value)) %>% 
    ungroup
  
  #kable(brkfst_desc_slice)
  
  
  plot_list[[i]] <- ggplot(brkfst_desc_slice, 
                           aes(x= reorder(name,value), y = value
                               ,fill = group_aggr
                           ))+
    geom_col() +
    coord_flip() + 
    #scale_x_discrete(limits = rev(levels(average_utility_df$lng_level))) +
    geom_text(aes(label = format(round(value, 2), nsmall = 2)), vjust = -0.5) +
    ggtitle(paste(iter, n_obs, sep = ", n=")) +
    guides(fill="none")
}
grid.arrange(grobs=plot_list,ncol=2)



#### by exercise per week - importance
plot_list <- list()
var_by = brkfst$exercise_per_week_cut
attr_levels <- unique(var_by)

for (i in seq_along(attr_levels)) {
  
  iter = attr_levels[i]
  n_obs = length(brkfst_part[var_by == iter,1])
  
  sub_importance <- 
    ca.importance(brkfst_part[var_by == iter,], attribute)
  
  sub_importance_df <- data.frame(
    sub_importance = sub_importance,
    attribute_txt = names(sub_importance)
  )
  sub_importance_df <- sub_importance_df %>% 
    mutate(group_fct = factor(attribute_txt, levels = c("Price", "Form", "PurchaseLocation",
                                                        "FunPackaging", "CognitiveSupport", "DigitalIntegration"
    )))
  
  plot_list[[i]] <- ggplot(sub_importance_df, aes(x=group_fct, y = sub_importance,
                                                  fill=group_fct)) +
    geom_col() +
    coord_flip() +
    scale_x_discrete(limits = rev(levels(sub_importance_df$group_fct))) +
    geom_text(aes(label = format(round(sub_importance, 2), nsmall = 2)), vjust = -0.5) +
    ggtitle(paste(iter, n_obs, sep = ", n =")) +
    guides(fill="none")
}

grid.arrange(grobs=plot_list,ncol=2)



## alcohol per week ----
#segmenting part-worth utilities
plot_list <- list()
var_by = brkfst$alcohol_per_week
attr_levels <- unique(var_by)

for (i in seq_along(attr_levels)) {
  
  iter = attr_levels[i]
  n_obs = length(brkfst_part[var_by == iter,1])
  
  average_utility <- colMeans(brkfst_part[var_by==iter,,drop=FALSE])
  average_utility_df <- data.frame(level = names(average_utility),
                                   avg_utility = average_utility)
  
  average_utility_df <- 
    average_utility_df %>% 
    filter(level != "intercept")
  
  average_utility_df <- 
    average_utility_df %>% 
    mutate(group =
             c("Price", "Price", "Price",
               "Form", "Form", "Form",
               "PurchaseLocation", "PurchaseLocation", "PurchaseLocation",
               "FunPackaging", "FunPackaging", "FunPackaging",
               "CognitiveSupport", "CognitiveSupport","CognitiveSupport",
               "DigitalIntegration", "DigitalIntegration", "DigitalIntegration"
             )) %>% 
    mutate(level_fct = factor(level)) %>% 
    mutate(rank = seq.int(1:length(average_utility_df$level))) %>% 
    mutate(order_rnk = level, rank) %>% 
    mutate(space_char = rep(c("L"), length(average_utility_df$level))) %>% 
    unite("lng_level", c("space_char","rank","level"))
  
  average_utility_df <- average_utility_df %>% 
    mutate(rank = seq.int(1:length(average_utility_df$level))) %>% 
    mutate(lng_level = as.factor(lng_level)) %>% 
    mutate(lng_level = fct_reorder(lng_level, rank )) %>% 
    mutate(group_fct = factor(group, levels = c("Price", "Form", "PurchaseLocation",
                                                "FunPackaging", "CognitiveSupport", "DigitalIntegration"
    )))
  
  
  plot_list[[i]] <- ggplot(average_utility_df, aes(x=lng_level, y = avg_utility
                                                   ,fill = group_fct
  ))+
    geom_col() +
    coord_flip() + 
    scale_x_discrete(limits = rev(levels(average_utility_df$lng_level))) +
    geom_text(aes(label = format(round(avg_utility, 2), nsmall = 2)), vjust = -0.5) +
    ggtitle(paste(iter, n_obs, sep = ", n=")) +
    guides(fill="none")
}
grid.arrange(grobs=plot_list,ncol=2)

## alcohol per week - sorted descending
plot_list <- list()
var_by = brkfst_cmb$alcohol_per_week
attr_levels <- unique(var_by)


for (i in seq_along(attr_levels)) {
  
  iter = attr_levels[i]
  n_obs = length(brkfst_cmb[var_by == iter,1])
  
  brkfst_desc_slice <-  brkfst_cmb[var_by == iter,] %>% 
    mutate(item_col = iter) %>% 
    group_by(item_col) %>% 
    summarise_at(vars(L_1_200:`L_18_Nutrition Advice`),
                 mean , na.rm = TRUE) %>% 
    pivot_longer(-item_col) %>% 
    mutate(group_aggr =
             c("Price", "Price", "Price",
               "Form", "Form", "Form",
               "PurchaseLocation", "PurchaseLocation", "PurchaseLocation",
               "FunPackaging", "FunPackaging", "FunPackaging",
               "CognitiveSupport", "CognitiveSupport","CognitiveSupport",
               "DigitalIntegration", "DigitalIntegration", "DigitalIntegration"
             )) %>%
    mutate(group_aggr = 
             factor(group_aggr, levels = c("Price", "Form", "PurchaseLocation",
                                           "FunPackaging", "CognitiveSupport", "DigitalIntegration"
             ))) %>% 
    arrange(desc(value)) %>% 
    ungroup
  
  #kable(brkfst_desc_slice)
  
  
  plot_list[[i]] <- ggplot(brkfst_desc_slice, 
                           aes(x= reorder(name,value), y = value
                               ,fill = group_aggr
                           ))+
    geom_col() +
    coord_flip() + 
    #scale_x_discrete(limits = rev(levels(average_utility_df$lng_level))) +
    geom_text(aes(label = format(round(value, 2), nsmall = 2)), vjust = -0.5) +
    ggtitle(paste(iter, n_obs, sep = ", n=")) +
    guides(fill="none")
}
grid.arrange(grobs=plot_list,ncol=2)


#### by alcohol per week - importance
plot_list <- list()
var_by = brkfst$alcohol_per_week
attr_levels <- unique(var_by)

for (i in seq_along(attr_levels)) {
  
  iter = attr_levels[i]
  n_obs = length(brkfst_part[var_by == iter,1])
  
  sub_importance <- 
    ca.importance(brkfst_part[var_by == iter,], attribute)
  
  sub_importance_df <- data.frame(
    sub_importance = sub_importance,
    attribute_txt = names(sub_importance)
  )
  sub_importance_df <- sub_importance_df %>% 
    mutate(group_fct = factor(attribute_txt, levels = c("Price", "Form", "PurchaseLocation",
                                                        "FunPackaging", "CognitiveSupport", "DigitalIntegration"
    )))
  
  plot_list[[i]] <- ggplot(sub_importance_df, aes(x=group_fct, y = sub_importance,
                                                  fill=group_fct)) +
    geom_col() +
    coord_flip() +
    scale_x_discrete(limits = rev(levels(sub_importance_df$group_fct))) +
    geom_text(aes(label = format(round(sub_importance, 2), nsmall = 2)), vjust = -0.5) +
    ggtitle(paste(iter, n_obs, sep = ", n =")) +
    guides(fill="none")
}

grid.arrange(grobs=plot_list,ncol=2)



## ============================





# Segmentation -----
## estimation and interpretation -----
head(brkfst_part, 1)
# exclude intercept column
brkfst_part_sg <- data.frame(brkfst_part[,-1])
names(brkfst_part_sg)[10] <- c("No special packaging")
names(brkfst_part_sg)[13] <- c("No cognitive support")
names(brkfst_part_sg)[16] <- c("No digital integration")

# silhouette plot
fviz_nbclust(brkfst_part_sg, kmeans, method="silhouette")

# estimating clusters

brkfst_sg_4 <- k.means(brkfst_part_sg, n.cluster=4)
brkfst_sg_7 <- k.means(brkfst_part_sg, n.cluster=7)

# viewing persons in clusters
brkfst_resegmented <- data.frame(
  cbind(brkfst, cluster = brkfst_sg_4$cluster)
)

brkfst_resegmented %>% 
  filter(cluster==1) %>% 
  select(nickname, country2, cluster)

# Cluster sizes
prop.table(brkfst_sg_4$size)
prop.table(brkfst_sg_7$size)

#Cluster centers
View(brkfst_sg_4$centers)


# interpreting clusters
snake.ch2(brkfst_sg_4)
snake.ch2(brkfst_sg_7)


## cluster plot
#Cluster plot
fviz_cluster(brkfst_sg_4, brkfst_part_sg, ellipse.type = "convex",
             ggtheme = theme_minimal())


## segment composition -----
# For analysis: Keep as separate vectors
brkfst_clst <- cbind(brkfst_cmb, 
                     cluster = brkfst_sg_4$cluster)

cluster1_names <- brkfst_clst[brkfst_clst$cluster == 1, "country2"]
cluster2_names <- brkfst_clst[brkfst_clst$cluster == 2, "country2"]
cluster3_names <- brkfst_clst[brkfst_clst$cluster == 3, "country2"]
cluster4_names <- brkfst_clst[brkfst_clst$cluster == 4, "country"]


# For presentation: Create NA-padded data frame
presentation_df <- data.frame(
  cluster1 = c(cluster1_names, rep(NA, length(cluster1_names) - length(cluster1_names))),
  cluster2 = c(cluster2_names, rep(NA, length(cluster1_names) - length(cluster2_names))),
  cluster3 = c(cluster3_names, rep(NA, length(cluster1_names) - length(cluster3_names))),
  cluster4 = c(cluster4_names, rep(NA, length(cluster1_names) - length(cluster4_names)))
)

kable(presentation_df)



# Trade-off calculation ----
## individual 
### allocate separate object for all trade-off calculation
average_utility <- colMeans(brkfst_part)
average_utility_df_all <- data.frame(level = names(average_utility),
                                     avg_utility = average_utility)

average_utility_df_all <- 
  average_utility_df_all %>% 
  filter(level != "intercept")

average_utility_df_all  <- 
  average_utility_df_all  %>% 
  mutate(group =
           c("Price", "Price", "Price",
             "Form", "Form", "Form",
             "PurchaseLocation", "PurchaseLocation", "PurchaseLocation",
             "FunPackaging", "FunPackaging", "FunPackaging",
             "CognitiveSupport", "CognitiveSupport","CognitiveSupport",
             "DigitalIntegration", "DigitalIntegration", "DigitalIntegration"
           )) %>% 
  mutate(level_fct = factor(level)) %>% 
  mutate(rank = seq.int(1:length(average_utility_df_all$level))) %>% 
  mutate(order_rnk = level, rank) %>% 
  mutate(space_char = rep(c("L"), length(average_utility_df_all$level))) %>% 
  unite("lng_level", c("space_char","rank","level"))

average_utility_df_all <- average_utility_df_all %>% 
  mutate(rank = seq.int(1:length(average_utility_df_all$level))) %>% 
  mutate(lng_level = as.factor(lng_level)) %>% 
  mutate(lng_level = fct_reorder(lng_level, rank )) %>% 
  mutate(group_fct = factor(group, levels = c("Price", "Form", "PurchaseLocation",
                                              "FunPackaging", "CognitiveSupport", "DigitalIntegration"
  )))



# average monetary value of 1 utiliy
# linear utility function
util_linear_value <-  (600-200)/
  (average_utility_df_all[average_utility_df_all$lng_level ==
                            "L_1_200",]$avg_utility-
     average_utility_df_all[average_utility_df_all$lng_level ==
                              "L_3_600",]$avg_utility)
util_linear_value

# piece-wise utility function
util_piecewise_1 <- (600-400)/(average_utility["400"]-average_utility["600"])
util_piecewise_1
util_piecewise_2 <- (400-200)/(average_utility["200"]-average_utility["400"])
util_piecewise_2  

# moving from none to natural mood stabilizers
delta_cognitive_support_none_naturalmood <- 
  (average_utility_df_all[average_utility_df_all$lng_level=="L_15_Natural Mood Stabilizer",]$avg_utility -
     average_utility_df_all[average_utility_df_all$lng_level=="L_13_None",]$avg_utility) * util_linear_value

# moving from none to caffeine or alertness boost
delta_cognitive_support_none_caffeine <- (average_utility_df_all[average_utility_df_all$lng_level=="L_14_Caffeine or Alertness boost",]$avg_utility -
                                            average_utility_df_all[average_utility_df_all$lng_level=="L_13_None",]$avg_utility) * util_linear_value


tri_df_points <- 
  data.frame(
    price = c(200, 600),
    utility = 
      c(average_utility_df_all[average_utility_df_all$lng_level ==
                                 "L_1_200",]$avg_utility,
        average_utility_df_all[average_utility_df_all$lng_level ==
                                 "L_3_600",]$avg_utility)
  )

ggplot(tri_df_points, 
       aes(x = price, y = utility)) +
  geom_point() + geom_line()



# Market Prediction ----
## Commercial considerations ----
## tastes of the Japanese consumer
## partworth - sorted descending For Japan
iter = "Japan"
n_obs = length(brkfst_cmb[brkfst_cmb$country2 == iter,1])

brkfst_desc_slice <-  brkfst_cmb[brkfst_cmb$country2 == iter,] %>% 
  mutate(item_col = iter) %>% 
  group_by(item_col) %>% 
  summarise_at(vars(L_1_200:`L_18_Nutrition Advice`),
               mean , na.rm = TRUE) %>% 
  pivot_longer(-item_col) %>% 
  mutate(group_aggr =
           c("Price", "Price", "Price",
             "Form", "Form", "Form",
             "PurchaseLocation", "PurchaseLocation", "PurchaseLocation",
             "FunPackaging", "FunPackaging", "FunPackaging",
             "CognitiveSupport", "CognitiveSupport","CognitiveSupport",
             "DigitalIntegration", "DigitalIntegration", "DigitalIntegration"
           )) %>%
  mutate(group_aggr = 
           factor(group_aggr, levels = c("Price", "Form", "PurchaseLocation",
                                         "FunPackaging", "CognitiveSupport", "DigitalIntegration"
           ))) %>% 
  arrange(desc(value)) %>% 
  ungroup

ggplot(brkfst_desc_slice, 
       aes(x= reorder(name,value), y = value
           ,fill = group_aggr
       ))+
  geom_col() +
  coord_flip() + 
  #scale_x_discrete(limits = rev(levels(average_utility_df$lng_level))) +
  geom_text(aes(label = format(round(value, 2), nsmall = 2)), vjust = -0.5) +
  ggtitle(paste(iter, n_obs, sep = ", n=")) +
  guides(fill="none")


## importance of attributes for the Japanese consumer
#### Japan - importance
iter = "Japan"
var_by = brkfst$country2

n_obs = length(brkfst_part[var_by == iter,1])

sub_importance <- 
  ca.importance(brkfst_part[var_by == iter,], attribute)

sub_importance_df <- data.frame(
  sub_importance = sub_importance,
  attribute_txt = names(sub_importance)
)
sub_importance_df <- sub_importance_df %>% 
  mutate(group_fct = factor(attribute_txt, levels = c("Price", "Form", "PurchaseLocation",
                                                      "FunPackaging", "CognitiveSupport", "DigitalIntegration"
  )))

ggplot(sub_importance_df, aes(x=group_fct, y = sub_importance,
                              fill=group_fct)) +
  geom_col() +
  coord_flip() +
  scale_x_discrete(limits = rev(levels(sub_importance_df$group_fct))) +
  geom_text(aes(label = format(round(sub_importance, 2), nsmall = 2)), vjust = -0.5) +
  ggtitle(paste(iter, n_obs, sep = ", n=")) +
  guides(fill="none")




## Compute market share -----
## 5 benchmarks
## Paul shimbashi, pastrami croissant
brkfst_sim <- data.frame(
  Price = c("600"),
  Form = c("Bread and Pastries"),
  PurchaseLocation = c("On Route to work"),
  FunPackaging = c("None"),
  CognitiveSupport = c("None"),
  DigitalIntegration = c("None")
)

# 7-eleven onigiri
brkfst_sim <- rbind(brkfst_sim,
                    c("200","Onigiri","On Route to work","None","None","None"))


# Shimbashi cafe
brkfst_sim <- rbind(brkfst_sim,
                    c("600","Bread and Pastries","On Route to work","Attack on Titan","None","None"))



# Family Mart protein stick
brkfst_sim <- rbind(brkfst_sim,
                    c("400","Protein sticks","Within 150m","None","None","None"))


# Our own product
brkfst_sim <- rbind(brkfst_sim,
                    c("400","Onigiri","Within 150m","Attack on Titan",
                      "Natural Mood Stabilizer","Loyalty points"))



ca.logit(brkfst_sim, brkfst_part, brkfst_design)
ca.logit(brkfst_sim, brkfst_part[brkfst$region == "East Asia",], brkfst_design)


market_share <- 
  data.frame(product = c("Paul pastrami croissant",
                         "7-eleven onigiri",
                         "Shimbashi cafe",
                         "Family Mart protein stick",
                         "MCU new excellent product"),
             mkt_share = ca.logit(brkfst_sim, brkfst_part, brkfst_design))

ggplot(market_share, aes(x= product, y = mkt_share,
                         fill=product)) +
  geom_col() +
  coord_flip() +
  scale_x_discrete(limits=rev) +
  geom_text(aes(label = format(round(mkt_share, 2), nsmall = 2)), vjust = -0.5)






# Appendix -----

## code for tables -----
### for variables
var_by <-  "exercise_per_week_cut"

kable(
  brkfst_cmb %>% 
    group_by(group_var = eval(as.name(paste(var_by)))) %>% 
    summarise_at(vars(L_1_200:`L_18_Nutrition Advice`),
                 mean , na.rm = TRUE) %>% 
    pivot_longer(-group_var) %>% 
    pivot_wider(id_cols = name, names_from = "group_var",
                values_from = "value")
)

### for specific observations
kable(
  brkfst_cmb %>% 
    filter(nickname %in% c("Marius", "Yun", "Abigail", "Pup", "Matt")) %>% 
    group_by(nickname) %>% 
    summarise_at(vars(L_1_200:`L_18_Nutrition Advice`),
                 mean , na.rm = TRUE) %>% 
    pivot_longer(-nickname) %>% 
    pivot_wider(id_cols = name, names_from = "nickname",
                values_from = "value")
)


## country comparison
brkfst_cmb %>% 
  filter(country2 %in% c("Japan", "China")) %>% 
  group_by(country2) %>% 
  summarise_at(vars(L_1_200:`L_18_Nutrition Advice`),
               mean , na.rm = TRUE) %>% 
  pivot_longer(-country2) %>% 
  pivot_wider(id_cols = name, names_from = "country2",
              values_from = "value") 

brkfst_cmb_plot <- brkfst_cmb %>% 
  filter(country2 %in% c("Japan", "China")) %>% 
  group_by(country2) %>% 
  summarise_at(vars(L_1_200:`L_18_Nutrition Advice`),
               mean , na.rm = TRUE) %>% 
  pivot_longer(-country2) %>% 
  # mutate(country2 = factor(country2,
  #                          levels = c("Japan", "China"))) %>% 
  mutate(name = factor(name,
                       levels = c("L_1_200", 
                                  "L_2_400",
                                  "L_3_600",
                                  "L_4_Bread and Pastries",
                                  "L_5_Onigiri",
                                  "L_6_Protein sticks",
                                  "L_7_On Route to work",
                                  "L_8_Within 150m",
                                  "L_9_More than 150m",
                                  "L_10_None",
                                  "L_11_Attack on Titan",
                                  "L_12_Doan Ritsu",
                                  "L_13_None",
                                  "L_14_Caffeine or Alertness boost",
                                  "L_15_Natural Mood Stabilizer",
                                  "L_16_None",
                                  "L_17_Loyalty points",
                                  "L_18_Nutrition Advice")))

ggplot(brkfst_cmb_plot, aes(x=name , y = value,
                            group = country2, fill=country2)) +
  geom_col(position = "dodge") +
  coord_flip() +
  scale_x_discrete(limits = rev(levels(brkfst_cmb_plot$name))) +
  geom_text(aes(label = format(round(value, 2), nsmall = 2)), vjust = -0.5) 
#ggtitle(paste(iter, n_obs, sep = ", n =")) +
#guides(fill="none")


brkfst_cmb %>% 
  filter(country2 %in% c("Japan")) %>% 
  group_by(country2) %>% 
  summarise_at(vars(L_1_200:`L_18_Nutrition Advice`),
               mean , na.rm = TRUE) %>% 
  pivot_longer(-country2) %>% 
  # mutate(country2 = factor(country2,
  #                          levels = c("Japan", "China"))) %>% 
  mutate(name = factor(name,
                       levels = c("L_1_200", 
                                  "L_2_400",
                                  "L_3_600",
                                  "L_4_Bread and Pastries",
                                  "L_5_Onigiri",
                                  "L_6_Protein sticks",
                                  "L_7_On Route to work",
                                  "L_8_Within 150m",
                                  "L_9_More than 150m",
                                  "L_10_None",
                                  "L_11_Attack on Titan",
                                  "L_12_Doan Ritsu",
                                  "L_13_None",
                                  "L_14_Caffeine or Alertness boost",
                                  "L_15_Natural Mood Stabilizer",
                                  "L_16_None",
                                  "L_17_Loyalty points",
                                  "L_18_Nutrition Advice"))) %>% 
  mutate(group_aggr =
           c("Price", "Price", "Price",
             "Form", "Form", "Form",
             "PurchaseLocation", "PurchaseLocation", "PurchaseLocation",
             "FunPackaging", "FunPackaging", "FunPackaging",
             "CognitiveSupport", "CognitiveSupport","CognitiveSupport",
             "DigitalIntegration", "DigitalIntegration", "DigitalIntegration"
           )) %>% 
  arrange(desc(value))





