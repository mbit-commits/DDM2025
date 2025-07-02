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

# check country
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
# check country
brkfst %>% 
  group_by(region) %>% 
  summarize(n = n())



# ratings cut
brkfst_rating <- 
  brkfst %>% 
  select(starts_with("PROD"))



# generating partworths
brkfst_part <- ca.part.util(brkfst_rating ,brkfst_design, attribute)
head(brkfst_part, 1)


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
  



# Importance
# Overall
ca.importance(brkfst_part, attribute)
# First respondent
ca.importance(brkfst_part[1,], attribute)
# First five respondents as a group
ca.importance(brkfst_part[brkfst$country=="Japan",], attribute)
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
                              fill=attribute_txt)) +
  geom_col() +
  coord_flip() +
  scale_x_discrete(limits = rev(levels(sub_importance_df$group_fct))) +
  geom_text(aes(label = format(round(sub_importance, 2), nsmall = 2)), vjust = -0.5) +
  ggtitle(paste("All sample", length(brkfst$nickname), sep = ", n=")) +
  guides(fill="none")





#breakdown of importance by subcomponents
#### by gender
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



#### by region
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



#### by age
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



#### by living with partner
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



#### by expected salary
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



#### by dietary restriction
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


#### by work start
plot_list <- list()
var_by = brkfst$work_start
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


#### by exercise per week
plot_list <- list()
var_by = brkfst$exercise_per_week
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


#### by alcohol per week
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




### Trade-off calculation
## ----> individual case
# average monetary value of 1 utiliy
# linear utility function
util_linear_value <-  (600-200)/(average_utility["200"]-average_utility["600"])
util_linear_value

# piece-wise utility function
util_piecewise_1 <- (600-400)/(average_utility["400"]-average_utility["600"])
util_piecewise_1
util_piecewise_2 <- (400-200)/(average_utility["200"]-average_utility["400"])
util_piecewise_2  

# moving from none to natural mood stabiizers
(average_utility_df[average_utility_df$lng_level=="L_15_Natural Mood Stabilizer",]$avg_utility -
  average_utility_df[average_utility_df$lng_level=="L_13_None",]$avg_utility) * util_linear_value

# moving from none to caffeine or alertness boost
(average_utility_df[average_utility_df$lng_level=="L_14_Caffeine or Alertness boost",]$avg_utility -
    average_utility_df[average_utility_df$lng_level=="L_13_None",]$avg_utility) * util_linear_value



# moving from caffeeine to natural mood stabiizers
(average_utility["Natural Mood Stabilizer"]-
    average_utility["Caffeine or Alertness boost"]) * util_linear_value


# trade-off of cognitive support
util_CognitiveSupport_None <- 
  average_utility_df[average_utility_df$group=="CognitiveSupport"&
                     average_utility_df$level_fct=="None",]$avg_utility

util_CognitiveSupport_NaturalMoodstabilizer <- 
  average_utility_df[average_utility_df$group=="CognitiveSupport"&
                     average_utility_df$level_fct=="Natural Mood Stabilizer",]$avg_utility


## ----> average case
v_min_price = 200
v_util_min_price = average_utility["200"] # associated utility for the lower price
v_max_price = 600 
v_util_max_price = average_utility["600"]  # associated utility for the higher price
v_util_delta_factor = util_CognitiveSupport_None - util_CognitiveSupport_NaturalMoodstabilizer

# 
# ## ----> average case
# v_min_price = 200
# v_util_min_price = average_utility["200"] # associated utility for the lower price
# v_max_price = 600 
# v_util_max_price = average_utility["600"]  # associated utility for the higher price
# v_util_delta_factor = average_utility["Powder"] - average_utility["Concentrate"]
# 


trade_off_calc <- function(min_price, max_price, 
                           util_min_price,util_max_price,
                           util_delta_factor) {
  trade_off_calc = min_price +
    (util_delta_factor/(util_min_price-util_max_price)) *
    (max_price-min_price)
  
  return(trade_off_calc)
}

# customer would pay $53 dollars  for a pre-mixed formulation vs $49 for powder
trade_off_calc(min_price = v_min_price, max_price = v_max_price,
               util_min_price = v_util_min_price, 
               util_max_price = v_util_max_price,
               util_delta_factor = v_util_delta_factor)






# Market Prediction
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





## segmentation
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
  filter(cluster==2) %>% 
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


cluster.mean()