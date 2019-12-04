
library(tidyverse)
library(GGally)
library(magrittr)
library(kableExtra)

library(corrplot)
library(RColorBrewer)

library(gRain)
library(gridExtra)
library(bnlearn)
library(RBGL)
library(Rgraphviz)
library(pROC)
library(arulesCBA)
library(kableExtra)
data <-
  read_csv("/home/joanes/uni/BayesianNetworks/data/Dataset.csv")

data %<>%  drop_na(DM_Gender)

data %<>% mutate_if(is.character, ~ replace_na(., names(which.max(table(.)))))
data_nominal <-
  c('DM_Gender',
    'DM_Handed',
    'DM_Village_Town',
    'DM_House_Flat',
    'DM_Only_child')
data[data_nominal] <-
  lapply(data[data_nominal], function(x) {
    factor(x)
  })


data_numeric <- c('DM_Height', 'DM_Weight', 'DM_Age', 'DM_Siblings')

data_ordinal_char <- c('HB_Alcohol', 'DM_Education', 'HB_Smoking')
data  %<>%
  mutate_at(vars(-c(
    data_numeric, data_ordinal_char, data_nominal
  )), ordered)


data$HB_Smoking <-
  factor(
    data$HB_Smoking,
    ordered = T,
    levels = c(
      "never smoked",
      "tried smoking",
      "former smoker",
      "current smoker"
    )
  )
data$HB_Alcohol <-
  factor(
    data$HB_Alcohol,
    ordered = T,
    levels = c("never",  "social drinker", "drink a lot")
  )
data$DM_Education <-
  factor(
    data$DM_Education,
    ordered = T,
    levels = c(
      "currently a primary school pupil",
      "primary school",
      "secondary school",
      "college/bachelor degree",
      "masters degree",
      "doctorate degree"
    )
  )

# We delete the 62cm person as is too small
data %<>%  filter(DM_Height > 62 | is.na(DM_Height))
# We delete the 160kg person, as is too fat
data %<>% filter(DM_Weight < 160 | is.na(DM_Weight))

# Change NA with median value
age_median <- data$DM_Age %>% median(na.rm = T)

data$DM_Age %<>% replace_na(age_median)



data %<>% group_by(DM_Gender) %>% mutate(DM_Height = ifelse(is.na(DM_Height), median(DM_Height, na.rm =
                                                                                       TRUE), DM_Height))
data %<>% ungroup

data %<>% group_by(DM_Gender) %>% mutate(DM_Weight = ifelse(is.na(DM_Weight), median(DM_Weight, na.rm =
                                                                                       TRUE), DM_Weight))
data %<>% ungroup

data %<>% mutate_if(is.ordered, ~ replace_na(., which.max(table(.))))


data$DM_Age %<>% discretize(breaks = 5)
data$DM_Height %<>% discretize(breaks = 5)
data$DM_Weight %<>% discretize(breaks = 5)


phobias <- data %>% select(starts_with("PH")) %>% colnames
hirutan <-
  c(
    "IN_Science_Technology",
    "IN_Shopping",
    "SP_Gadgets" ,
    "SP_Branded_clothing",
    "SP_Shopping_centres",
    "IN_Passive_sport",
    "IN_Languages",
    "IN_Biology",
    "IN_Economy",
    "IN_History",
    "MO_Action",
    "MO_Documentary",
    "IN_Geography",
    "MO_Fantasy",
    "MO_War",
    "MO_Scifi",
    "MO_Romantic",
    "SP_Looks",
    "MO_Rock_n_Roll",
    "MO_Swing_Jazz",
    "MO_Reggae_Ska",
    "IN_Theatre",
    "MO_Slow_Fast",
    "SP_Finances",
    "SP_Healthy_eating",
    "SP_Entertainment",
    "SP_Looks",
    "MO_Dance",
    "MO_Classical",
    "MO_Musical",
    "IN_PC",
    "IN_Art"
  )


# 4-5 eta beste danak
biTop <-
  c(
    phobias,
    "IN_Politics",
    "IN_Medicine",
    "MO_Animated",
    "MO_Opera",
    "MO_Punk",
    "HB_Healthy",
    "IN_Internet",
    "IN_Socializing",
    "MO_Comedy",
    "MO_Movies",
    "MO_Music",
    "IN_Pets" ,
    "MO_Folk",
    "MO_Country",
    "MO_Rock"
  )

# 1-2 eta beste danak
biBot <-
  c(
    "IN_Outdoors",
    "IN_Chemistry",
    "IN_Celebrities",
    "IN_Dancing",
    "IN_Gardening",
    "IN_Mathematics",
    "IN_Physics",
    "IN_Playing_instruments",
    "IN_Religion",
    "IN_Writing",
    "MO_Techno_Trance",
    "MO_Western"  ,
    "MO_Metal_Hardrock",
    "IN_Law"
  )


# Hemen aldatzen dittugu hiru binetan jarriz
data %<>% mutate_at(c(hirutan),  ~ fct_collapse(
  .x,
  "1" = c("1", "2"),
  "2" = c("3"),
  "3" = c("4", "5")
))

# Hemen aldatzen dittugu bi binetan jarriz, goiko biak eta azpiko hiruak
data %<>% mutate_at(c(biTop),  ~ fct_collapse(.x, "1" = c("1", "2", "3"),
                                              "2" = c("4", "5")))

# Hemen aldatzen dittugu bi binetan jarriz, beheko biak eta goiko hiruak
data %<>% mutate_at(c(biBot),  ~ fct_collapse(.x, "1" = c("1", "2"),
                                              "2" = c("3", "4", "5")))

