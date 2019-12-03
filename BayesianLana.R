#install.packages("tidyverse")
#install.packages("GGally")

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

# Galderak:
# 1. Aging phobiak ze zerikusi dauka with healthy habits. Emakume eta gizonen arteko alderatzia.
# 2. Pisua eta pelikulen preferentzian lotura.
# 3. Zertan gastatzen daben aurreikusi interes eta musika/film gustuen arabera. Sailkatzaile bat sortu (BN)

# TODO Hau ezin dogu egin, hemen gorde biharko ginuke datu danen
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

N <- nrow(data)
n <- round(N * 0.8)
set.seed(2020)
indexes <- sample(1:N, n, replace = F)
train <- data[indexes, ]
test <- data[-indexes, ]
names(which(sapply(test, anyNA)))

data %>%
  select(DM_Age, DM_Height, DM_Weight, DM_Gender, DM_Education) %>%
  ggplot(aes(DM_Gender, DM_Height, fill=DM_Gender)) +
  geom_boxplot() +
  labs(x="Gender", y="Height (cm)", fill = "Gender") + 
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14))
  

data %>%
  select(DM_Age, DM_Height, DM_Weight, DM_Gender, DM_Education) %>%
  ggplot(aes(DM_Gender, DM_Weight)) +
  geom_boxplot() +
  facet_grid(~ DM_Education)


data %>%
  select(DM_Age, DM_Height, DM_Weight, DM_Gender, DM_Education) %>%
  ggplot(aes(DM_Gender, DM_Weight, fill = DM_Gender)) +
  geom_boxplot() +
  labs(x="Gender", y="Weight (kg)", fill = "Gender") +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14))


data %>%
  select(DM_Age, DM_Height, DM_Weight, DM_Gender, DM_Education) %>%
  ggplot(aes(DM_Gender, DM_Age)) +
  geom_boxplot() 

# Hemen ikusi da 62cm-ko enana puta bat daola, eta hil ein dou
data %<>%  filter(DM_Height > 62 | is.na(DM_Height))
# Jaja puta gorda
data %<>% filter(DM_Weight < 160)

# TODO Hau igual orokorrian ein leike, en plan mutate

data$DM_Age %<>% discretize(breaks = 5)
data$DM_Height %<>% discretize(breaks = 5)
data$DM_Weight %<>% discretize(breaks = 5)


data %>%
  select(DM_Age, DM_Height, DM_Weight, DM_Gender) %>%
  ggpairs

# Age NA aldatu medianagaittik

age_median <- data$DM_Age %>% median(na.rm = T)

data$DM_Age %<>% replace_na(age_median)

# Genderreko NAdun danak (transexualak sutara!)

data %<>% drop_na(DM_Gender)


data %>% group_by(DM_Gender) %>% summarise(avg = mean(DM_Height, na.rm =T))
data %<>% ungroup

data %<>% group_by(DM_Gender) %>% mutate(DM_Height = ifelse(is.na(DM_Height), median(DM_Height, na.rm =
                                                                                       TRUE), DM_Height))
data %<>% ungroup

data %<>% group_by(DM_Gender) %>% mutate(DM_Weight = ifelse(is.na(DM_Weight), median(DM_Weight, na.rm =
                                                                                       TRUE), DM_Weight))
data %<>% ungroup

data %>% mutate(DM_Weight = ifelse(is.na(DM_Weight), median(DM_Weight, na.rm =
                                                              TRUE), DM_Weight))

data %<>% mutate_if(is.ordered, ~ replace_na(., which.max(table(.))))

data %>% 
  select_if(is.ordered) %>% 
  gather %>%
  ggplot(aes(value)) + 
  geom_bar() + 
  facet_wrap(~key, scales = 'free_x')


prop.table(table(data$IN_Playing_instruments))


# Hirutan banatu biharrekuak:
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


data %>% select_if(is.ordered) %>% select(-c(hirutan, biTop, biBot)) %>% names

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

data %>%
  select_if(is.ordered) %>%
  gather %>%
  ggplot(aes(value)) +
  geom_bar() +
  facet_wrap(~ key, scales = 'free_x')

data_aging <-
  data %>% select("PH_Ageing",
                  "HB_Smoking",
                  "HB_Alcohol",
                  "HB_Healthy",
                  "DM_Gender")
N <- nrow(data)
n <- round(N * 0.8)
set.seed(2019)
indexes <- sample(1:N, n, replace = F)
train_data_aging  <- as.data.frame(data_aging[indexes, ])
test_data_aging <- as.data.frame(data_aging[-indexes, ])
summary(data_aging)

functions <- c(hc, hc,  tabu, hc, hc)
args <- c("loglik", "bic", "bic", "bde", "k2")

eval_struct <- function(size, bn, train, test){
  d <- train[1:size, ]
  bn <- bn.fit(x = bn, data = d, method = "bayes")
  gener <- stats::logLik(bn, test)/(n*dim(test)[1])
  fit <- stats::logLik(bn, d)/(n*dim(d)[1])
  return (c(gener, fit))
}

lims <- c(min(res.method1, res.method2...), max(res.method1, res.method2...))
layout(matrix(c(1, 2, 3), ncol = 3))
plot(log10(sizes), res.method[1,], main = "Method graph", ylab = "LL/nN", xlab = "log(Size)", type = "1", ylim = lims, col = "blue")
lines(log10(sizes), res.method[2,], col = "red")


scores <- c()
sizes <- round(exp(seq(1, log(nrow(data)), (log(nrow(data)) - 1 ) /50)))

for (idx in 1:(length(functions))) {

  net_model <- functions[[idx]](train_data_aging, score = args[idx])
  
  res.method <- sapply(sizes, FUN = eval_struct, bn=net_model, train = train_data_aging, test = test_data_aging)
  plot(log10(sizes), res.method[1,], main = "Method graph", ylab = "LL/nN", xlab = "log(Size)", type = "1", ylim = lims, col = "blue")
  lines(log10(sizes), res.method[2,], col = "red")
}

# Honekin lortzen dogu ze konfigurazinokin euki dan emaitza onenak
best_index <- length(functions) - which.max(scores) + 1


net <-
  functions[[best_index]](as.data.frame(data_aging), score = args[best_index])

net.grain <-
  grain(as(amat(net), "graphNEL"), data = as.data.frame(data_aging))

net.compiled <- compile(net.grain)
net.propagated <- propagate(net.compiled)
net.propagated



healthyAging <- net.propagated %>%
  querygrain(nodes = "HB_Healthy", type = "marginal")

healthyAging.No <- net.propagated %>%
  setEvidence(nodes = "PH_Ageing",
              states = "1",
              propagate = FALSE) %>%
  propagate %>%
  querygrain(nodes = "HB_Healthy", type = "marginal")

healthyAging.Yes <- net.propagated %>%
  setEvidence(nodes = "PH_Ageing",
              states = "2",
              propagate = FALSE) %>%
  propagate %>%
  querygrain(nodes = "HB_Healthy", type = "marginal")


healthyAging.Table <-
  as.data.frame(
    rbind(
      healthyAging$HB_Healthy,
      healthyAging.No$HB_Healthy,
      healthyAging.Yes$HB_Healthy
    )
  )
row.names(healthyAging.Table) <-
  c('Healthy', 'Healthy|NO Phobia', 'Healthy|YES Phobia')
healthyAging.Table

healthyAging.Table %>%
  kable("latex", caption = "Probabilities for Healthy Habits conditioned by Phobia of Aging", booktabs = T) %>%
  kable_styling(latex_options = c("striped", "hold_position"))



healthyAging.Male.No <- net.propagated %>%
  setEvidence(
    nodes = c("PH_Ageing", "DM_Gender"),
    states = c("1", "male"),
    propagate = FALSE
  ) %>%
  propagate %>%
  querygrain(nodes = "HB_Healthy", type = "marginal")



healthyAging.Male.Yes <- net.propagated %>%
  setEvidence(
    nodes = c("PH_Ageing", "DM_Gender"),
    states = c("2", "male"),
    propagate = FALSE
  ) %>%
  propagate %>%
  querygrain(nodes = "HB_Healthy", type = "marginal")


healthyAging.Female.No <- net.propagated %>%
  setEvidence(
    nodes = c("PH_Ageing", "DM_Gender"),
    states = c("1", "female"),
    propagate = FALSE
  ) %>%
  propagate %>%
  querygrain(nodes = "HB_Healthy", type = "marginal")



healthyAging.Female.Yes <- net.propagated %>%
  setEvidence(
    nodes = c("PH_Ageing", "DM_Gender"),
    states = c("2", "female"),
    propagate = FALSE
  ) %>%
  propagate %>%
  querygrain(nodes = "HB_Healthy", type = "marginal")



healthyAgingGender.Table <-
  as.data.frame(
    rbind(
      healthyAging$HB_Healthy,
      healthyAging.Male.No$HB_Healthy,
      healthyAging.Male.Yes$HB_Healthy,
      healthyAging.Female.No$HB_Healthy,
      healthyAging.Female.Yes$HB_Healthy
    )
  )
row.names(healthyAgingGender.Table) <-
  c(
    'Healthy',
    'Healthy|Male,NO Phobia',
    'Healthy|Male,YES Phobia',
    'Healthy|Female,NO Phobia',
    'Healthy|Female,YES Phobia'
  )
healthyAgingGender.Table

healthyAgingGender.Table %>%
  kable("latex", caption = "Probabilities for Healthy Habits conditioned by Gender,Phobia of Aging", booktabs = T) %>%
  kable_styling(latex_options = c("striped", "hold_position"))


# 1. Aging phobiak ze zerikusi dauka with healthy habits. Emakume eta gizonen arteko alderatzia.
# 2. Pisua eta pelikulen preferentzian lotura.
# 3. Zertan gastatzen daben aurreikusi interes eta musika/film gustuen arabera. Sailkatzaile bat sortu (BN)
