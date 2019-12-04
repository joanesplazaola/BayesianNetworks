
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

functions <- c(hc, tabu, hc,  tabu, hc, tabu, hc, tabu)
args <- c("loglik","loglik",  "bic", "bic", "bde","bde", "k2", "k2")
par(mfrow=c(1,1))
scores = c()

for (idx in 1:(length(functions))){
  scores <- functions[[idx]](train_data_aging, score = args[idx]) %T>%
    graphviz.plot(layout="dot", shape = "ellipse") %>%
    bnlearn::score(data= train_data_aging, type=args[idx]) %T>%
    print %>%
    c(scores)
}


# Honekin lortzen dogu ze konfigurazinokin euki dan emaitza onenak
best_index <- length(functions) - which.max(scores) + 1


net <-
  functions[[best_index]](as.data.frame(data_aging), score = args[best_index])

net.grain <- grain(as(amat(net), "graphNEL"), data = as.data.frame(data_aging))

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
