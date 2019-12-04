phobias <- data %>% select(starts_with("PH")) %>% colnames

data_aging <-
  data %>% select("DM_Education",
                  phobias)
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

net.grain <- grain(as(amat(net), "graphNEL"), data = as.data.frame(data_aging), smooth = 0.001)

net.compiled <- compile(net.grain)
net.propagated <- propagate(net.compiled)
net.propagated



phobiaEducation <- net.propagated %>%
  querygrain(nodes = "PH_Public_speaking", type = "marginal")

phobiaEducation.1 <- net.propagated %>%
  setEvidence(nodes = "DM_Education",
              states = "currently a primary school pupil",
              propagate = FALSE) %>%
  propagate %>%
  querygrain(nodes = "PH_Public_speaking", type = "marginal")

phobiaEducation.2 <- net.propagated %>%
  setEvidence(nodes = "DM_Education",
              states = "primary school",
              propagate = FALSE) %>%
  propagate %>%
  querygrain(nodes = "PH_Public_speaking", type = "marginal")

phobiaEducation.3 <- net.propagated %>%
  setEvidence(nodes = "DM_Education",
              states = "secondary school",
              propagate = FALSE) %>%
  propagate %>%
  querygrain(nodes = "PH_Public_speaking", type = "marginal")

phobiaEducation.4 <- net.propagated %>%
  setEvidence(nodes = "DM_Education",
              states = "college/bachelor degree",
              propagate = FALSE) %>%
  propagate %>%
  querygrain(nodes = "PH_Public_speaking", type = "marginal")

phobiaEducation.5 <- net.propagated %>%
  setEvidence(nodes = "DM_Education",
              states = "masters degree",
              propagate = FALSE) %>%
  propagate %>%
  querygrain(nodes = "PH_Public_speaking", type = "marginal")

phobiaEducation.6 <- net.propagated %>%
  setEvidence(nodes = "DM_Education",
              states = "doctorate degree",
              propagate = FALSE) %>%
  propagate %>%
  querygrain(nodes = "PH_Public_speaking", type = "marginal")


phobiaEducation.Table <-
  as.data.frame(
    rbind(
      phobiaEducation$PH_Public_speaking,
      phobiaEducation.1$PH_Public_speaking,
      phobiaEducation.2$PH_Public_speaking,
      phobiaEducation.3$PH_Public_speaking,
      phobiaEducation.4$PH_Public_speaking,
      phobiaEducation.5$PH_Public_speaking,
      phobiaEducation.6$PH_Public_speaking
    )
  )

phobiaEducation.Table

