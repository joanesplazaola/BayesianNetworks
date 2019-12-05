#data$DM_Weight <- factor(ifelse(data$DM_Weight < 70, "<70", ">70"))

data_weight <-
  data %>% select("MO_Movies",
                  "MO_Horror",
                  "MO_Thriller",
                  "MO_Comedy",
                  "MO_Romantic",
                  "MO_Scifi",
                  "MO_War",
                  "MO_Fantasy",
                  "MO_Animated",
                  "MO_Documentary",
                  "MO_Western",
                  "MO_Action",
                  "DM_Gender")

data_weight$DM_Weight <- factor(ifelse(data$DM_Weight < 70, "<70", ">70"))

N <- nrow(data)
n <- round(N * 0.8)
set.seed(2019)
indexes <- sample(1:N, n, replace = F)
train_data_weight  <- as.data.frame(data_weight[indexes, ])
test_data_weight <- as.data.frame(data_weight[-indexes, ])
summary(data_weight)

functions <- c(hc, tabu, hc,  tabu, hc, tabu, hc, tabu)
args <- c("loglik","loglik",  "bic", "bic", "bde","bde", "k2", "k2")
par(mfrow=c(1,1))
scores = c()

for (idx in 1:(length(functions))){
  scores <- functions[[idx]](train_data_weight, score = args[idx]) %T>%
    graphviz.plot(layout="dot", shape = "ellipse") %>%
    bnlearn::score(data= train_data_weight, type=args[idx]) %T>%
    print %>%
    c(scores)
}


# Honekin lortzen dogu ze konfigurazinokin euki dan emaitza onenak
best_index <- length(functions) - which.max(scores) + 1


net <-
  functions[[best_index]](as.data.frame(data_weight), score = args[best_index])

net.grain <- grain(as(amat(net), "graphNEL"), data = as.data.frame(data_weight), smooth = 0.0001)

net.compiled <- compile(net.grain)
net.propagated <- propagate(net.compiled)
net.propagated

###########


weightRomantic <- net.propagated %>%
  querygrain(nodes = "MO_Romantic", type = "marginal")

weightRomantic.1 <- net.propagated %>%
  setEvidence(nodes = "DM_Weight",
              states = "<70",
              propagate = FALSE) %>%
  propagate %>%
  querygrain(nodes = "MO_Romantic", type = "marginal")

weightRomantic.2 <- net.propagated %>%
  setEvidence(nodes = "DM_Weight",
              states = ">70",
              propagate = FALSE) %>%
  propagate %>%
  querygrain(nodes = "MO_Romantic", type = "marginal")


weightRomantic.Table <-
  as.data.frame(
    rbind(
      weightRomantic$MO_Romantic,
      weightRomantic.1$MO_Romantic,
      weightRomantic.2$MO_Romantic
    )
  )
weightRomantic.Table



weightWestern <- net.propagated %>%
  querygrain(nodes = "MO_Romantic", type = "marginal")

weightWestern.1.female <- net.propagated %>%
  setEvidence(nodes = c("DM_Weight","DM_Gender"),
              states = c("<70","female"),
              propagate = FALSE) %>%
  propagate %>%
  querygrain(nodes = "MO_Romantic", type = "marginal")

weightWestern.1.male <- net.propagated %>%
  setEvidence(nodes = c("DM_Weight","DM_Gender"),
              states = c("<70","male"),
              propagate = FALSE) %>%
  propagate %>%
  querygrain(nodes = "MO_Romantic", type = "marginal")

weightWestern.2.female <- net.propagated %>%
  setEvidence(nodes = c("DM_Weight","DM_Gender"),
              states = c(">70","female"),
              propagate = FALSE) %>%
  propagate %>%
  querygrain(nodes = "MO_Romantic", type = "marginal")

weightWestern.2.male <- net.propagated %>%
  setEvidence(nodes = c("DM_Weight","DM_Gender"),
              states = c(">70","male"),
              propagate = FALSE) %>%
  propagate %>%
  querygrain(nodes = "MO_Romantic", type = "marginal")

weightWestern.Table <-
  as.data.frame(
    rbind(
      weightWestern$MO_Romantic,
      weightWestern.1.male$MO_Romantic,
      weightWestern.2.male$MO_Romantic,
      weightWestern.1.female$MO_Romantic,
      weightWestern.2.female$MO_Romantic
    )
  )
weightWestern.Table

