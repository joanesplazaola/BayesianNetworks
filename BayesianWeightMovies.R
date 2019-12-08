

data_weight <-
  data %>% select(
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
    "DM_Gender"
  )
data_weight$DM_Weight <- discretize(data$DM_Weight, breaks = 5)


N <- nrow(data)
n <- round(N * 0.8)
set.seed(2019)
indexes <- sample(1:N, n, replace = F)
train_data_weight  <- as.data.frame(data_weight[indexes,])
test_data_weight <- as.data.frame(data_weight[-indexes,])
summary(data_weight)

functions <- c(hc, tabu, hc,  tabu, hc, tabu, hc, tabu)
args <- c("loglik", "loglik",  "bic", "bic", "bde", "bde", "k2", "k2")
par(mfrow = c(1, 1))
scores = c()


eval_struct <- function(size, bn, train, test, name) {
  d <- train[1:size,]
  bn <- bn.fit(x = bn, data = d, method = "bayes")
  gener <- stats::logLik(bn, test) / (n * dim(test)[1])
  fit <- stats::logLik(bn, d) / (n * dim(d)[1])
  res <-
    data.frame(
      "size_train" = size,
      "fitness" = fit,
      "gener" = gener,
      "structure" = name
    )
  return (c(gener, fit))
}


sizes <- round(exp(seq(1, log(
  nrow(train_data_aging)
),+(log(
  nrow(train_data_aging)
) - 1) / 50)))


for (idx in 1:(length(functions))) {
  scores <-
    functions[[idx]](train_data_weight, score = args[idx]) %T>%
    graphviz.plot(layout = "dot", shape = "ellipse") %>%
    bnlearn::score(data = train_data_weight, type = args[idx]) %T>%
    print %>%
    c(scores)
  
  struct <- functions[[idx]](train_data_aging, score = args[idx])
  eval_result <-
    sapply(
      sizes,
      FUN = eval_struct,
      bn = struct,
      train = train_data_aging,
      test = test_data_aging,
      name = args[idx]
    )
  plot(
    log10(sizes),
    eval_result[1, ],
    main = args[idx],
    ylab = "LL/nN",
    xlab = "log(Size)",
    type = "l",
    ylim = c(min(eval_result), max(eval_result)),
    col = "blue"
  )
  lines(log10(sizes), eval_result[2,], col = "red")
  legend(
    "topright",
    legend = c("Fitting", "Generalization"),
    col = c("red", "blue"),
    pch = 15
  )
}


# Honekin lortzen dogu ze konfigurazinokin euki dan emaitza onenak
best_index <- length(functions) - which.max(scores) + 1


net <-
  functions[[best_index]](as.data.frame(data_weight), score = args[best_index])

net.grain <-
  grain(as(amat(net), "graphNEL"),
        data = as.data.frame(data_weight),
        smooth = 0.0001)

net.compiled <- compile(net.grain)
net.propagated <- propagate(net.compiled)
net.propagated

###########

weightRomantic <- net.propagated %>%
  querygrain(nodes = "MO_Romantic", type = "marginal")

weightRomantic.1.female <- net.propagated %>%
  setEvidence(
    nodes = c("DM_Weight", "DM_Gender"),
    states = c("[41,55)", "female"),
    propagate = FALSE
  ) %>%
  propagate %>%
  querygrain(nodes = "MO_Romantic", type = "marginal")

weightRomantic.1.male <- net.propagated %>%
  setEvidence(
    nodes = c("DM_Weight", "DM_Gender"),
    states = c("[41,55)", "male"),
    propagate = FALSE
  ) %>%
  propagate %>%
  querygrain(nodes = "MO_Romantic", type = "marginal")

weightRomantic.2.female <- net.propagated %>%
  setEvidence(
    nodes = c("DM_Weight", "DM_Gender"),
    states = c("[55,60)", "female"),
    propagate = FALSE
  ) %>%
  propagate %>%
  querygrain(nodes = "MO_Romantic", type = "marginal")

weightRomantic.2.male <- net.propagated %>%
  setEvidence(
    nodes = c("DM_Weight", "DM_Gender"),
    states = c("[55,60)", "male"),
    propagate = FALSE
  ) %>%
  propagate %>%
  querygrain(nodes = "MO_Romantic", type = "marginal")

weightRomantic.3.female <- net.propagated %>%
  setEvidence(
    nodes = c("DM_Weight", "DM_Gender"),
    states = c("[60,68)", "female"),
    propagate = FALSE
  ) %>%
  propagate %>%
  querygrain(nodes = "MO_Romantic", type = "marginal")

weightRomantic.3.male <- net.propagated %>%
  setEvidence(
    nodes = c("DM_Weight", "DM_Gender"),
    states = c("[60,68)", "male"),
    propagate = FALSE
  ) %>%
  propagate %>%
  querygrain(nodes = "MO_Romantic", type = "marginal")

weightRomantic.4.female <- net.propagated %>%
  setEvidence(
    nodes = c("DM_Weight", "DM_Gender"),
    states = c("[68,78)", "female"),
    propagate = FALSE
  ) %>%
  propagate %>%
  querygrain(nodes = "MO_Romantic", type = "marginal")

weightRomantic.4.male <- net.propagated %>%
  setEvidence(
    nodes = c("DM_Weight", "DM_Gender"),
    states = c("[68,78)", "male"),
    propagate = FALSE
  ) %>%
  propagate %>%
  querygrain(nodes = "MO_Romantic", type = "marginal")

weightRomantic.5.female <- net.propagated %>%
  setEvidence(
    nodes = c("DM_Weight", "DM_Gender"),
    states = c("[78,150)", "female"),
    propagate = FALSE
  ) %>%
  propagate %>%
  querygrain(nodes = "MO_Romantic", type = "marginal")

weightRomantic.5.male <- net.propagated %>%
  setEvidence(
    nodes = c("DM_Weight", "DM_Gender"),
    states = c("[78,150)", "male"),
    propagate = FALSE
  ) %>%
  propagate %>%
  querygrain(nodes = "MO_Romantic", type = "marginal")


weightRomantic.1 <- net.propagated %>%
  setEvidence(nodes = "DM_Weight",
              states = "[41,5)",
              propagate = FALSE) %>%
  propagate %>%
  querygrain(nodes = "MO_Romantic", type = "marginal")
weightRomantic.2 <- net.propagated %>%
  setEvidence(nodes = "DM_Weight",
              states = "[55,60)",
              propagate = FALSE) %>%
  propagate %>%
  querygrain(nodes = "MO_Romantic", type = "marginal")

weightRomantic.3 <- net.propagated %>%
  setEvidence(nodes = "DM_Weight",
              states = "[60,68)",
              propagate = FALSE) %>%
  propagate %>%
  querygrain(nodes = "MO_Romantic", type = "marginal")

weightRomantic.4 <- net.propagated %>%
  setEvidence(nodes = "DM_Weight",
              states = "[68,78)",
              propagate = FALSE) %>%
  propagate %>%
  querygrain(nodes = "MO_Romantic", type = "marginal")

weightRomantic.5 <- net.propagated %>%
  setEvidence(nodes = "DM_Weight",
              states = "[78,150)",
              propagate = FALSE) %>%
  propagate %>%
  querygrain(nodes = "MO_Romantic", type = "marginal")

weightRomantic.Table <-
  as.data.frame(
    rbind(
      weightRomantic$MO_Romantic,
      weightRomantic.1$MO_Romantic,
      weightRomantic.2$MO_Romantic,
      weightRomantic.3$MO_Romantic,
      weightRomantic.4$MO_Romantic,
      weightRomantic.5$MO_Romantic
    )
  )

weightRomanticGender.Table <-
  as.data.frame(
    rbind(
      weightRomantic$MO_Romantic,
      weightRomantic.1.male$MO_Romantic,
      weightRomantic.2.male$MO_Romantic,
      weightRomantic.3.male$MO_Romantic,
      weightRomantic.4.male$MO_Romantic,
      weightRomantic.5.male$MO_Romantic,
      weightRomantic.1.female$MO_Romantic,
      weightRomantic.2.female$MO_Romantic,
      weightRomantic.3.female$MO_Romantic,
      weightRomantic.4.female$MO_Romantic,
      weightRomantic.5.female$MO_Romantic
    )
  )
weightRomantic.Table
weightRomanticGender.Table

row.names(weightRomantic.Table) <-
  c('Marginal',
    '[41, 55)',
    '[55, 60)',
    '[60, 68)',
    '[68, 78)',
    '[78, 60)')


weightRomantic.Table %>%
  kable("latex", caption = "Probabilities for liking Romantic movies conditioned to weight and gender", booktabs = T) %>%
  kable_styling(latex_options = c("striped", "hold_position"))


row.names(weightRomanticGender.Table) <-
  c(
    'Marginal',
    '[41, 55) | male',
    '[55, 60) | male',
    '[60, 68) | male',
    '[68, 78) | male',
    '[78, 60) | male',
    '[41, 55) | female',
    '[55, 60) | female',
    '[60, 68) | female',
    '[68, 78) | female',
    '[78, 60) | female'
  )


weightRomanticGender.Table %>%
  kable("latex", caption = "Probabilities for liking Romantic movies conditioned to weight and gender", booktabs = T) %>%
  kable_styling(latex_options = c("striped", "hold_position"))