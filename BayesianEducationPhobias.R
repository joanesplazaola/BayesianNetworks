phobias <- data %>% select(starts_with("PH")) %>% colnames

data_educ_ph <-
  data %>% select("DM_Education",
                  phobias)
N <- nrow(data)
n <- round(N * 0.8)
set.seed(2019)
indexes <- sample(1:N, n, replace = F)
train_data_educ_ph  <- as.data.frame(data_educ_ph[indexes,])
test_data_educ_ph <- as.data.frame(data_educ_ph[-indexes,])
summary(data_educ_ph)

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
    functions[[idx]](train_data_educ_ph, score = args[idx]) %T>%
    graphviz.plot(layout = "dot", shape = "ellipse") %>%
    bnlearn::score(data = train_data_educ_ph, type = args[idx]) %T>%
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
  functions[[best_index]](as.data.frame(data_educ_ph), score = args[best_index])

net.grain <-
  grain(as(amat(net), "graphNEL"),
        data = as.data.frame(data_educ_ph),
        smooth = 0.001)

net.compiled <- compile(net.grain)
net.propagated <- propagate(net.compiled)
net.propagated



phobiaEducation <- net.propagated %>%
  querygrain(nodes = "PH_Darkness", type = "marginal")

phobiaEducation.1 <- net.propagated %>%
  setEvidence(nodes = "DM_Education",
              states = "currently a primary school pupil",
              propagate = FALSE) %>%
  propagate %>%
  querygrain(nodes = "PH_Darkness", type = "marginal")

phobiaEducation.2 <- net.propagated %>%
  setEvidence(nodes = "DM_Education",
              states = "primary school",
              propagate = FALSE) %>%
  propagate %>%
  querygrain(nodes = "PH_Darkness", type = "marginal")

phobiaEducation.3 <- net.propagated %>%
  setEvidence(nodes = "DM_Education",
              states = "secondary school",
              propagate = FALSE) %>%
  propagate %>%
  querygrain(nodes = "PH_Darkness", type = "marginal")

phobiaEducation.4 <- net.propagated %>%
  setEvidence(nodes = "DM_Education",
              states = "college/bachelor degree",
              propagate = FALSE) %>%
  propagate %>%
  querygrain(nodes = "PH_Darkness", type = "marginal")

phobiaEducation.5 <- net.propagated %>%
  setEvidence(nodes = "DM_Education",
              states = "masters degree",
              propagate = FALSE) %>%
  propagate %>%
  querygrain(nodes = "PH_Darkness", type = "marginal")

phobiaEducation.6 <- net.propagated %>%
  setEvidence(nodes = "DM_Education",
              states = "doctorate degree",
              propagate = FALSE) %>%
  propagate %>%
  querygrain(nodes = "PH_Darkness", type = "marginal")


phobiaEducation.Table <-
  as.data.frame(
    rbind(
      phobiaEducation$PH_Darkness,
      phobiaEducation.1$PH_Darkness,
      phobiaEducation.2$PH_Darkness,
      phobiaEducation.3$PH_Darkness,
      phobiaEducation.4$PH_Darkness,
      phobiaEducation.5$PH_Darkness,
      phobiaEducation.6$PH_Darkness
    )
  )

phobiaEducation.Table

row.names(phobiaEducation.Table) <-
  c(
    'Marginal',
    'Primary pupil',
    'Primary school',
    'Secondary school',
    'College/Bachelor',
    'Master',
    'Doctor'
  )


phobiaEducation.Table %>%
  kable("latex", caption = "Probabilities for Darkness Phobia conditioned by education grade", booktabs = T) %>%
  kable_styling(latex_options = c("striped", "hold_position"))
