

data_onlyChild <-
  data %>% select("DM_Only_child",
                  "HB_Smoking",
                  "HB_Alcohol",
                  "HB_Healthy")
N <- nrow(data)
n <- round(N * 0.8)
set.seed(2019)
indexes <- sample(1:N, n, replace = F)
train_data_onlyChild  <- as.data.frame(data_onlyChild[indexes,])
test_data_onlyChild <- as.data.frame(data_onlyChild[-indexes,])
summary(data_onlyChild)

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
    functions[[idx]](train_data_onlyChild, score = args[idx]) %T>%
    graphviz.plot(layout = "dot", shape = "ellipse") %>%
    bnlearn::score(data = train_data_onlyChild, type = args[idx]) %T>%
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
  functions[[best_index]](as.data.frame(data_onlyChild), score = args[best_index])

net.grain <-
  grain(as(amat(net), "graphNEL"), data = as.data.frame(data_onlyChild))

net.compiled <- compile(net.grain)
net.propagated <- propagate(net.compiled)
net.propagated



healthyOnlyChild <- net.propagated %>%
  querygrain(nodes = "HB_Healthy", type = "marginal")

healthyOnlyChild.No <- net.propagated %>%
  setEvidence(nodes = "DM_Only_child",
              states = "no",
              propagate = FALSE) %>%
  propagate %>%
  querygrain(nodes = "HB_Healthy", type = "marginal")

healthyOnlyChild.Yes <- net.propagated %>%
  setEvidence(nodes = "DM_Only_child",
              states = "yes",
              propagate = FALSE) %>%
  propagate %>%
  querygrain(nodes = "HB_Healthy", type = "marginal")


healthyOnlyChild.Table <-
  as.data.frame(
    rbind(
      healthyOnlyChild$HB_Healthy,
      healthyOnlyChild.No$HB_Healthy,
      healthyOnlyChild.Yes$HB_Healthy
    )
  )
row.names(healthyOnlyChild.Table) <-
  c('Healthy', 'Healthy|NO Only Child', 'Healthy|YES Only Child')
healthyOnlyChild.Table

healthyOnlyChild.Table %>%
  kable("latex", caption = "Probabilities for Healthy Habits conditioned by being Only Child", booktabs = T) %>%
  kable_styling(latex_options = c("striped", "hold_position"))
