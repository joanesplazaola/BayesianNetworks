data_only <-
  data %>% select(DM_Age,
                  DM_Gender,
                  DM_Only_child,
                  HB_Alcohol,
                  HB_Healthy)
N <- nrow(data)
n <- round(N * 0.8)
set.seed(2019)
indexes <- sample(1:N, n, replace = F)
train_data_only  <- as.data.frame(data_only[indexes, ])
test_data_only <- as.data.frame(data_only[-indexes, ])
summary(data_only)

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

