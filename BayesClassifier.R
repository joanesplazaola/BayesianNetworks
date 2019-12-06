estimateError <- function(classifier, class.index, test) {
  predicted.class <- predict(classifier, test)
  true.class <- test[, class.index]
  sum(predicted.class != true.class) / length(predicted.class)
}

target <- "SP_Looks"
class_data <- data %>%
  select(target,
         starts_with("IN"),
         starts_with("MO"))


class_data <- as.data.frame(class_data)
rows <- nrow(class_data)
train_ratio <- 0.75
train_rows <- round(train_ratio * rows)
set.seed(42)

index_train <- sample(1:rows, train_rows)

train <- class_data[index_train,]
test <- class_data[-index_train,]

nb.class <- train %>%
  naive.bayes(training = names(train)[1], explanatory = names(train)[-1]) %T>%
  graphviz.plot %>%
  bn.fit(data = train, method = "mle")

tan.class <- train %>%
  tree.bayes(training = names(train)[1], explanatory = names(train)[-1]) %T>%
  graphviz.plot %>%
  bn.fit(data = train, method = "mle")



estimateError(nb.class, 1, train)
estimateError(nb.class, 1, test)
estimateError(tan.class, 1, train)
estimateError(nb.class, 1, test)




learnSelectiveNB <- function (data, training) {
  net <- hc(data)
  selected <- mb(net, training)
  
  data.filtered <- data[, c(training, selected)]
  model <-
    naive.bayes(training = training,
                explanatory = selected,
                x = data.filtered)
  model_fitted <-
    bn.fit(x = model, data = data.filtered, method = "bayes")
  return(list(
    model = model_fitted,
    features = selected,
    training = training
  ))
  return (net)
 
}

predictSelectiveNB <- function (model, data) {
  data.filtered <- data[, c(model$training, model$features)]
  return(predict(model$model, data.filtered))
}

snb <- learnSelectiveNB(class_data, training=target)

sum(predictSelectiveNB(snb, data=test) !=test[, 1]) / length(test[, 1])
