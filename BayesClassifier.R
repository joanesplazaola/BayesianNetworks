estimateError <- function(classifier, class.index, test) {
  predicted.class <- predict(classifier, test)
  true.class <- test[, class.index]
  sum(predicted.class != true.class) / length(predicted.class)
}




# We first select the needed variables (predictors and the target)

target <- "SP_Healthy_eating"
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

# With shuffled data we create train and testing subsets
train <- class_data[index_train, ]
test <- class_data[-index_train, ]

# We train the classifiers (tan and nb) and plot the structures.
par(mfrow=c(1,1))
nb.class <- train %>%
  naive.bayes(training = names(train)[1], explanatory = names(train)[-1]) %T>%
  graphviz.plot %>%
  bn.fit(data = train, method = "mle")

tan.class <- train %>%
  tree.bayes(training = names(train)[1], explanatory = names(train)[-1]) %T>%
  graphviz.plot %>%
  bn.fit(data = train, method = "mle")


bic.class <- train %>% 
  hc( score="bic") %>%
  bn.fit(data=train, method="bayes")


k2.class <- train %>% 
  hc( score="k2") %>%
  bn.fit(data=train, method="bayes")


# We calculate the errors for the fitted classifiers in the test and train subsets

getError(data=train, nb.class, id.clase=1)
getError(data=train, tan.class, id.clase=1)
getError(data=train, bic.class, id.clase=1)
getError(data=train, k2.class, id.clase=1)

getError(data=test, nb.class, id.clase=1)
getError(data=test, tan.class, id.clase=1)
getError(data=test, bic.class, id.clase=1)
getError(data=test, k2.class, id.clase=1)


# With this function we learn the most important variables for the classifier
learnSelectiveNB <- function (data, training) {
  selected  <- hc(data) %>% mb( training)
  data.filtered <- data[, c(training, selected)]
  model_fitted <-
    naive.bayes(training = training, explanatory = selected, x = data.filtered) %>%
    bn.fit( data = data.filtered, method = "bayes")
  return(list(
    model = model_fitted,
    features = selected,
    training = training
  ))
}

predictSelectiveNB <- function (model, data) {
  data.filtered <- data[, c(model$training, model$features)]
  return(predict(model$model, data.filtered))
}
applyBayesRule <- function(x, model, id.clase = 1) {
  options(warn = -1)
  omega.C <- levels(x[[id.clase]])
  
  log.p <-matrix(rep(0, dim(x)[1] * length(omega.C)), ncol = length(omega.C))
  for (i in 1:length(omega.C)) {
    cl <- omega.C[i]
    x[[id.clase]] <- factor(rep(cl, dim(x)[1]), levels = omega.C)
    log.p[, i] <- logLik(object = model, data = x, by.sample = TRUE)
  }
  options(warn = 0)
  pred.class <- apply(log.p, MARGIN = 1,
    FUN = function(x) {
      id <- which(x == max(x))
      omega.C[id]
    }
  )
  return (pred.class)
}
getError <- function(data, model, id.clase = 1) {
  error <- sum(applyBayesRule(data, model) != data[, id.clase]) / dim(data)[1]
  return(error)
}

# Cross validation 10 folds
set.seed(42)

train_10_folds <- sample(rep(1:10, length.out = nrow(train)), size = nrow(train), replace = F)


#get errors
#entertainment
id.clase <- 1
net.bic.class.err <- vector()
for (i in 1:10){
  tr_data <- train[train_10_folds != i,]
  net.bic.struct <- tr_data %>% 
    hc( score="bic") 
  net.bic.class <- bn.fit(net.bic.struct, tr_data, method="bayes")
  net.bic.class.err[i] <- getError(tr_data, net.bic.class, id.clase=id.clase)
}


net.k2.class.err <- vector()
for (i in 1:10){
  tr_data <- train[train_10_folds != i,]
  net.k2.struct <- tr_data %>% 
    hc( score="k2") 
  net.k2.class <- bn.fit(net.k2.struct, tr_data, method="bayes")
  net.k2.class.err[i] <- getError(tr_data, net.k2.class, id.clase=id.clase)
}

net.nb.class.err <- vector()
for (i in 1:10){
  tr_data <- train[train_10_folds != i,]
  net.bn.struct <- tr_data %>% 
    naive.bayes( training=names(tr_data)[id.clase], explanatory=names(tr_data)[-id.clase])
  net.bn.class <- bn.fit(net.bn.struct , tr_data, method="bayes")
  net.nb.class.err[i] <- getError(tr_data, net.bn.class, id.clase=id.clase)
}

net.tan.class.err <- vector()
for (i in 1:10){
  tr_data <- train[train_10_folds != i,]
  net.tan.struct <- tr_data %>% 
    tree.bayes( training=names(tr_data)[id.clase], explanatory=names(tr_data)[-id.clase]) 
  net.tan.class <- bn.fit(net.tan.struct, tr_data, method="bayes")
  net.tan.class.err[i] <- getError(tr_data, net.tan.class, id.clase=id.clase)
}

as.data.frame(cbind(mean(net.bic.class.err),mean(net.k2.class.err), 
                                     mean(net.nb.class.err), mean(net.tan.class.err)))


N.test <- rows * 0.2
total.train <- rows * 0.8
N.train <- round(exp(seq(1, log(rows - N.test),
                           + (log(N.test)-1) / 50)))


res <- data.frame()
for (r in 1:10) {
  for (s in unique(N.train)) {
    sampled_index <- sample(1:rows, total.train)
    train_sample <- class_data[sampled_index[1:s], ]
    test_sample <- class_data[-sampled_index, ]
    model.bic <-
      bn.fit(x = net.bic.struct,
             data = train_sample,
             method = "bayes")
    res <- rbind(
      res,
      data.frame(
        "size_train" = s,
        "error" = getError(
          data = test_sample,
          model = model.bic,
          id.clase = 1
        ),
        "data" = "test",
        "rep" = r,
        "structure" = "net.bic"
      )
    )
    res <- rbind(
      res,
      data.frame(
        "size_train" = s,
        "error" = getError(
          data = train_sample,
          model = model.bic,
          id.clase = 1
        ),
        "data" = "train",
        "rep" = r,
        "structure" = "net.bic"
      )
    )
    
    model.k2 <-
      bn.fit(x = net.k2.struct,
             data = train_sample,
             method = "bayes")
    res <- rbind(
      res,
      data.frame(
        "size_train" = s,
        "error" = getError(
          data = test_sample,
          model = model.k2,
          id.clase = 1
        ),
        "data" = "test",
        "rep" = r,
        "structure" = "net.k2"
      )
    )
    res <- rbind(
      res,
      data.frame(
        "size_train" = s,
        "error" = getError(
          data = train_sample,
          model = model.k2,
          id.clase = 1
        ),
        "data" = "train",
        "rep" = r,
        "structure" = "net.k2"
      )
    )
    model.naive <- bn.fit(x = net.bn.struct,
                          data = train_sample,
                          method = "bayes")
    res <- rbind(
      res,
      data.frame(
        "size_train" = s,
        "error" = getError(
          data = test_sample,
          model = model.naive,
          id.clase = 1
        ),
        "data" = "test",
        "rep" = r,
        "structure" = "net.nb"
      )
    )
    res <- rbind(
      res,
      data.frame(
        "size_train" = s,
        "error" = getError(
          data = train_sample,
          model = model.naive,
          id.clase = 1
        ),
        "data" = "train",
        "rep" = r,
        "structure" = "net.nb"
      )
    )
    model.tan <-
      bn.fit(x = net.tan.struct,
             data = train_sample,
             method = "bayes")
    res <- rbind(
      res,
      data.frame(
        "size_train" = s,
        "error" = getError(
          data = test_sample,
          model =model.tan,
          id.clase = 1
        ),
        "data" = "test",
        "rep" = r,
        "structure" = "net.tan"
      )
    )
    res <- rbind(
      res,
      data.frame(
        "size_train" = s,
        "error" = getError(
          data = train_sample,
          model = model.tan,
          id.clase = 1
        ),
        "data" = "train",
        "rep" = r,
        "structure" = "net.tan"
      )
    )
    
  }
}


ggplot(data = res, aes(x = size_train, y = error, col = data)) +
  geom_line(stat = "summary", fun.y = "mean", size = 1.1) +
  scale_x_log10() + facet_wrap( ~ structure)

ggplot(data = res, aes(x = size_train, y = error, col = structure)) +
  geom_line(stat = "summary", fun.y = "mean", size = 1.1) +
  scale_x_log10() + facet_wrap( ~ data)



snb <- learnSelectiveNB(class_data, training = target)

sum(predictSelectiveNB(snb, data = test) != test[, 1]) / length(test[, 1])


ggplot(data = res, aes(x = size_train, y = error, col = data)) +
  geom_line(stat = "summary", fun.y = "mean", size = 1.1) +
  scale_x_log10() + facet_wrap( ~ structure)



