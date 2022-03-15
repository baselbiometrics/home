#############################################
# Exercises on machine learning techniques: #
#############################################



# Penalized regression:
# =====================

dat <- read.csv("dat.csv")

# Some functions only accept numeric inputs:
dat.num <- dat
for (i in 1:ncol(dat)) {
  if (is.factor(dat[, i])) {
    dat.num[, i] <- as.numeric(dat[, i]) - 1
  }
}


# Lasso:
# ------

# Load package:
require(glmnet)

# Fit a number of LASSO models:
fit.glmnet <- glmnet(as.matrix(dat.num[, 3:ncol(dat)]), dat[, 1], alpha = 1,
                     family = "binomial")
# alpha parameter to choose between ridge regression and lasso (or elastic net).
# alpha = 1 -> Lasso.
# alpha = 0 -> ridge regression.

# Save the estimated beta parameters as well as the lambda values:
b.vec <- fit.glmnet$beta
l.vec <- fit.glmnet$lambda

# Visualize the LASSO paths:
cols <- rainbow(ncol(dat)-2)
plot(b.vec[1, ] ~ l.vec, type = "l", ylim = c(-.1, .1), col = cols[1], lwd = 2,
     ylab = expression(beta), xlab = expression(lambda))
for (i in 2:(ncol(dat)-2)) {
  lines(b.vec[i, ] ~ l.vec, col = cols[i], lwd = 2)
}
abline(h = 0, lty = 2)

# Let us take a subset:
cols <- rainbow(10)
plot(b.vec[1, ] ~ l.vec, type = "l", ylim = c(-.1, .1), col = cols[1], lwd = 2,
     ylab = expression(beta), xlab = expression(lambda))
for (i in 2:10) {
  lines(b.vec[i, ] ~ l.vec, col = cols[i], lwd = 2)
}
abline(h = 0, lty = 2)


# Elastic net with cross-validation to find parameters:
# -----------------------------------------------------

# Load package:
require(caret)

# Fit an initial set of models:
fit.glmnet <- glmnet(as.matrix(dat.num[, 3:ncol(dat)]), dat.num[, 1], alpha = 1,
                     family = "binomial")

# Save the generated lambda values:
l.vec <- fit.glmnet$lambda

# Define a set of alpha values:
a.vec <- seq(0, 1, by = 0.1)

# Generate a grid over all lambda and alpha parameters:
gridelnet <- expand.grid(lambda = l.vec, alpha = a.vec)

# Define some general parameters for cross-validation:
elnet.ctrl <- trainControl(method = "cv", number = 5)

# Train the model:
set.seed(12345)
elnet.train <- train(as.matrix(dat.num[, 3:ncol(dat)]), dat[, 1], method = "glmnet",
                     preProcess = c("center","scale"), metric = "Accuracy",
                     tuneGrid = gridelnet, trControl = elnet.ctrl)

# Have a look at the results:
head(elnet.train$results)

# Optimal parameters:
elnet.train$bestTune$alpha
elnet.train$bestTune$lambda

# Fit lasso with optimal lambda and return the results:
fit.glmnet <- glmnet(as.matrix(dat.num[, 3:ncol(dat)]), dat[, 1], family = "binomial",
                     alpha = elnet.train$bestTune$alpha,
                     lambda = elnet.train$bestTune$lambda)
fit.glmnet$beta[which(fit.glmnet$beta != 0), ]


# Graphical representation:
fit.glmnet <- glmnet(as.matrix(dat.num[, 3:ncol(dat)]), dat[, 1], family = "binomial",
                     alpha = elnet.train$bestTune$alpha)
l.vec <- fit.glmnet$lambda
b.vec <- fit.glmnet$beta

cols <- rainbow(ncol(dat)-2)
plot(b.vec[1, ] ~ l.vec, type = "l", ylim = c(-.1, .1), col = cols[1], lwd = 2,
     ylab = expression(beta), xlab = expression(lambda))
for (i in 2:(ncol(dat)-2)) {
  lines(b.vec[i, ] ~ l.vec, col = cols[i], lwd = 2)
}
abline(h = 0, lty = 2)
abline(v = elnet.train$bestTune$lambda, lty = 2)

# Let us take a subset:
cols <- rainbow(10)
plot(b.vec[1, ] ~ l.vec, type = "l", ylim = c(-.1, .1), col = cols[1], lwd = 2,
     ylab = expression(beta), xlab = expression(lambda))
for (i in 2:10) {
  lines(b.vec[i, ] ~ l.vec, col = cols[i], lwd = 2)
}
abline(h = 0, lty = 2)
abline(v = elnet.train$bestTune$lambda, lty = 2)



# Trees, Bagging, Random forests, and Boosting:
# =============================================


# Fitting a single tree:
# ----------------------

# Load package:
require(rpart)

# Fit a single tree:
dat.tree <- rpart(ACR20 ~ ., data = dat, minbucket = 50)

# Load required package and plot the tree:
require(rpart.plot)
prp(dat.tree, type = 4, extra = 1, varlen = 0, main = 'Classification tree')


# Random forests:
# ---------------

# Load package:
require(randomForest)

# Fit a random forest model:
rand.forest <- randomForest(ACR20 ~ ., data=dat, ntree = 1000)
rand.forest

# Get some importance measure (based on the decrease in the Gini index)
# and sort variables:
varimp <- importance(rand.forest)
rownames(varimp)[order(varimp, decreasing = TRUE)]
varimp[order(varimp, decreasing = TRUE), ]

# Predict values:
table(predict(rand.forest)) # See parameter "newdata" to do predictions on new data.


# Boosting (Xgboost):
# ===================

# Load packages:
require(xgboost)
require(caret)
require(e1071)

# Just fit any boosting model (not recommended):
boost.fit <- xgboost(data = as.matrix(dat.num[, -1]), label = dat.num$ACR20, max.depth = 2,
                     nrounds = 10, objective = "binary:logistic", verbose = 1)


# Now, start with training the model first:
# Define some general parameters for cross-validation:
tr.control <- trainControl(method = "cv", number = 5)

# Define a grid of parameters:
tune_grid <- expand.grid(nrounds = seq(50, 500, by = 50), # max number of boosting iterations
                         max_depth = c(2, 5), # maximum depth of a tree
                         eta = c(0.01, 0.05), # control the learning rate
                         gamma = 0, # minimum loss reduction required to make a further partition on a leaf node of the tree
                         min_child_weight = 1, # minimum sum of instance weight (hessian) needed in a child
                         colsample_bytree = 2/3, # subsample ratio of columns when constructing each tree
                         subsample = 2/3) #  subsample ratio of the training instance

# Do the training:
boost.train <- train(ACR20 ~., data = dat, method = "xgbTree",
                     trControl=tr.control,
                     tuneGrid = tune_grid)

# Look at the results:
boost.train

# Plot the results:
ggplot(boost.train) +
  coord_cartesian(ylim = c(min(boost.train$results$Accuracy),
                                               max(boost.train$results$Accuracy))) +
  theme_bw()

# Identify the best parameters and look at the results:
ind.best <- best(boost.train$results, metric = "Accuracy", maximize = T)
boost.train$results[ind.best, ]

# You could also do the following:
final.parms <- expand.grid(nrounds = boost.train$bestTune$nrounds,
                         max_depth = boost.train$bestTune$max_depth,
                         eta = boost.train$bestTune$eta,
                         gamma = boost.train$bestTune$gamma,
                         colsample_bytree = boost.train$bestTune$colsample_bytree,
                         min_child_weight = boost.train$bestTune$min_child_weight,
                         subsample = boost.train$bestTune$subsample)

# Look at the "optimal" model:
boost.train$finalModel

# Predictions:
# It automatically chooses the "optimal" parameters:
head(predict(boost.train, dat, type = "prob"))
# You might want to exchange "dat" with new data.



# Finding subgroups:
# ==================

# Virtual twins:
# --------------

# Define some variables to simplify the code:
ACR20 <- dat$ACR20
TRT <- dat$TRT
X <- dat.num[, 3:56]
df_vt1 <- cbind(X, TRT, ACR20)

# Load package:
require(randomForest)

# Fit a random forest for the treatment group
# and another one for the placebo group.
fit0 <- randomForest(ACR20 ~ ., data=df_vt1[df_vt1$TRT == "Pbo", ], ntree = 1000)
fit1 <- randomForest(ACR20 ~ ., data=df_vt1[df_vt1$TRT == "Treat", ], ntree = 1000)

# Get potential outcomes:
Y0 <- as.vector(predict(fit0, newdata=df_vt1, type = "prob")[, 2])
Y1 <- as.vector(predict(fit1, newdata=df_vt1, type = "prob")[, 2])

df_vt2 <- df_vt1
df_vt2[c("ACR20", "TRT")] <- NULL

# Define Treatment difference:
trt.diff <- Y1 - Y0

# Fit randomforest on difference to get most important features
rfit <- randomForest(trt.diff ~ ., data=df_vt2, ntree = 1000)
varimp <- randomForest::importance(rfit)
ord <- order(varimp, decreasing = TRUE)
nams <- rownames(varimp)

# Take the 10 most important features:
vars_imp_vt <- as.character(nams[ord[1:10]])

# Fit a single tree based on treatment differences:
require(rpart)
vt_tree <- rpart(trt.diff ~ ., X[, vars_imp_vt], model = TRUE, minbucket = 100)

# Plot the tree:
require(rpart.plot)
prp(vt_tree, type = 4, extra = 1, cex = 1, varlen = 0, main = 'Virtual twins')



# Unsupervised learning:
# ======================


# k-means clustering:
# -------------------

# Based only on continuous laboratory variables:
dat.clust <- dat[, 14:ncol(dat)]

kmeans.res <- kmeans(dat.clust, centers = 3)

# Compare ACR20 rates between clusters:
prop.table(table(dat$ACR20[which(kmeans.res$cluster == 1)]))
prop.table(table(dat$ACR20[which(kmeans.res$cluster == 2)]))
prop.table(table(dat$ACR20[which(kmeans.res$cluster == 3)]))