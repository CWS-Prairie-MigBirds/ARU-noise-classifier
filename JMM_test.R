
## proportional odds test

## first screen using fixed effects only, the nominal test is not supported
## in the ordinal package. More involved steps required if keeping random effects in the mix

m6<- clm(acceptable ~ lowfreq_ratio + rms + bgn_bird_med +  pow_low_med +
           lowfreq_ratio*rms + lowfreq_ratio*bgn_bird_med, data = train_scaled)

nominal_test(m6)




### Predict using clmm

predict_clmm_fixed <- function(model, newdata) {
  # 1. Extract thresholds and fixed-effect coefficients
  theta <- model$alpha
  beta <- model$beta
  
  # 2. Build the design matrix for fixed effects (removing the intercept)
  tt <- delete.response(terms(model))
  X <- model.matrix(tt, data = newdata)[, -1, drop = FALSE]
  
  # 3. Compute linear predictor (eta = X * beta)
  eta <- as.vector(X %*% beta)
  
  # 4. Calculate cumulative probabilities using the inverse link function
  # (Assumes default logit link; use pnorm() for probit)
  cum_probs <- sapply(theta, function(a) plogis(a - eta))
  if (is.null(dim(cum_probs))) cum_probs <- matrix(cum_probs, nrow = 1)
  
  # 5. Convert cumulative probabilities to category probabilities
  probs <- cbind(cum_probs, 1) - cbind(0, cum_probs)
  colnames(probs) <- levels(model$model[[1]])
  
  return(probs)
}


# Run on new data

## four boreal recordings did not get noise covs??

test_scaled <- test_scaled %>% filter(!is.na(rms))

pred_probs <- predict_clmm_fixed(clm6, test_scaled)

# Get the most likely predicted category for each row
pred_class <- colnames(pred_probs)[max.col(pred_probs)]

test_scaled$pred_class <- colnames(pred_probs)[max.col(pred_probs)]

# 3. Bind the individual class probability columns to newdata
test_scaled <- cbind(test_scaled, as.data.frame(pred_probs))


# Confusion matrix (Observed vs. Predicted class)
table(Observed = test_scaled$acceptable, Predicted = test_scaled$pred_class)

# Overall classification accuracy
mean(test_scaled$acceptable == test_scaled$pred_class, na.rm = TRUE)

# Multiclass AUC (requires the matrix of predicted probabilities)
auc_res <- multiclass.roc(test_scaled$acceptable, predictor = pred_probs)
auc_res$auc
