## Model Building

library(dplyr)
library(stringr)
library(ordinal)

## z-transform noise covariates

covs <- c("lowfreq_ratio", "rms",
          "bgn_bird_med", "bgn_bird_max", "bgn_low_med",
          "pow_bird_med", "pow_bird_max", "pow_low_med",
          "ent_bird_med", "ent_bird_max", "ent_low_med")

z_df <- df |>
  mutate(across(all_of(covs),
                ~ as.numeric(scale(.x))))


## split into 70% of property locations for grassland
set.seed(123)
grassland_df <- z_df %>% filter(habitat == "grassland")
grassland_split <- grassland_df %>% slice_sample(prop = 0.7)

## split into 70% of PSUs for boreal
boreal_df <- z_df %>% filter(habitat == "boreal")

# 2. Pull the PSU out of the path (the folder right after the second BBMP)
#    Handles both / and \ in paths
boreal_df <- boreal_df %>%
  mutate(psu = str_split_fixed(gsub("\\\\", "/", as.character(site)), "/", 7)[, 5])

sum(is.na(boreal_df$psu))   # should be 0
table(boreal_df$psu)

# 4. Randomly pick 70% of the PSUs
set.seed(123)
psus_70 <- boreal_df %>%
  distinct(psu) %>%
  slice_sample(prop = 0.7)

# 5. Keep every recording from those PSUs
boreal_70 <- boreal_df %>%
  semi_join(psus_70, by = "psu")

# 6. (Optional) the other 30% of PSUs, e.g. as a test set
boreal_30 <- boreal_df %>%
  anti_join(psus_70, by = "psu")

## then graph covs again


##


############################################################
################################################################
## code below is from Steve's initial pilot

### global model for boreal
boreal_ord <- boreal_70 %>%
  filter(acceptable %in% c("y", "b", "n")) %>%
  mutate(acceptable = factor(acceptable,
                             levels = c("y", "b", "n"),
                             labels = c("Good", "Borderline", "Bad"),
                             ordered = TRUE))

##global model
clm1<- clm(acceptable ~ lowfreq_ratio +rms + bgn_bird_med + bgn_bird_max +
bgn_low_med + pow_bird_med + pow_bird_max + pow_low_med + ent_bird_med +ent_bird_max +ent_low_med, 
data = boreal_ord)
summary(clm1)

clm2<- clm(acceptable ~ lowfreq_ratio +rms + bgn_bird_med + pow_bird_med + pow_bird_max + pow_low_med + ent_bird_med +ent_bird_max +ent_low_med, 
           data = boreal_ord)
summary(clm2)

clm3<- clm(acceptable ~ lowfreq_ratio +rms + bgn_bird_max +
             pow_bird_med + pow_bird_max + pow_low_med + ent_bird_med +ent_bird_max +ent_low_med, 
           data = boreal_ord)
summary(clm3)

clm4<- clm(acceptable ~ lowfreq_ratio +rms +
             bgn_low_med + pow_bird_med + pow_bird_max + pow_low_med + ent_bird_med +ent_bird_max +ent_low_med, 
           data = boreal_ord)
summary(clm4)

## two-variable

clm5<- clm(acceptable ~ rms + bgn_low_med,
           data = boreal_ord)
summary(clm5)

# 1. Expected vs observed counts
probs <- predict(clm5, newdata = boreal_ord, type = "prob")$fit
colSums(probs)     # model-expected Good / Borderline / Bad
table(boreal_ord$acceptable)     # observed

# 2. Can it tell Bad from not-Bad? (AUC: 0.5 = coin flip, 1 = perfect)
library(pROC)
roc(boreal_ord$acceptable == "Bad", probs[, "Bad"], na.rm = TRUE)

glm2<- glm(formula = acceptable ~ z_lowfreq + z_rms, family = binomial, data = noise_metrics)
glm3<- glm(formula = acceptable ~ z_lowfreq + z_aci, family = binomial, data = noise_metrics)
glm4<- glm(formula = acceptable ~  z_rms + z_aci, family = binomial, data = noise_metrics)

# single variable
glm5<- glm(formula = acceptable ~ z_lowfreq, family = binomial, data = noise_metrics)
glm6<- glm(formula = acceptable ~ z_aci, family = binomial, data = noise_metrics)
glm7<- glm(formula = acceptable ~  z_rms, family = binomial, data = noise_metrics)

#Null
glm8<- glm(formula = acceptable ~  1, family = binomial, data = noise_metrics) # NULL


glm9<- glm(formula = acceptable ~ z_lowfreq * z_rms, family = binomial, data = noise_metrics)
glm10<- glm(formula = acceptable ~ z_lowfreq * z_aci, family = binomial, data = noise_metrics)


# Candidate models
models <- list(
  "lowfreq + rms + aci"      = glm1,
  "lowfreq + rms"            = glm2,
  "lowfreq + aci"            = glm3,
  "rms + aci"                = glm4,
  "lowfreq"                  = glm5,
  "aci"                      = glm6,
  "rms"                      = glm7,
  "null"                     = glm8,
)

# Build model selection table
aic_tab <- data.frame(
  Model = names(models),
  K = sapply(models, function(x) attr(logLik(x), "df")),
  AIC = sapply(models, AIC)
)

# Rank models
aic_tab <- aic_tab[order(aic_tab$AIC), ]

# Delta AIC
aic_tab$Delta_AIC <- aic_tab$AIC - min(aic_tab$AIC)

# Akaike weights
aic_tab$Weight <- exp(-0.5 * aic_tab$Delta_AIC)
aic_tab$Weight <- aic_tab$Weight / sum(aic_tab$Weight)

# Cumulative weights
aic_tab$CumWt <- cumsum(aic_tab$Weight)

# Round for display
aic_tab$AIC <- round(aic_tab$AIC, 2)
aic_tab$Delta_AIC <- round(aic_tab$Delta_AIC, 2)
aic_tab$Weight <- round(aic_tab$Weight, 3)
aic_tab$CumWt <- round(aic_tab$CumWt, 3)

aic_tab

# Optional: print nicely
print(aic_tab, row.names = FALSE)


colMeans(noise_metrics[, c("time_read","time_spec","time_rms","time_aci","time_total")],na.rm = TRUE)



library(gbm)

brt1 <- gbm(
  formula = acceptable ~ z_lowfreq + z_rms + z_aci,
  data = noise_metrics,
  distribution = "bernoulli",
  n.trees = 5000,
  interaction.depth = 3,
  shrinkage = 0.01,
  bag.fraction = 0.5,
  cv.folds = 10,
  n.minobsinnode = 5,
  verbose = FALSE
)

best_iter <- gbm.perf(
  brt1,
  method = "cv"
)

noise_metrics$pred_brt <- predict(
  brt1,
  newdata = noise_metrics,
  n.trees = best_iter,
  type = "response"
)

summary(brt1, n.trees = best_iter)

library(pROC)

roc_brt <- roc(
  response = wind_metrics$acceptable,
  predictor = wind_metrics$pred_brt
)

auc(roc_brt)

plot(
  roc_brt,
  col = "darkgreen",
  lwd = 2,
  main = paste(
    "BRT ROC Curve (AUC =",
    round(auc(roc_brt), 3),
    ")"
  )
)

abline(a = 0, b = 1, lty = 2, col = "grey")



glm2 <- glm(
  acceptable ~ z_lowfreq + z_rms,
  family = binomial,
  data = noise_metrics
)

noise_metrics$pred_glm2 <- predict(
  glm2,
  type = "response"
)
roc_glm2 <- roc(
  response = noise_metrics$acceptable,
  predictor = noise_metrics$pred_glm2
)
auc(roc_glm2)



glm2 <- glm(
  acceptable ~ z_lowfreq + z_rms,
  family = binomial,
  data = noise_metrics
)

noise_metrics$pred_prob <- predict(
  glm2,
  type = "response"
)
library(pROC)

roc_glm2 <- roc(
  response = noise_metrics$acceptable,
  predictor = noise_metrics$pred_prob
)

best <- coords(
  roc_glm2,
  x = "best",
  best.method = "youden",
  ret = c(
    "threshold",
    "sensitivity",
    "specificity"
  )
)

best

opt_thresh <- as.numeric(best["threshold"])

noise_metrics$pred_class <- ifelse(
  noise_metrics$pred_prob >= opt_thresh,
  1,
  0
)

### Confusiotn matrix
(cm <- table(
  Observed = noise_metrics$acceptable,
  Predicted = noise_metrics$pred_class
))

library(pROC)

# ROC object
roc_glm2 <- roc(
  response = noise_metrics$acceptable,
  predictor = noise_metrics$pred_prob
)

# Optimal threshold
best <- coords(
  roc_glm2,
  x = "best",
  best.method = "youden",
  ret = c("threshold", "sensitivity", "specificity")
)

# Plot ROC
plot(
  roc_glm2,
  col = "blue",
  lwd = 2,
  main = paste(
    "ROC Curve (AUC =",
    round(auc(roc_glm2), 3),
    ")"
  )
)

abline(a = 0, b = 1, lty = 2, col = "grey50")

# Add optimal threshold point
points(
  x = 1 - best["specificity"],
  y = best["sensitivity"],
  pch = 19,
  col = "red",
  cex = 1.5
)

# Label the point
text(
  x = 1 - best["specificity"],
  y = best["sensitivity"],
  labels = paste(
    "Threshold =",
    round(best["threshold"], 3)
  ),
  pos = 4,
  col = "red"
)