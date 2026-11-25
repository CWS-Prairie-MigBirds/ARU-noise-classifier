## Model Building

##installing packages
install.packages("ordinal")
install.packages("bbmle")
install.packages("irr")
install.packages("caret")

##library
library(dplyr)
library(stringr)
library(ordinal)
library(performance)
library(bbmle)
library(irr)
library(caret)
library(broom)
library(tidyr)
library(ggplot2)
library(GGally)
library(brms)


df <- readRDS("data/complete_noise_dataset.rds")

#############
#---01 Remove malfunctions and order factor ------------------------------------
#############

## filtering for only values of y,n ,b in columm acceptable and 
## renaming to good, bad, borderline and ranking good < borderline < bad
df <- df |>
  filter(acceptable %in% c("y", "b", "n")) |>
  mutate(acceptable = factor(acceptable,
                             levels = c("y", "b", "n"),
                             labels = c("Good", "Borderline", "Bad"),
                             ordered = TRUE))

#############
#---02 Split data into training and testing ------------------------------------
#############

##split into 70% of property locations for grassland and 30% for testing
set.seed(6)

grassland_df <- df |> 
  filter(habitat == "grassland") |>
  mutate(SSU = str_split_i(site, "/", -1),
         PSU = str_remove(SSU, "-[^-]+$"),
         row_id = row_number())

## ok so we have PSUs with varying numbers of recordings, let's check it out

grass_psu <- grassland_df  %>% group_by(PSU) %>% summarise(n = n())

grass_ssu <- grassland_df  %>% group_by(SSU) %>% summarise(n = n())

## take 70% of the SSUs for training, 
g_ssu_70 <- grass_ssu |> 
  slice_sample(prop = 0.7)

grassland_70 <- grassland_df |>
  filter(SSU %in% g_ssu_70$SSU)

grassland_30 <- grassland_df |>
  filter(!row_id %in% grassland_70$row_id)
  
##split into 70% of PSUs for boreal
boreal_df <- df |> filter(habitat == "boreal")

## pulling PSU out of the path (after the second BBMP)
boreal_df <- boreal_df |>
  mutate(PSU = str_split_fixed(gsub("\\\\", "/", as.character(site)), "/", 7)[, 5])

boreal_psu <- boreal_df %>% group_by(PSU) %>% summarise(n = n())


#splitting into 70% of PSUs
set.seed(6)
b_psu_70 <- boreal_df |>
  distinct(PSU) |>
  slice_sample(prop = 0.7)

##keeping the recording from those PSUs and keep remaining 30% for test data
boreal_70 <- boreal_df |>
  semi_join(b_psu_70, by = "PSU")

boreal_30 <- boreal_df |>
  anti_join(b_psu_70, by = "PSU")

##combining the 70% boreal and grassland
df_70 <- bind_rows(grassland_70, boreal_70)

##combining the 30% boreal and grassland
df_30 <- bind_rows(grassland_30, boreal_30)

#############
#---03 Check correlations  ------------------------------------
#############

##correlogram for the combined 70% df
covs_70 <- df_70 |>
  select(all_of(covs))

corr_70 <- ggcorr(covs_70,
                      method = c("pairwise", "spearman"),
                      label = TRUE, label_size = 3, label_round = 2,
                      hjust = 0.9, layout.exp = 2,) +
  labs(title = "Correlogram of Covariates") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold")) 
corr_70


#############
#---04 Z-score training data ------------------------------------
#############

## z-transform noise covariates
## use mean and SD from training data to scale the test data
# 1. Compute parameters from training data
scaler <- preProcess(ordered_70, method = c("center", "scale"))

# 2. Apply to both sets (non-numeric columns are ignored automatically)
train_scaled <- predict(scaler, ordered_70)
test_scaled  <- predict(scaler, ordered_30)


#############
#---05 Test covariate subgroups from Ruido package ------------------------------------
#############

#### Build models for candidate variable subgroups and select which to retain to avoid inclusion of correlated variables

### entropy
ent1<-clmm(acceptable ~ ent_bird_med + (1|site), data = train_scaled)
ent2<-clmm(acceptable ~ ent_bird_max + (1|site), data = train_scaled)
ent3<-clmm(acceptable ~ ent_low_med + (1|site), data = train_scaled)
AICtab(ent1, ent2, ent3) ### retain only ent_low_med

### power
pow1<-clmm(acceptable ~ pow_bird_med + (1|site), data = train_scaled)
pow2<-clmm(acceptable ~ pow_bird_max + (1|site), data = train_scaled)
pow3<-clmm(acceptable ~ pow_low_med + (1|site), data = train_scaled)
AICtab(pow1, pow2, pow3) ### retain only pow_low_med

### bgn
bgn1<-clmm(acceptable ~ bgn_bird_med + (1|site), data = train_scaled)
bgn2<-clmm(acceptable ~ bgn_bird_max + (1|site), data = train_scaled)
bgn3<-clmm(acceptable ~ bgn_low_med + (1|site), data = train_scaled)
AICtab(bgn1, bgn2, bgn3) ### retain only bgn_bird_med


cor(train_scaled$lowfreq_ratio, train_scaled$rms,use = "pairwise.complete.obs")
plot(train_scaled$lowfreq_ratio, train_scaled$rms)

plot(train_scaled$bgn_bird_med, train_scaled$pow_low_med)
plot(train_scaled$ent_low_med, train_scaled$pow_low_med)

#############
#---03 Main Model Testing  ------------------------------------
#############

##global main effects model
clm1<- clmm(acceptable ~ lowfreq_ratio + rms + bgn_bird_med +  pow_low_med + ent_low_med + (1|site), data = train_scaled)

##checking for collinearity 
performance::check_collinearity(clm1)

## can't use pow and ent low med in the same model, test which one is better:

pve1<-clmm(acceptable ~ pow_low_med + (1|site), data = train_scaled)
pve2<-clmm(acceptable ~ ent_low_med + (1|site), data = train_scaled)
AICtab(pve1, pve2) ### retain only pow_low_med

##global model with all interactions

clm2<- clmm(acceptable ~ lowfreq_ratio + rms + bgn_bird_med +  pow_low_med + 
              lowfreq_ratio*rms  + 
              bgn_bird_med*pow_low_med +
              bgn_bird_med*rms +
              (1|site), data = train_scaled)

##checking for collinearity 
performance::check_collinearity(clm2)

# additional candidate models

clm3<- clmm(acceptable ~ lowfreq_ratio + rms + bgn_bird_med +  pow_low_med + lowfreq_ratio*rms + (1|site), data = train_scaled)
clm4<- clmm(acceptable ~ lowfreq_ratio + rms + bgn_bird_med +  pow_low_med + lowfreq_ratio*bgn_bird_med + (1|site), data = train_scaled)
clm5<- clmm(acceptable ~ lowfreq_ratio + rms + bgn_bird_med +  pow_low_med + bgn_bird_med*pow_low_med +(1|site), data = train_scaled)
clm6<- clmm(acceptable ~ lowfreq_ratio + rms + bgn_bird_med +  pow_low_med + lowfreq_ratio*rms + lowfreq_ratio*bgn_bird_med + (1|site), data = train_scaled)
clm7<- clmm(acceptable ~ lowfreq_ratio + rms + bgn_bird_med +  pow_low_med + lowfreq_ratio*rms + bgn_bird_med*pow_low_med + (1|site), data = train_scaled)
clm8<- clmm(acceptable ~ lowfreq_ratio + rms + bgn_bird_med +  pow_low_med + bgn_bird_med*pow_low_med + rms*bgn_bird_med +(1|site), data = train_scaled)
clm9<- clmm(acceptable ~ lowfreq_ratio + rms + bgn_bird_med + lowfreq_ratio*rms + lowfreq_ratio*bgn_bird_med + (1|site), data = train_scaled)

####clmm2 model attempt####

##rebuilding levels for site because i was getting an error saying i had less than 2 sites
ordered_70$site_id <- factor(as.character(ordered_70$site))

##confirming number of sites
nlevels(ordered_70$site_id)

##model
clm9 <- clmm2(
  acceptable ~ rms + lowfreq_ratio:rms + lowfreq_ratio:bgn_bird_med,
  nominal = ~ lowfreq_ratio + bgn_bird_med + pow_low_med,
  random = site_id,
  data = ordered_70,
  Hess = TRUE,
  control = clmm2.control(method = "nlminb", iter.max = 4000)
)
summary(clm9)

##checking for collinearity 
performance::check_collinearity(clm8)

AICtab(clm1, clm2, clm3, clm4, clm5, clm6, clm7, clm8, clm9)

#### can we simplify the top model?
drop1(clm6)


##Candidate models
models <- list(clm1 = clm1, clm2 = clm2, clm3 = clm3, clm4 = clm4, clm5 = clm5,
               clm6 = clm6, clm7 = clm7, clm8 = clm8, clm9 = clm9)

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

##analyzing models##
summary(clm6)
confint(clm6)

summary(clm2)
confint(clm2)

saveRDS(clm6, "clm6.rds")


#############
#---04 Derive optimal thresholds  ------------------------------------
#############


##drop rows with missing predictors, so predictions and observed line up
vars <- all.vars(formula(clm6))[-1]
nd_70 <- train_scaled[complete.cases(train_scaled[, vars]), ]


# Actual observed categories
obs_70 <- factor(
  train_scaled$acceptable,
  levels = c("Good", "Borderline", "Bad"),
  ordered = TRUE
)

##calculating probabilities manually for each row in nd_30 because clmm cannot use predict()
fixed_formula <- lme4::nobars(formula(clm6)) ##removing random effect term

X <- model.matrix(
  delete.response(terms(fixed_formula)),
  data = nd_70
) ##model matrix converts nd_70 data to numerical values to calculate predicted values

##removing intercept because clmm stores the intercept as a parameter
X <- X[, colnames(X) != "(Intercept)", drop = FALSE]

beta <- clm6$beta ##extracting coefficients from model
X <- X[, names(beta), drop = FALSE] ##keeping only coefficients extracted from model

##calculate linear predictor
linearp <- as.vector(X %*% clm6$beta)

alpha <- clm6$alpha ##extracting threshold parameters
cumu_probs <- sapply(alpha, function(a) plogis(a - linearp)) ##calculating cumulative probabilities

##calculate probability of each category from cumulative probabilities
probs_70 <- cbind(
  Good = cumu_probs[, 1],
  Borderline = cumu_probs[, 2] - cumu_probs[, 1],
  Bad = 1 - cumu_probs[, 2]
)

##checking dimensions
dim(probs_70)

##Checking probabilities sum to 1
head(rowSums(probs_70))

classify_two_threshold <- function(probs, good_cut, bad_cut) {
  
  pred <- rep(NA_character_, nrow(probs))
  
  pred[probs[, "Bad"] >= bad_cut] <- "Bad"
  
  pred[
    is.na(pred) &
      probs[, "Good"] >= good_cut
  ] <- "Good"
  
  pred[is.na(pred)] <- "Borderline"
  
  factor(
    pred,
    levels = c("Good", "Borderline", "Bad"),
    ordered = TRUE
  )
}

#creating threshold combinations
grid <- expand.grid(
  good_cut = seq(0.50, 0.95, by = 0.01),
  bad_cut = seq(0.05, 0.80, by = 0.01)
)

##evaluating every combination
results_train <- vector("list", nrow(grid))

for (i in seq_len(nrow(grid))) {
  
  pred <- classify_two_threshold(
    probs_70,
    good_cut = grid$good_cut[i],
    bad_cut = grid$bad_cut[i]
  )
  
  results_train[[i]] <- data.frame(
    good_cut = grid$good_cut[i],
    bad_cut = grid$bad_cut[i],
    kappa = irr::kappa2(
      data.frame(obs_70, pred),
      weight = "squared"
    )$value
  )
}

results_train <- do.call(rbind, results_train)

##finding optimal thresholds
best_train <- results_train[
  which.max(results_train$kappa),
]

best_train ##optimal thresholds##

##############applying the thresholds##############
##classifying based on highest probability 
###same thing from optimized thresholdes but for nd_30 now###
vars <- all.vars(formula(clm6))[-1]
nd_30 <- test_scaled[complete.cases(test_scaled[, vars]), ]

fixed_formula <- lme4::nobars(formula(clm6))

X <- model.matrix(delete.response(terms(fixed_formula)), data = nd_30)
X <- X[, colnames(X) != "(Intercept)", drop = FALSE]
X <- X[, names(clm6$beta), drop = FALSE]

linearp <- as.vector(X %*% clm6$beta)
cumu <- sapply(clm6$alpha, function(a) plogis(a - linearp))

probs_30 <- cbind(Good       = cumu[, 1],
                  Borderline = cumu[, 2] - cumu[, 1],
                  Bad        = 1 - cumu[, 2])

dim(probs_30)              # n rows, 3 columns
nrow(probs_30) == nrow(nd_30)   # should be TRUE
range(rowSums(probs_30))   # should be 1 1



default_30 <- colnames(probs_30)[max.col(probs_30, ties.method = "first")]

##override with good and bad thresholds from optimizing thresholds best_train
good_cutoff <- 0.86
bad_cutoff <- 0.38

adjusted_30 <- rep(NA_character_, nrow(probs_30))

##classifying as bad if the probability exceeds the bad threshold
adjusted_30[probs_30[, "Bad"] >= bad_cutoff] <- "Bad"

##classifying as good if the probability exceeds the threshold
adjusted_30[
  is.na(adjusted_30) & probs_30[, "Good"] >= good_cutoff
] <- "Good"

##classify all remaining rows as Borderline
adjusted_30[is.na(adjusted_30)] <- "Borderline"

##levels
lv <- c("Good", "Borderline", "Bad")
##converting back to ordered factor
default_30 <- factor(default_30, levels = lv, ordered = TRUE)  ##predicted categories without cutoff
adjusted_30     <- factor(adjusted_30,     levels = lv, ordered = TRUE) ##predicted categories with cutoff
observed   <- factor(nd_30$acceptable, levels = lv, ordered = TRUE) ##categories scored by rater

##creating confusion matrices
confusionMatrix(default_30, observed)   
confusionMatrix(adjusted_30, observed)    




####plots and graphs####
library(pROC)

###ROC curve testing good vs other###
#actual test outcomes scored from rater
actual_good <- ifelse(nd_30$acceptable == "Good", 1, 0)

#predicted probability of good
pred_good <- probs_30[, "Good"]

# ROC curve
roc_good <- roc(
  response = actual_good,
  predictor = pred_good
)

# AUC
auc(roc_good)

# Plot
good <- plot(
  roc_good,
  col = "darkgreen",
  lwd = 2,
  main = paste(
    "Good vs Other ROC (AUC =",
    round(auc(roc_good), 3),
    ")"
  )
)

abline(a = 0, b = 1, lty = 2, col = "grey50")


##ROC curve testing borerline vs other##
actual_borderline <- ifelse(nd_30$acceptable == "Borderline", 1, 0)

#predicted probability of good
pred_borderline <- probs_30[, "Borderline"]

# ROC curve
roc_borderline <- roc(
  response = actual_borderline,
  predictor = pred_borderline
)

# AUC
auc(roc_borderline)

# Plot
plot(
  roc_borderline,
  col = "darkgreen",
  lwd = 2,
  main = paste(
    "Borderline vs Other ROC (AUC =",
    round(auc(roc_borderline), 3),
    ")"
  )
)

abline(a = 0, b = 1, lty = 2, col = "grey50")

##ROC curve testing bad vs other##
actual_bad <- ifelse(nd_30$acceptable == "Bad", 1, 0)

#predicted probability of good
pred_bad <- probs_30[, "Bad"]

# ROC curve
roc_bad <- roc(
  response = actual_bad,
  predictor = pred_bad
)

# AUC
auc(roc_bad)

# Plot
plot(
  roc_bad,
  col = "darkgreen",
  lwd = 2,
  main = paste(
    "Bad vs Other ROC (AUC =",
    round(auc(roc_bad), 3),
    ")"
  )
)

abline(a = 0, b = 1, lty = 2, col = "grey50")

##predicted probabilities##
library(dplyr)

fixed_formula <- lme4::nobars(formula(clm6))
vals <- all.vars(fixed_formula)[-1]    

##lowfreq values
lf_vals <- quantile(nd_70$lowfreq_ratio, probs = c(0.025, 0.5, 0.975), na.rm = TRUE)

##rms sequence from the 1st to the 99th percentile
rms_seq <- seq(quantile(nd_70$rms, 0.01, na.rm = TRUE),
               quantile(nd_70$rms, 0.99, na.rm = TRUE),
               length.out = 100)

##every rms value at each of the three lowfreq_ratio values
newdat <- expand.grid(rms = rms_seq, lowfreq_ratio = lf_vals)

##hold the other predictors at their median
hold_vars <- setdiff(vals, c("rms", "lowfreq_ratio"))
for (v in hold_vars) newdat[[v]] <- median(nd_70[[v]], na.rm = TRUE)

##legend labels
newdat$lf_level <- factor(rep(c("Low (2.5%)", "Median", "High (97.5%)"), each = length(rms_seq)),
                          levels = c("Low (2.5%)", "Median", "High (97.5%)"))
nrow(newdat)    # 300
##model matrix converting data to numerical
X_grid <- model.matrix(delete.response(terms(fixed_formula)), data = newdat)
X_grid <- X_grid[, colnames(X_grid) != "(Intercept)", drop = FALSE]
X_grid <- X_grid[, names(clm6$beta), drop = FALSE]

linearp_grid <- as.vector(X_grid %*% clm6$beta)
cumu_grid <- sapply(clm6$alpha, function(a) plogis(a - linearp_grid))

probs_grid <- cbind(Good       = cumu_grid[, 1],
                    Borderline = cumu_grid[, 2] - cumu_grid[, 1],
                    Bad        = 1 - cumu_grid[, 2])
dim(probs_grid)   # 300 rows, 3 columns

plot_df <- bind_cols(newdat, as.data.frame(probs_grid)) |>
  pivot_longer(c(Good, Borderline, Bad),
               names_to = "quality", values_to = "prob") |>
  mutate(quality = factor(quality, levels = c("Good", "Borderline", "Bad")))


p_all <- ggplot(plot_df, aes(x = rms, y = prob, colour = lf_level)) +
  geom_line(linewidth = 1) +
  facet_wrap(~ quality, ncol = 1) +
  scale_colour_brewer(palette = "Set1") +
  labs(x = "rms (z-score)",
       y = "Predicted probability",
       colour = "lowfreq_ratio") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

p_all

ggsave("p_all.png", plot = p_all, width = 8, height = 6, units = "px", dpi = 300)

##predicted probabilities on raw rms values

##original mean and SD of rms
mean_rms <- mean(df$rms, na.rm = TRUE)
sd_rms <- sd(df$rms, na.rm = TRUE)

##################################################################################
#################################################################################
#############################################################################
#### older code I moved down here for now



covs <- c("lowfreq_ratio", "rms",
          "bgn_bird_med", "bgn_bird_max", "bgn_low_med",
          "pow_bird_med", "pow_bird_max", "pow_low_med",
          "ent_bird_med", "ent_bird_max", "ent_low_med")

z_df <- df |>
  mutate(across(all_of(covs),
                ~ as.numeric(scale(.x))))



##filtering for only values of y,n ,b in acceptable and renaming to good, bad, borderline and ranking good < borderline < bad
ordered_70 <- df_70 |>
  filter(acceptable %in% c("y", "b", "n")) |>
  mutate(acceptable = factor(acceptable,
                             levels = c("y", "b", "n"),
                             labels = c("Good", "Borderline", "Bad"),
                             ordered = TRUE))

ordered_30 <- df_30 |>
  filter(acceptable %in% c("y", "b", "n")) |>
  mutate(acceptable = factor(acceptable,
                             levels = c("y", "b", "n"),
                             labels = c("Good", "Borderline", "Bad"),
                             ordered = TRUE))

### Drop some of the highly correlated variables
##JMM: I HAVEN:T UPDATED THE OBJECTS IN THIS FIGURE

covs_70 <- df_70 |>
  select(all_of(covs))
covs_70<-covs_70[, !names(covs_70) %in% c("ent_bird_med", "ent_bird_max", "pow_bird_med", "pow_bird_max", "bgn_bird_max", "bgn_low_med")]

corr_70 <- ggcorr(covs_70,
                  method = c("pairwise", "spearman"),
                  label = TRUE, label_size = 3, label_round = 2,
                  hjust = 0.9, layout.exp = 2,) +
  labs(title = "Correlogram of Covariates") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold")) 
corr_70
##back transforming plot data##
plot_df$rms_raw <- plot_df$rms * sd_rms + mean_rms

##plot
p_raw <- ggplot(plot_df, aes(x = rms_raw, y = prob, colour = lf_level)) +
  geom_line(linewidth = 1) +
  facet_wrap(~ quality, ncol = 1) +
  scale_colour_brewer(palette = "Set1") +
  labs(x = "rms",
       y = "Predicted probability",
       colour = "lowfreq_ratio") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))
p_raw
