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

## z-transform noise covariates

covs <- c("lowfreq_ratio", "rms",
          "bgn_bird_med", "bgn_bird_max", "bgn_low_med",
          "pow_bird_med", "pow_bird_max", "pow_low_med",
          "ent_bird_med", "ent_bird_max", "ent_low_med")

z_df <- df |>
  mutate(across(all_of(covs),
                ~ as.numeric(scale(.x))))

##split into 70% of property locations for grassland and 30% for testing
set.seed(6)
grassland_df <- z_df |> 
  filter(habitat == "grassland") |>
  mutate(row_id = row_number())
grassland_70 <- grassland_df |> 
  slice_sample(prop = 0.7)

grassland_30 <- grassland_df |>
  filter(!row_id %in% grassland_70$row_id)
  
##split into 70% of PSUs for boreal
boreal_df <- z_df |> filter(habitat == "boreal")

## pulling PSU out of the path (after the second BBMP)
boreal_df <- boreal_df |>
  mutate(psu = str_split_fixed(gsub("\\\\", "/", as.character(site)), "/", 7)[, 5])

#splitting into 70% of PSUs
set.seed(6)
psus_70 <- boreal_df |>
  distinct(psu) |>
  slice_sample(prop = 0.7)

##keeping the recording from those PSUs and keep remaining 30% for test data
boreal_70 <- boreal_df |>
  semi_join(psus_70, by = "psu")

boreal_30 <- boreal_df |>
  anti_join(psus_70, by = "psu")

##combining the 70% boreal and grassland
df_70 <- bind_rows(grassland_70, boreal_70)

##combining the 30% boreal and grassland
df_30 <- bind_rows(grassland_30, boreal_30)

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

##filtering for only values of y,n ,b in acceptable and renaming to good, bad, borderline and ranking good < borderline < bad
ordered_70 <- df_70 |>
  filter(acceptable %in% c("y", "b", "n")) |>
  mutate(acceptable = factor(acceptable,
                             levels = c("y", "b", "n"),
                             labels = c("Good", "Borderline", "Bad"),
                             ordered = TRUE))
##global model
clm1<- clmm(acceptable ~ lowfreq_ratio + rms + bgn_bird_med + bgn_bird_max +
bgn_low_med + pow_bird_med + pow_bird_max + pow_low_med + ent_bird_med + ent_bird_max + ent_low_med + (1|site), 
data = ordered_70)

##checking for collinearity 
performance::check_collinearity(clm1)

#### all bgns are highly correlated so we will choose which to keep ####

##global model with bgn_bird_med
clm2<- clmm(acceptable ~ lowfreq_ratio +rms + bgn_bird_med + pow_bird_med + pow_bird_max + pow_low_med + ent_bird_med +
             ent_bird_max + ent_low_med  + (1|site) , 
           data = ordered_70)

##global model with bgn_bird_max
clm3<- clmm(acceptable ~ lowfreq_ratio +rms + bgn_bird_max +
             pow_bird_med + pow_bird_max + pow_low_med + ent_bird_med + ent_bird_max + ent_low_med  + (1|site), 
           data = ordered_70)

##global model with bgn_low_Med
clm4<- clmm(acceptable ~ lowfreq_ratio +rms +
             bgn_low_med + pow_bird_med + pow_bird_max + pow_low_med + ent_bird_med + ent_bird_max + ent_low_med  + (1|site), 
           data = ordered_70)

##selcting which model is best using AIC 
AICtab(clm1, clm2, clm3, clm4)

##Other models -- clm1 but testing without each of the highly correlated variables

clm5 <- clmm(acceptable ~ lowfreq_ratio +rms +
              bgn_low_med + bgn_bird_max + pow_bird_max + pow_low_med + pow_bird_med + ent_bird_med + ent_bird_max + ent_low_med
             + (1|site), 
            data = ordered_70) ##no bgn_bird_med

clm6 <- clmm(acceptable ~ lowfreq_ratio +rms +
              bgn_low_med + bgn_bird_med + pow_bird_max + pow_low_med + pow_bird_med + ent_bird_med + ent_bird_max + ent_low_med
             + (1|site), 
            data = ordered_70) ##no bgn_bird_max

clm7 <- clmm(acceptable ~ lowfreq_ratio +rms +
              bgn_low_med + bgn_bird_med + bgn_bird_max + pow_bird_max + pow_low_med + ent_bird_med + ent_bird_max + ent_low_med
             + (1|site), 
            data = ordered_70) ##no pow_bird_med

clm8 <- clmm(acceptable ~ lowfreq_ratio +rms +
              bgn_low_med + bgn_bird_med + bgn_bird_max + pow_low_med + pow_bird_med + ent_bird_med + ent_bird_max + ent_low_med
             + (1|site), 
            data = ordered_70) ##no pow_bird_max

clm9 <- clmm(acceptable ~ lowfreq_ratio +rms +
              bgn_low_med + bgn_bird_med + bgn_bird_max + pow_bird_max + pow_low_med + pow_bird_med + ent_bird_med + ent_bird_max + 
              ent_low_med + (1|site), 
            data = ordered_70) ##no pow_low_med

clm10 <- clmm(acceptable ~ lowfreq_ratio +rms +
               bgn_low_med + bgn_bird_med + bgn_bird_max + pow_bird_max + pow_low_med + pow_bird_med + ent_bird_max + ent_low_med
              + (1|site), 
             data = ordered_70) ##no ent_bird_med

clm11 <- clmm(acceptable ~ lowfreq_ratio +rms +
               bgn_low_med + bgn_bird_med + bgn_bird_max + pow_bird_max + pow_low_med + pow_bird_med + ent_bird_med + ent_bird_max
              + ent_low_med + (1|site), 
             data = ordered_70) ##no ent_bird_max

##model with only variables from the noise band (low frequency band)
clm12 <- clmm(acceptable ~ lowfreq_ratio + rms + bgn_low_med + pow_low_med + ent_low_med + (1|site),
             data = ordered_70) 

## clm13 and clm14 are clm12 with either rms or lowfreq_ratio
clm13 <- clmm(acceptable ~ lowfreq_ratio + bgn_low_med + pow_low_med + ent_low_med + (1|site),
             data = ordered_70)

clm14 <- clmm(acceptable ~ rms + bgn_low_med + pow_low_med + ent_low_med + (1|site),
             data = ordered_70)

##model with only variables from the bird band, and then with rms or lowfreq_ratio
clm15 <- clmm(acceptable ~ lowfreq_ratio + rms + bgn_bird_med + pow_bird_med + ent_bird_med + (1|site),
             data = ordered_70)

clm16 <- clmm(acceptable ~ lowfreq_ratio + bgn_bird_med + pow_bird_med + ent_bird_med + (1|site),
             data = ordered_70)

clm17 <- clmm(acceptable ~ rms + bgn_bird_med + pow_bird_med + ent_bird_med + (1|site),
             data = ordered_70)

##model with rms and lowfreq_ratio removed
clm18 <- clmm(acceptable ~ bgn_low_med + pow_low_med + ent_low_med + (1|site),
data = ordered_70)

##model based on clm8 (best model) with covariates CI encompassing 0 removed
clm19 <- clmm(acceptable ~ rms + lowfreq_ratio + bgn_low_med + bgn_bird_max + pow_low_med + pow_bird_med + (1|site),
             data = ordered_70)

####replicated models with interactions included####

##calculating new variable##
ordered_70 <- ordered_70 |>
  mutate(snr_bird = pow_bird_med/bgn_bird_med)

clm20<- clmm(acceptable ~ lowfreq_ratio + rms + bgn_bird_med + bgn_bird_max +
             bgn_low_med + pow_bird_med + pow_bird_max + pow_low_med + ent_bird_med + ent_bird_max + ent_low_med + snr_bird +
              lowfreq_ratio * pow_bird_med + bgn_low_med * pow_bird_med + lowfreq_ratio * ent_bird_med + rms * lowfreq_ratio +
              rms * ent_bird_med + (1|site), 
           data = ordered_70)

##global model with bgn_bird_med
clm21<- clmm(acceptable ~ lowfreq_ratio +rms + bgn_bird_med + pow_bird_med + pow_bird_max + pow_low_med + ent_bird_med +
             ent_bird_max + ent_low_med + snr_bird + lowfreq_ratio * pow_bird_med + bgn_low_med * pow_bird_med + 
             lowfreq_ratio * ent_bird_med + rms * lowfreq_ratio +
             rms * ent_bird_med + (1|site), 
           data = ordered_70)

##global model with bgn_bird_max
clm22<- clmm(acceptable ~ lowfreq_ratio +rms + bgn_bird_max +
             pow_bird_med + pow_bird_max + pow_low_med + ent_bird_med + ent_bird_max + ent_low_med + snr_bird +
             lowfreq_ratio * pow_bird_med + bgn_low_med * pow_bird_med + lowfreq_ratio * ent_bird_med + rms * lowfreq_ratio +
             rms * ent_bird_med + (1|site), 
           data = ordered_70)

##global model with bgn_low_Med
clm23<- clmm(acceptable ~ lowfreq_ratio + rms +
             bgn_low_med + pow_bird_med + pow_bird_max + pow_low_med + ent_bird_med + ent_bird_max + ent_low_med + snr_bird +
              lowfreq_ratio * pow_bird_med + bgn_low_med * pow_bird_med + lowfreq_ratio * ent_bird_med + rms * lowfreq_ratio +
              rms * ent_bird_med + (1|site), 
           data = ordered_70)

##selcting which model is best using AIC 
AICtab(clm20, clm21, clm22, clm23)

##Other models -- clm1 but testing without each of the highly correlated variables

clm24 <- clmm(acceptable ~ lowfreq_ratio + rms +
              bgn_low_med + bgn_bird_max + pow_bird_max + pow_low_med + pow_bird_med + ent_bird_med + ent_bird_max + ent_low_med + 
               snr_bird + lowfreq_ratio * pow_bird_med + bgn_low_med * pow_bird_med + lowfreq_ratio * ent_bird_med + rms * lowfreq_ratio +
               rms * ent_bird_med + (1|site), 
            data = ordered_70) ##no bgn_bird_med

clm25 <- clmm(acceptable ~ lowfreq_ratio + rms +
              bgn_low_med + bgn_bird_med + pow_bird_max + pow_low_med + pow_bird_med + ent_bird_med +ent_bird_max +ent_low_med + snr_bird +
              lowfreq_ratio * pow_bird_med + bgn_low_med * pow_bird_med + lowfreq_ratio * ent_bird_med + rms * lowfreq_ratio +
              rms * ent_bird_med + (1|site), 
            data = ordered_70) ##no bgn_bird_max

clm26 <- clmm(acceptable ~ lowfreq_ratio +rms +
              bgn_low_med + bgn_bird_med + bgn_bird_max + pow_bird_max + pow_low_med + ent_bird_med +ent_bird_max +ent_low_med + snr_bird +
               lowfreq_ratio * pow_bird_med + bgn_low_med * pow_bird_med + lowfreq_ratio * ent_bird_med + rms * lowfreq_ratio +
               rms * ent_bird_med + (1|site), 
            data = ordered_70) ##no pow_bird_med

clm27 <- clmm(acceptable ~ lowfreq_ratio +rms +
              bgn_low_med + bgn_bird_med + bgn_bird_max + pow_low_med + pow_bird_med + ent_bird_med +ent_bird_max +ent_low_med + snr_bird +
               lowfreq_ratio * pow_bird_med + bgn_low_med * pow_bird_med + lowfreq_ratio * ent_bird_med + rms * lowfreq_ratio +
               rms * ent_bird_med + (1|site), 
            data = ordered_70) ##no pow_bird_max

clm28 <- clmm(acceptable ~ lowfreq_ratio +rms +
              bgn_low_med + bgn_bird_med + bgn_bird_max + pow_bird_max + pow_low_med + pow_bird_med + ent_bird_med +ent_bird_max +ent_low_med + snr_bird +
               lowfreq_ratio * pow_bird_med + bgn_low_med * pow_bird_med + lowfreq_ratio * ent_bird_med + rms * lowfreq_ratio +
               rms * ent_bird_med + (1|site), 
            data = ordered_70) ##no pow_low_med

clm29 <- clmm(acceptable ~ lowfreq_ratio +rms +
               bgn_low_med + bgn_bird_med + bgn_bird_max + pow_bird_max + pow_low_med + pow_bird_med + ent_bird_max + ent_low_med + snr_bird +
               lowfreq_ratio * pow_bird_med + bgn_low_med * pow_bird_med + lowfreq_ratio * ent_bird_med + rms * lowfreq_ratio +
               rms * ent_bird_med + (1|site), 
             data = ordered_70) ##no ent_bird_med

clm30 <- clmm(acceptable ~ lowfreq_ratio +rms +
               bgn_low_med + bgn_bird_med + bgn_bird_max + pow_bird_max + pow_low_med + pow_bird_med + ent_bird_med +ent_bird_max +ent_low_med + snr_bird +
               lowfreq_ratio * pow_bird_med + bgn_low_med * pow_bird_med + lowfreq_ratio * ent_bird_med + rms * lowfreq_ratio +
               rms * ent_bird_med + (1|site), 
             data = ordered_70) ##no ent_bird_max

clm31 <- clmm(acceptable ~ lowfreq_ratio + rms + bgn_low_med + pow_low_med + ent_low_med + snr_bird +
               lowfreq_ratio * pow_bird_med + bgn_low_med * pow_bird_med + lowfreq_ratio * ent_bird_med + rms * lowfreq_ratio +
               rms * ent_bird_med + (1|site),
             data = ordered_70) 

clm32 <- clmm(acceptable ~ lowfreq_ratio + bgn_low_med + pow_low_med + ent_low_med + snr_bird +
               lowfreq_ratio * pow_bird_med + bgn_low_med * pow_bird_med + lowfreq_ratio * ent_bird_med + rms * lowfreq_ratio +
               rms * ent_bird_med + (1|site),
             data = ordered_70)

clm33 <- clmm(acceptable ~ rms + bgn_low_med + pow_low_med + ent_low_med + snr_bird +
               lowfreq_ratio * pow_bird_med + bgn_low_med * pow_bird_med + lowfreq_ratio * ent_bird_med + rms * lowfreq_ratio +
               rms * ent_bird_med + (1|site),
             data = ordered_70)

clm34 <- clmm(acceptable ~ lowfreq_ratio + rms + bgn_bird_med + pow_bird_med + ent_bird_med + snr_bird +
               lowfreq_ratio * pow_bird_med + bgn_low_med * pow_bird_med + lowfreq_ratio * ent_bird_med + rms * lowfreq_ratio +
               rms * ent_bird_med + (1|site),
             data = ordered_70)

clm35 <- clmm(acceptable ~ lowfreq_ratio + bgn_bird_med + pow_bird_med + ent_bird_med + snr_bird +
               lowfreq_ratio * pow_bird_med + bgn_low_med * pow_bird_med + lowfreq_ratio * ent_bird_med + rms * lowfreq_ratio +
               rms * ent_bird_med + (1|site),
             data = ordered_70)

clm36 <- clmm(acceptable ~ rms + bgn_bird_med + pow_bird_med + ent_bird_med + snr_bird +
               lowfreq_ratio * pow_bird_med + bgn_low_med * pow_bird_med + lowfreq_ratio * ent_bird_med + rms * lowfreq_ratio +
               rms * ent_bird_med + (1|site),
             data = ordered_70)

clm37 <- clmm(acceptable ~ bgn_low_med + pow_low_med + ent_low_med + snr_bird +
               lowfreq_ratio * pow_bird_med + bgn_low_med * pow_bird_med + lowfreq_ratio * ent_bird_med + rms * lowfreq_ratio +
               rms * ent_bird_med + (1|site),
             data = ordered_70)

##clm19 (best model) with interactions
clm38 <- clmm(acceptable ~ rms + lowfreq_ratio + bgn_low_med + bgn_bird_max + pow_low_med + pow_bird_med + snr_bird +
               lowfreq_ratio * pow_bird_med + bgn_low_med * pow_bird_med + lowfreq_ratio * ent_bird_med + rms * lowfreq_ratio +
               rms * ent_bird_med + (1|site),
             data = ordered_70)

##clm38 with CIs containing 0 removed, other than rms * ent_bird_med
clm39 <- clmm(acceptable ~ rms + lowfreq_ratio + bgn_low_med + bgn_bird_max + pow_low_med + pow_bird_med +
               lowfreq_ratio * pow_bird_med + bgn_low_med * pow_bird_med + lowfreq_ratio * ent_bird_med + rms * lowfreq_ratio
              + (1|site),
             data = ordered_70)

##clm39 with rms * ent_bird_med removed
clm40 <- clmm(acceptable ~ rms + lowfreq_ratio + bgn_low_med + bgn_bird_max + pow_low_med + pow_bird_med +
               lowfreq_ratio * pow_bird_med + bgn_low_med * pow_bird_med + rms * lowfreq_ratio + (1|site),
             data = ordered_70)

##Candidate models
models <- list(clm1 = clm1, clm2 = clm2, clm3 = clm3, clm4 = clm4, clm5 = clm5,
               clm6 = clm6, clm7 = clm7, clm8 = clm8, clm9 = clm9, clm10 = clm10,
               clm11 = clm11, clm12 = clm12, clm13 = clm13, clm14 = clm14, clm15 = clm15,
               clm16 = clm16, clm17 = clm17, clm18 = clm18, clm19 = clm19, clm20 = clm20, 
               clm21 = clm21, clm22 = clm22, clm23 = clm23, clm24 = clm24, clm25 = clm25,
               clm26 = clm26, clm27 = clm27, clm28 = clm28, clm29 = clm29, clm30 = clm30,
               clm31 = clm31, clm32 = clm32, clm33 = clm33, clm34 = clm34, clm35 = clm35, 
               clm36 = clm36, clm37 = clm37, clm38 = clm38, clm39 = clm39, clm40 = clm40)

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

##analyzing candidate models
summary(clm8)
confint(clm8)                 

summary(clm19) ##this is clm8 with variables removed and now none of the CIs have 0
confint(clm19)

summary(clm38) ##this is clm19 with interactions
confint(clm38)

summary(clm39) ##clm38 with CIs containing 0 removed
confint(clm39)

summary(clm40) ##clm39 with rms * ent_bird_med removed
confint(clm40)

############testing to see how well model clm40 predicts the test data###########
ordered_30 <- df_30 |>
  filter(acceptable %in% c("y", "b", "n")) |>
  mutate(acceptable = factor(acceptable,
                             levels = c("y", "b", "n"),
                             labels = c("Good", "Borderline", "Bad"),
                             ordered = TRUE))

##drop rows with missing predictors, so predictions and observed line up
vars <- all.vars(formula(clm39))[-1]
nd_30 <- ordered_30[complete.cases(ordered_30[, vars]), ]

##calculating probabilities manually for each row in nd_30 because clmm cannot use predict()
fixed_formula <- lme4::nobars(formula(clm39)) ##removing random effect term

X <- model.matrix(
  delete.response(terms(fixed_formula)),
  data = nd_30
) ##model matrix converts nd_30 data to numerical values to calculate predicted values

##removing intercept because clmm stores the intercept as a parameter
X <- X[, colnames(X) != "(Intercept)", drop = FALSE]

beta <- clm39$beta ##extracting coefficients from clm39
X <- X[, names(beta), drop = FALSE] ##

##calculate linear predictor
eta <- as.vector(X %*% beta)

alpha <- clm39$alpha ##extracting threshold parameters
cum_probs <- sapply(alpha, function(a) plogis(a - eta)) ##calculating cumulative probabilities

##calculate probability of each category from cumulative probabilities
probs_30 <- cbind(
  Good = cum_probs[, 1],
  Borderline = cum_probs[, 2] - cum_probs[, 1],
  Bad = 1 - cum_probs[, 2]
)

dim(probs_30)

lv <- c("Good", "Borderline", "Bad")

##classifying based on highest probability 
default_30 <- colnames(probs_30)[max.col(probs_30, ties.method = "first")]

##override with good and bad thresholds
good_cutoff <- 0.74
bad_cutoff <- 0.4

adjusted_30 <- rep(NA_character_, nrow(probs_30))

##classifying as bad if the probability exceeds the bad threshold
adjusted_30[probs_30[, "Bad"] >= bad_cutoff] <- "Bad"

##classifying as good if the probability exceeds the threshold
adjusted_30[
  is.na(adjusted_30) & probs_30[, "Good"] >= good_cutoff
] <- "Good"

##classify all remaining rows as Borderline
adjusted_30[is.na(adjusted_30)] <- "Borderline"

##converting back to ordered factor
default_30 <- factor(default_30, levels = lv, ordered = TRUE)  ##predicted categories without cutoff
adjusted_30     <- factor(adjusted_30,     levels = lv, ordered = TRUE) ##predicted categories with cutoff
observed   <- factor(nd_30$acceptable, levels = lv, ordered = TRUE) ##categories scored by rater

##creating confusion matrix before and after the 0.25 cutoff
confusionMatrix(default_30, observed)   
confusionMatrix(adjusted_30, observed)     

##############deriving optimized threshold###########
# Identify variables used in clm39
vars <- all.vars(formula(clm39))[-1]

# Keep complete observations from the training dataset
nd_train <- ordered_70[
  complete.cases(ordered_70[, vars]),
]

# Actual observed categories
obs_train <- factor(
  nd_train$acceptable,
  levels = c("Good", "Borderline", "Bad"),
  ordered = TRUE
)
# Extract fixed-effects formula
fixed_formula <- lme4::nobars(formula(clm39))

# Create model matrix
X <- model.matrix(
  delete.response(terms(fixed_formula)),
  data = nd_train
)

# Remove intercept
X <- X[, colnames(X) != "(Intercept)", drop = FALSE]

# Extract fixed-effect coefficients
beta <- clm39$beta

# Match columns to coefficient names
X <- X[, names(beta), drop = FALSE]

# Calculate linear predictor
eta <- as.vector(X %*% beta)

# Extract thresholds
alpha <- clm39$alpha

# Calculate cumulative probabilities
cum_probs <- sapply(alpha, function(a) {
  plogis(a - eta)
})

# Calculate probabilities for each category
probs_train <- cbind(
  Good = cum_probs[, 1],
  Borderline = cum_probs[, 2] - cum_probs[, 1],
  Bad = 1 - cum_probs[, 2]
)

# Check dimensions
dim(probs_train)
length(obs_train)

# Check probabilities sum to 1
head(rowSums(probs_train))

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

# Create threshold combinations
grid <- expand.grid(
  good_cut = seq(0.50, 0.95, by = 0.01),
  bad_cut = seq(0.05, 0.80, by = 0.01)
)

# Evaluate every combination
results_train <- vector("list", nrow(grid))

for (i in seq_len(nrow(grid))) {
  
  pred <- classify_two_threshold(
    probs_train,
    good_cut = grid$good_cut[i],
    bad_cut = grid$bad_cut[i]
  )
  
  results_train[[i]] <- data.frame(
    good_cut = grid$good_cut[i],
    bad_cut = grid$bad_cut[i],
    kappa = irr::kappa2(
      data.frame(obs_train, pred),
      weight = "squared"
    )$value
  )
}

results_train <- do.call(rbind, results_train)

# Find optimal thresholds
best_train <- results_train[
  which.max(results_train$kappa),
]

best_train

best_pred_train <- classify_two_threshold(
  probs_train,
  good_cut = best_train$good_cut,
  bad_cut = best_train$bad_cut
)

# Training confusion matrix
caret::confusionMatrix(
  data = best_pred_train,
  reference = obs_train
)


#########testing to see how well model clm40 predicts the ordered_70 data###########

##drop rows with missing predictors, so predictions and observed line up
vars <- all.vars(formula(clm40))[-1]
nd_70 <- ordered_70[complete.cases(ordered_70[, vars]), ]

##calculating probabilities for each row in nd_30
probs_70 <- predict(clm40, newdata = nd_70[vars], type = "prob")$fit
dim(probs_30)   #how many rows and how many columns

lv <- c("Good", "Borderline", "Bad")

##classifying based on highest probability 
default_70 <- colnames(probs_70)[max.col(probs_70, ties.method = "first")]

##override with "Borderline" if its probability exceeds 0.25
borderline_cutoff <- 0.25
adjusted_70 <- default_70
adjusted_70[probs_70[, "Borderline"] > borderline_cutoff] <- "Borderline"

##converting back to ordered factor
default_70 <- factor(default_70, levels = lv, ordered = TRUE)  ##predicted categories without cutoff
adjusted_70     <- factor(adjusted_70,     levels = lv, ordered = TRUE) ##predicted categories with cutoff
observed   <- factor(nd_70$acceptable, levels = lv, ordered = TRUE) ##categories scored by rater

##creating confusion matrix before and after the 0.25 cutoff
confusionMatrix(default_70, observed)   
confusionMatrix(adjusted_70, observed) 


################## the rest of steves scripts that I don't understand right now ####################
library(pROC)

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
plot(
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