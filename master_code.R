# =====================================================
# loading libraries
# =====================================================
library(car)
library(caret)
library(corrplot)
library(DescTools)
library(dplyr)
library(e1071)
library(gbm)
library(ggplot2)
library(kernlab)
library(LiblineaR)
library(lubridate)
library(pROC)
library(ranger)
library(readr) # for loading of the data
library(reshape2)
library(rpart)
library(rpart.plot)
library(shapviz)
library(stringr)
library(tidymodels)
library(vip)
library(xgboost)





# =====================================================
# Read the data
# =====================================================

# set seed for reproduceablility
set.seed(123)


# read dataset
data <- read_csv("~/Desktop/speciale/kode/master/explore/WhoScoredFinal1.csv")

# examine the season structure
unique(data$seasonName)


# =====================================================
# feature engeneering of age
# =====================================================

#initialize the year the data was collected
data_colect_year <- 2019

# create approximations of the year the player was born
data <- data %>%
  mutate(
    birth_year_approx = data_colect_year - age
  )

# find the start year of the season start
data <- data %>%
  mutate(
    season_start_year = as.numeric(str_sub(seasonName, 1, 4)) 
  )

# create the age_season variable 
data <- data %>%
  mutate(
    age_season = season_start_year - birth_year_approx 
  )

# =====================================================
# data cleaning of missing values due to dublicate variables
# =====================================================

summary(data)# check for na's
# remove variables not relevant dublication of other variables
data <- data[,-c(63:153)]
# cols to remove additionally
no_cols <- c(2,5,6,8,52,53,18,34,35,15,14,11,12)
data <- data[,-no_cols]
summary(data)# check for na's
#apears to be alot of observations with missing 149267 in almost the variables lets remove those
data <- data %>%
  filter(!is.na(data$goal_y))# remove na's

summary(data)# no na's
# removing additional dublication columns
dub_cols <- c(27,19,20,37,23,39) # col's with dublicates
data <- data[,-dub_cols]
# remove cols from age calculations
data <- data[,-c(44,45)]


# =====================================================
# feature engeneering of postion dummy's
# =====================================================

data <- data %>%
  mutate(
    gk = ifelse(positionText == "Goalkeeper", 1, 0),# where later removed
    def = ifelse(positionText == "Defender", 1, 0),# where removed due to one hot encoding
    mid = ifelse(positionText == "Midfielder", 1, 0),
    atk = ifelse(positionText == "Forward", 1, 0))


# =====================================================
# filtering of the data
# =====================================================
# check for regions avaleable
unique(data$tournamentRegionName) 
### filter so only domestic leagues and classic season format 
data <- data %>%
  filter(!tournamentRegionName %in% c("Europe", "International"))

data <- data %>%
  filter(grepl("/", seasonName))

# find players with min 5 seasons in best tournaments
#select relevant tournaments
tournaments <- c("EPL","GB","FLI","SLL","ISA") #selected top 5 legues
num_min5 <- data %>%
  filter(tournamentShortName %in% tournaments) %>%   # filter for leagues
  distinct(playerId, seasonName) %>%                 # only one season pr player
  count(playerId) %>%                                
  filter(n >= 5)

# extract player ID's for those meeting 5 season criteria
data <- data %>%
  filter(playerId %in% num_min5$playerId)

# remove goalkeepers
data <- data %>%
  filter(data$gk != 1)

# =====================================================
# data cleaning dublicates
# =====================================================

# check for dublicates regarding seasons and league
data <- data %>%
  arrange(playerId, seasonName) %>%
  mutate(row = row_number()) # insert rownumber to trace dubs 


dub_seasons <-data %>%
  count(playerId, seasonName, teamName, tournamentName) %>%
  filter(n > 1)


# investigate championship tournament
dub_seasons_championship <- dub_seasons %>%
  filter(tournamentName == "Championship")

data_chamionship <- data %>% 
  filter(tournamentName == "Championship")

data_chamionship_bf2020 <- data_chamionship %>%
  filter(seasonName != "2019/2020")
# only unique entries removal
data_chamionship_bf2020_unique <- data_chamionship_bf2020 %>%
  count(playerId, seasonName, name) %>%
  filter(n == 1)
# unique player names removal
data_chamionship_bf2020 <- data_chamionship_bf2020 %>%
  filter(!(name == "James Morrison" & seasonName == "2018/2019"),
         !(name == "Cameron Jerome" & seasonName == "2017/2018"),
         !(name == "Graham Dorrans" & seasonName == "2014/2015"))
  


# check if seasons exactly the same
data_chamionship_bf2020 %>%
  count(playerId, seasonName, minsPlayed) %>%
  filter(n > 1)
# not the case for all of them
data_chamionship_bf2020_diff <- data_chamionship_bf2020 %>%
  count(playerId, seasonName, minsPlayed, name) %>%
  filter(n==1)

name_count <-data_chamionship_bf2020_diff %>%
  count(name) 
## seems to take championship Playoff in every second we will not use this so select
# minimum minute, all even
# joining the orginal data
filtered_data_championship <- data %>%
  semi_join(data_chamionship_bf2020_diff, by = c("name", "seasonName"))
# find rows to remove
rmv_rows <- filtered_data_championship %>%
  group_by(name, seasonName) %>%
  slice_max(minsPlayed, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(name, seasonName, minsPlayed, row)

rows_to_rmv <- rmv_rows$row  

data <- data %>%
  filter(!row %in% rows_to_rmv)
# check the rest championship exact dublicates
dubs_cs <- data %>%
  filter(tournamentName == "Championship") %>%
  count(name, seasonName, teamName, minsPlayed, rating) %>%
  filter(n>1)
# sets of two filter orignal data
filtered_data_championship2 <- data %>%
  semi_join(dubs_cs, by = c("name", "seasonName")) %>%
  arrange(playerId, seasonName, teamName)

rows_to_rmv2 <- filtered_data_championship2 %>%
  slice(seq(2, n(), by = 2)) %>%
  pull(row)

# rmv row
data <- data %>%
  filter(!row %in% rows_to_rmv2)


dubs_cs2 <- data %>%
  filter(tournamentName == "Championship") %>%
  count(name, seasonName, teamName,) %>%
  filter(n>1)

data %>%
  filter(tournamentName == "Championship") %>%
  count(name, seasonName) %>%
  filter(n>1)
# ashley Barnes remains but that is due to winter transfer
# check erdevise

dub_seasons_ed <- data %>%
  filter(tournamentName == "Eredivisie") %>%
  count(name, seasonName, teamName, minsPlayed, rating) %>%
  filter(n>1)

filtered_data_ed <- data %>%
  semi_join(dub_seasons_ed, by = c("name", "seasonName")) %>%
  arrange(playerId, seasonName, teamName)

#remove identical entry's
row_rmved <- filtered_data_ed %>%
  slice(seq(2, n(), by = 2)) %>%
  pull(row)

data <- data %>%
  filter(!row %in% row_rmved)

# check min
dub_seasons_ed2 <- data %>%
  filter(tournamentName == "Eredivisie") %>%
  count(name, seasonName,) %>%
  filter(n>1)


filtered_data_ed2 <- data %>%
  semi_join(dub_seasons_ed2, by = c("name", "seasonName")) %>%
  arrange(playerId, seasonName)
# play offs alter data selecting the min minuetes


rmv_rows_ed <- filtered_data_ed2 %>%
  group_by(name, seasonName) %>%
  slice_max(minsPlayed, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(name, seasonName, minsPlayed, row)

rows_to_rmv_ed <- rmv_rows_ed$row  

data <- data %>%
  filter(!row %in% rows_to_rmv_ed)

# check dubs outside tournaments
dub_seasons2 <-data %>%
  count(playerId, seasonName) %>%
  filter(n > 1)


filtered_data_gen <- data %>%
  semi_join(dub_seasons2, by = c("playerId", "seasonName")) %>%
  arrange(playerId, seasonName, teamName)
# merge data due to winter transfer use avarge of non percentage statestics
col_mean <- c(1,18,21,24:30,33:36,38:43)
col_sum <- c(15:17,19,20,31,32,37)
col_same <- c(2,7,10:14,22,23,44:48)

col_mean_names <- names(filtered_data_gen)[col_mean]
col_sum_names  <- names(filtered_data_gen)[col_sum]
col_same_names <- names(filtered_data_gen)[col_same]
# row number to merge
mapping <- tibble(
  row = c(2051,3956,4276),
  row_match = c(2050, 3595, 4275)
)
#mege everything
merged <- mapping %>%
  left_join(data, by = "row") %>%
  left_join(data, by = c("row_match" = "row"), suffix = c("_1", "_2"))

# rmv dub comns
merged_final <- merged %>%
  select(row, ends_with("_1"))
names(merged_final) <- gsub("_1$", "", names(merged_final))
# SUM
for (col in col_sum_names) {
  merged_final[[col]] <- merged[[paste0(col, "_1")]] + 
    merged[[paste0(col, "_2")]]
}

# MEAN
for (col in col_mean_names) {
  merged_final[[col]] <- (merged[[paste0(col, "_1")]] + 
                            merged[[paste0(col, "_2")]]) / 2
}

# SAME
for (col in col_same_names) {
  merged_final[[col]] <- merged[[paste0(col, "_1")]]
}
# filter back
data <-  data %>%
  filter(!row %in% mapping$row_match) %>%
  rows_update(merged_final, by = "row")

data %>% 
  count(playerId, seasonName) %>%
  filter(n>1)

# no dublicates 
# =====================================================
# feature engeneering percentage minutes played
# =====================================================

## adding pct_min played
data <- data |>
  mutate(
    Max_min = NA)

# add max_min for diff tournaments
# for 38 match leagues
data$Max_min[data$tournamentId %in% c(2,4,5,22)] <- 3420


# for 34 match leagues

data$Max_min[data$tournamentId %in% c(3,6,13,17,21)] <- 3060

# for alp diff maks min for seasons
data$Max_min[data$tournamentId == 68 & data$seasonName == "2016/2017"] <- 2700
data$Max_min[data$tournamentId == 68 & data$seasonName == "2017/2018"] <- 2430
data$Max_min[data$tournamentId == 68 & data$seasonName == "2018/2019"] <- 2250
data$Max_min[data$tournamentId == 68 & data$seasonName == "2019/2020"] <- 2070

#for rpl
data$Max_min[data$tournamentId == 77] <- 2700

#for championship
data$Max_min[data$tournamentId == 7] <- 4140

# making pct min played
data$pct_minplayed <- pmin(data$minsPlayed / data$Max_min, 1) # ensure max value of 1 can exceed due to additional 
#added time
# converting from pr game to pr minute


data <- data %>%
  rename_with(~ gsub("(_x|_y)$", "", .)) # renaming variables removing surfix _x and _ y

data <- data %>%
  mutate(MinPerGame = minsPlayed / apps)
# convert total to pr 90 minutes and adding surfix _pr90 
cols_pr90 <- names(data)[c(19,20,31,32,37)] # cols with total stats
data <- data %>%
  mutate(across(
    all_of(cols_pr90),
    ~ ifelse(minsPlayed == 0, NA, (. / minsPlayed) * 90),
    .names = "{.col}_per90"
  ))
# convert to decimal from %
data <- data %>%
  mutate(passSuccess = passSuccess / 100)
# remove PerGame surfix
cols_convert <- names(data)[c(21, 24:30, 33:36, 38:42)]


names(data)[names(data) %in% cols_convert] <- 
  gsub("PerGame$", "", names(data)[names(data) %in% cols_convert])

# make pr_90

cols90 <- names(data)[c(21, 24:30, 33:36, 38:42)]

data <- data %>%
  mutate(across(
    all_of(cols90),
    ~ ifelse(minsPlayed == 0, NA, (. * apps / minsPlayed) * 90),
    .names = "{.col}_per90"
  ))
#remove old colomns
cols_rmv <- names(data)[c(21, 24:30, 33:36, 38:42, 19,20,31,32,37)]

data <- data %>%
  select(-all_of(cols_rmv))

# rmv coloumns not relevent
col_not <- names(data)[c(4,6,10,11,14,15,16,27,28)]
print(col_not)
data <- data %>%
  select(-all_of(col_not))

#reorder columns
new_order <- c(5,13,6,7,3,4,2,15,8,9,12,16:19,10,20,21,22:26,27:34,37:43,14,35,36,1,11)
data <- data[, new_order]

# =====================================================
# feature engeneering performance: future and history along  with decline and decline binary
# =====================================================

# performance definition
data <- data %>%
  mutate(
    Performance = rating * sqrt(pct_minplayed)
  )

data <- data %>%
  arrange(playerId, seasonName) %>%
  group_by(playerId) %>%
  
  # Historical performance (last 3 seasons incl. current)
  mutate(
    hist_perf = sapply(row_number(), function(i) {
      
      start_idx <- max(1, i - 2)
      past <- Performance[start_idx:i]
      
      # weights
      base_weights <- c(1, 1.25, 1.5)
      weights <- tail(base_weights, length(past))#ensuring only weights used for the seasons are considered
      
      weighted.mean(past, weights)
    })
  ) %>%
  
  # Future performance (next 3 seasons)
  mutate(
    future_perf = sapply(row_number(), function(i) {
      
      future <- c(
        lead(Performance, 1)[i],
        lead(Performance, 2)[i],
        lead(Performance, 3)[i]
      )
      
      # Remove missing future seasons
      future <- future[!is.na(future)]
      
      if(length(future) == 0) {
        return(NA)
      }
      
      # weights
      base_weights <- c(1.5, 1.25, 1)
      weights <- head(base_weights, length(future))
      
      weighted.mean(future, weights)
    })
  ) %>%
  
  # Decline calculation
  mutate(
    decline = future_perf - hist_perf
  ) %>%
  
  mutate(
  decline_binary = ifelse(decline < 0, 1, 0)
  )%>%
  
  ungroup()

# rmv last season for players hence no decline

data <- data %>%
  filter(!is.na(decline))
# =====================================================
# remove info variables eg id's,  remove the def dummy, and ensure decline_dummy is factor 
# =====================================================

#save data as csv
write.csv(data, "main.csv", row.names = F)
# remove variabels
# and make one hot dummy by rmv def
# keep info variable data set
info_data <- data
col_to_rmv <- c(1:7,11:13,16,18,42:44,46,47)
data <- data[,-col_to_rmv]



# make decline a factor
data$decline_binary <- as.factor(data$decline_binary)

# =====================================================
# data exploreation
# =====================================================
#examine nzv
nzv <- nearZeroVar(data)
data <- data[, -nzv]
# rmv goalOwn pr 90

# corelation
# create numeric dataset
numeric_data <- data %>%
  select(where(is.numeric))

cor_matrix <- cor(
  numeric_data,
  use = "pairwise.complete.obs"
)

# Remove highly correlated variables
high_corr <- findCorrelation(
  cor_matrix,
  cutoff = 0.9
)

# correaltaion plot
corrplot(
  cor_matrix,
  method = "color",
  type = "upper",
  order = "hclust",
  diag = FALSE,
  tl.col = "black",
  tl.cex = 0.7,
  cl.cex = 0.7
)
# no extreme correlation however both height and weight is not needed so will remove weight, 
# will also remove pct_minplayed hence hist_perf is partially calculated with basis in this

# check distrubution



skew_values <- sapply(
  numeric_data,
  skewness,
  na.rm = TRUE
)
# get summary statistics of skeewed value
summary(data$accurateThroughBall_per90)
summary(data$redCard_per90)

# will remove red card hence it is a rare event and skewed
# keep acc through ball hence it is a way do describe a style of play

# rmv red card, weight and min_played
var_rmv <- c(3,6,8)
data <- data[,-var_rmv]

# =====================================================
# data pattioning
# =====================================================
## split the data
data$decline_binary <- as.factor(data$decline_binary)
#examine classs distrubution
summary(data$decline_binary)
# distrubution fair around 62% decline
# split 80/20 pation random 

train_index <- createDataPartition(
  data$decline_binary,
  p = 0.80,
  list = FALSE
)

# Create train and test data
train_data <- data[train_index, ]
test_data  <- data[-train_index, ]
#check distrubution
summary(train_data$decline_binary)
summary(test_data$decline_binary)
# around 62% decline in both
# check for na again
sum(is.na(data))
#0
# how many diffrent players
unique(info_data$playerId)
# 869 unique player ID's

test_probtable <-prop.table(table(test_data$decline_binary))
# ensure correct levels on data
test_data$decline_binary <- factor(
  test_data$decline_binary,
  levels = c("1", "0")
)

train_data$decline_binary <- factor(
  train_data$decline_binary,
  levels = c("1", "0")
)
# =====================================================
# ANALYSIS
# =====================================================


# =====================================================
# Logistic Regression 
# =====================================================

# recipe
lr_recipe <- recipe(
  decline_binary ~ .,
  data = train_data
) %>%
  step_dummy(all_nominal_predictors()) %>%
  step_zv(all_predictors()) %>%
  step_normalize(all_numeric_predictors())# due to shrinkage


# Logistic Regression Model




# Workflow


lr_workflow <- workflow() %>%
  add_recipe(lr_recipe) %>%
  add_model(lr_model)


# 10-fold Cross Validation

cv_folds <- vfold_cv(
  train_data,
  v = 10,
  strata = decline_binary
)


# Hyperparameter Grid


lr_grid <- grid_regular(
  penalty(range = c(-5, 1)),
  mixture(),
  levels = 5
)


# Hyperparameter Tuning


lr_tuned <- tune_grid(
  lr_workflow,
  resamples = cv_folds,
  grid = lr_grid,
  metrics = metric_set(
    roc_auc,
    accuracy,
    precision,
    recall,
    f_meas
  )
)


# Best Hyperparameters


best_lr <- select_best(
  lr_tuned,
  metric = "roc_auc"
)

print(best_lr)
#penalty 0.00001
#mixture 1


# Finalize Workflow


final_lr_workflow <- finalize_workflow(
  lr_workflow,
  best_lr
)


# Train Final Model


final_lr_fit <- fit(
  final_lr_workflow,
  data = train_data
)


# Predictions


lr_predictions <- predict(
  final_lr_fit,
  test_data,
  type = "prob"
) %>%
  bind_cols(
    predict(final_lr_fit, test_data),
    test_data
  )


# Model Evaluation


# ROC-AUC
roc_auc(
  lr_predictions,
  truth = decline_binary,
  .pred_1
)

# Accuracy
accuracy(
  lr_predictions,
  truth = decline_binary,
  estimate = .pred_class
)

# Precision
precision(
  lr_predictions,
  truth = decline_binary,
  estimate = .pred_class
)

# Recall
recall(
  lr_predictions,
  truth = decline_binary,
  estimate = .pred_class
)

# F1-score
f_meas(
  lr_predictions,
  truth = decline_binary,
  estimate = .pred_class
)


# Confusion Matrix


conf_mat(
  lr_predictions,
  truth = decline_binary,
  estimate = .pred_class
)


# Combined Metrics Table


metric_set(
  accuracy,
  kap,
  precision,
  recall,
  f_meas,
  roc_auc
)(
  lr_predictions,
  truth = decline_binary,
  estimate = .pred_class,
  .pred_1
)


# train metrics


# Train Predictions


train_predictions <- predict(
  final_lr_fit,
  train_data,
  type = "prob"
) %>%
  bind_cols(
    predict(final_lr_fit, train_data),
    train_data
  )
metric_set(
  accuracy,
  kap,
  precision,
  recall,
  f_meas,
  roc_auc
)(
  train_predictions,
  truth = decline_binary,
  estimate = .pred_class,
  .pred_1
)


# VIF

# fit glm as the relationship is alike and better for plots odds ratio plot
glm_model <- glm(
  decline_binary ~ .,
  data = train_data,
  family = binomial()
)




#  odds ratio dataframe on glm model


or_plot <- tidy(
  glm_model,
  conf.int = TRUE,
  exponentiate = TRUE
) %>%
  filter(term != "(Intercept)") %>%
  mutate(
    significance = ifelse(
      conf.low > 1 | conf.high < 1,
      "Significant",
      "Not Significant"
    ),
    
    direction = case_when(
      estimate > 1 ~ "Positive Effect",
      estimate < 1 ~ "Negative Effect"
    )
  )


# Improved Forest Plot


ggplot(
  or_plot,
  aes(
    x = reorder(term, estimate),
    y = estimate,
    ymin = conf.low,
    ymax = conf.high,
    color = significance
  )
) +
  
  # Confidence intervals
  geom_pointrange(size = 0.8) +
  
  # Reference line
  geom_hline(
    yintercept = 1,
    linetype = "dashed",
    color = "black",
    linewidth = 0.8
  ) +
  
  # Flip coordinates
  coord_flip() +
  
  # Log scale for odds ratios
  scale_y_log10() +
  
  # Custom colors
  scale_color_manual(
    values = c(
      "Significant" = "#D55E00",
      "Not Significant" = "grey50"
    )
  ) +
  
  # Labels
  labs(
    title = "Logistic Regression Odds Ratios",
    subtitle = "95% Confidence Intervals",
    x = "Variables",
    y = "Odds Ratio (log scale)",
    color = "Statistical Significance"
  ) +
  
  # Theme
  theme_minimal(base_size = 14) +
  
  theme(
    plot.title = element_text(
      face = "bold",
      size = 18
    ),
    
    plot.subtitle = element_text(
      size = 12
    ),
    
    axis.text.y = element_text(
      size = 11
    ),
    
    legend.position = "top",
    
    panel.grid.minor = element_blank()
  )



# Logistic Regression Variable Importance

vip(
  final_lr_fit$fit$fit$fit,
  num_features = 20,
  geom = "col"
) +
  labs(
    title = "Variable Importance - Logistic Regression",
    subtitle = "Top 20 Most Important Predictors",
    x = "Importance",
    y = "Variables"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(
      face = "bold",
      size = 18
    ),
    
    plot.subtitle = element_text(
      size = 12
    ),
    
    axis.text.y = element_text(
      size = 11
    )
  )









# -------------------------------------------------
# Decision tree
# -------------------------------------------------

set.seed(123)



# Create 10-fold cross-validation


cv_folds <- vfold_cv(
  train_data,
  v = 10,
  strata = decline_binary
)


# Create decision tree model


tree_model <- decision_tree(
  cost_complexity = tune(),
  tree_depth = tune(),
  min_n = tune()
) %>%
  set_engine("rpart") %>%
  set_mode("classification")


# Create workflow


tree_workflow <- workflow() %>%
  add_model(tree_model) %>%
  add_formula(decline_binary ~ .)


#  Hyperparameter grid


tree_grid <- grid_regular(
  cost_complexity(),
  tree_depth(),
  min_n(),
  levels = 5
)


# Tune model with 10-fold CV


tree_tuned <- tune_grid(
  tree_workflow,
  resamples = cv_folds,
  grid = tree_grid,
  metrics = metric_set(
    accuracy,
    kap,
    recall,
    precision,
    f_meas
  )
)


#  View results


collect_metrics(tree_tuned)


#  Select best model


best_tree <- select_best(
  tree_tuned,
  metric = "accuracy"
)

best_tree


# Finalize workflow


final_tree_workflow <- finalize_workflow(
  tree_workflow,
  best_tree
)


#  Fit final model on full training data


final_tree_fit <- fit(
  final_tree_workflow,
  data = train_data
)


# Predict on test data


tree_predictions <- predict(
  final_tree_fit,
  test_data,
  type = "class"
) %>%
  bind_cols(
    predict(final_tree_fit, test_data, type = "prob")
  ) %>%
  bind_cols(test_data)


# Confusion matrix


conf_mat(
  tree_predictions,
  truth = decline_binary,
  estimate = .pred_class
)


# Final metrics on test data


metric_set(
  accuracy,
  kap,
  recall,
  precision,
  f_meas,
  roc_auc
)(
  tree_predictions,
  truth = decline_binary,
  estimate = .pred_class,
  .pred_1
)


# Train predictions


tree_train_predictions <- predict(
  final_tree_fit,
  train_data,
  type = "class"
) %>%
  bind_cols(
    predict(final_tree_fit, train_data, type = "prob")
  ) %>%
  bind_cols(train_data)

# Train metrics

metric_set(
  accuracy,
  kap,
  recall,
  precision,
  f_meas,
  roc_auc
)(
  tree_train_predictions,
  truth = decline_binary,
  estimate = .pred_class,
  .pred_1
)
# Visualzation of DT

final_tree_fit %>%
  extract_fit_engine() %>%
  rpart.plot()

# variable importance
final_tree_fit %>%
  extract_fit_engine() %>%
  vip::vip()


# =====================================================
# RANDOM FOREST 
# =====================================================



set.seed(123)


# Create 10-fold cross-validation


cv_folds <- vfold_cv(
  train_data,
  v = 10,
  strata = decline_binary
)


#  Random Forest Model


rf_model <- rand_forest(
  trees = tune(),
  mtry = tune(),
  min_n = tune()
) %>%
  set_engine(
    "ranger",
    importance = "impurity"
  ) %>%
  set_mode("classification")


#  Workflow


rf_workflow <- workflow() %>%
  add_model(rf_model) %>%
  add_formula(decline_binary ~ .)


#  Hyperparameter Grid


rf_grid <- grid_regular(
  trees(range = c(200, 800)),
  mtry(range = c(2, 10)),
  min_n(range = c(10, 50)),
  levels = 5
)


# Tune model using CV


rf_tuned <- tune_grid(
  rf_workflow,
  resamples = cv_folds,
  grid = rf_grid,
  metrics = metric_set(
    accuracy,
    kap,
    recall,
    precision,
    f_meas,
    roc_auc
  )
)


# View tuning results


collect_metrics(rf_tuned)


#  Select best model


best_rf <- select_best(
  rf_tuned,
  metric = "roc_auc"
)

best_rf


# Finalize workflow


final_rf_workflow <- finalize_workflow(
  rf_workflow,
  best_rf
)


# Fit final model


final_rf_fit <- fit(
  final_rf_workflow,
  data = train_data
)


# Test predictions


rf_predictions <- predict(
  final_rf_fit,
  test_data,
  type = "class"
) %>%
  bind_cols(
    predict(
      final_rf_fit,
      test_data,
      type = "prob"
    )
  ) %>%
  bind_cols(test_data)


# Confusion Matrix


conf_mat(
  rf_predictions,
  truth = decline_binary,
  estimate = .pred_class
)


#  Final Metrics


rf_metrics <- metric_set(
  accuracy,
  kap,
  recall,
  precision,
  f_meas,
  roc_auc
)(
  rf_predictions,
  truth = decline_binary,
  estimate = .pred_class,
  .pred_1 
)

rf_metrics





# Train Metrics


rf_train_predictions <- predict(
  final_rf_fit,
  train_data,
  type = "class"
) %>%
  bind_cols(
    predict(
      final_rf_fit,
      train_data,
      type = "prob"
    )
  ) %>%
  bind_cols(train_data)

metric_set(
  accuracy,
  kap,
  recall,
  precision,
  f_meas,
  roc_auc
)(
  rf_train_predictions,
  truth = decline_binary,
  estimate = .pred_class,
  .pred_1
)


#  Variable Importance


final_rf_fit %>%
  extract_fit_engine() %>%
  vip()



# =====================================================
# GRADIENT BOOSTING  (GBM)
# =====================================================
set.seed(123)

# convert labels to categorical hence its needed for gbm binary classification
train_data$decline_binary <- factor(
  train_data$decline_binary,
  levels = c("1", "0"),
  labels = c("Yes", "No")
)

test_data$decline_binary <- factor(
  test_data$decline_binary,
  levels = c("1", "0"),
  labels = c("Yes", "No")
)


# Create tuning grid


gbm_grid <- expand.grid(
  
  interaction.depth = c(1, 3, 5),
  
  n.trees = c(100, 300, 500, 800),
  
  shrinkage = c(0.01, 0.05, 0.1),
  
  n.minobsinnode = c(5, 10, 20)
)


# Cross-validation setup


train_control <- trainControl(
  
  method = "cv",
  
  number = 10,
  
  classProbs = TRUE,
  
  summaryFunction = twoClassSummary,
  
  savePredictions = TRUE
)


# Tune GBM model


gbm_tuned <- train(
  
  decline_binary ~ .,
  
  data = train_data,
  
  method = "gbm",
  
  distribution = "bernoulli",
  
  trControl = train_control,
  
  tuneGrid = gbm_grid,
  
  metric = "ROC",
  
  verbose = FALSE
)


# View tuning results


gbm_tuned

gbm_tuned$bestTune


#  Plot tuning performance


plot(gbm_tuned)


#  Predict on test data


gbm_prob <- predict(
  gbm_tuned,
  newdata = test_data,
  type = "prob"
)

gbm_class <- predict(
  gbm_tuned,
  newdata = test_data
)


#  Combine predictions


gbm_predictions <- data.frame(
  decline_binary = test_data$decline_binary,
  .pred_1 = gbm_prob$Yes,
  .pred_class = gbm_class
)


# Confusion Matrix


conf_mat(
  gbm_predictions,
  truth = decline_binary,
  estimate = .pred_class
)


#  Final Test Metrics


gbm_metrics <- metric_set(
  accuracy,
  kap,
  recall,
  precision,
  f_meas,
  roc_auc
)(
  gbm_predictions,
  truth = decline_binary,
  estimate = .pred_class,
  .pred_1
)

gbm_metrics




# Train Predictions


gbm_train_prob <- predict(
  gbm_tuned,
  newdata = train_data,
  type = "prob"
)

gbm_train_class <- predict(
  gbm_tuned,
  newdata = train_data
)


# Combine Train Predictions


gbm_train_predictions <- data.frame(
  decline_binary = train_data$decline_binary,
  .pred_1 = gbm_train_prob$Yes,
  .pred_class = gbm_train_class
)


# Train Metrics


metric_set(
  accuracy,
  kap,
  recall,
  precision,
  f_meas,
  roc_auc
)(
  gbm_train_predictions,
  truth = decline_binary,
  estimate = .pred_class,
  .pred_1
)


#  Variable Importance


vip(gbm_tuned$finalModel)


# Raw Variable Importance


summary(gbm_tuned$finalModel)


#  Best Hyperparameters


gbm_tuned$bestTune




# =====================================================
# XGBOOST
# =====================================================



set.seed(123)


#  Cross Validation


cv_folds <- vfold_cv(
  train_data,
  v = 10,
  strata = decline_binary
)


#  XGBoost Model


xgb_model <- boost_tree(
  
  trees = tune(),
  
  tree_depth = tune(),
  
  learn_rate = tune(),
  
  min_n = tune(),
  
  loss_reduction = tune(),
  
  sample_size = tune(),
  
  mtry = tune()
  
) %>%
  set_engine(
    "xgboost"
  ) %>%
  set_mode("classification")


# Workflow


xgb_workflow <- workflow() %>%
  add_model(xgb_model) %>%
  add_formula(decline_binary ~ .)


#  Hyperparameter Grid


xgb_grid <- grid_regular(
  
  trees(range = c(200, 1000)),
  
  tree_depth(range = c(2, 8)),
  
  learn_rate(range = c(-3, -1)),
  
  min_n(range = c(2, 20)),
  
  loss_reduction(range = c(-5, 1)),
  
  sample_prop(range = c(0.5, 1)),
  
  finalize(mtry(), train_data),
  
  levels = 3
)


#  Tune Model


xgb_tuned <- tune_grid(
  
  xgb_workflow,
  
  resamples = cv_folds,
  
  grid = xgb_grid,
  
  metrics = metric_set(
    accuracy,
    kap,
    recall,
    precision,
    f_meas,
    roc_auc
  )
)


#  View Results


collect_metrics(xgb_tuned)


# Select Best Hyperparameters


best_xgb <- select_best(
  xgb_tuned,
  metric = "roc_auc"
)

best_xgb


# Finalize Workflow


final_xgb_workflow <- finalize_workflow(
  xgb_workflow,
  best_xgb
)


#  Fit Final Model


final_xgb_fit <- fit(
  final_xgb_workflow,
  data = train_data
)


# Predict on Test Data


xgb_prob <- predict(
  final_xgb_fit,
  test_data,
  type = "prob"
)

xgb_class <- predict(
  final_xgb_fit,
  test_data,
  type = "class"
)


#  Combine Predictions


xgb_predictions <- bind_cols(
  test_data,
  xgb_prob,
  xgb_class
)


# Confusion Matrix


conf_mat(
  xgb_predictions,
  truth = decline_binary,
  estimate = .pred_class
)


# Test Metrics


xgb_metrics <- metric_set(
  accuracy,
  kap,
  recall,
  precision,
  f_meas,
  roc_auc
)(
  xgb_predictions,
  truth = decline_binary,
  estimate = .pred_class,
  .pred_Yes
)

xgb_metrics



# Train Predictions


xgb_train_prob <- predict(
  final_xgb_fit,
  train_data,
  type = "prob"
)

xgb_train_class <- predict(
  final_xgb_fit,
  train_data,
  type = "class"
)


#  Combine Train Predictions


xgb_train_predictions <- bind_cols(
  train_data,
  xgb_train_prob,
  xgb_train_class
)


# Train Metrics


metric_set(
  accuracy,
  kap,
  recall,
  precision,
  f_meas,
  roc_auc
)(
  xgb_train_predictions,
  truth = decline_binary,
  estimate = .pred_class,
  .pred_Yes
)


#  Variable Importance


final_xgb_fit %>%
  extract_fit_parsnip() %>%
  vip()


#  Extract Final Engine


xgb_engine <- extract_fit_engine(
  final_xgb_fit
)


# XGBoost Native Importance


xgb.importance(
  model = xgb_engine
)


#  Plot Importance


xgb.plot.importance(
  xgb.importance(model = xgb_engine)
)



# =====================================================
# SUPPORT VECTOR MACHINE (SVM)
# =====================================================


set.seed(123)


# Recipe normalize due to requirement

svm_recipe <- recipe(
  decline_binary ~ .,
  data = train_data
) %>%
  
  
  step_dummy(all_nominal_predictors()) %>%
  
  # Normalize predictors
  step_normalize(all_predictors())


# Cross Validation


cv_folds <- vfold_cv(
  train_data,
  v = 10,
  strata = decline_binary
)


# Define SVM model radial kernel


svm_model <- svm_rbf(
  
  cost = tune(),
  
  rbf_sigma = tune()
  
) %>%
  set_engine("kernlab") %>%
  set_mode("classification")


# Workflow


svm_workflow <- workflow() %>%
  add_recipe(svm_recipe) %>%
  add_model(svm_model)


# Hyperparameter Grid


svm_grid <- grid_regular(
  
  cost(
    range = c(-2, 5)
  ),
  
  rbf_sigma(
    range = c(-4, 2)
  ),
  
  levels = 10
)


# Tune Model


svm_tuned <- tune_grid(
  
  svm_workflow,
  
  resamples = cv_folds,
  
  grid = svm_grid,
  
  metrics = metric_set(
    accuracy,
    kap,
    recall,
    precision,
    f_meas,
    roc_auc
  ),
  
  control = control_grid(
    save_pred = TRUE
  )
)


#  View Results


collect_metrics(svm_tuned)


# Select Best Hyperparameters


best_svm <- select_best(
  svm_tuned,
  metric = "roc_auc"
)

best_svm


# Finalize Workflow


final_svm_workflow <- finalize_workflow(
  svm_workflow,
  best_svm
)


#  Fit Final Model


final_svm_fit <- fit(
  final_svm_workflow,
  data = train_data
)


# Predict Test Data


svm_prob <- predict(
  final_svm_fit,
  test_data,
  type = "prob"
)

svm_class <- predict(
  final_svm_fit,
  test_data,
  type = "class"
)


#  Combine Predictions


svm_predictions <- bind_cols(
  test_data,
  svm_prob,
  svm_class
)


#. Confusion Matrix


conf_mat(
  svm_predictions,
  truth = decline_binary,
  estimate = .pred_class
)


# Test Metrics


svm_metrics <- metric_set(
  accuracy,
  kap,
  recall,
  precision,
  f_meas,
  roc_auc
)(
  svm_predictions,
  truth = decline_binary,
  estimate = .pred_class,
  .pred_Yes
)

svm_metrics



# Train Predictions


svm_train_prob <- predict(
  final_svm_fit,
  train_data,
  type = "prob"
)

svm_train_class <- predict(
  final_svm_fit,
  train_data,
  type = "class"
)

svm_train_predictions <- bind_cols(
  train_data,
  svm_train_prob,
  svm_train_class
)


# Train Metrics


metric_set(
  accuracy,
  kap,
  recall,
  precision,
  f_meas,
  roc_auc
)(
  svm_train_predictions,
  truth = decline_binary,
  estimate = .pred_class,
  .pred_Yes
)


# Best Hyperparameters


best_svm


# =====================================================
# Linear SVM 
# =====================================================


# Convert decline variable back too to 1/0

train_data <- train_data%>%
  
  mutate(
    
    decline_binary =
      case_when(
        
        decline_binary == "Yes" ~ "1",
        
        decline_binary == "No" ~ "0",
        
        TRUE ~ as.character(decline_binary)
      )
  )

test_data <- test_data %>%
  
  mutate(
    
    decline_binary =
      case_when(
        
        decline_binary == "Yes" ~ "1",
        
        decline_binary == "No" ~ "0",
        
        TRUE ~ as.character(decline_binary)
      )
  )

# ensure factor and levels

train_data$decline_binary <- factor(
  
  train_data$decline_binary,
  
  levels = c("1", "0")
)

test_data$decline_binary <- factor(
  
  test_data$decline_binary,
  
  levels = c("1", "0")
)


# normalize
linear_svm_recipe <- recipe(
  decline_binary ~ .,
  data = train_data
) %>%
  
  step_dummy(all_nominal_predictors()) %>%
  
  step_nzv(all_predictors()) %>%
  
  step_normalize(all_predictors())


# Cross Validation


cv_folds <- vfold_cv(
  train_data,
  v = 10,
  strata = decline_binary
)


#  Define Linear SVM model


linear_svm_model <- svm_linear(
  
  cost = tune()
  
) %>%
  
  set_engine("LiblineaR") %>%
  
  set_mode("classification")


#  Workflow


linear_svm_workflow <- workflow() %>%
  
  add_recipe(linear_svm_recipe) %>%
  
  add_model(linear_svm_model)


#  Hyperparameter Grid


linear_svm_grid <- grid_regular(
  
  cost(
    range = c(-3, 3)
  ),
  
  levels = 15
)


# Tune model


linear_svm_tuned <- tune_grid(
  
  linear_svm_workflow,
  
  resamples = cv_folds,
  
  grid = linear_svm_grid,
  
  metrics = metric_set(
    accuracy,
    kap,
    recall,
    precision,
    f_meas,
    roc_auc
  ),
  
  control = control_grid(
    save_pred = TRUE
  )
)


#  View tuning results


collect_metrics(
  linear_svm_tuned
)


# Best hyperparameter


best_linear_svm <- select_best(
  
  linear_svm_tuned,
  
  metric = "accuracy"
)

best_linear_svm


# Finalize workflow


final_linear_svm_workflow <- finalize_workflow(
  
  linear_svm_workflow,
  
  best_linear_svm
)


# Fit final model


final_linear_svm_fit <- fit(
  
  final_linear_svm_workflow,
  
  data = train_data
)


# Test predictions


linear_svm_prob <- predict(
  
  final_linear_svm_fit,
  
  test_data,
  
  type = "raw"
)

linear_svm_class <- predict(
  
  final_linear_svm_fit,
  
  test_data,
  
  type = "class"
)

linear_svm_predictions <- bind_cols(
  
  test_data,
  
  linear_svm_prob,
  
  linear_svm_class
)


# Confusion Matrix


conf_mat(
  
  linear_svm_predictions,
  
  truth = decline_binary,
  
  estimate = .pred_class
)


# Test Metrics


linear_svm_metrics <- metric_set(
  
  accuracy,
  kap,
  recall,
  precision,
  f_meas
  
)(
  
  linear_svm_predictions,
  
  truth = decline_binary,
  
  estimate = .pred_class,
  
  .pred_1
)

linear_svm_metrics




# Train Metrics


linear_svm_train_prob <- predict(
  
  final_linear_svm_fit,
  
  train_data,
  
  type = "raw"
)

linear_svm_train_class <- predict(
  
  final_linear_svm_fit,
  
  train_data,
  
  type = "class"
)

linear_svm_train_predictions <- bind_cols(
  
  train_data,
  
  linear_svm_train_prob,
  
  linear_svm_train_class
)

metric_set(
  
  accuracy,
  kap,
  recall,
  precision,
  f_meas
  
  
)(
  
  linear_svm_train_predictions,
  
  truth = decline_binary,
  
  estimate = .pred_class,
  
  .pred_1
)



# =====================================================
# XGB Beeswamp plot
# =====================================================


# PREPARE TRAIN DATA for shap values


xgb_recipe <- recipe(
  decline_binary ~ .,
  data = train_data
) %>%
  
  step_dummy(all_nominal_predictors()) %>%
  
  step_nzv(all_predictors())


# PREP RECIPE


xgb_prep <- prep(xgb_recipe)


# BAKE TRAIN DATA


train_processed <- bake(
  xgb_prep,
  new_data = train_data
)

test_processed <- bake(
  xgb_prep,
  new_data = test_data
)


#  CREATE MATRIX


x_train <- train_processed %>%
  select(-decline_binary) %>%
  as.matrix()

y_train <- as.numeric(
  train_processed$decline_binary == "1"
)

x_test <- test_processed %>%
  select(-decline_binary) %>%
  as.matrix()


# Create DMatrix


dtrain <- xgb.DMatrix(
  
  data = x_train,
  
  label = y_train
)


# Train model with optimal values from model


xgb_fit <- xgb.train(
  
  params = list(
    
    objective = "binary:logistic",
    
    eval_metric = "auc",
    
    max_depth = 8,
    
    eta = 0.01,
    
    min_child_weight = 2,
    
    gamma = 0.00001,
    
    subsample = 0.75,
    
    colsample_bytree = 14 / ncol(x_train)
  ),
  
  data = dtrain,
  
  nrounds = 600,
  
  verbose = 0
)


# CREATE SHAP VALUES


shap_values <- shapviz(
  
  xgb_fit,
  
  X_pred = x_train,
  
  X = as.data.frame(x_train)
)


# GLOBAL SHAP IMPORTANCE


sv_importance(
  shap_values,
  kind = "bar"
) +
  
  ggtitle(
    "SHAP Variable Importance - XGBoost"
  ) +
  
  theme_minimal(base_size = 14)


# SHAP BEE SWARM PLOT


sv_importance(
  shap_values,
  kind = "beeswarm"
) +
  
  ggtitle(
    "SHAP Beeswarm Plot - XGBoost"
  ) +
  
  theme_minimal(base_size = 14)


# =====================================================
# Error analysis
# =====================================================




# CREATE UNIQUE ROW ID VARIABLE


test_data <- test_data %>%
  
  mutate(
    row_id = row_number()
  )


# ADD ROW ID TO ALL PREDICTIONS


lr_predictions <- lr_predictions %>%
  
  mutate(
    row_id = test_data$row_id
  )

tree_predictions <- tree_predictions %>%
  
  mutate(
    row_id = test_data$row_id
  )

rf_predictions <- rf_predictions %>%
  
  mutate(
    row_id = test_data$row_id
  )

gbm_predictions <- gbm_predictions %>%
  
  mutate(
    row_id = test_data$row_id
  )

xgb_predictions <- xgb_predictions %>%
  
  mutate(
    row_id = test_data$row_id
  )

svm_predictions <- svm_predictions %>%
  
  mutate(
    row_id = test_data$row_id
  )

linear_svm_predictions <- linear_svm_predictions %>%
  
  mutate(
    row_id = test_data$row_id
  )

# CREATE ANALYSIS DATAFRAME


analysis_df <- test_data %>%
  
  left_join(
    
    lr_predictions %>%
      select(
        row_id,
        lr_pred = .pred_class
      ),
    
    by = "row_id"
  ) %>%
  
  left_join(
    
    tree_predictions %>%
      select(
        row_id,
        dt_pred = .pred_class
      ),
    
    by = "row_id"
  ) %>%
  
  left_join(
    
    rf_predictions %>%
      select(
        row_id,
        rf_pred = .pred_class
      ),
    
    by = "row_id"
  ) %>%
  
  left_join(
    
    gbm_predictions %>%
      select(
        row_id,
        gbm_pred = .pred_class
      ),
    
    by = "row_id"
  ) %>%
  
  left_join(
    
    xgb_predictions %>%
      select(
        row_id,
        xgb_pred = .pred_class
      ),
    
    by = "row_id"
  ) %>%
  
  left_join(
    
    svm_predictions %>%
      select(
        row_id,
        svm_pred = .pred_class
      ),
    
    by = "row_id"
  ) %>%
  left_join(
    
    linear_svm_predictions %>%
      
      select(
        row_id,
        linear_svm_pred = .pred_class
      ),
    
    by = "row_id"
  )



# CONVERT FACTORS TO CHARACTER


analysis_df <- analysis_df %>%
  
  mutate(
    
    across(
      
      c(
        decline_binary,
        lr_pred,
        dt_pred,
        rf_pred,
        gbm_pred,
        xgb_pred,
        svm_pred,
        linear_svm_pred
      ),
      
      as.character
    )
  )


# FIX  LABELS


analysis_df <- analysis_df %>%
  
  mutate(
    
    gbm_pred =
      case_when(
        
        gbm_pred == "Yes" ~ "1",
        
        gbm_pred == "No" ~ "0",
        
        TRUE ~ gbm_pred
      )
  )

analysis_df <- analysis_df %>%
  
  mutate(
    
    xgb_pred =
      case_when(
        
        xgb_pred == "Yes" ~ "1",
        
        xgb_pred == "No" ~ "0",
        
        TRUE ~ xgb_pred
      )
  )

analysis_df <- analysis_df %>%
  
  mutate(
    
    svm_pred =
      case_when(
        
        svm_pred == "Yes" ~ "1",
        
        svm_pred == "No" ~ "0",
        
        TRUE ~ svm_pred
      )
  )


# CREATE ERROR FLAGS


analysis_df <- analysis_df %>%
  
  mutate(
    
    lr_wrong =
      lr_pred != decline_binary,
    
    dt_wrong =
      dt_pred != decline_binary,
    
    rf_wrong =
      rf_pred != decline_binary,
    
    gbm_wrong =
      gbm_pred != decline_binary,
    
    xgb_wrong =
      xgb_pred != decline_binary,
    
    svm_wrong =
      svm_pred != decline_binary,
    linear_svm_wrong =
      linear_svm_pred != decline_binary
  )


# COUNT NUMBER OF MODELS WRONG


analysis_df <- analysis_df %>%
  
  mutate(
    
    n_models_wrong =
      rowSums(
        
        select(
          
          .,
          
          lr_wrong,
          dt_wrong,
          rf_wrong,
          gbm_wrong,
          xgb_wrong,
          svm_wrong,
          linear_svm_wrong
        )
      )
  )


# DISTRIBUTION OF MISSCLASSIFIED OBSERVATIONS


table(
  analysis_df$n_models_wrong
)

prop.table(
  table(
    analysis_df$n_models_wrong
  )
)



# ALL WRONG DF
all_wrong_cases <- analysis_df %>%
  
  filter(
    n_models_wrong >= 7
  )

fp_all <- all_wrong_cases %>%
  filter(decline_binary == 0)

fn_all <- all_wrong_cases %>%
  filter(decline_binary == 1)

true_posetive <- test_data%>%
  filter(decline_binary == 1)


true_negative <- test_data%>%
  filter(decline_binary == 0)

# COMPARISON SUMMARY TRUE VALUES AND MISSCLASSIFICATIONS
comparison_summary <- tibble(
  
  Group = c(
    
    "TP",
    "TN",
    
    "Shared FN ",
    "Shared FP "
  ),
  
  Observations = c(
    
    nrow(true_posetive),
    nrow(true_negative),
    
    nrow(fn_all),
    nrow(fp_all)
  ),
  
  Mean_HistPerf = c(
    
    mean(true_posetive$hist_perf),
    mean(true_negative$hist_perf),
    
    mean(fn_all$hist_perf),
    mean(fp_all$hist_perf)
  ),
  
  Mean_Age = c(
    
    mean(true_posetive$age_season),
    mean(true_negative$age_season),
    
    mean(fn_all$age_season),
    mean(fp_all$age_season)
  )
)

comparison_summary


# =====================================================
# MODEL COMPARISON TESTS PREPERATIONS
# =====================================================


# STANDARDIZE PREDICTION LABELS


lr_predictions$.pred_class <- as.character(
  lr_predictions$.pred_class
)

tree_predictions$.pred_class <- as.character(
  tree_predictions$.pred_class
)

rf_predictions$.pred_class <- as.character(
  rf_predictions$.pred_class
)

gbm_predictions$.pred_class <- as.character(
  gbm_predictions$.pred_class
)

xgb_predictions$.pred_class <- as.character(
  xgb_predictions$.pred_class
)

svm_predictions$.pred_class <- as.character(
  svm_predictions$.pred_class
)


# Convert Yes/No -> 1/0


convert_labels <- function(x) {
  
  case_when(
    
    x %in% c("Yes", "1") ~ "1",
    
    x %in% c("No", "0") ~ "0",
    
    TRUE ~ x
  )
}

lr_predictions$.pred_class <-
  convert_labels(
    lr_predictions$.pred_class
  )

tree_predictions$.pred_class <-
  convert_labels(
    tree_predictions$.pred_class
  )

rf_predictions$.pred_class <-
  convert_labels(
    rf_predictions$.pred_class
  )

gbm_predictions$.pred_class <-
  convert_labels(
    gbm_predictions$.pred_class
  )

xgb_predictions$.pred_class <-
  convert_labels(
    xgb_predictions$.pred_class
  )

svm_predictions$.pred_class <-
  convert_labels(
    svm_predictions$.pred_class
  )


# Convert back to factor


lr_predictions$.pred_class <- factor(
  
  lr_predictions$.pred_class,
  
  levels = c("1", "0")
)

tree_predictions$.pred_class <- factor(
  
  tree_predictions$.pred_class,
  
  levels = c("1", "0")
)

rf_predictions$.pred_class <- factor(
  
  rf_predictions$.pred_class,
  
  levels = c("1", "0")
)

gbm_predictions$.pred_class <- factor(
  
  gbm_predictions$.pred_class,
  
  levels = c("1", "0")
)

xgb_predictions$.pred_class <- factor(
  
  xgb_predictions$.pred_class,
  
  levels = c("1", "0")
)

svm_predictions$.pred_class <- factor(
  
  svm_predictions$.pred_class,
  
  levels = c("1", "0")
)




# 1. CREATE PREDICTION TABLE


model_compare <- tibble(
  
  truth = test_data$decline_binary,
  
  lr  = lr_predictions$.pred_class,
  
  dt  = tree_predictions$.pred_class,
  
  rf  = rf_predictions$.pred_class,
  
  gbm = gbm_predictions$.pred_class,
  
  xgb = xgb_predictions$.pred_class,
  
  svm = svm_predictions$.pred_class,
  
  svm_lin = linear_svm_predictions$.pred_class
)


# =====================================================
# ACCURACY COMPARISON McNemar Test
# =====================================================


# LR vs RF


lr_rf_table <- table(
  
  lr_correct =
    model_compare$lr ==
    model_compare$truth,
  
  rf_correct =
    model_compare$rf ==
    model_compare$truth
)

mcnemar.test(
  lr_rf_table
)


# LR vs XGB


lr_xgb_table <- table(
  
  lr_correct =
    model_compare$lr ==
    model_compare$truth,
  
  xgb_correct =
    model_compare$xgb ==
    model_compare$truth
)

mcnemar.test(
  lr_xgb_table
)


# LR vs SVM


lr_svm_table <- table(
  
  lr_correct =
    model_compare$lr ==
    model_compare$truth,
  
  svm_correct =
    model_compare$svm ==
    model_compare$truth
)

mcnemar.test(
  lr_svm_table
)


# LR vs dt

lr_dt_table <- table(
  
  lr_correct =
    model_compare$lr ==
    model_compare$truth,
  
  dt_correct =
    model_compare$dt ==
    model_compare$truth
)

mcnemar.test(
  lr_dt_table
)

# LR vs lin_svm

lr_lin_svm_table <- table(
  
  lr_correct =
    model_compare$lr ==
    model_compare$truth,
  
  lin_svim_correct =
    model_compare$svm_lin ==
    model_compare$truth
)

mcnemar.test(
  lr_lin_svm_table
)



# LR vs gbm

lr_GBM_table <- table(
  
  lr_correct =
    model_compare$lr ==
    model_compare$truth,
  
  gbm_correct =
    model_compare$gbm ==
    model_compare$truth
)

mcnemar.test(
  lr_GBM_table
)


# Lin_SVM vs dt

lin_SVM_dt_table <- table(
  
  lin_svm_correct =
    model_compare$svm_lin ==
    model_compare$truth,
  
  dt_correct =
    model_compare$dt ==
    model_compare$truth
)

mcnemar.test(
  lin_SVM_dt_table
)


# Lin_SVM vs RF

lin_SVM_rf_table <- table(
  
  lin_svm_correct =
    model_compare$svm_lin ==
    model_compare$truth,
  
  rf_correct =
    model_compare$rf ==
    model_compare$truth
)

mcnemar.test(
  lin_SVM_rf_table
)


# Lin_SVM vs GBM

lin_SVM_gbm_table <- table(
  
  lin_svm_correct =
    model_compare$svm_lin ==
    model_compare$truth,
  
  gbm_correct =
    model_compare$gbm ==
    model_compare$truth
)

mcnemar.test(
  lin_SVM_gbm_table
)


# Lin_SVM vs XGB

lin_SVM_XGB_table <- table(
  
  lin_svm_correct =
    model_compare$svm_lin ==
    model_compare$truth,
  
  XGB_correct =
    model_compare$gbm ==
    model_compare$truth
)

mcnemar.test(
  lin_SVM_XGB_table
)

# Lin_SVM vs SVM

lin_SVM_SVM_table <- table(
  
  lin_svm_correct =
    model_compare$svm_lin ==
    model_compare$truth,
  
  SVM_correct =
    model_compare$svm ==
    model_compare$truth
)

mcnemar.test(
  lin_SVM_SVM_table
)


# dt vs SVM

dt_SVM_table <- table(
  
  dt_correct =
    model_compare$dt ==
    model_compare$truth,
  
  SVM_correct =
    model_compare$svm ==
    model_compare$truth
)

mcnemar.test(
  dt_SVM_table
)


# dt vs RF

dt_rf_table <- table(
  
  dt_correct =
    model_compare$dt ==
    model_compare$truth,
  
  SVM_correct =
    model_compare$rf ==
    model_compare$truth
)

mcnemar.test(
  dt_rf_table
)


# dt vs gbm

dt_gbm_table <- table(
  
  dt_correct =
    model_compare$dt ==
    model_compare$truth,
  
  gbm_correct =
    model_compare$gbm ==
    model_compare$truth
)

mcnemar.test(
  dt_gbm_table
)


# dt vs XGB

dt_xgb_table <- table(
  
  dt_correct =
    model_compare$dt ==
    model_compare$truth,
  
  xgb_correct =
    model_compare$xgb ==
    model_compare$truth
)

mcnemar.test(
  dt_xgb_table
)

# rf vs SVM

rf_SVM_table <- table(
  
  rf_correct =
    model_compare$rf ==
    model_compare$truth,
  
  SVM_correct =
    model_compare$svm ==
    model_compare$truth
)

mcnemar.test(
  rf_SVM_table
)


# rf vs gbm

rf_gbm_table <- table(
  
  rf_correct =
    model_compare$rf ==
    model_compare$truth,
  
  gbm_correct =
    model_compare$gbm ==
    model_compare$truth
)

mcnemar.test(
  rf_gbm_table
)


# rf vs XGB

rf_xgb_table <- table(
  
  rf_correct =
    model_compare$rf ==
    model_compare$truth,
  
  xgb_correct =
    model_compare$xgb ==
    model_compare$truth
)

mcnemar.test(
  rf_xgb_table
)

# gbm vs XGB

gbm_xgb_table <- table(
  
  gbm_correct =
    model_compare$gbm ==
    model_compare$truth,
  
  xgb_correct =
    model_compare$xgb ==
    model_compare$truth
)

mcnemar.test(
  gbm_xgb_table
)


# gbm vs svm

gbm_svm_table <- table(
  
  gbm_correct =
    model_compare$gbm ==
    model_compare$truth,
  
  svm_correct =
    model_compare$svm ==
    model_compare$truth
)

mcnemar.test(
  gbm_svm_table
)


# xgb vs svm

xgb_svm_table <- table(
  
  xgb_correct =
    model_compare$xgb ==
    model_compare$truth,
  
  svm_correct =
    model_compare$svm ==
    model_compare$truth
)

mcnemar.test(
  xgb_svm_table
)

# =====================================================
# COCHRAN'S Q TEST
# =====================================================

correct_matrix <- model_compare %>%
  
  transmute(
    
    lr  = as.numeric(lr  == truth),
    
    dt  = as.numeric(dt  == truth),
    
    rf  = as.numeric(rf  == truth),
    
    gbm = as.numeric(gbm == truth),
    
    xgb = as.numeric(xgb == truth),
    
    svm = as.numeric(svm == truth),
    
    svm_lin = as.numeric(svm_lin)
  )

CochranQTest(
  as.matrix(correct_matrix)
)



# =====================================================
# regression with future and history performance
# =====================================================
lm_future <- lm(
  future_perf ~ hist_perf,
  data = info_data
)
summary(lm_future)
lm_future



# =====================================================
# linear most confident missclassified
# =====================================================

# false posetives

top5_lr_fp <- lr_predictions %>%
  
  
  filter(
    
    decline_binary == "0",
    
    .pred_class == "1"
  ) %>%
  
  arrange(
    desc(.pred_1)
  ) %>%
  
  slice(1:5)

top5_lr_fp


# false negatives


top5_lr_fn <- lr_predictions %>%
  
  
  
  filter(
    
    decline_binary == "1",
    
    .pred_class == "0"
  ) %>%
  
  arrange(
    .pred_1
  ) %>%
  
  slice(1:5)

top5_lr_fn


