rm(list = ls())
# Set working directory
work_dir <- file.path("~/Copy/Berkeley/ph290-spring-2015/heritagehealth")
setwd(work_dir)
data_dir <- file.path(getwd(), "HHP_release3")
seed <- 123
library(dplyr)
library(tidyr)
library(ggplot2)
source("process-data.R")
dir.create("data", showWarnings = FALSE)
dir.create("results", showWarnings = FALSE)


# Resources:
# https://github.com/owenzhang/Kaggle-AmazonChallenge2013/blob/master/__final_model.R
# http://cran.r-project.org/web/views/MachineLearning.html
# http://www.math.uci.edu/icamp/summer/research_12/student_research/ensemble_learning/ensemblelearning.pdf

# Members: MemberID, AgeAtFirstClaim (10-yr age groups), Sex
# Claims: MemberID, ProviderID, Vendor, PCP, Year, Specialty, PlaceSvc, PayDelay,
#         LengthOfStay, DSFS, PrimaryConditionGroup, CharlsonIndex, ProcedureGroup, SupLOS
# days_y2/3: MemberID, ClaimsTruncated, DaysInHospital
#----------------------------------------------------------------------
# Data processing
if (FALSE) {
  members <- read.csv(file.path(data_dir, "Members.csv"), stringsAsFactors = FALSE)
  claims <- read.csv(file.path(data_dir, "Claims.csv"), stringsAsFactors = FALSE)
  drugs <- read.csv(file.path(data_dir, "DrugCount.csv"), stringsAsFactors = FALSE)
  labs <- read.csv(file.path(data_dir, "LabCount.csv"), stringsAsFactors = FALSE)
  days_y2 <- read.csv(file.path(data_dir, "DaysInHospital_Y2.csv"), stringsAsFactors = FALSE)
  submit <- days_y3 <- read.csv(file.path(data_dir, "DaysInHospital_Y3.csv"), stringsAsFactors = FALSE)
  # Data processing for additional analysis
  claims_proc <- process_claims2(claims)
  save(claims_proc, file = "data/claims_proc.rda")
  # Data processing: Summarise data by member-year
  claims_by_member_year <- process_claims(claims)
  # save(claims_by_member_year, file = "data/claims_by_member_year.rda")
  # load(file = "data/claims_by_member_year.rda")
  members_processed <- process_members(members)
  drugs_by_member_year <- process_drugs(drugs)
  labs_by_member_year <- process_labs(labs)
  days_y2 <- days_y2 %>% 
    mutate(Year = "Y2")
  days_y3 <- days_y3 %>% 
    mutate(Year = "Y3")
  claims_all <- claims_by_member_year %>% 
    left_join(members_processed) %>% 
    left_join(drugs_by_member_year) %>% 
    left_join(labs_by_member_year) %>% 
    left_join(rbind(days_y2, days_y3)) %>%
    mutate(LogDaysInHospital = log(DaysInHospital + 1))
  dih_by_member_year <- claims_all %>%
    select(MemberID, Year, LogDaysInHospital) %>%
    filter(Year != "Y1") %>%
    mutate(Year = ifelse(Year == "Y2", "Y1", "Y2")) %>%
    rename(LogDaysInHospitalNext = LogDaysInHospital)
  claims_all <- claims_all %>%
    left_join(dih_by_member_year) %>%
    select(-AgeAtFirstClaim, -ClaimsTruncated, -LogDaysInHospital) %>% 
    replace_NA_with_mean_by_year
  save(claims_all, file = "data/claims_all.rda")
}
load(file = "data/claims_all.rda")
# Check no NAs!
# check_NA <- sapply(claims_all, function(x) any(is.na(x)))
# check_NA[check_NA]

# Training data: Year 1 data to predict DIH_Y2  
train <- claims_all %>%
  filter(Year %in% c("Y1")) %>%
  select(-Year, -DaysInHospital)
train_dropNA <- train %>%
  filter(!is.na(LogDaysInHospitalNext))
# Test data: Year 2 data to predict DIH_Y3
test <- claims_all %>%
  filter(Year %in% c("Y2")) %>%
  select(-Year, -DaysInHospital)
# Check no NAs!
# check_NA <- sapply(train_dropNA, function(x) any(is.na(x)))
# check_NA[check_NA]
# check_NA <- sapply(test, function(x) any(is.na(x)))
# check_NA[check_NA]
#----------------------------------------------------------------------
# Testing some dummy predictions
# Predict DIH_Y3 = 0 for all
rmse(log(0 + 1), test$LogDaysInHospitalNext)
# Predict DIH_Y3 = mean(DIH_Y2)
rmse(mean(train$LogDaysInHospital, na.rm = TRUE), test$LogDaysInHospitalNext)
#----------------------------------------------------------------------
# Linear model
mod_lm <- lm(LogDaysInHospitalNext ~ ., data = train_dropNA[, -1])
pred_lm <- predict(mod_lm, test[, -c(1, ncol(test))])
rmse(pred_lm, test$LogDaysInHospitalNext)
#----------------------------------------------------------------------
# GLM
train_dropNA_glm <- train_dropNA %>%
  rename(DaysInHospitalNext = LogDaysInHospitalNext) %>%
  mutate(DaysInHospitalNext = floor(exp(DaysInHospitalNext) - 1))
mod_glm <- glm(DaysInHospitalNext ~ ., family = "poisson", data = train_dropNA_glm[, -1])
pred_glm_orig <- predict(mod_glm, test[, -c(1, ncol(test))])
pred_glm <- log(ifelse(pred_glm_orig < 0, 0, pred_glm_orig) + 1)
rmse(pred_glm, test$LogDaysInHospitalNext)
#----------------------------------------------------------------------
# Random forest
# Source: http://trevorstephens.com/post/73770963794/titanic-getting-started-with-r-part-5-random
library(randomForest)
set.seed(seed)
mod_rf <- randomForest(LogDaysInHospitalNext ~ ., 
                       # data = train_dropNA[, -1],
                       data = train_dropNA[1:100, c(2:11, 144)],
                       importance = TRUE, ntree = 2000, mtry = 3, 
                       nodesize = 10, maxnodes = 500, replace = FALSE, 
                       do.trace = 10)
pred_rf <- predict(mod_rf, test[, -c(1, ncol(test))])
rmse(pred_rf, test$LogDaysInHospitalNext)
save(mod_rf, file = "results/mod_rf.rda")
save(pred_rf, file = "results/pred_rf.rda")

load(file = "results/mod_rf.rda")
load(file = "results/pred_rf.rda")

# Plot of variable importance
mod_rf_impt <- importance(mod_rf)
# varImpPlot(mod_rf)
mod_rf_impt_df <- data.frame(Variable = dimnames(mod_rf_impt)[[1]], 
                             MeanDecreaseMSE = mod_rf_impt[, 1],
                             MeanDecreaseNodeImpurity = mod_rf_impt[, 2])
mod_rf_impt_df <- mod_rf_impt_df[order(mod_rf_impt_df$MeanDecreaseMSE, decreasing = TRUE), ]
mod_rf_impt_df$Variable <- factor(mod_rf_impt_df$Variable, 
                                  levels = rev(mod_rf_impt_df$Variable))
# Plot num_plot top variables 
num_plot <- 20
p <- ggplot(data = head(mod_rf_impt_df, num_plot),
            aes(x = MeanDecreaseMSE, y = Variable)) +
  geom_point(shape = 19, size = 3, col = "blue") +
  xlab("Relative importance") + ylab("") + 
  xlim(range(head(mod_rf_impt_df$MeanDecreaseMSE, num_plot))) +
  theme(plot.background = element_rect(fill = "transparent", colour = NA))
pdf("results/rf-importance.pdf", height = 10)
print(p)
dev.off()
#----------------------------------------------------------------------
# GBM
GBM_NTREES = 2000
GBM_SHRINKAGE = 0.05
GBM_DEPTH = 4
GBM_MINOBS = 50
library(gbm)
mod_gbm <- gbm.fit(
  x = train_dropNA[, -c(1, ncol(train_dropNA))],
  y = train_dropNA[, ncol(train_dropNA)],
  distribution = "gaussian"
  , n.trees = GBM_NTREES
  , shrinkage = GBM_SHRINKAGE
  , interaction.depth = GBM_DEPTH
  , n.minobsinnode = GBM_MINOBS
  , verbose = TRUE)
pred_gbm <- predict.gbm(object = mod_gbm, 
                        newdata = test[, -c(1, ncol(test))],
                        GBM_NTREES)
rmse(pred_gbm, test$LogDaysInHospitalNext)
save(mod_gbm, file = "results/mod_gbm.rda")
save(pred_gbm, file = "results/pred_gbm.rda")

load(file = "results/mod_gbm.rda")
load(file = "results/pred_gbm.rda")
# List variable importance
mod_gbm_impt <- summary(mod_gbm, GBM_NTREES)
mod_gbm_impt_df <- data.frame(Variable = mod_gbm_impt[, 1], 
                              RelativeImportance = mod_gbm_impt[, 2])
mod_gbm_impt_df <- mod_gbm_impt_df[order(mod_gbm_impt_df$RelativeImportance, decreasing = TRUE), ]
mod_gbm_impt_df$Variable <- factor(mod_gbm_impt_df$Variable, 
                                   levels = rev(mod_gbm_impt_df$Variable))
p <- ggplot(data = head(mod_gbm_impt_df, num_plot),
            aes(x = RelativeImportance, y = Variable)) +
  geom_point(shape = 19, size = 3, col = "blue") +
  xlab("Relative importance") + ylab("") + 
  xlim(range(head(mod_gbm_impt_df$RelativeImportance, num_plot))) +
  theme(plot.background = element_rect(fill = "transparent", colour = NA))
pdf("results/gbm-importance.pdf", height = 10)
print(p)
dev.off()

# Top 20 for both model
intersect(mod_rf_impt_df$Variable[1:20], mod_gbm_impt_df$Variable[1:20])
#----------------------------------------------------------------------
load(file = "data/claims_proc.rda")
claims_proc2 <- claims_proc %>%
  mutate(LengthOfStay = factor(LengthOfStay)) %>%
  mutate(DSFS = factor(DSFS))
varnames <- c("Year", "Specialty", "PlaceSvc", "PrimaryConditionGroup", "ProcedureGroup",
              "LengthOfStay", "DSFS")
pdf("results/boxplots.pdf", height = 20, width = 10)
for (i in seq_along(varnames)) {
  p <- ggplot(data = claims_proc2, aes_string(x = varnames[i], y = "PayDelay")) +
    geom_boxplot() + coord_flip() + 
    geom_hline(yintercept = mean(claims_proc$PayDelay), linetype = "longdash")
  print(p)
}
dev.off()
#----------------------------------------------------------------------
claims_proc_paydelay <- process_claims2(claims)
save(claims_proc_paydelay, file = "data/claims_proc_paydelay.rda")
load(file = "data/claims_proc_paydelay.rda")


load(file = "results/mod_gbm_paydelay.rda")
load(file = "results/pred_gbm_paydelay.rda")
# List variable importance
mod_gbm_impt <- summary(mod_gbm, GBM_NTREES)
mod_gbm_impt_df <- data.frame(Variable = mod_gbm_impt[, 1], 
                              RelativeImportance = mod_gbm_impt[, 2])
mod_gbm_impt_df <- mod_gbm_impt_df[order(mod_gbm_impt_df$RelativeImportance, decreasing = TRUE), ]
mod_gbm_impt_df$Variable <- factor(mod_gbm_impt_df$Variable, 
                                   levels = rev(mod_gbm_impt_df$Variable))
head(mod_gbm_impt_df)
p <- ggplot(data = head(mod_gbm_impt_df, num_plot),
            aes(x = RelativeImportance, y = Variable)) +
  geom_point(shape = 19, size = 3, col = "blue") +
  xlab("Relative importance") + ylab("") + 
  xlim(range(head(mod_gbm_impt_df$RelativeImportance, num_plot))) +
  theme(plot.background = element_rect(fill = "transparent", colour = NA))
pdf("results/gbm-paydelay-importance.pdf", height = 10)
print(p)
dev.off()
