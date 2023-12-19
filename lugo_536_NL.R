library(tidyverse)
library(readxl)
library(gt)
library(rsample)
library(glmnet)
library(glmnetUtils)
library(glue)
library(gridExtra)
library(gtsummary)
library(modelsummary)
library(pROC)
library(ROCR)
library(webshot2)

inmate_release_offenses_cps_1 <- read_excel("data/Inmate_Release.xlsx", "Inmate Release Offenses CPS_1")
inmate_active_offenses_cps <- read_excel("data/FDOC_Database_Active.XLSX", "Inmate Active Offenses CPS")
inmate_release_root <- read_excel("data/Inmate_Release.xlsx", "Inmate Release Root")
inmate_active_root <- read_excel("data/FDOC_Database_Active.XLSX", "Inmate Active Root")


##filter for DCNumbers with multiple dates 1 year apart (recidivism)
inmate_release_offenses_cps_1$DateAdjudicated <- as.Date(inmate_release_offenses_cps_1$DateAdjudicated)
time_diff_release <- inmate_release_offenses_cps_1 |>
  arrange(DCNumber, DateAdjudicated) |>
  group_by(DCNumber) |>
  mutate(time_diff = c(0, diff(DateAdjudicated))) |>
  ungroup()

recidivate_release <- time_diff_release |>
  group_by(DCNumber) |>
  filter(any(time_diff >= 365)) |>
  distinct(DCNumber)

inmate_release_offenses_cps_1 <- inmate_release_offenses_cps_1 |>
  mutate(recidivate = as.integer(DCNumber %in% recidivate_release$DCNumber))


inmate_active_offenses_cps$DateAdjudicated <- as.Date(inmate_active_offenses_cps$DateAdjudicated)
time_diff_active <- inmate_active_offenses_cps |>
  arrange(DCNumber, DateAdjudicated) |>
  group_by(DCNumber) |>
  mutate(time_diff = c(0, diff(DateAdjudicated))) |>
  ungroup()

recidivate_active <- time_diff_active |>
  group_by(DCNumber) |>
  filter(any(time_diff >= 365)) |>
  distinct(DCNumber)

inmate_active_offenses_cps <- inmate_active_offenses_cps |>
  mutate(recidivate = as.integer(DCNumber %in% recidivate_active$DCNumber))

combined_dataset <- bind_rows(
  inmate_release_offenses_cps_1, inmate_active_offenses_cps
)

##Turn prison term into months
combined_dataset <- combined_dataset |>
  mutate(
    PrisonTermInMonths = (as.numeric(substr(prisonterm, 1, 3)) * 12) +  
      as.numeric(substr(prisonterm, 4, 5)) +            
      as.numeric(substr(prisonterm, 6, 7)) / 30          
  )

##Filter out those whose only offense resulted in a life or death sentence
combined_dataset <- combined_dataset |>
  group_by(DCNumber) |>
  filter(!(recidivate == 0 & any(prisonterm %in% c(9999998, 9999999)))) |>
  ungroup()

##Take only the first offense for people who recidivated and combine with people who did not
##This is so individuals are not double counted
##The goal is to see if prison term for first offense affects recidivism probability

recidivate_data <- combined_dataset |>
  filter(recidivate == 1) |>
  group_by(DCNumber) |>
  slice(1) |>
  ungroup()

non_recidivate_data <- combined_dataset |>
  filter(recidivate == 0)

model_data <- bind_rows(recidivate_data, non_recidivate_data)

##Add columns with additional information for model
model_data <- model_data |>
  left_join(inmate_release_root, by = "DCNumber")

model_data <- model_data |>
  left_join(inmate_active_root, by = "DCNumber")

##combine so there are no duplicate columns
model_data <- model_data |>
  mutate(
    LastName = coalesce(LastName.x, LastName.y),
    FirstName = coalesce(FirstName.x, FirstName.y),
    MiddleName = coalesce(MiddleName.x, MiddleName.y),
    NameSuffix = coalesce(NameSuffix.x, NameSuffix.y),
    Race = coalesce(Race.x, Race.y),
    Sex = coalesce(Sex.x, Sex.y),
    BirthDate = coalesce(BirthDate.x, BirthDate.y),
    PrisonReleaseDate = coalesce(PrisonReleaseDate.x, PrisonReleaseDate.y),
    releasedateflag_descr = coalesce(releasedateflag_descr.x, releasedateflag_descr.y),
    race_descr = coalesce(race_descr.x, race_descr.y),
    custody_description = coalesce(custody_description.x, custody_description.y)
  ) |>
  select(-ends_with(".x"), -ends_with(".y"))

##Filter out deceased (they cannot recidivate)
model_data <- model_data |>
  filter(!grepl("deceased", releasedateflag_descr, ignore.case = TRUE))

##Filter out rows with incomplete custody data
model_data <- model_data |>
  filter(!is.na(custody_description)) |>
  filter(County != "BOP EXCHANGE") |>
  filter(County != "UNKNOWN")


##Make age at adjudication column to include in model
model_data <- model_data |>
  mutate(
    BirthDate = as.Date(BirthDate),
    DateAdjudicated = as.Date(DateAdjudicated),
    Age_At_Adjudication = as.numeric(difftime(DateAdjudicated, BirthDate, units = "days")) / 365.25
  ) |>
  filter(!is.na(Age_At_Adjudication))


##See which does better when sample in split
set.seed(06511)
model_data_small <- model_data[sample(nrow(model_data), size = 100000, replace = FALSE), ]

##Prediction formulas
simple_model <- lm(recidivate ~ PrisonTermInMonths, data = model_data_small)
summary(simple_model)

complex_model <- lm(recidivate ~ PrisonTermInMonths + Race + Sex + Age_At_Adjudication +
                      custody_description, data = model_data_small)
summary(complex_model)
##

mod_split <- rsample::initial_split(model_data_small, prop = .8)
mod_train <- rsample::training(mod_split)
mod_test <- rsample::testing(mod_split)

mod1 <- lm(recidivate ~ PrisonTermInMonths, data = mod_train)
mod2 <- lm(recidivate ~ PrisonTermInMonths + Race + Sex + Age_At_Adjudication +
             custody_description + County, data = mod_train)

RMSE_train <- mod_train |>
  mutate(pred1 = predict(mod1, mod_train),
         pred2 = predict(mod2, mod_train)) |>
  summarise(rmse_1 = sqrt(mean((recidivate - pred1)^2)),
            rmse_2 = sqrt(mean((recidivate - pred2)^2)))

RMSE_test <- mod_test |>
  mutate(pred1 = predict(mod1, mod_test),
         pred2 = predict(mod2, mod_test)) |>
  summarise(rmse_1 = sqrt(mean((recidivate - pred1)^2)),
            rmse_2 = sqrt(mean((recidivate - pred2)^2)))

##Visualize the model 
mod1 <- glm(recidivate ~ PrisonTermInMonths, data = mod_train, family = "binomial")
mod2 <- glm(recidivate ~ PrisonTermInMonths + Race + Sex + Age_At_Adjudication +
              custody_description + County, data = mod_train, family = "binomial")

mod_test <- mod_test |>
  mutate(pred1 = predict(mod1, mod_test, type = "response"),
         pred2 = predict(mod2, mod_test, type = "response"))

mod1_test <- glm(recidivate ~ PrisonTermInMonths, data = mod_test, family = "binomial")
mod2_test <- glm(recidivate ~ PrisonTermInMonths + Race + Sex + Age_At_Adjudication +
              custody_description + County, data = mod_test, family = "binomial")


##Table 1
categorical_columns <- c("Sex", "race_descr", "custody_description")
categorical_summary <- model_data_small |>
  pivot_longer(cols = all_of(categorical_columns), names_to = "Variable", values_to = "Value") |>
  count(Variable, Value) |>
  ungroup() |>
  rename(Count = n) |>
  mutate(Variable = ifelse(Variable == "custody_description", "Custody Type", 
                           ifelse(Variable == "race_descr", "Race", Variable)))
  
Table_1 <- categorical_summary |>
  gt() |>
  tab_header(
    title = "Summary Statistics for Categorical Columns"
  ) |>
  gtsave("Table_1.png")
print(Table_1)


##Figure 1
roc_curve_mod1 <- roc(mod_test$recidivate, mod_test$pred1)
roc_curve_mod2 <- roc(mod_test$recidivate, mod_test$pred2)
auc_mod1 <- auc(roc_curve_mod1)
auc_mod2 <- auc(roc_curve_mod2)
plot(roc_curve_mod1, col = "blue", main = "ROC Curve for Recidivism Models", lwd = 3, 
     xlab = "False Positive Rate", ylab = "True Positive Rate")
plot(roc_curve_mod2, col = "red", add = TRUE, lwd = 3)
legend("bottomright", legend = c(paste("Model 1 (AUC = ", round(auc_mod1, 2), ")", sep = ""),
                                 paste("Model 2 (AUC = ", round(auc_mod2, 2), ")", sep = "")),
       col = c("blue", "red"), lwd = 3, cex = 1.5)
par(cex.axis = 1.5, cex.lab = 1.5)


##Table 2
model_list <- list(
  "Model 1 Train" = mod1,
  "Model 2 Train" = mod2,
  "Model 1 Test" = mod1_test,
  "Model 2 Test" = mod2_test
)

variable_labels <- c(
  "PrisonTermInMonths" = "Prison Term In Months",
  "RaceB" = "Race: Black",
  "RaceH" = "Race: Hispanic",
  "RaceI" = "Race: Indigenous",
  "RaceU" = "Race: Unknown",
  "RaceW" = "Race: White",
  "SexM" = "Sex: Male",
  "Age_At_Adjudication" = "Age At Adjudication",
  "custody_descriptionCOMMUNITY" = "Custody: Community",
  "custody_descriptionMAXIMUM" = "Custody: Maximum",
  "custody_descriptionMEDIUM" = "Custody: Medium",
  "custody_descriptionMINIMUM" = "Custody: Minimum",
  "custody_descriptionN/A" = "Custody: Not Applicable"
)

Table_2 <- modelsummary(model_list, stars = TRUE,
                        gof_map = c("nobs", "rmse"),
                        coef_omit = ("Intercept|County"),
                        coef_rename = variable_labels, output = "Table_2.png")

