## Reads in raw qualtrics data (with PII removed) and cleans it.

# set wd to base replication folder
# setwd("~/Dropbox/Mo_Salah/replication_files/")

source("code/survey_experiment/99pca_fun.R")

library(tidyverse)
library(lubridate)

salah = read.csv("data/salah_survey.csv", stringsAsFactors = FALSE)


# Create treatment variables ----------------------------------------------
salah$treatment_niceguy_relid = NA
salah$treatment_niceguy_relid[salah$tr_character %in% 1] = "Character"
salah$treatment_niceguy_relid[salah$tr_relid %in% 1] = "Religion"

salah$treatment_success_fail = NA
salah$treatment_success_fail[salah$tr_success %in% 1] = "Success"
salah$treatment_success_fail[salah$tr_failure %in% 1] = "Failure"

salah$treatment = NA
salah$treatment[salah$treatment_niceguy_relid == "Character" & salah$treatment_success_fail == "Success"]  = "Character - Success"
salah$treatment[salah$treatment_niceguy_relid == "Religion" & salah$treatment_success_fail == "Success"]  = "Religion - Success"
salah$treatment[salah$treatment_niceguy_relid == "Character" & salah$treatment_success_fail == "Failure"]  = "Character - Failure"
salah$treatment[salah$treatment_niceguy_relid == "Religion" & salah$treatment_success_fail == "Failure"]  = "Religion - Failure"
salah$treatment[is.na(salah$treatment_niceguy_relid) & is.na(salah$treatment_success_fail)] = "Pure Control"
salah$treatment[salah$treatment == "Pure Control" & !is.na(salah$tr_intro)] = NA

# indicator if they saw any questions post-treatment
# if they didn't see any questions post-treatment yet they're currently classified as
# being in the control group, recode their treatment to NA
post_treatment_qs = c("world_cup", "improve_lfc_1", "improve_lfc_2", "improve_lfc_3", 
                      "improve_lfc_6", "improve_lfc_7", 
                      "win_lfc", "karius", "muslims_common", "islam_compatible", "imm_impact", 
                      "Q50", "brexit", "Q48")
any_posttreatment = apply(salah, 1, function(x) any(x[post_treatment_qs] != "") | !is.na(x["salah_perform_12"]))
salah$treatment[salah$treatment == "Pure Control" & !any_posttreatment] = NA

# now update factorial treatment
salah$treatment_niceguy_relid[salah$treatment == "Pure Control"] = "Control"
salah$treatment_success_fail[salah$treatment == "Pure Control"] = "Control"


table(salah$treatment)
table(salah$treatment_niceguy_relid, salah$treatment_success_fail)


# Add weights -------------------------------------------------------------
# add weights based on inverse probability of treatment
# Pr(Pure control) before 11:20, 12/14/18: 4/5
# Pr(Pure control) between 11:20 and 12:20: 1/5
# Pr(Pure control) after 12:20: 1/8

salah = salah %>% 
  rename(startdate = V8) %>% 
  mutate(startdate = as_date(startdate, tz = "US/Pacific"),
         weight_cat = NA)
salah$weight_cat[salah$startdate <= as_date("2018-12-14 11:20:00", tz = "US/Pacific")] = 1
salah$weight_cat[salah$startdate > as_date("2018-12-14 11:20:00", tz = "US/Pacific") & salah$startdate <= as_date("2018-12-14 12:20:00", tz = "US/Pacific")] = 2
salah$weight_cat[salah$startdate > as_date("2018-12-14 12:20:00", tz = "US/Pacific")] = 3  

weightdf = data.frame(expand.grid(weight_cat = 1:3, treatment = unique(salah$treatment))) 
weightdf = subset(weightdf, !is.na(treatment))

weightdf$weight = NA
weightdf$weight[weightdf$weight_cat == 1 & weightdf$treatment == "Pure Control"] = 4/5
weightdf$weight[weightdf$weight_cat == 1 & weightdf$treatment != "Pure Control"] = (1/5) / 4

weightdf$weight[weightdf$weight_cat == 2 & weightdf$treatment == "Pure Control"] = 1/5
weightdf$weight[weightdf$weight_cat == 2 & weightdf$treatment != "Pure Control"] = (4/5) / 4

weightdf$weight[weightdf$weight_cat == 3 & weightdf$treatment == "Pure Control"] = 1/8
weightdf$weight[weightdf$weight_cat == 3 & weightdf$treatment != "Pure Control"] = (7/8) / 4

weightdf$weight = 1 / weightdf$weight

t = nrow(salah)
salah = left_join(salah, weightdf, by = c("weight_cat", "treatment"))
assertthat::assert_that(nrow(salah) == t)

# Recoding questions for regressions --------------------------------------
salah = salah %>% 
  mutate(muslims_common = factor(muslims_common, levels = c("Nothing in common", 
                                                            "A little in common",
                                                            "A lot in common")),
         muslims_common = as.numeric(muslims_common),
         muslims_bin = ifelse(muslims_common > 1, 1, ifelse(muslims_common <= 1, 0, NA)),
         
         #rescaling imm_impact to 5 point scale for polychoric regression
         imm_impact = ifelse(imm_impact == "Negative", "Very negative",
                             ifelse(imm_impact == "Positive", "Very positive", imm_impact)),
         imm_impact = factor(imm_impact, levels = c("Very negative", "Somewhat negative",
                                                   "Neither positive nor negative", "Somewhat positive", 
                                                   "Very positive")),
         imm_impact = as.numeric(imm_impact),
         imm_bin =  ifelse(imm_impact > 3, 1, ifelse(imm_impact <= 3, 0, NA)),
         
         islam_compatible = recode(islam_compatible, "Islam is generally compatible with the values of British society" = "1",
                                   "There is a fundamental clash between Islam and the values of British society" = "-1",
                                   "Neither" = "0", "Don’t know" = "0", .default = NA_character_),
         islam_compatible = as.numeric(islam_compatible),
         islam_bin = ifelse(islam_compatible == 1, 1, ifelse(islam_compatible < 1, 0, NA)))

salah = salah %>% 
  mutate(treatment = factor(treatment, levels = c("Pure Control", "Character - Failure", "Character - Success", 
                                                  "Religion - Failure", "Religion - Success")),
         treatment_success_fail = factor(treatment_success_fail, levels = c("Control", "Failure", "Success")),
         treatment_niceguy_relid = factor(treatment_niceguy_relid, levels = c("Control", "Character", "Religion")))

vars_pca = c("muslims_bin", "imm_bin", "islam_bin")


# Principal Component -----------------------------------------------------
salah_pca = salah %>% filter(complete.cases(muslims_bin, imm_bin, islam_compatible))


salah_pca$pc_outcome = PCA_extract(vars = salah_pca[, vars_pca], cor_type = "poly")[[2]] %>% as.vector()
PCA_extract(vars = salah[, vars_pca], cor_type = "poly")[[1]]

salah_pca = salah_pca %>% select(ID, pc_outcome)
salah = salah %>% left_join(salah_pca)

salah$pc_outcome %>% sd(na.rm = T)

# Variables for heterogeneous effects -------------------------------------
salah$fav_plyr %>% unique
salah$salah_fav = ifelse(salah$fav_plyr == "Mo Salah", 1, 
                         ifelse(salah$fav_plyr %in% c("Bobby Firmino", "Daniel Sturridge", "Other (insert name)", 
                                                      "Daniel Sturridge"), 0, NA))

salah$conservative = ifelse(salah$pid %in% c("Conservative", "UK Independence Party (UKIP)"), 1, 
                            ifelse(salah$pid %in% c("Labour", "Liberal Democrat", "None of the above", "Other", "Scottish National Party"), 0, NA))  #Note: there are no missing values
salah$conservative_liberal = salah$conservative
salah$conservative_liberal = ifelse(salah$conservative == 0 & !salah$pid %in% c("Labour", "Liberal Democrat"), NA, salah$conservative_liberal)
salah$conservative_liberal %>% table(useNA = "always")

salah$liverpool_res = ifelse(salah$city_uk_1 == "Liverpool", 1, 0)

salah$follow = recode(salah$follow_lfc, "Not at all (never watch matches or read news)" = "0",
                      "Not very closely (rarely watch matches or read news)" = "0",
                      "Somewhat closely (watch big matches, read news occasionally)" = "0",
                      "Very closely (watch every match, read news almost daily)" = "1", .default = NA_character_)

salah$follow = as.numeric(salah$follow)

salah$karius_empathy = recode(salah$karius, "Strongly agree" = "0", "Somewhat agree" = "0",
                                "Neither agree nor disagree" = "1", "Strongly disagree" = "1",
                                "Somewhat disagree" = "1", .default = NA_character_)
salah$karius_empathy = as.numeric(salah$karius_empathy)


# Control variables -------------------------------------------------------
salah$year_born_1[salah$year_born_1 < 1] = NA
salah$age = 2018 - salah$year_born_1

salah$female = ifelse(salah$gender == "Woman", 1, ifelse(salah$gender %in% c("Man", "Other"), 0, NA))
salah$educ = factor(salah$educ, levels = c("Primary school", "Secondary school", "Vocational training", "Some university", "University degree", "Master’s degree or higher"))
salah$univ = ifelse(as.numeric(salah$educ) >= 4, 1,
                    ifelse(as.numeric(salah$educ) < 4, 0 , NA))


# Removing unnecessary columns -----------------------------------------------------


salah = salah %>%
  select(-c(year_born_1, gender, educ, city_uk_1, pid, tr_intro, tr_failure, tr_success, tr_character, tr_relid, purecontrol_TEMP, weight_cat, Q50, Q48, end))

salah %>% names

# Export data -------------------------------------------------------------
salah %>% saveRDS("data/survey_clean.rds")

