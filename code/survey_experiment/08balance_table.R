
library(tidyverse)
library(estimatr)
library(texreg)
library(ggthemes)

salah = readRDS("data/survey_clean.rds")

salah$treatment = recode(salah$treatment, "Pure Control" = "Control", "Character - Failure" = "Char. - Fail.", 
                         "Character - Success" = "Char. - Succ.", "Religion - Failure" = "Rlgn. - Fail.", 
                         "Religion - Success" = "Rlgn - Succ.")

library(arsenal)

age = lm(age ~ treatment, data = salah, weights = weight) %>% summary
age = c(age$fstatistic[1], pf(age$fstatistic[1],age$fstatistic[2],age$fstatistic[3],lower.tail=FALSE))
age = paste0(round(age[1], digits = 2), " (",round(age[2], digits = 2),")")

female = lm(female ~ treatment, data = salah, weights = weight) %>% summary
female = c(female$fstatistic[1], pf(female$fstatistic[1],female$fstatistic[2],female$fstatistic[3],lower.tail=FALSE))
female = paste0(round(female[1], digits = 2), " (",round(female[2], digits = 2),")")

univ = lm(univ ~ treatment, data = salah, weights = weight) %>% summary
univ = c(univ$fstatistic[1], pf(univ$fstatistic[1],univ$fstatistic[2],univ$fstatistic[3],lower.tail=FALSE))
univ = paste0(round(univ[1], digits = 2), " (",round(univ[2], digits = 2),")")

salah_fav = lm(salah_fav ~ treatment, data = salah, weights = weight) %>% summary
salah_fav = c(salah_fav$fstatistic[1], pf(salah_fav$fstatistic[1],salah_fav$fstatistic[2],salah_fav$fstatistic[3],lower.tail=FALSE))
salah_fav = paste0(round(salah_fav[1], digits = 2), " (",round(salah_fav[2], digits = 2),")")

karius_empathy = lm(karius_empathy ~ treatment, data = salah, weights = weight) %>% summary
karius_empathy = c(karius_empathy$fstatistic[1], pf(karius_empathy$fstatistic[1],karius_empathy$fstatistic[2],karius_empathy$fstatistic[3],lower.tail=FALSE))
karius_empathy = paste0(round(karius_empathy[1], digits = 2), " (",round(karius_empathy[2], digits = 2),")")

conservative = lm(conservative ~ treatment, data = salah, weights = weight) %>% summary
conservative = c(conservative$fstatistic[1], pf(conservative$fstatistic[1],conservative$fstatistic[2],conservative$fstatistic[3],lower.tail=FALSE))
conservative = paste0(round(conservative[1], digits = 2), " (",round(conservative[2], digits = 2),")")

liverpool_res =  lm(liverpool_res ~ treatment, data = salah, weights = weight) %>% summary
liverpool_res = c(liverpool_res$fstatistic[1], pf(liverpool_res$fstatistic[1],liverpool_res$fstatistic[2],liverpool_res$fstatistic[3],lower.tail=FALSE))
liverpool_res = paste0(round(liverpool_res[1], digits = 2), " (",round(liverpool_res[2], digits = 2),")")

salah$salah_perform_12 = ifelse(salah$salah_perform_12 == -99, NA, salah$salah_perform_12)
salah$salah_perform_bin = ifelse(salah$salah_perform_12 > 9, 1,
                                 ifelse(salah$salah_perform_12 <= 9, 0, NA))
salah_perform_bin =  lm(salah_perform_bin ~ treatment, data = salah, weights = weight) %>% summary
salah_perform_bin = c(salah_perform_bin$fstatistic[1], pf(salah_perform_bin$fstatistic[1],salah_perform_bin$fstatistic[2],salah_perform_bin$fstatistic[3],lower.tail=FALSE))
salah_perform_bin = paste0(round(salah_perform_bin[1], digits = 2), " (",round(salah_perform_bin[2], digits = 2),")")


tab = tableby(treatment ~ age + female + univ + salah_fav + karius_empathy + liverpool_res + salah_perform_bin + conservative, data = salah, 
            weights = weight, total = F, digits = 2L, numeric.stats =  c("Nmiss2", "meansd", "range")) %>% 
  summary(text = "latex", labelTranslations = list(age = "Age (Years)", female = "Female", univ = "University Edu.",
                                                   salah_fav = "Salah Favorite", karius_empathy = "Karius Empathy",
                                                   liverpool_res = "Liverpool Resident", 
                                                   salah_perform_bin = "Salah Performance", 
                                                   conservative = "Conservative"),
          pfootnote = T, width = 5)


tab$object$treatment$`F-Stat (p.value)` = c(age, "", "", "", female, "", "", "", univ, "", "", "", salah_fav, "", "", "", karius_empathy, "", "", "", liverpool_res, "", "", "", salah_perform_bin, "", "", "", conservative, "", "", "")
capture.output(tab, file="tables/balance_table.tex")


