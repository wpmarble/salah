# 	- Tests how survey outcomes changed around Liverpool victories and Salah goals

library(tidyverse)
library(estimatr)
library(texreg)
library(ggthemes)
library(lubridate)

salah = readRDS("data/survey_clean.rds")

games = read.csv("data/svy_games.csv", stringsAsFactors = F)
games = games %>% as_tibble()

# preparing the games data ------------------------------------------------
games = games %>% mutate(date = mdy(date))


#Marking the dates for 1 day and 2 days after Liverpool victory
games = games %>% mutate(after = date + 1)
games = games %>% mutate(before = date - 1)

games = games %>% select(- date)

#reshape to long
games = reshape2::melt(games, id.vars = c("result", "salah_scored"))
games = games %>% 
  rename(date = value) %>% 
  mutate(after = recode(variable, "before" = 0, "after" = 1),
         result = recode(result, "l" = 0 , "w" = 1)) %>% 
  select(- variable)

salah = salah %>% left_join(games, by = c("startdate" = "date"))

# Analysis 1 --------------------------------------------------------------
#For games that Liverpool won, compare responses in the day after with those in the day before
salah1 = salah %>% filter(result == 1)

mod1 = lm_robust(pc_outcome ~ after, weights = weight, data = salah1)
mod2 = lm_robust(muslims_bin ~ after, weights = weight, data = salah1)
mod3 = lm_robust(islam_bin ~ after, weights = weight, data = salah1)
mod4 = lm_robust(imm_bin ~ after, weights = weight, data = salah1)

texreg(list(mod1, mod2, mod3, mod4), include.ci = F, custom.model.names = c("PC Outcome", "Muslims Common", "Islam Compatible", "Immigrant Impact"), 
       custom.coef.names = c("Constant", "Day After\nVictory"), 
       caption = "Relationship between victory and outcomes. This compares responses on the day after a Liverpool victory 
       with responses on the day before a Liverpool victory).", 
       file = "tables/fb_victory1.tex", fontsize = "footnotesize", float.pos = "H",
       label = "tab:fb_victory1")


# Analysis 2 --------------------------------------------------------------
#Compare resopnses in the day after a Liverpool victory with all other responses
salah2 = salah %>% mutate(after = replace_na(after, 0))

mod1 = lm_robust(pc_outcome ~ after, weights = weight, data = salah2)
mod2 = lm_robust(muslims_bin ~ after, weights = weight, data = salah2)
mod3 = lm_robust(islam_bin ~ after, weights = weight, data = salah2)
mod4 = lm_robust(imm_bin ~ after, weights = weight, data = salah2)

texreg(list(mod1, mod2, mod3, mod4), include.ci = F, custom.model.names = c("PC Outcome", "Muslims Common", "Islam Compatible", "Immigrant Impact"), 
       custom.coef.names = c("Constant", "Day After\nVictory"), 
       caption = "Relationship between victory and outcomes. This compares responses on the day after a Liverpool victory 
       with responses on all other days.", 
       file = "tables/fb_victory2.tex", fontsize = "footnotesize", float.pos = "H",
       label = "tab:fb_victory2")




# Analysis 3 --------------------------------------------------------------
#Compare responses in the day after Salah scored to responses in the day before he scored
salah3 = salah %>% filter(salah_scored == 1)

mod1 = lm_robust(pc_outcome ~ after, weights = weight, data = salah3)
mod2 = lm_robust(muslims_bin ~ after, weights = weight, data = salah3)
mod3 = lm_robust(islam_bin ~ after, weights = weight, data = salah3)
mod4 = lm_robust(imm_bin ~ after, weights = weight, data = salah3)

texreg(list(mod1, mod2, mod3, mod4), include.ci = F, custom.model.names = c("PC Outcome", "Muslims Common", "Islam Compatible", "Immigrant Impact"), 
       custom.coef.names = c("Constant", "Day After\nSalah Scores"), 
       caption = "Relationship between victory and outcomes. This compares responses on the day after Salah scored 
       with responses on the day before Salah scored.", 
       file = "tables/fb_scored1.tex", fontsize = "footnotesize", float.pos = "H",
       label = "tab:fb_scored1")





# Analysis 4 --------------------------------------------------------------
#Compare responses in the day after Salah scored with all other responses
salah4 = salah %>% mutate(salah_scored = replace_na(salah_scored, 0))

mod1 = lm_robust(pc_outcome ~ after, weights = weight, data = salah4)
mod2 = lm_robust(muslims_bin ~ after, weights = weight, data = salah4)
mod3 = lm_robust(islam_bin ~ after, weights = weight, data = salah4)
mod4 = lm_robust(imm_bin ~ after, weights = weight, data = salah4)

texreg(list(mod1, mod2, mod3, mod4), include.ci = F, custom.model.names = c("PC Outcome", "Muslims Common", "Islam Compatible", "Immigrant Impact"), 
       custom.coef.names = c("Constant", "Day After\nSalah Scores"), 
       caption = "Relationship between victory and outcomes. This compares responses on the day after Salah scored 
       with responses on all other days.", 
       file = "tables/fb_scored2.tex", fontsize = "footnotesize", float.pos = "H",
       label = "tab:fb_scored2")

