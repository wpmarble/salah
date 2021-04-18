# - Inputs survey_clean.rds
# - Produces bar charts for the outcomes by treatment groups 



library(tidyverse)
library(estimatr)
library(texreg)
library(ggthemes)

salah = readRDS("data/survey_clean.rds")


# Across all treatments ---------------------------------------------------
salah2 = salah %>% mutate(outcome = pc_outcome, type = "PC Outcome") %>% select(treatment, outcome, weight, type) %>% 
  bind_rows(salah %>% mutate(outcome = muslims_bin, type = "Feel common with\n Muslims (0-1)") %>% select(treatment, outcome, weight, type)) %>% 
  bind_rows(salah %>% mutate(outcome = islam_bin, type = "Islam compatible with\n British values (0-1)") %>% select(treatment, outcome, weight, type)) %>% 
  bind_rows(salah %>% mutate(outcome = imm_bin, type = "Impact of Immigrants\n on the U.K. (0-1)") %>% select(treatment, outcome, weight, type))

salah2 %>% 
  filter(!is.na(outcome)) %>% 
  ggplot(aes(x = treatment, y = outcome, weights = weight, fill = treatment)) + 
  geom_bar(stat = "summary", fun.y = "mean") + 
  stat_summary(fun.data = mean_se, geom = "errorbar", width = .2) + 
  facet_wrap(~type,scales = "free") + 
  theme_bw() + 
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(),
        legend.position = "bottom", legend.title = element_blank()) + 
  scale_fill_hc()
ggsave("figs/fb_svy_averages.pdf", width = 7, height = 6)

