# - Inputs survey_clean.rds
# - Runs the treatment check and the ATE regressions reported in the appendix
# - Outputs regression tables and figures for ATE that are shown in the appendix

library(tidyverse)
library(estimatr)
library(texreg)
library(ggthemes)


salah = readRDS("data/survey_clean.rds")

# Treatment Check ---------------------------------------------------------
salah = salah %>% mutate(salah_perform = salah_perform_12,
                         salah_perform = ifelse(salah_perform < 0, NA, salah_perform))


mod = lm_robust(salah_perform ~ treatment_success_fail, weights = weight, data = salah) %>% tidy
mod = mod %>% filter(term != "(Intercept)") %>% mutate(term = str_remove_all(term, "treatment_success_fail"))

mod %>% 
  ggplot(aes(x = term, y = estimate, ymin = conf.low, ymax = conf.high)) + 
  geom_pointrange() + 
  labs(x = "Treatment", y = "Estimate", subtitle = "Treatment Effect on Views \nof Salah's Performance") + 
  geom_hline(yintercept = 0, linetype = 2) + 
  theme_bw() + 
  scale_colour_hc() + 
  coord_flip()

ggsave("figs/manipulation_check.pdf", width = 4, height = 4)


salah$salah_perform_bin = ifelse(salah$salah_perform > 8, 1, ifelse(salah$salah_perform <= 8, 0, NA))


# Main regressions --------------------------------------------------------
mod1 = lm_robust(pc_outcome ~ treatment, weights = weight, data = salah)
mod2 = lm_robust(muslims_bin ~ treatment, weights = weight, data = salah)
mod3 = lm_robust(islam_bin ~ treatment, weights = weight, data = salah)
mod4 = lm_robust(imm_bin ~ treatment, weights = weight, data = salah)
0.061134460/sd(salah$islam_bin, na.rm = T)
0.043170912/sd(salah$islam_bin, na.rm = T)

texreg(list(mod1, mod2, mod3, mod4), include.ci = F, custom.model.names = c("PC Outcome", "Muslims Common", "Islam Compatible", "Immigrant Impact"), 
       custom.coef.names = c("Constant", "Character - Failure", "Character - Success", "Religion - Failure", "Religion - Success"), 
       caption = "Average treatment effects for the main outcomes.", file = "tables/fb_reg1.tex", 
       fontsize = "footnotesize", float.pos = "H",
       label = "tab:ate_main")

df_plot = tidy(mod1) %>% 
  bind_rows(tidy(mod2)) %>% 
  bind_rows(tidy(mod3)) %>% 
  bind_rows(tidy(mod4)) %>% 
  filter(term != "(Intercept)") %>% 
  mutate(term = str_remove(term, "treatment"),
         outcome = recode(outcome, "pc_outcome" = "PC Outcome",
                          "muslims_bin" = "Feel common with\n Muslims (0-1)",
                          "islam_bin" = "Islam compatible with\n British values (0-1)",
                          "imm_bin" = "Impact of Immigrants\n on the U.K. (0-1)"))

df_plot %>% 
  ggplot(aes(x=term, y=estimate, ymin=conf.low, ymax=conf.high)) + 
  geom_pointrange() + 
  facet_wrap(~outcome) + 
  theme_bw() + 
  geom_hline(yintercept = 0, linetype = 2) +
  labs(y = "Estimate", x = "Treatment") +
  coord_flip() + 
  scale_color_hc()
ggsave("figs/coef_plot_main.pdf", width = 6, height = 6)
salah$islam_bin[salah$treatment=="Pure Control"] %>% summary

# Lin regressions ---------------------------------------------------------
mod1 = lm_lin(pc_outcome ~ treatment, covariates = ~ age + female + univ, weights = weight, data = salah)
mod2 = lm_lin(muslims_bin ~ treatment, covariates = ~ age + female + univ, weights = weight, data = salah)
mod3 = lm_lin(islam_bin ~ treatment, covariates = ~ age + female + univ, weights = weight, data = salah)
mod4 = lm_lin(imm_bin ~ treatment, covariates = ~ age + female + univ, weights = weight, data = salah)

names = names(mod1$coefficients)

names = names %>% str_replace_all("treatment", "") %>% str_replace_all("age_c", "Age") %>% str_replace_all("univ_c", "Univ. Edu.") %>% 
  str_replace_all("female_c", "Female")

texreg(list(mod1, mod2, mod3, mod4), include.ci = F, custom.model.names = c("PC Outcome", "Muslims Common", "Islam Compatible", "Immigrant Impact"), 
       custom.coef.names = names, caption = "Lin regressions for the main outcomes.", file = "tables/fb_reg2.tex", fontsize = "scriptsize", float.pos = "H",
       label = "tab:ate_lin")

# Salah fav. vs not  ------------------------------------------------------
mod1 = lm_robust(pc_outcome ~ treatment * salah_fav, weights = weight, data = salah)
mod2 = lm_robust(muslims_bin ~ treatment * salah_fav, weights = weight, data = salah)
mod3 = lm_robust(islam_bin ~ treatment * salah_fav, weights = weight, data = salah)
mod4 = lm_robust(imm_bin ~ treatment * salah_fav, weights = weight, data = salah)

names = names(mod1$coefficients) %>% str_replace_all("treatment", "") %>% str_replace_all("salah_fav", "Salah Fav.")

texreg(list(mod1, mod2, mod3, mod4), include.ci = F, custom.model.names = c("PC Outcome", "Muslims Common", "Islam Compatible", "Immigrant Impact"), 
       custom.coef.names = names, label = "tab:ate_salah_fav",
       caption = "Interacting the treatments with selecting Salah as the favorite player.", file = "tables/fb_reg3.tex", fontsize = "footnotesize", float.pos = "H")



# Karius criticize vs not  ------------------------------------------------------
mod1 = lm_robust(pc_outcome ~ treatment * karius_empathy, weights = weight, data = salah)
mod2 = lm_robust(muslims_bin ~ treatment * karius_empathy, weights = weight, data = salah)
mod3 = lm_robust(islam_bin ~ treatment * karius_empathy, weights = weight, data = salah)
mod4 = lm_robust(imm_bin ~ treatment * karius_empathy, weights = weight, data = salah)

names = names(mod1$coefficients) %>% str_replace_all("treatment", "") %>% str_replace_all("karius_empathy", "Karius Empathy")

texreg(list(mod1, mod2, mod3, mod4), include.ci = F, custom.model.names = c("PC Outcome", "Muslims Common", "Islam Compatible", "Immigrant Impact"), 
       custom.coef.names = names, label = "tab:ate_karius",
       caption = "Interacting the treatments with expressing sympathy with Karius. Respondents are coded as empathetic 
       with Karius if they did not agree with the criticism that Karius received after following the 2018 Champions 
       League final.", file = "tables/ate_karius.tex", fontsize = "footnotesize", float.pos = "H")


# Conservatives --------------------------------------------------------
mod1 = lm_robust(pc_outcome ~ treatment * conservative, weights = weight, data = salah)
mod2 = lm_robust(muslims_bin ~ treatment * conservative, weights = weight, data = salah)
mod3 = lm_robust(islam_bin ~ treatment * conservative, weights = weight, data = salah)
mod4 = lm_robust(imm_bin ~ treatment * conservative, weights = weight, data = salah)

names = names(mod1$coefficients) %>% str_replace_all("treatment", "") %>% str_replace_all("conservative", "Conservative")

texreg(list(mod1, mod2, mod3, mod4), include.ci = F, custom.model.names = c("PC Outcome", "Muslims Common", "Islam Compatible", "Immigrant Impact"), 
       custom.coef.names = names, label = "tab:ate_conservative",
       caption = "Interacting the treatments with an indicator for conservative views. This indicator is coded as 
       1 if the respondent identifies with the Conservative Party or the UK Independence Party. It is coded as 0 
       if the respondent identifies with the Labour Party, Liberal Democrats, other parties, or none of these 
       parties.", file = "tables/fb_reg4.tex", fontsize = "footnotesize", float.pos = "H")


# Live in Liverpool -------------------------------------------------------
mod1 = lm_robust(pc_outcome ~ treatment * liverpool_res, weights = weight, data = salah)
mod2 = lm_robust(muslims_bin ~ treatment * liverpool_res, weights = weight, data = salah)
mod3 = lm_robust(islam_bin ~ treatment * liverpool_res, weights = weight, data = salah)
mod4 = lm_robust(imm_bin ~ treatment * liverpool_res, weights = weight, data = salah)

names = names(mod1$coefficients) %>% str_replace_all("treatment", "") %>% str_replace_all("liverpool_res", "Liverpool Res.")

texreg(list(mod1, mod2, mod3, mod4), include.ci = F, custom.model.names = c("PC Outcome", "Muslims Common", "Islam Compatible", "Immigrant Impact"), 
       custom.coef.names = names, label = "tab:ate_residence",
       caption = "Interacting the treatments with an indicator for residing in Liverpool.", 
       file = "tables/fb_reg5.tex", fontsize = "footnotesize", float.pos = "H")


# Follow Liverpool -------------------------------------------------------
mod1 = lm_robust(pc_outcome ~ treatment * follow, weights = weight, data = salah)
mod2 = lm_robust(muslims_bin ~ treatment * follow, weights = weight, data = salah)
mod3 = lm_robust(islam_bin ~ treatment * follow, weights = weight, data = salah)
mod4 = lm_robust(imm_bin ~ treatment * follow, weights = weight, data = salah)

names = names(mod1$coefficients) %>% str_replace_all("treatment", "") %>% str_replace_all("follow", "Follow Liverpool")

texreg(list(mod1, mod2, mod3, mod4), include.ci = F, custom.model.names = c("PC Outcome", "Muslims Common", "Islam Compatible", "Immigrant Impact"), 
       custom.coef.names = names, label = "tab:ate_follow",
       caption = "Interacting the treatments with an indicator for closely following Liverpool FC. People who follow 
       Liverpool very closely (watch every match, read news almost daily) are coded as 1 and people who follow Liverpool 
       less often are coded as 0.", 
       file = "tables/fb_reg6.tex", fontsize = "footnotesize", float.pos = "H")
