
# Conducts regressions and outputs tables and figures for AMCE for the
# success/failure treatments that are shown in the appendix


library(tidyverse)
library(estimatr)
library(texreg)
library(ggthemes)

salah = readRDS("data/survey_clean.rds")


# Main regressions: Success/Fail --------------------------------------------------------
mod1 = lm_robust(pc_outcome ~ treatment_success_fail, weights = weight, data = salah)
mod2 = lm_robust(muslims_bin ~ treatment_success_fail, weights = weight, data = salah)
mod3 = lm_robust(islam_bin ~ treatment_success_fail, weights = weight, data = salah)
mod4 = lm_robust(imm_bin ~ treatment_success_fail, weights = weight, data = salah)
0.02932764/sd(salah$islam_bin, na.rm = T)

texreg(list(mod1, mod2, mod3, mod4), include.ci = F, custom.model.names = c("PC Outcome", "Muslims Common", "Islam Compatible", "Immigrant Impact"), 
       custom.coef.names = c("Constant", "Failure", "Success"), label = "tab:success_main",
       caption = "Main regressions using the success/failure treatments.", 
       file = "tables/fb_reg13.tex", fontsize = "footnotesize", float.pos = "H")

df_plot = tidy(mod1) %>% 
  bind_rows(tidy(mod2)) %>% 
  bind_rows(tidy(mod3)) %>% 
  bind_rows(tidy(mod4)) %>% 
  filter(term != "(Intercept)") %>% 
  mutate(term = str_remove(term, "treatment_success_fail"),
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
ggsave("figs/coef_plot_amce2.pdf", width = 6, height = 6)

# karius ------------------------------------------------------------------
mod1 = lm_robust(pc_outcome ~ treatment_niceguy_relid * karius_empathy, weights = weight, data = salah)
mod2 = lm_robust(muslims_bin ~ treatment_niceguy_relid * karius_empathy, weights = weight, data = salah)
mod3 = lm_robust(islam_bin ~ treatment_niceguy_relid * karius_empathy, weights = weight, data = salah)
mod4 = lm_robust(imm_bin ~ treatment_niceguy_relid * karius_empathy, weights = weight, data = salah)

names = names(mod1$coefficients) %>% str_replace_all("treatment_niceguy_relid", "") %>% str_replace_all("karius_empathy", "Karius Empathy")

texreg(list(mod1, mod2, mod3, mod4), include.ci = F, custom.model.names = c("PC Outcome", "Muslims Common", "Islam Compatible", "Immigrant Impact"), 
       custom.coef.names = names, caption = "Interacting the success/failure treatments with an indicator for 
       empathy with Karius. Respondents are coded as empathetic with Karius if they did not agree with the 
       criticism that Karius received after following the 2018 Champions League final.", 
       file = "tables/success_karius.tex", 
       fontsize = "footnotesize", float.pos = "H", label = "tab:success_karius")


# Lin regressions ---------------------------------------------------------
mod1 = lm_lin(pc_outcome ~ treatment_success_fail, covariates = ~ age + female + univ, weights = weight, data = salah)
mod2 = lm_lin(muslims_bin ~ treatment_success_fail, covariates = ~ age + female + univ, weights = weight, data = salah)
mod3 = lm_lin(islam_bin ~ treatment_success_fail, covariates = ~ age + female + univ, weights = weight, data = salah)
mod4 = lm_lin(imm_bin ~ treatment_success_fail, covariates = ~ age + female + univ, weights = weight, data = salah)

names = names(mod1$coefficients)

names = names %>% str_replace_all("treatment_success_fail", "") %>% str_replace_all("age_c", "Age") %>% str_replace_all("univ_c", "Univ. Edu.") %>% 
  str_replace_all("female_c", "Female")

texreg(list(mod1, mod2, mod3, mod4), include.ci = F, custom.model.names = c("PC Outcome", "Muslims Common", "Islam Compatible", "Immigrant Impact"), 
       custom.coef.names = names, caption = "Lin regressions using the success/failure treatments.", 
       file = "tables/fb_reg14.tex", fontsize = "footnotesize", 
       float.pos = "H", label = "tab:success_lin")


# Salah fav. vs not  ------------------------------------------------------
mod1 = lm_robust(pc_outcome ~ treatment_success_fail * salah_fav, weights = weight, data = salah)
mod2 = lm_robust(muslims_bin ~ treatment_success_fail * salah_fav, weights = weight, data = salah)
mod3 = lm_robust(islam_bin ~ treatment_success_fail * salah_fav, weights = weight, data = salah)
mod4 = lm_robust(imm_bin ~ treatment_success_fail * salah_fav, weights = weight, data = salah)

names = names(mod1$coefficients) %>% str_replace_all("treatment_success_fail", "") %>% str_replace_all("salah_fav", "Salah Fav.")

texreg(list(mod1, mod2, mod3, mod4), include.ci = F, custom.model.names = c("PC Outcome", "Muslims Common", "Islam Compatible", "Immigrant Impact"), 
       custom.coef.names = names, label = "tab:success_fav",
       caption = "Interacting the success/failure treatments with selecting Salah as the favorite player.", 
       file = "tables/fb_reg15.tex", fontsize = "footnotesize", float.pos = "H")


# Conservatives --------------------------------------------------------
mod1 = lm_robust(pc_outcome ~ treatment_success_fail * conservative, weights = weight, data = salah)
mod2 = lm_robust(muslims_bin ~ treatment_success_fail * conservative, weights = weight, data = salah)
mod3 = lm_robust(islam_bin ~ treatment_success_fail * conservative, weights = weight, data = salah)
mod4 = lm_robust(imm_bin ~ treatment_success_fail * conservative, weights = weight, data = salah)

names = names(mod1$coefficients) %>% str_replace_all("treatment_success_fail", "") %>% str_replace_all("conservative", "Conservative")

texreg(list(mod1, mod2, mod3, mod4), include.ci = F, custom.model.names = c("PC Outcome", "Muslims Common", "Islam Compatible", "Immigrant Impact"), 
       custom.coef.names = names, label = "tab:success_conservative",
       caption = "Interacting the success/failure treatments with an indicator for conservative views. This indicator is coded as 
       1 if the respondent identifies with the Conservative Party or the UK Independence Party. It is coded as 0 
       if the respondent identifies with the Labour Party, Liberal Democrats, other parties, or none of these 
       parties.", file = "tables/fb_reg16.tex", fontsize = "footnotesize", float.pos = "H")


# Live in Liverpool -------------------------------------------------------
mod1 = lm_robust(pc_outcome ~ treatment_success_fail * liverpool_res, weights = weight, data = salah)
mod2 = lm_robust(muslims_bin ~ treatment_success_fail * liverpool_res, weights = weight, data = salah)
mod3 = lm_robust(islam_bin ~ treatment_success_fail * liverpool_res, weights = weight, data = salah)
mod4 = lm_robust(imm_bin ~ treatment_success_fail * liverpool_res, weights = weight, data = salah)

names = names(mod1$coefficients) %>% str_replace_all("treatment_success_fail", "") %>% str_replace_all("liverpool_res", "Liverpool Res.")

texreg(list(mod1, mod2, mod3, mod4), include.ci = F, custom.model.names = c("PC Outcome", "Muslims Common", "Islam Compatible", "Immigrant Impact"), 
       custom.coef.names = names, label = "tab:success_residence",
       caption = "Interacting the success/failure treatments with an indicator for residing in Liverpool.", 
       file = "tables/fb_reg17.tex", fontsize = "footnotesize", float.pos = "H")


# Follow Liverpool -------------------------------------------------------
mod1 = lm_robust(pc_outcome ~ treatment_success_fail * follow, weights = weight, data = salah)
mod2 = lm_robust(muslims_bin ~ treatment_success_fail * follow, weights = weight, data = salah)
mod3 = lm_robust(islam_bin ~ treatment_success_fail * follow, weights = weight, data = salah)
mod4 = lm_robust(imm_bin ~ treatment_success_fail * follow, weights = weight, data = salah)

names = names(mod1$coefficients) %>% str_replace_all("treatment_success_fail", "") %>% str_replace_all("follow", "Follow Liverpool")

texreg(list(mod1, mod2, mod3, mod4), include.ci = F, custom.model.names = c("PC Outcome", "Muslims Common", "Islam Compatible", "Immigrant Impact"), 
       custom.coef.names = names, label = "tab:success_follow",
       caption = "Interacting the success/failure treatments with an indicator for closely following Liverpool FC. People who follow 
       Liverpool very closely (watch every match, read news almost daily) are coded as 1 and people who follow Liverpool 
       less often are coded as 0.", file = "tables/fb_reg18.tex", fontsize = "footnotesize", 
       float.pos = "H")
