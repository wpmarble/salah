# - Inputs survey_clean.rds 
# - Conducts regressions and outputs tables and
# figures for AMCE for the religion/character treatments that are shown in the
# appendix



library(tidyverse)
library(estimatr)
library(texreg)
library(ggthemes)

salah = readRDS("data/survey_clean.rds")



# heterogeneous effects presented differently -----------------------------
#liverpool residence
mod1 = lh_robust(islam_bin ~ liverpool_res * treatment_niceguy_relid, weights = weight, data = salah, linear_hypothesis = "treatment_niceguy_relidReligion + liverpool_res:treatment_niceguy_relidReligion = 0")

#Salah favorite
mod2 = lh_robust(islam_bin ~ salah_fav * treatment_niceguy_relid, weights = weight, data = salah, linear_hypothesis = "treatment_niceguy_relidReligion + salah_fav:treatment_niceguy_relidReligion = 0")

#Karius empathy
mod3 = lh_robust(islam_bin ~ karius_empathy * treatment_niceguy_relid, weights = weight, data = salah, linear_hypothesis = "treatment_niceguy_relidReligion + karius_empathy:treatment_niceguy_relidReligion = 0")

df_plot = tidy(mod1) %>% 
  bind_rows(tidy(mod2)) %>% 
  bind_rows(tidy(mod3)) %>% 
  filter(str_detect(term, "treatment_niceguy_relidReligion"),
         !term %in% c("liverpool_res:treatment_niceguy_relidReligion", "salah_fav:treatment_niceguy_relidReligion",
                      "karius_empathy:treatment_niceguy_relidReligion")) %>% 
  mutate(term2 = c("Not Liverpool \nResident", "Liverpool Resident", "Favorite player \n not Salah",
                   "Favorite player \n Salah", "No Karius \nempathy", "Karius empathy"),
         group = c(1, 1, 2, 2, 3, 3))

salah$islam_bin[salah$salah_perform_bin==1 & salah$treatment == "Pure Control"] %>% summary(na.rm = T)
salah$islam_bin[salah$salah_perform_bin==1 & str_detect(salah$treatment, "Religion")] %>% summary(na.rm = T)

p1 = df_plot %>% 
  filter(group == 1) %>% 
  ggplot(aes(x=term2, y=estimate, ymin=conf.low, ymax=conf.high)) + 
  geom_pointrange() + 
  theme_bw() + 
  theme(legend.position = "none") + 
  geom_hline(yintercept = 0, linetype = 2) +
  labs(x = "", y = "") +
  coord_flip(ylim = c(-0.05, 0.2)) + 
  scale_color_few()

p2 = df_plot %>% 
  filter(group == 2) %>% 
  ggplot(aes(x=term2, y=estimate, ymin=conf.low, ymax=conf.high)) + 
  geom_pointrange() + 
  theme_bw() + 
  theme(legend.position = "none") + 
  geom_hline(yintercept = 0, linetype = 2) +
  labs(x = "", y = "") +
  coord_flip(ylim = c(-0.05, 0.2)) + 
  scale_color_few()

p3 = df_plot %>% 
  filter(group == 3) %>% 
  ggplot(aes(x=term2, y=estimate, ymin=conf.low, ymax=conf.high)) + 
  geom_pointrange() + 
  theme_bw() + 
  theme(legend.position = "none") + 
  geom_hline(yintercept = 0, linetype = 2) +
  labs(x = "", y = "") +
  coord_flip(ylim = c(-0.05, 0.2)) + 
  scale_color_few()

p1
p2
p3

ggsave(filename = "figures/liverpool_res.pdf", plot = p1, height = 3, width = 4)
ggsave(filename = "figures/salah_fav.pdf", plot = p2, height = 3, width = 4)
ggsave(filename = "figures/karius_empathy.pdf", plot = p3, height = 3, width = 4)



# Main regressions: Religion/Character --------------------------------------------------------
mod1 = lm_robust(pc_outcome ~ treatment_niceguy_relid, weights = weight, data = salah)
mod2 = lm_robust(muslims_bin ~ treatment_niceguy_relid, weights = weight, data = salah)
mod3 = lm_robust(islam_bin ~ treatment_niceguy_relid, weights = weight, data = salah)
mod4 = lm_robust(imm_bin ~ treatment_niceguy_relid, weights = weight, data = salah)
0.079320775/sd(salah$pc_outcome, na.rm = T)
0.052119631/sd(salah$islam_bin, na.rm = T)

texreg(list(mod1, mod2, mod3, mod4), include.ci = F, custom.model.names = c("PC Outcome", "Muslims Common", "Islam Compatible", "Immigrant Impact"), 
       custom.coef.names = c("Constant", "Character", "Religion"), label = "tab:rlgn_main",
       caption = "Main regressions using the character/religion treatments.", 
       file = "tables/fb_reg7.tex", fontsize = "footnotesize", float.pos = "H")


df_plot = tidy(mod1) %>% 
  bind_rows(tidy(mod2)) %>% 
  bind_rows(tidy(mod3)) %>% 
  bind_rows(tidy(mod4)) %>% 
  filter(term != "(Intercept)") %>% 
  mutate(term = str_remove(term, "treatment_niceguy_relid"),
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
ggsave("figures/coef_plot_amce1.pdf", width = 6, height = 6)


# karius ------------------------------------------------------------------
mod1 = lm_robust(pc_outcome ~ treatment_niceguy_relid * karius_empathy, weights = weight, data = salah)
mod2 = lm_robust(muslims_bin ~ treatment_niceguy_relid * karius_empathy, weights = weight, data = salah)
mod3 = lm_robust(islam_bin ~ treatment_niceguy_relid * karius_empathy, weights = weight, data = salah)
mod4 = lm_robust(imm_bin ~ treatment_niceguy_relid * karius_empathy, weights = weight, data = salah)

names = names(mod1$coefficients) %>% str_replace_all("treatment_niceguy_relid", "") %>% str_replace_all("karius_empathy", "Karius Empathy")

texreg(list(mod1, mod2, mod3, mod4), include.ci = F, custom.model.names = c("PC Outcome", "Muslims Common", "Islam Compatible", "Immigrant Impact"), 
       custom.coef.names = names, caption = "Interacting the character/religion treatments with an indicator for 
       empathy with Karius. Respondents are coded as empathetic with Karius if they did not agree with the 
       criticism that Karius received after following the 2018 Champions League final.", 
       file = "tables/rlgn_karius.tex", 
       fontsize = "footnotesize", float.pos = "H", label = "tab:rlgn_karius")


# Lin regressions ---------------------------------------------------------
mod1 = lm_lin(pc_outcome ~ treatment_niceguy_relid, covariates = ~ age + female + univ, weights = weight, data = salah)
mod2 = lm_lin(muslims_bin ~ treatment_niceguy_relid, covariates = ~ age + female + univ, weights = weight, data = salah)
mod3 = lm_lin(islam_bin ~ treatment_niceguy_relid, covariates = ~ age + female + univ, weights = weight, data = salah)
mod4 = lm_lin(imm_bin ~ treatment_niceguy_relid, covariates = ~ age + female + univ, weights = weight, data = salah)

names = names(mod1$coefficients)

names = names %>% str_replace_all("treatment_niceguy_relid", "") %>% str_replace_all("age_c", "Age") %>% str_replace_all("univ_c", "Univ. Edu.") %>% 
  str_replace_all("female_c", "Female")

texreg(list(mod1, mod2, mod3, mod4), include.ci = F, custom.model.names = c("PC Outcome", "Muslims Common", "Islam Compatible", "Immigrant Impact"), 
       custom.coef.names = names, caption = "Lin regressions using the character/religion treatments.", 
       file = "tables/fb_reg8.tex", fontsize = "footnotesize", float.pos = "H", label = "tab:rlgn_lin")


# Salah fav. vs not  ------------------------------------------------------
mod1 = lm_robust(pc_outcome ~ treatment_niceguy_relid * salah_fav, weights = weight, data = salah)
mod2 = lm_robust(muslims_bin ~ treatment_niceguy_relid * salah_fav, weights = weight, data = salah)
mod3 = lm_robust(islam_bin ~ treatment_niceguy_relid * salah_fav, weights = weight, data = salah)
mod4 = lm_robust(imm_bin ~ treatment_niceguy_relid * salah_fav, weights = weight, data = salah)

names = names(mod1$coefficients) %>% str_replace_all("treatment_niceguy_relid", "") %>% str_replace_all("salah_fav", "Salah Fav.")

texreg(list(mod1, mod2, mod3, mod4), include.ci = F, custom.model.names = c("PC Outcome", "Muslims Common", "Islam Compatible", "Immigrant Impact"), 
       custom.coef.names = names, label = "tab:rlgn_fav",
       caption = "Interacting the character/religion treatments with selecting Salah as the favorite player.", file = "tables/fb_reg9.tex", fontsize = "footnotesize", 
       float.pos = "H")


# Conservatives --------------------------------------------------------
mod1 = lm_robust(pc_outcome ~ treatment_niceguy_relid * conservative, weights = weight, data = salah)
mod2 = lm_robust(muslims_bin ~ treatment_niceguy_relid * conservative, weights = weight, data = salah)
mod3 = lm_robust(islam_bin ~ treatment_niceguy_relid * conservative, weights = weight, data = salah)
mod4 = lm_robust(imm_bin ~ treatment_niceguy_relid * conservative, weights = weight, data = salah)

names = names(mod1$coefficients) %>% str_replace_all("treatment_niceguy_relid", "") %>% str_replace_all("conservative", "Conservative")

texreg(list(mod1, mod2, mod3, mod4), include.ci = F, custom.model.names = c("PC Outcome", "Muslims Common", "Islam Compatible", "Immigrant Impact"), 
       custom.coef.names = names, label = "tab:rlgn_conservative",
       caption = "Interacting the character/religion treatments with an indicator for conservative views. This indicator is coded as 
       1 if the respondent identifies with the Conservative Party or the UK Independence Party. It is coded as 0 
       if the respondent identifies with the Labour Party, Liberal Democrats, other parties, or none of these 
       parties.", file = "tables/fb_reg10.tex", fontsize = "footnotesize", 
       float.pos = "H")


# Live in Liverpool -------------------------------------------------------
mod1 = lm_robust(pc_outcome ~ treatment_niceguy_relid * liverpool_res, weights = weight, data = salah)
mod2 = lm_robust(muslims_bin ~ treatment_niceguy_relid * liverpool_res, weights = weight, data = salah)
mod3 = lm_robust(islam_bin ~ treatment_niceguy_relid * liverpool_res, weights = weight, data = salah)
mod4 = lm_robust(imm_bin ~ treatment_niceguy_relid * liverpool_res, weights = weight, data = salah)

names = names(mod1$coefficients) %>% str_replace_all("treatment_niceguy_relid", "") %>% str_replace_all("liverpool_res", "Liverpool Res.")

texreg(list(mod1, mod2, mod3, mod4), include.ci = F, custom.model.names = c("PC Outcome", "Muslims Common", "Islam Compatible", "Immigrant Impact"), 
       custom.coef.names = names, label = "tab:rlgn_residence",
       caption = "Interacting the character/religion treatments with an indicator for residing in Liverpool.", 
       file = "tables/fb_reg11.tex", fontsize = "footnotesize", float.pos = "H")


# Follow Liverpool -------------------------------------------------------
mod1 = lm_robust(pc_outcome ~ treatment_niceguy_relid * follow, weights = weight, data = salah)
mod2 = lm_robust(muslims_bin ~ treatment_niceguy_relid * follow, weights = weight, data = salah)
mod3 = lm_robust(islam_bin ~ treatment_niceguy_relid * follow, weights = weight, data = salah)
mod4 = lm_robust(imm_bin ~ treatment_niceguy_relid * follow, weights = weight, data = salah)

names = names(mod1$coefficients) %>% str_replace_all("treatment_niceguy_relid", "") %>% str_replace_all("follow", "Follow Liverpool")

texreg(list(mod1, mod2, mod3, mod4), include.ci = F, custom.model.names = c("PC Outcome", "Muslims Common", "Islam Compatible", "Immigrant Impact"), 
       custom.coef.names = names, label = "tab:rlgn_follow",
       caption = "Interacting the character/religion treatments with an indicator for closely following Liverpool FC. People who follow 
       Liverpool very closely (watch every match, read news almost daily) are coded as 1 and people who follow Liverpool 
       less often are coded as 0.", 
       file = "tables/fb_reg12.tex", fontsize = "footnotesize", float.pos = "H")
