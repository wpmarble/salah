
# - Inputs survey_clean.rds
# - Outputs the main figure in the survey experiment section of the paper


library(tidyverse)
library(estimatr)
library(texreg)
library(ggthemes)

# save greyscale version for final submission? you'll have to change the figure
# path to avoid an error. cmd+f "Final_Submission" to fix that. 
save_bw = TRUE


salah = readRDS("data/survey_clean.rds")

salah = salah %>% filter(!str_detect(treatment, "Character"))

# Main regressions --------------------------------------------------------
mod1 = lm_robust(pc_outcome ~ treatment_niceguy_relid, weights = weight, data = salah)
mod2 = lm_robust(muslims_bin ~ treatment_niceguy_relid, weights = weight, data = salah)
mod3 = lm_robust(islam_bin ~ treatment_niceguy_relid, weights = weight, data = salah)
mod4 = lm_robust(imm_bin ~ treatment_niceguy_relid, weights = weight, data = salah)

texreg(list(mod1, mod2, mod3, mod4))
texreg(list(mod1, mod2, mod3, mod4), include.ci = F, custom.model.names = c("PC Outcome", "Muslims Common", "Islam Compatible", "Immigrant Impact"),
       custom.coef.names = c("Constant", "Religion"), label = "tab:fb_reg_final",
       caption = "Main regressions using the character/religion treatments.",
       file = "tables/fb_reg_final.tex", fontsize = "footnotesize", float.pos = "H")

df_plot = tidy(mod1) %>% 
  bind_rows(tidy(mod2)) %>% 
  bind_rows(tidy(mod3)) %>% 
  bind_rows(tidy(mod4)) %>% 
  filter(term != "(Intercept)") %>% 
  mutate(term = str_remove(term, "treatment_niceguy_relid"),
         outcome = recode(outcome, "pc_outcome" = "PC Outcome",
                          "muslims_bin" = "Feel common with\n Muslims (0-1)",
                          "islam_bin" = "Islam compatible with\n British values (0-1)",
                          "imm_bin" = "Impact of Immigrants\n on the U.K. (0-1)"),
         outcome = factor(outcome, levels = c("PC Outcome", 
                                              "Islam compatible with\n British values (0-1)",
                                              "Feel common with\n Muslims (0-1)",
                                              "Impact of Immigrants\n on the U.K. (0-1)"
                                              )),
         outcome = fct_rev(outcome))

df_plot %>% 
  ggplot(aes(x=outcome, y=estimate, ymin=conf.low, ymax=conf.high)) + 
  geom_pointrange() + 
  theme_bw() + 
  geom_hline(yintercept = 0, linetype = 2) +
  labs(y = "Estimate", x = "Treatment") +
  coord_flip() + 
  scale_color_hc()
ggsave("figs/coef_plot_final.pdf", width = 6, height = 6)

if (save_bw) ggsave("../04drafts/Final_Submission/Fig5.pdf", width = 6, height = 4)

