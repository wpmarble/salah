## Replication file for "Can Exposure to Celebrities Reduce Prejudice" ##

# Analyze whether there is a "Mane effect" on hate crimes before Salah
# joined Liverpool. He joined Liverpool in July 2016; truncate outcome
# data at June 2017 (the month before Salah joined). 

library(ggplot2)
library(dplyr)
library(gsynth)
library(panelView)


theme_set(theme_bw())


# load data and fill in some dates
dat = read.csv("data/hate-crimes/merged_hate_crimes.csv", stringsAsFactors = FALSE)
dat$date = as.Date(dat$date)

# generate Mane treatment indicator
dat$treated_mane = 0
dat$treated_mane[dat$county_str == "merseyside" & dat$date >= as.Date("2016-07-01")] = 1


# the gsynth package doesn't handle missing values, so subset down 
estdat = dat %>% 
  filter(!is.na(hate_crimes_pc),
         date < as.Date("2017-07-01"))




# Plot data ---------------------------------------------------------------

panelView(hate_crimes_pc ~ treated_mane, data = dat, index = c("county", "date"), pre.post = TRUE) + 
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 90, hjust=1, vjust = .5),
        legend.position = "bottom") + 
  guides(fill = guide_legend(title = NULL)) + 
  ggtitle(NULL) + labs(x = NULL, y = "County") 
ggsave("figs/panel_structure_mane.pdf", width=6, height=4)

panelView(hate_crimes_pc ~ treated_mane, data = dat, index = c("county", "date"), type = "raw") + 
  theme(axis.text.x = element_text(angle = 90,hjust=1, vjust = .5)) + 
  ggtitle(NULL) + 
  labs(x = NULL, y = "Annualized hate crimes\nper 1,000 residents")
ggsave("figs/panel_outcomes_percap_mane.pdf", width=6, height=4)

panelView(hate_crimes ~ treated_mane, data = dat, index = c("county", "date"), type = "raw") + 
  theme(axis.text.x = element_text(angle = 90, hjust=1, vjust = .5), 
        panel.grid.minor = element_blank()) + 
  ggtitle(NULL) + 
  labs(x = NULL, y = "Hate crimes per month")
ggsave("figs/panel_outcomes_raw_mane.pdf", width=6, height=4)



# Matrix Completion - normalized outcome ----------------------------------

## THIS IS THE METHOD REPORTED IN THE PAPER ##

# Matrix completion method of Athey et al. on per capita outcome

# takes a while to run b/c of the CV and bootstrap
matcomp_pc = gsynth(hate_crimes_pc ~ treated_mane, 
                    data = estdat, index = c("county", "date"),
                    estimator = "mc", 
                    lambda = seq(0, 2, length.out = 50), 
                    se = TRUE, nboots = 1e3, 
                    r = c(0, 5), 
                    CV = TRUE, k = 20,
                    force = "two-way", 
                    seed = 167, # taken from random.org
                    parallel = TRUE, cores = 3)

# Plot matrix completion results - counterfactuals
plot(matcomp_pc, type = "ct") + 
  theme(axis.text.x = element_text(angle = 90, hjust=1, vjust = .5),
        panel.grid.minor = element_blank(),
        legend.position = "bottom") +
  ggtitle(NULL) + 
  labs(x = NULL, y = "Monthly hate crime rate") + 
  scale_y_continuous(breaks = seq(0, 4, .25)) 
ggsave("figs/panel_matrix_completion_pc_mane.pdf",width=6,height=4)

# Plot matrix completion results - ATT
plot(matcomp_pc, shade.post = TRUE) + 
  geom_hline(yintercept = 0, lty = 3) +
  coord_cartesian(ylim = c(-.5, .5)) + 
  scale_y_continuous(breaks = seq(-.5, .5, .25)) + 
  theme_bw() + 
  ggtitle(NULL) + 
  labs(x = "Months relative to treatment", y = "Observed - Imputed for Merseyside")
ggsave(filename = "figs/panel_matrix_completion_pc_att_mane.pdf", width=6, height=4)


## Extract ATT estimates
matcomp_pc$est.att
matcomp_pc$est.avg

## compare to pre-treatment average in merseyside
pretreat_avg = with(subset(estdat, county=="merseyside" & treated_mane == 0), mean(hate_crimes_pc))
print(reduction_pct <- (matcomp_pc$est.avg[,"ATT.avg"] / pretreat_avg) * 100)

# Save estimates in text files for use in latex
cat(round(matcomp_pc$est.avg[, "ATT.avg"], 3), file = "tables/hate_crime_att_avg_mane.tex")
cat(round(matcomp_pc$est.avg[, "S.E."], 3), file = "tables/hate_crime_att_avg_se_mane.tex")
cat(abs(round(reduction_pct, 1)), file = "tables/hate_crime_att_avg_pct_drop_mane.tex")
