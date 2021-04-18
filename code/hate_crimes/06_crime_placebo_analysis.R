## Replication file for "Can Exposure to Celebrities Reduce Prejudice" ##

# Implement placebo test using shoplifting and all crimes, using the same
# matrix completion method used in the hate crime analysis. 

# DEPENDS ON RUNNING code/hate_crimes/02_synth_analysis.R first


library(ggplot2)
library(dplyr)
library(gsynth)
library(lubridate)

# save greyscale version for final submission? you'll have to change the figure
# path to avoid an error. cmd+f "Final_Submission" to fix that. 
save_bw = TRUE

set.seed(1584206) # random.org

theme_set(theme_bw())

# load data 
dat = readRDS("data/crime-archive/cleaned_all_crime.rds")

# create treatment indicator for merseyside post-July 2017
dat = dat %>% 
  mutate(treated = ifelse(county == "merseyside" & month >= as.Date("2017-07-01"), 1, 0))

# Filter out the British Transit Police, which has non-geographic jurisdiction
# so it's hard to say what the per capita rate is. 
dat = subset(dat, !is.na(crimes_pc))

# subset just to data that's used in the hate crime analysis
dat = subset(dat, hate_crime_sample == 1)

# Limit sample to match hate crime coverage
estdat = subset(dat, hate_crime_sample==1)

# Split into a list of df's for the different types of crimes
split_dfs = estdat %>% 
  filter(crime.type != "All") %>% 
  group_by(crime.type) %>% 
  group_split()

# number of months observed for merseyside before and after treatment
print(n_merseyside <- split_dfs[[1]] %>% filter(county=="merseyside") %>% group_by(treated) %>% summarise(n=n()))

# save number of pre-treatment observations. this is how many folds we'll use for
# training, so that it's LOO-CV. 
n_folds = n_merseyside$n[n_merseyside$treated == 0]





# Estimate matrix completion for each crime type --------------------------

# Estimate ATT for each different type of crime using the same matrix completion
# method as the hate crime analysis. Calculate the effect estimates 
# normalized by the pre-treatment mean for merseyside, then compare the hate
# crime estimate to those from all the other types of crime. 

mc_results = list()
i = 1
for (d in split_dfs){
  
  crimetype = d$crime.type[1]
  
  mod = gsynth(crimes_pc ~ treated, 
               data = d,
               k = n_folds,
               index = c("county", "month"),
               estimator = "mc", 
               lambda = seq(0, 10, length.out = 50), 
               se = FALSE, 
               CV = TRUE, 
               force = "two-way", 
               seed = 167, # taken from random.org
               parallel = TRUE, 
               cores = 3)
  
  mod[["crime.type"]] = crimetype
  mc_results[[i]] = mod
  i = i + 1
}

# load hate crime results from 02code/hate_crimes/02_synth_analysis.R
hc_results = readRDS("data/hate-crimes/matrix_completion_normalized_outcomes.rds")
hc_results[["crime.type"]] = "Hate crimes"
mc_results[[(length(mc_results) + 1)]] = hc_results

atts = lapply(mc_results, function(x){
  atts = x$att
  y = x$Y.dat[, "merseyside"]
  imp = x$Y.ct[, "merseyside"]
  date = as.Date(names(atts))
  crimetype = x$crime.type
  
  out = data.frame(y.true = y,
                   y.imputed = imp,
                   att.est = atts,
                   month = date, 
                   crime.type = crimetype,
                   stringsAsFactors = FALSE, row.names = NULL)
  out$month2 = 1:nrow(out)
  out$month2 = out$month2 - which(out$month == as.Date("2017-06-01"))
  
  # calculate att's as a percent of the pre-treatment mean
  out$pretreatment_mean = mean(out$y.true[out$month2 <= 0])
  out$counterfactual_mean = mean(out$y.imputed[out$month2 > 0])
  
  return(out)
})

atts = bind_rows(atts) %>% 
  mutate(att.est.pct = 100*(att.est / pretreatment_mean),
         att.est.imp.pct = 100*(att.est / counterfactual_mean),
         post_treatment = as.numeric(month2 > 0),
         hatecrime = crime.type == "Hate crimes")




# Summarize and plot ------------------------------------------------------

## FIGURE 3 ## 

# summarize ATTs as % of pre-treatment mean, by crime type, averaging across
# post-treatment periods
att_sum = atts %>% 
  filter(post_treatment == 1) %>% 
  group_by(crime.type) %>% 
  summarise(att.mean = mean(att.est.pct)) %>% 
  arrange(att.mean)
print(att_sum)

# plot results 
g = ggplot(atts) + 
  aes(x = month2, y = att.est.pct, group = crime.type, colour = hatecrime) + 
  annotate(geom="rect", xmin = 0, xmax = Inf, ymin = -Inf, ymax = Inf, alpha = .3) + 
  geom_hline(yintercept = 0, lty = 3) +
  geom_line() + 
  scale_colour_manual(values = c("#99999950", "#fb0d1b"), 
                      label = c("Other crime categories", "Hate crimes"), 
                      name = NULL) + 
  scale_y_continuous(breaks = seq(-40, 40, 20),
                     label = function(x) paste0(x, "%")) + 
  coord_cartesian(ylim = c(-45, 45)) + 
  guides(colour = guide_legend(nrow=1, rev = TRUE)) + 
  labs(x = "Months relative to treatment",
       y = "Observed - Imputed\nRelative to pre-treatment baseline") + 
  theme(legend.position = "bottom")
ggsave(g, filename = "figs/all_crime_types.pdf", width=6,height=4)
  

if (save_bw) {
  ggsave(
    g + 
      scale_colour_manual(values = c("#99999950", "black"), 
                          label = c("Other crime categories", "Hate crimes"), 
                          name = NULL) ,
    filename = "../04drafts/Final_Submission/Fig3.pdf",
    width=6,height=4
  )
}



# Run matrix completion on all crime outcome ------------------------------

all_crimes = subset(estdat, crime.type == "All")

all_crimes_mc = gsynth(crimes_pc ~ treated, 
                       data = all_crimes,
                       k = n_folds,
                       index = c("county", "month"),
                       estimator = "mc", 
                       lambda = seq(0, 20, length.out = 50), 
                       se = TRUE, nboots = 1.5e4, 
                       CV = TRUE, 
                       force = "two-way", 
                       seed = 167, # taken from random.org
                       parallel = TRUE, 
                       cores = 3)


# Plot matrix completion results - counterfactuals
plot(all_crimes_mc, type = "ct") + 
  theme(axis.text.x = element_text(angle = 90, hjust=1, vjust = .5),
        panel.grid.minor = element_blank(),
        legend.position = "bottom") +
  ggtitle(NULL) +
  labs(x = NULL, y = "Monthly total crime rate") 
ggsave("figs/all_crimes.pdf", width=6,height=4)

## Extract ATT estimates
all_crimes_mc$est.att
all_crimes_mc$est.avg


