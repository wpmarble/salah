## Replication file for "Can Celebrities Reduce Prejudice" ##

# Implement the synthetic control method to see whether hate crimes in Liverpool
# declined after Salah joined, relative to what we otherwise would have expected.
# The main outcome is a normalized hate crime rate (expressed in annualized 
# hate crimes per 1k residents). The main analysis uses the matrix completion
# method developed by Athey et al.

library(ggplot2)
library(ggthemes)
library(dplyr)
library(gsynth)
library(panelView)
library(lubridate)
library(tools)


set.seed(874783) # from random.org


# save greyscale version for final submission? you'll have to change the figure
# path to avoid an error. cmd+f "Final_Submission" to fix that. 
save_bw = TRUE

# set wd to replication project directory
# setwd("~/Dropbox/Mo_Salah/replication_files/")


theme_set(theme_bw())



# load data and fill in some dates
dat = read.csv("data/hate-crimes/merged_hate_crimes.csv", stringsAsFactors = FALSE)
dat$date = as.Date(dat$date)

# filter to months in which we have merseyside data
dat = dat %>% filter(date <= as.Date("2018-04-01"))

# filter out cheshire b/c no post-treatment period data
dat = dat %>% filter(county_str!="cheshire")


# the gsynth package doesn't handle missing values, so subset down 
estdat = dat %>% filter(!is.na(hate_crimes_pc))

# save the county-date pairs used for estimation
estdat %>% distinct(county_str, date) %>% saveRDS(file = "data/hate-crimes/estimation_sample.rds")

# summary statistics on sample size
# total number of obs:
nrow(estdat)
cat(nrow(estdat), file = "tables/hatecrimes_n_obs.tex")

# total number of units:
length(unique(estdat$county))
cat(length(unique(estdat$county)), file = "tables/hatecrimes_n_counties.tex")

# number of months observed for merseyside before and after treatment
print(n_merseyside <- estdat %>% filter(county_str=="merseyside") %>% group_by(treated) %>% summarise(n=n()))

# save number of pre-treatment observations. this is how many folds we'll use for
# training, so that it's leave-on-out cross validation. consistent with Abadie et al.
# optimization procedure.
n_folds = n_merseyside$n[n_merseyside$treated == 0]


# get nicer formatted county + fix misspelling
dat = dat %>% 
  mutate(county = toTitleCase(gsub("_|-", " ", county)))
dat$county[dat$county == "West Marcia"] <- "West Mercia"

# Visualize the data ------------------------------------------------------


panelView(hate_crimes_pc ~ treated, data = dat, index = c("county", "date"), pre.post=TRUE) + 
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 90, hjust=1, vjust = .5),
        legend.position = "bottom") + 
  guides(fill = guide_legend(title = NULL)) + 
  # scale_x_continuous(label = function(x) substr(as.character(x), 1, nchar(x) - 3)) +
  ggtitle(NULL) + 
  labs(x = NULL, y = "Police Jurisdiction")
ggsave("figs/panel_structure.pdf", width=6, height=4)

panelView(hate_crimes_pc ~ treated, 
          data = dat, 
          index = c("county", "date"), type = "raw") + 
  theme(axis.text.x = element_text(angle = 90, hjust=1, vjust = .5),
        legend.position = "bottom") + 
  ggtitle(NULL) + 
  labs(x = NULL, y = "Annualized hate crimes\nper 1,000 residents")
ggsave("figs/panel_outcomes_percap.pdf", width=6, height=4)

panelView(hate_crimes ~ treated, data = dat, index = c("county", "date"), type = "raw") + 
  theme(axis.text.x = element_text(angle = 90, hjust=1, vjust = .5), 
        panel.grid.minor = element_blank()) + 
  ggtitle(NULL) + 
  labs(x = NULL, y = "Hate crimes per month")
ggsave("figs/panel_outcomes_raw.pdf", width=6, height=4)



# Summary stats -----------------------------------------------------------



# normalized
dat %>% summarise(mean = mean(hate_crimes_pc,na.rm=TRUE),
                  sd   = sd(hate_crimes_pc,na.rm=TRUE),
                  min  = min(hate_crimes_pc,na.rm=TRUE),
                  max  = max(hate_crimes_pc,na.rm=TRUE))

# raw
dat %>% summarise(mean = mean(hate_crimes,na.rm=TRUE),
                  sd   = sd(hate_crimes,na.rm=TRUE),
                  min  = min(hate_crimes,na.rm=TRUE),
                  max  = max(hate_crimes,na.rm=TRUE))

# is merseyside typical? Rank counties based on hate crimes per capita
# in the pre-treament period. Merseyside has the 20th highest rate
# out of 25 counties.
sum_pre = estdat %>% 
  filter( date < as.Date("2017-07-01")) %>% 
  group_by(county) %>% 
  summarise(mean_hc_pc = mean(hate_crimes_pc),
            mean_hc = mean(hate_crimes)) %>% 
  arrange(mean_hc_pc)
which(sum_pre$county=="merseyside")



# Matrix Completion - normalized outcome ----------------------------------

## THIS IS THE METHOD REPORTED IN THE PAPER ##

# Matrix completion method of Athey et al. on per capita outcome
# use LOO-CV for tuning of penalty parameter. The idea is that 
# each iteration leaves out one of the observations for Merseyside
# in the pre-treatment period. Then compute mean square prediction
# error for each CV iteration. 

# takes a while to run b/c of the CV and bootstrap
matcomp_pc = gsynth(hate_crimes_pc ~ treated, 
                    data = estdat, index = c("county", "date"),
                    estimator = "mc", 
                    lambda = seq(0, 2, length.out = 50), 
                    se = TRUE, nboots = 1.5e4, 
                    CV = TRUE, k = n_folds,
                    force = "two-way", 
                    seed = 167, # taken from random.org
                    parallel = TRUE, cores = 3)
saveRDS(matcomp_pc, file = "data/hate-crimes/matrix_completion_normalized_outcomes.rds")


## FIGURE 2a ##
# Plot matrix completion results - counterfactuals. Color version from preprint.
plot(matcomp_pc, type = "ct") + 
  theme(axis.text.x = element_text(angle = 90, hjust=1, vjust = .5),
        panel.grid.minor = element_blank(),
        legend.position = "bottom") +
  ggtitle(NULL) +
  labs(x = NULL, y = "Monthly hate crime rate") + 
  scale_y_continuous(breaks = seq(0, 4, .25))  
ggsave("figs/panel_matrix_completion_pc.pdf",width=6,height=4)


# Black and white version for publication
if (save_bw){
  
  t_treat = matcomp_pc$T0
  
  plot(matcomp_pc, type = "ct", legendOff = TRUE) + 
    theme(axis.text.x = element_text(hjust = .5, vjust = .5)) +
    ggtitle(NULL) +
    labs(x = "Months relative to treatment", y = "Monthly hate crime rate") + 
    scale_x_continuous(breaks = seq(0, 50, 10), 
                       labels = function(x) x - t_treat,
                       ) + 
    scale_y_continuous(breaks = seq(0, 4, .25))  +
    scale_colour_manual(values = c("black", "black")) + 
    guides(colour = FALSE, lty = FALSE) + 
    annotate(geom = "text", x = 39, y = 1.9, label = "Imputed", size = 6, hjust = 1) + 
    annotate(geom = "text", x = 39, y = 1.08, label = "Observed", size = 6, hjust = 1)
  ggsave("../04drafts/Final_Submission/Fig2a.pdf", width=6,height=3.75)
 

}


# Plot matrix completion results - ATT
plot(matcomp_pc, shade.post = TRUE) + 
  geom_hline(yintercept = 0, lty = 3) + 
  coord_cartesian(ylim = c(-.5, .5)) + 
  scale_y_continuous(breaks = seq(-.5, .5, .25)) + 
  ggtitle(NULL) +
  labs(x = "Months relative to treatment", y = "Observed - Imputed Hate Crime Rate in Merseyside")
ggsave(filename = "figs/panel_matrix_completion_pc_att.pdf", width=6, height=4)



# make ATT plot with confidence intervals
ests = data.frame(matcomp_pc$est.att)
ests$month = 1:nrow(ests)
ests$month = ests$month - ests$month[31]
ests$CI.lower[!ests$n.Treated] = NA
ests$CI.upper[!ests$n.Treated] = NA
ggplot(ests) + 
  aes(x = month,y = ATT, ymin = CI.lower, ymax = CI.upper) + 
  annotate(geom="rect", xmin = 0, xmax = Inf, ymin = -Inf, ymax = Inf, alpha = .3) + 
  geom_hline(yintercept = 0, lty = 3) + 
  geom_ribbon(alpha = .5) + 
  geom_line(lwd=1.25) + 
  coord_cartesian(ylim = c(-.5, .5)) + 
  scale_y_continuous(breaks = seq(-.5, .5, .25)) + 
  labs(x = "Months relative to treatment", y = "Observed - Imputed Hate Crime Rate in Merseyside")
ggsave("figs/panel_matrix_completion_pc_att_ci.pdf", width=6,height=4)


## Extract ATT estimates
matcomp_pc$est.att
matcomp_pc$est.avg

## compare to pre-treatment average in merseyside
pretreat_avg = with(subset(estdat, county=="merseyside" & treated == 0), mean(hate_crimes_pc))
print(reduction_pct_pre <- (matcomp_pc$est.avg[,"ATT.avg"] / pretreat_avg) * 100)

## compare to post-treatment imputed average -- use this in the paper
## CHANGED after first draft, which used pre-treatment average
imputed_avg = mean(matcomp_pc$Y.ct[matcomp_pc$time >= as.Date("2017-07-01")])
print(reduction_pct_impu <- (matcomp_pc$est.avg[, "ATT.avg"] / imputed_avg) * 100)



# Save estimates in text files for use in latex
cat(round(matcomp_pc$est.avg[, "ATT.avg"], 3), file = "tables/hate_crime_att_avg.tex")
cat(round(matcomp_pc$est.avg[, "S.E."], 3), file = "tables/hate_crime_att_avg_se.tex")
cat(round(matcomp_pc$est.avg[, c("CI.lower", "CI.upper")], 3), sep=",", file = "tables/hate_crime_att_avg_ci.tex")
cat(abs(round(reduction_pct_impu, 0)), file = "tables/hate_crime_att_avg_pct_drop.tex")




# Re-run without London and Manchester ------------------------------------

# There were terrorist attacks slightly before Salah was signed in London
# and Manchester, suggesting that they might not be good control units. 
# Take them out and re-estimate the model as a robustness check.

no_lon_man = subset(estdat, !county %in% c("london", "manchester"))

no_lon_man_mod = gsynth(hate_crimes_pc ~ treated, 
                        data = no_lon_man, index = c("county", "date"),
                        estimator = "mc", 
                        lambda = seq(0, 2, length.out = 50), 
                        se = TRUE, nboots = 1.5e4 ,
                        CV = TRUE, k = n_folds,
                        force = "two-way", 
                        seed = 167, # taken from random.org
                        parallel = TRUE, cores = 3)
print(no_lon_man_mod)
plot(no_lon_man_mod, "ct")


# Plot the estimated treatment effect in every period against each other
# for the full data and the model w/o London and Manchester
comparison = data.frame(trimmed = no_lon_man_mod$att, orig = matcomp_pc$att)
comparison$date = as.Date(rownames(comparison))
comparison$post_treatment = comparison$date >= as.Date("2017-07-01")
cor(comparison[, 1:2])

ggplot(comparison) + 
  aes(x = orig, y = trimmed, colour = post_treatment) + 
  geom_abline(slope = 1, intercept = 0, lty = 3) + 
  geom_point() + 
  scale_colour_hc(name = NULL, label = c("Pre-treatment", "Post-treatment")) + 
  theme(legend.position="bottom") + 
  labs(x = "ATT using all counties", 
       y = "ATT omitting London and Manchester") 
ggsave("figs/hatecrimes_compare_wo_london_manchester.pdf", width=6,height=4)




# Placebo Analysis --------------------------------------------------------

## Run placebo analysis using MC method. What we'll do is assume each of the
## other counties is treated at the same time as Liverpool actually was.
## We'll then estimate the treatment effect for each county.
## We're omitting West Yorkshire because there's not enough pre-treatment data.

## This produces Figure 2b. 

# make a list to store gsynth results for each county
placebo_results = list()
placebo_results[["merseyside"]] = matcomp_pc

for(co in unique(estdat$county_str)){
  if (co %in% c("west_yorkshire", "merseyside")) next
  
  # set placebo treatment
  thisdat = estdat 
  thisdat$treated = 0
  thisdat$treated[thisdat$county_str == co & thisdat$date >= as.Date("2017-07-01")] = 1
  n_placebo_folds = length(thisdat$hate_crimes_pc[thisdat$county_str == co & thisdat$date < as.Date("2017-07-01")])
  
  # fit matrix completion model. we're going to do the same procedure as above,
  # including selecting the regularization parameter by LOO-CV
  mod = gsynth(hate_crimes_pc ~ treated, 
               data = thisdat, index = c("county", "date"),
               estimator = "mc", lambda = seq(0, 2, length.out = 50),
               se = FALSE, k = n_folds, 
               force = "two-way", 
               parallel = TRUE, cores = 3)
  placebo_results[[co]] = mod
  
}


# extract results - both ATT in each period and averaged over 
# post-treatment periods
results = lapply(placebo_results, function(x){
  out = data.frame(county = x$id.tr, 
                   month = names(x$att),
                   est = x$att, 
                   att.avg = x$att.avg,
                   stringsAsFactors = FALSE,
                   row.names = NULL)
  out$month2 = 1:nrow(out)
  out$month2 = out$month2 - which(out$month == as.Date("2017-06-01"))
  return(out)
})
results = bind_rows(results)
results = results %>% 
  mutate(merseyside = county == "merseyside",
         treated = month2 >= 0)

# split into 2 df's for plotting - one for merseyside and one for the rest
ggresults = results %>% 
  group_by(merseyside) %>% 
  group_split()


# In the merseyside df, add another observation that's exactly the same as 
# time = 0 but mark it as untreated. Purely so the lines on either side of 
# time = 0 meet.
row_i_need = results %>% filter(merseyside, month2 == 0)
row_i_need$treated = FALSE
ggresults[[2]] = bind_rows(ggresults[[2]], row_i_need) %>% 
  arrange(treated, month)

# plot the results
gg1 = ggplot() + 
  aes(x = month2, y = est) +
  annotate(geom="rect", xmin = 0, xmax = Inf, ymin = -Inf, ymax = Inf, alpha = .3) + 
  geom_hline(yintercept = 0, lty = 3) +
  geom_line(data = ggresults[[1]], 
            mapping = aes(group = county),
            color = "#99999950") + 
  geom_line(data = ggresults[[2]], 
            aes(colour = treated)) + 
  scale_colour_manual(values = c("#fcc6b3", "#fb0d1b")) + 
  coord_cartesian(ylim = c(-.6, .6)) + 
  labs(x = "Months relative to treatment", 
       y = "Observed - Imputed Hate Crime Rate") +
  theme(legend.position = "bottom")
  
# save black and white version for print
if (save_bw){
  ggsave(gg1 + scale_colour_manual(values = c("black", "black")) + guides(colour = FALSE),
         file = "../04drafts/Final_Submission/Fig2b.pdf",
         width = 6, height=4)
}







## WORKING PAPER VERSION OF THE FIGURE ###
# In the preprint, we used a color figure. Need to do some hacky stuff
# to get the legend to look the way we want, as it does in panelView()
# https://stackoverflow.com/a/46079299
gg2 = panelView(hate_crimes_pc ~ treated, 
                data = dat, index = c("county", "date"), 
                type = "raw")

# Extract legend from ggplot object
extractLegend <- function(gg) {
  grobs <- ggplot_gtable(ggplot_build(gg))
  foo <- which(sapply(grobs$grobs, function(x) x$name) == "guide-box")
  grobs$grobs[[foo]]
}

# Extract wanted legend
wantedLegend <- extractLegend(gg2)

# Extract grobs from plot
grobsToReplace <- ggplot_gtable(ggplot_build(gg1))
foo <- which(sapply(grobsToReplace$grobs, function(x) x$name) == "guide-box")
# Replace legend with wanted legend
grobsToReplace$grobs[[foo]] <- wantedLegend

# save placebo plot
pdf("figs/hate_crime_matcomp_placebo.pdf", width=6,height=4)
plot(grobsToReplace)
dev.off()



  
## Get p-values for average across all post-treatment periods
placebo_att_avg = ggresults[[1]] %>% distinct(county, att.avg)
placebo_att_avg = bind_rows(placebo_att_avg, 
                            data.frame(county = "merseyside", 
                                       att.avg = matcomp_pc$att.avg))

# one-sided
mean(matcomp_pc$att.avg >= placebo_att_avg$att.avg)

# two-sided
mean(abs(matcomp_pc$att.avg) <= abs(placebo_att_avg$att.avg))








