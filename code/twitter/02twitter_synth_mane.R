## Replication file for "Can Celebrities Reduce Prejudice" ##

# Implement the synthetic control method to see whether anti-Muslim tweets in
# Liverpool declined after Mane joined, relative to what we otherwise would have
# expected. The main outcome is a proportion of anti-Muslim tweets in tweets
# sent by Liverpool fans and fans of other teams The main analysis uses the
# matrix completion method developed by Athey et al.

library(ggplot2)
library(dplyr)
library(gsynth)
library(panelView)
library(lubridate)
library(tidyr)

# set wd to Replication Archive base directory if not already set
#setwd("~/Dropbox/Mo_Salah/replication_files")

# filter() is often overloaded
filter <- dplyr::filter

#Read in Data
load("data/classified_fan_tweets.RData")

#################################################


# Combine Data for Analysis 


##################################################

#All Teams

AppendMe  <-  function(dfNames) {
  do.call(rbind, lapply(dfNames, function(x) {
    cbind(get(x), club = x)
  }))
}

all_teams <- AppendMe(c("liverpool", "chelsea", "city", "manutd", "arsenal", "everton"))
all_teams$liverpool <- ifelse(all_teams$club=="liverpool", 1,0)
all_teams$chelsea <- ifelse(all_teams$club=="chelsea", 1,0)
all_teams$arsenal <- ifelse(all_teams$club=="arsenal", 1,0)
all_teams$manutd <- ifelse(all_teams$club=="manutd", 1,0)
all_teams$city <- ifelse(all_teams$club=="city", 1,0)

#Create Post-Mane Dummy Var
all_teams <- all_teams %>% 
  mutate(post_mane = ifelse(date > ymd("2016-07-01"), 1, 0))

#Get Monthly Proportions (Anti-Muslim / Relevant) 


# proportion of tweets about muslims that are negative (truncate to before Salah)
all_teams_muslims_neg2 = all_teams %>% 
  group_by(month=floor_date(date, "month"), club) %>%
  summarise(prop = sum(anti_muslim_predicted_class)/sum(muslim),
            count_anti_muslim = sum(anti_muslim_predicted_class)) %>% 
  filter(month >= ymd("2014-01-01")& month < ymd("2017-06-01"))%>% 
  mutate(post_mane = ifelse(month > ymd("2016-07-01"), 1, 0))%>% 
  mutate(liverpool = ifelse(club=="liverpool", 1, 0))%>% 
  mutate(treated = ifelse(liverpool==1&post_mane==1, 1, 0))


# Make NAs 0 (numerator & denominator = 0) 
all_teams_muslims_neg2 <- all_teams_muslims_neg2 %>% mutate_all(funs(replace_na(.,0)))

# Visualize the data ------------------------------------------------------

panelView(prop ~ treated, data = all_teams_muslims_neg2, index = c("club", "month"), pre.post=TRUE) + 
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 90, hjust=1, vjust = .5),
        legend.position = "bottom") + 
  guides(fill = guide_legend(title = NULL)) + 
  ggtitle(NULL) + 
  labs(x = "Date", y = "Club")
ggsave("figs/panel_structure_twitter_mane.pdf", width=11, height=7)

panelView(prop ~ treated, data = all_teams_muslims_neg2, index = c("club", "month"), type = "raw") + 
  theme(axis.text.x = element_text(angle = 90, hjust=1, vjust = .5),
        legend.position = "bottom") + 
  ggtitle(NULL) + 
  labs(x = NULL, y = "Proportion Anti-Muslim Tweets Per Month")
ggsave("figs/panel_outcomes_twitter_mane.pdf", width=11, height=7)

# Matrix Completion - normalized outcome ----------------------------------

## THIS IS THE METHOD REPORTED IN THE PAPER ##

# Matrix completion method of Athey et al. on per capita outcome
# use LOO-CV for tuning of penalty parameter. The idea is that 
# each iteration leaves out one of the observations for Merseyside
# in the pre-treatment period. Then compute mean square prediction
# error for each CV iteration. 

# takes a while to run b/c of the CV and bootstrap
matcomp_pc = gsynth(prop ~ treated, 
                    data = all_teams_muslims_neg2, index = c("club", "month"),
                    estimator = "mc", 
                    lambda = seq(0, 2, length.out = 50), 
                    se = TRUE, nboots = 1.5e4, 
                    r = c(0, 5), 
                    CV = TRUE, k = 30,
                    force = "two-way", 
                    seed = 167, # taken from random.org
                    parallel = TRUE, cores = 6)

# Plot matrix completion results - counterfactuals
plot(matcomp_pc, type = "ct") + 
  theme(axis.text.x = element_text(angle = 90, hjust=1, vjust = .5),
        panel.grid.minor = element_blank(),
        legend.position = "bottom") +
  ggtitle(NULL) +
  labs(x = NULL, y = "Monthly Proportion of anti-Muslim Tweets") + 
  scale_y_continuous(breaks = seq(0, 4, .25)) 
ggsave("figs/panel_matrix_completion_twitter_mane.pdf",width=11,height=7)

# Plot matrix completion results - ATT
plot(matcomp_pc, shade.post = TRUE) + 
  geom_hline(yintercept = 0, lty = 3) + 
  coord_cartesian(ylim = c(-.2, .2)) + 
  scale_y_continuous(breaks = seq(-.2, .2, .05)) +
  scale_x_continuous(breaks= seq(-40, 40, 5))+
  ggtitle(NULL) +
  labs(x = "Months relative to treatment", y = "Observed - Imputed for Liverpool")
ggsave(filename = "figs/panel_matrix_completion_twitter_att_mane.pdf", width=11, height=7)



# make ATT plot with confidence intervals
ests = data.frame(matcomp_pc$est.att)
ests$month = 1:nrow(ests)
ests$month = ests$month - ests$month[31]
ggplot(ests) + 
  aes(x = month,y = ATT, ymin = CI.lower, ymax = CI.upper) + 
  annotate(geom="rect", xmin = 0, xmax = Inf, ymin = -Inf, ymax = Inf, alpha = .3) + 
  geom_hline(yintercept = 0, lty = 3) + 
  geom_ribbon(alpha = .5) + 
  geom_line() + 
  coord_cartesian(ylim = c(-.2, .2)) + 
  scale_y_continuous(breaks = seq(-.5, .5, .05)) + 
  labs(x = "Months relative to treatment", y = "Observed - Imputed for Liverpool")
ggsave("figs/panel_matrix_completion_pc_att_ci_twitter_mane.pdf", width=6,height=4)


## Extract ATT estimates
matcomp_pc$est.att
matcomp_pc$est.avg

## compare to pre-treatment average in liverpool
pretreat_avg = with(subset(all_teams_muslims_neg2, club=="liverpool" & treated == 0), mean(prop))
print(reduction_pct <- (matcomp_pc$est.avg[,"ATT.avg"] / pretreat_avg) * 100)

# Save estimates in text files for use in latex
cat(round(matcomp_pc$est.avg[, "ATT.avg"], 3), file = "tables/twitter_att_avg_mane.tex")
cat(round(matcomp_pc$est.avg[, "S.E."], 3), file = "tables/twitter_att_avg_se_mane.tex")
cat(abs(round(reduction_pct, 1)), file = "tables/twitter_att_avg_pct_drop_mane.tex")


# Placebo Analysis --------------------------------------------------------

## Run placebo analysis using MC method. What we'll do is assume each of the
## other clubs is treated at the same time as Liverpool actually was.
## We'll then estimate the treatment effect for each county.


# make a list to store gsynth results for each county
#rm(placebo_results)
placebo_results = list()
not_liv<-subset(all_teams_muslims_neg2, all_teams_muslims_neg2$club!="liverpool")

for(team in unique(not_liv$club)){
  # set placebo treatment
  thisdat = not_liv
  thisdat$treated = FALSE
  thisdat$treated[thisdat$club == team & thisdat$month >= as.Date("2016-07-01")] = 1
  
  # fit matrix completion model. we're going to do the same procedure as above,
  # including selecting the regularization parameter by LOO-CV
  mod = gsynth(prop ~ treated, 
               data = thisdat, index = c("club", "month"),
               estimator = "mc", lambda = seq(0, 2, length.out = 50),
               se = FALSE, nboots=1.5e4, k = 30, force = "two-way", 
               parallel = TRUE, cores = 6, seed=167)
  placebo_results[[team]] = mod
  
}

# extract results - both ATT in each period and averaged over 
# post-treatment periods
results = lapply(placebo_results, function(x){
  out = data.frame(club = x$id.tr, 
                   month = names(x$att),
                   est = x$att, 
                   att.avg = x$att.avg,
                   stringsAsFactors = FALSE,
                   row.names = NULL)
  out$month2 = 1:nrow(out)
  out$month2 = out$month2 - which(out$month == as.Date("2016-07-01"))
  return(out)
})
results = bind_rows(results)
results = results %>% 
  mutate(treated = month2 >= 0)


liv_results<-as.data.frame(matcomp_pc$att)
liv_results$est
liv_results$att.avg<-matcomp_pc$att.avg
liv_results$month<-rownames(liv_results)
liv_results$est<-liv_results$`matcomp_pc$att`
liv_results$month2<-1:nrow(liv_results)
liv_results$month2<-liv_results$month2 - which(liv_results$month == as.Date("2016-07-01"))
liv_results$treated<-liv_results$month2>0
liv_results$club<-"liverpool"
liv_results$liverpool<-TRUE
liv_results<-liv_results[2:7]

# In the liverpool df, add another observation that's exactly the same as 
# time = 0 but mark it as untreated. Purely so the lines on either side of 
# time = 0 meet.
row_i_need = liv_results %>% filter(month2==0)
row_i_need$treated<-TRUE
liv_results = rbind(liv_results, row_i_need)
liv_results = liv_results %>% arrange(month)

# plot the results
gg1 = ggplot() + 
  aes(x = month2, y = est) +
  annotate(geom="rect", xmin = 0, xmax = Inf, ymin = -Inf, ymax = Inf, alpha = .3) + 
  geom_hline(yintercept = 0, lty = 3) +
  geom_line(data = results, 
            mapping = aes(group = club),
            color = "#99999950") + 
  geom_line(data = liv_results, 
            aes(colour = treated)) + 
  scale_colour_manual(values = c("#fcc6b3", "#fb0d1b")) + 
  scale_x_continuous(breaks= seq(-40, 40, 5))+
  scale_y_continuous(breaks=seq(-.20, .30, .05))+
  labs(x = "Months relative to treatment", 
       y = "Observed - Imputed") +
  theme(legend.position = "bottom")

# add the legend exactly from basic panelview plot
# code from https://stackoverflow.com/questions/46079033/extract-legend-from-plot-a-and-add-it-to-plot-b
gg2 = panelView(prop ~ treated, 
                data = all_teams_muslims_neg2, index = c("club", "month"), 
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
pdf("figs/twitter_matcomp_placebo_mane.pdf", width=6,height=4)
plot(grobsToReplace)
dev.off()


## Get a p-value for average across all post-treatment periods
placebo_att_avg = results %>% distinct(club, att.avg)

# one-sided
mean(matcomp_pc$att.avg >= placebo_att_avg$att.avg)

# two-sided
mean(abs(matcomp_pc$att.avg) <= abs(placebo_att_avg$att.avg))













