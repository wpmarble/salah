## Replication file for "Can Exposure to Celebrities Reduce Prejudice" ##

# Implement the synthetic control method to see whether anti-Muslim tweets in
# Liverpool declined after Salah joined, relative to what we otherwise would
# have expected. The main outcome is a proportion of anti-Muslim tweets in
# tweets sent by Liverpool fans and fans of other teams The main analysis uses
# the matrix completion method developed by Athey et al.

library(ggplot2)
library(dplyr)
library(gsynth)
library(panelView)
library(lubridate)
library(tidyr)


# save greyscale version for final submission? you'll have to change the figure
# path to avoid an error. cmd+f "Final_Submission" to fix that. 
save_bw = TRUE

theme_set(theme_bw())

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

#Get Monthly Proportions (Anti-Muslim / Relevant) 

# proportion of all tweets that are negative and about muslims
all_teams_muslims_neg = all_teams %>% 
  group_by(month=floor_date(date, "month"), club) %>% 
  summarise(prop = sum(anti_muslim_predicted_class)/n(),
            count_anti_muslim = sum(anti_muslim_predicted_class)) %>% 
  filter(month >= ymd("2014-01-01")& month < ymd("2018-09-01"))%>% 
  mutate(post_salah = ifelse(month >= ymd("2017-07-01"), 1, 0))%>% 
  mutate(liverpool = ifelse(club=="liverpool", 1, 0))%>% 
  mutate(treated = ifelse(liverpool==1&post_salah==1, 1, 0))


# proportion of tweets about muslims that are negative
all_teams_muslims_neg2 = all_teams %>% 
  group_by(month=floor_date(date, "month"), club) %>%
  summarise(prop = sum(anti_muslim_predicted_class)/sum(muslim),
            count_anti_muslim = sum(anti_muslim_predicted_class)) %>% 
  filter(month >= ymd("2014-01-01")& month < ymd("2018-09-01"))%>% 
  mutate(post_salah = ifelse(month >= ymd("2017-07-01"), 1, 0))%>% 
  mutate(liverpool = ifelse(club=="liverpool", 1, 0))%>% 
  mutate(treated = ifelse(liverpool==1&post_salah==1, 1, 0))



# proportion of tweets that reference muslims
all_teams_muslims = all_teams %>% 
  group_by(month=floor_date(date, "month"), club) %>%
  summarise(prop = sum(muslim)/n(),
            count_anti_muslim = sum(anti_muslim_predicted_class)) %>% 
  filter(month >= ymd("2014-01-01")& month < ymd("2018-09-01"))%>% 
  mutate(post_salah = ifelse(month >= ymd("2017-07-01"), 1, 0))%>% 
  mutate(liverpool = ifelse(club=="liverpool", 1, 0))%>% 
  mutate(treated = ifelse(liverpool==1&post_salah==1, 1, 0))


# Make NAs 0 (numerator & denominator = 0) 
all_teams_muslims <- all_teams_muslims %>% mutate_all(funs(replace_na(.,0)))
all_teams_muslims_neg <- all_teams_muslims_neg %>% mutate_all(funs(replace_na(.,0)))
all_teams_muslims_neg2 <- all_teams_muslims_neg2 %>% mutate_all(funs(replace_na(.,0)))




# Visualize the data ------------------------------------------------------
panelView(prop ~ treated, data = all_teams_muslims_neg2, 
          index = c("club", "month"), pre.post=TRUE) + 
  theme_bw(base_size = 20) + 
  theme(axis.text.x = element_text(angle = 90, hjust=1, vjust = .5),
        legend.position = "bottom") + 
  guides(fill = guide_legend(title = NULL)) + 
  ggtitle(NULL) + 
  labs(x = "Date", y = "Club")
ggsave("figs/panel_structure_twitter.pdf", width=13, height=7)

panelView(prop ~ treated, data = all_teams_muslims_neg2, index = c("club", "month"), type = "raw") + 
  theme_bw(base_size=20) +
  theme(axis.text.x = element_text(angle = 90, hjust=1, vjust = .5),
        legend.position = "bottom") + 
  ggtitle(NULL) + 
  labs(x = NULL, y = "Proportion Anti-Muslim Tweets Per Month")
ggsave("figs/panel_outcomes_twitter.pdf", width=13, height=8)


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
  theme_bw(base_size=20)+
  theme(axis.text.x = element_text(angle = 90, hjust=1, margin = margin(t = 0, r = 20, b = 0, l = 20), vjust = .5),
        panel.grid.minor = element_blank(),
        legend.position = "bottom") +
  ggtitle(NULL) +
  labs(x = NULL, y = "Monthly Proportion of anti-Muslim Tweets") + 
  scale_y_continuous(breaks = seq(-.2, .2, .05)) 
ggsave("figs/panel_matrix_completion_twitter.pdf",width=14,height=8)


# save black and white version for print
if (save_bw) {
  t_treat = matcomp_pc$T0
  plot(matcomp_pc, type = "ct", legendOff = TRUE) + 
    theme(axis.text.x = element_text(hjust = .5, vjust = .5)) +
    ggtitle(NULL) +
    labs(x = "Months relative to treatment", y = "Monthly Proportion of Anti-Muslim Tweets") + 
    scale_x_continuous(breaks = seq(2, 60, 10), 
                       labels = function(x) x - t_treat,
    ) + 
    scale_colour_manual(values = c("black", "black")) + 
    coord_cartesian(ylim = c(0, .2)) + 
    guides(colour = FALSE, lty = FALSE)  +
    annotate(geom = "text", x = 43, y = .175, label = "Imputed", size = 6, hjust = 0) +
    annotate(geom = "text", x = 43, y = .03, label = "Observed", size = 6, hjust = 0)
  ggsave("../04drafts/Final_Submission/Fig4a.pdf", width=6,height=3.75)
  
}



# Plot matrix completion results - ATT
plot(matcomp_pc, shade.post = TRUE) + 
  geom_hline(yintercept = 0, lty = 3) + 
  coord_cartesian(ylim = c(-.2, .2)) + 
  scale_y_continuous(breaks = seq(-.2, .2, .05)) +
  scale_x_continuous(breaks= seq(-40, 40, 5))+
  ggtitle(NULL) +
  labs(x = "Months relative to treatment", y = "Observed - Imputed for Liverpool")
ggsave(filename = "figs/panel_matrix_completion_twitter_att.pdf", width=11, height=7)



# make ATT plot with confidence intervals
ests = data.frame(matcomp_pc$est.att)
t_treat = matcomp_pc$T0
ests$month = 1:nrow(ests)
ests$month = ests$month - ests$month[t_treat]
ggplot(ests) + 
  aes(x = month,y = ATT, ymin = CI.lower, ymax = CI.upper) + 
  annotate(geom="rect", xmin = 0, xmax = Inf, ymin = -Inf, ymax = Inf, alpha = .3) + 
  geom_hline(yintercept = 0, lty = 3) + 
  geom_ribbon(alpha = .5) + 
  geom_line() + 
  coord_cartesian(ylim = c(-.2, .2)) + 
  scale_y_continuous(breaks = seq(-.5, .5, .05)) + 
  labs(x = "Months relative to treatment", y = "Observed - Imputed for Liverpool")
ggsave("figs/panel_matrix_completion_pc_att_ci_twitter.pdf", width=6,height=4)


## Extract ATT estimates
matcomp_pc$est.att
matcomp_pc$est.avg

## compare to pre-treatment average in liverpool
pretreat_avg = with(subset(all_teams_muslims_neg2, club=="liverpool" & treated == 0), mean(prop))
print(reduction_pct <- (matcomp_pc$est.avg[,"ATT.avg"] / pretreat_avg) * 100)


# Placebo Analysis --------------------------------------------------------

## Run placebo analysis using MC method. What we'll do is assume each of the
## other clubs is treated at the same time as Liverpool actually was.
## We'll then estimate the treatment effect for each county.
## We're omitting West Yorkshire because there's not enough pre-treatment data.


# make a list to store gsynth results for each county
#rm(placebo_results)
placebo_results = list()
not_liv<-subset(all_teams_muslims_neg2, all_teams_muslims_neg2$club!="liverpool")

for (team in unique(not_liv$club)){
  # set placebo treatment
  thisdat = not_liv
  thisdat$treated = FALSE
  thisdat$treated[thisdat$club == team & thisdat$month >= as.Date("2017-07-01")] = 1
  
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
  out$month2 = out$month2 - which(out$month == as.Date("2017-06-01"))
  return(out)
})
results = bind_rows(results)
results = results %>% 
  mutate(treated = month2 >= 0)



liv_results <- as.data.frame(matcomp_pc$att)
liv_results$est
liv_results$att.avg <- matcomp_pc$att.avg
liv_results$month <- rownames(liv_results)
liv_results$est <- liv_results$`matcomp_pc$att`
liv_results$month2 <- 1:nrow(liv_results)
liv_results$month2 <-
  liv_results$month2 - which(liv_results$month == as.Date("2017-06-01"))
liv_results$treated <- liv_results$month2 > 0
liv_results$club <- "liverpool"
liv_results$liverpool <- TRUE
liv_results <- liv_results[2:7]

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


if (save_bw){
  ggsave(gg1 + scale_colour_manual(values = c("black", "black")) + 
           guides(colour = FALSE) + 
           labs(x = "Months relative to treatment", 
                y = "Observed - Imputed Montly Anti-Muslim Tweet Rate") + 
           scale_x_continuous(breaks = seq(-100, 20, 10)),
         file = "../04drafts/Final_Submission/Fig4b.pdf",
         width = 6, height=4)
}




## WORKING PAPER VERSION OF THE FIGURE ###
# add the legend exactly from basic panelview plot
# code from https://stackoverflow.com/questions/46079033/extract-legend-from-plot-a-and-add-it-to-plot-b
gg2 = panelView(prop ~ treated, 
                data = all_teams_muslims_neg2, index = c("club", "month"), 
                type = "raw")+theme_bw(base_size=20)

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
pdf("figs/twitter_matcomp_placebo.pdf", width=13,height=7)
plot(grobsToReplace)
dev.off()


## Get a p-value for average across all post-treatment periods
placebo_att_avg = results %>% distinct(club, att.avg)

# one-sided
mean(matcomp_pc$att.avg >= placebo_att_avg$att.avg)

# two-sided
mean(abs(matcomp_pc$att.avg) <= abs(placebo_att_avg$att.avg))

#for econ

all_results<-rbind(liv_results, results)
write.csv(all_results, "data/all_results_fixed.csv")



# Descriptive Statistics (in Appendix)  -----------------------------------


#############################################

all_teams_muslims_neg3<-subset(all_teams_muslims_neg2, club!="everton"& club!="liverpool")
sum_clubs <- all_teams_muslims_neg3 %>% 
  group_by(post_salah) %>% 
  summarize(mean=mean(prop, na.rm=TRUE))
sum_clubs



sum_clubs <- all_teams_muslims_neg2 %>% 
  group_by(club, post_salah) %>% 
  summarize(mean=mean(prop, na.rm=TRUE))
sum_clubs











