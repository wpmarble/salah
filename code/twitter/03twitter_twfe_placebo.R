# placebo analysis for twitter data 
# randomly sample one of the clubs to be "treated" at a randomly-chosen time,
# then estimate the Diff in Diff model that's in the appendix and compare the
# observed coefficient to the placebo distribution. 

library(ggplot2)
library(dplyr)
library(gsynth)
library(panelView)
library(lubridate)
library(tidyr)

#Get Data to Analyze 

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

#Create Post-Salah Dummy Var
all_teams <- all_teams %>% 
  mutate(post_salah = ifelse(date > ymd("2017-07-01"), 1, 0))

#Each Team Treated Separately 

# proportion of tweets about muslims that are negative
liverpool_treated = all_teams %>% 
  group_by(month=floor_date(date, "month"), club) %>%
  summarise(prop = sum(anti_muslim_predicted_class)/sum(muslim),
            count_anti_muslim = sum(anti_muslim_predicted_class)) %>% 
  filter(month >= ymd("2014-01-01")& month < ymd("2018-09-01"))%>% 
  mutate(post_salah = ifelse(month > ymd("2017-07-01"), 1, 0))%>% 
  mutate(treated1 = ifelse(club=="liverpool", 1, 0))%>% 
  mutate(treated2 = ifelse(treated1==1&post_salah==1, 1, 0))
liverpool_treated$treated_team<-"liverpool"

chelsea_treated = all_teams %>% 
  group_by(month=floor_date(date, "month"), club) %>%
  summarise(prop = sum(anti_muslim_predicted_class)/sum(muslim),
            count_anti_muslim = sum(anti_muslim_predicted_class)) %>% 
  filter(month >= ymd("2014-01-01")& month < ymd("2018-09-01"))%>% 
  mutate(post_salah = ifelse(month > ymd("2017-07-01"), 1, 0))%>% 
  mutate(treated1 = ifelse(club=="chelsea", 1, 0))%>% 
  mutate(treated2 = ifelse(treated1==1&post_salah==1, 1, 0))
chelsea_treated$treated_team<-"chelsea"

arsenal_treated = all_teams %>% 
  group_by(month=floor_date(date, "month"), club) %>%
  summarise(prop = sum(anti_muslim_predicted_class)/sum(muslim),
            count_anti_muslim = sum(anti_muslim_predicted_class)) %>% 
  filter(month >= ymd("2014-01-01")& month < ymd("2018-09-01"))%>% 
  mutate(post_salah = ifelse(month > ymd("2017-07-01"), 1, 0))%>% 
  mutate(treated1 = ifelse(club=="arsenal", 1, 0))%>% 
  mutate(treated2 = ifelse(treated1==1&post_salah==1, 1, 0))
arsenal_treated$treated_team<-"arsenal"

city_treated = all_teams %>% 
  group_by(month=floor_date(date, "month"), club) %>%
  summarise(prop = sum(anti_muslim_predicted_class)/sum(muslim),
            count_anti_muslim = sum(anti_muslim_predicted_class)) %>% 
  filter(month >= ymd("2014-01-01")& month < ymd("2018-09-01"))%>% 
  mutate(post_salah = ifelse(month > ymd("2017-07-01"), 1, 0))%>% 
  mutate(treated1 = ifelse(club=="city", 1, 0))%>% 
  mutate(treated2 = ifelse(treated1==1&post_salah==1, 1, 0))
city_treated$treated_team<-"city"

manutd_treated = all_teams %>% 
  group_by(month=floor_date(date, "month"), club) %>%
  summarise(prop = sum(anti_muslim_predicted_class)/sum(muslim),
            count_anti_muslim = sum(anti_muslim_predicted_class)) %>% 
  filter(month >= ymd("2014-01-01")& month < ymd("2018-09-01"))%>% 
  mutate(post_salah = ifelse(month > ymd("2017-07-01"), 1, 0))%>% 
  mutate(treated1 = ifelse(club=="manutd", 1, 0))%>% 
  mutate(treated2 = ifelse(treated1==1&post_salah==1, 1, 0))
manutd_treated$treated_team<-"manutd"

everton_treated = all_teams %>% 
  group_by(month=floor_date(date, "month"), club) %>%
  summarise(prop = sum(anti_muslim_predicted_class)/sum(muslim),
            count_anti_muslim = sum(anti_muslim_predicted_class)) %>% 
  filter(month >= ymd("2014-01-01")& month < ymd("2018-09-01"))%>% 
  mutate(post_salah = ifelse(month > ymd("2017-07-01"), 1, 0))%>% 
  mutate(treated1 = ifelse(club=="everton", 1, 0))%>% 
  mutate(treated2= ifelse(treated1==1&post_salah==1, 1, 0))
everton_treated$treated_team<-"everton"


all_treated<-rbind(arsenal_treated, chelsea_treated, city_treated, liverpool_treated, manutd_treated, everton_treated)

# run simulations ---------------------------------------------------------

nsims = 1e4
store_betas = data.frame(start = NA, treat = NA, coef = numeric(nsims))
teams = c("arsenal", "chelsea", "city", "liverpool", "everton", "manutd")

# random select months between 4 months after the start of the data and the
# actual observed treatment date (July 1, 2017)
minmonth = as.Date("2014-05-01")
maxmonth = as.Date("2017-07-01")
dates = seq.Date(minmonth, maxmonth, by = "month")

# do the simulations
for (i in 1:nsims){
  
  # select one team to be "treated"
  team_name = sample(teams,1)
  d = subset(all_treated, treated_team==team_name)
  
  # select treatment date to kick in
  start = sample(dates, 1)
  
  # run regression
  reg = lm(prop ~ treated1*I(month > start), d)
  
  store_betas$start[i] = start
  store_betas$treat[i] = team_name
  store_betas$coef[i] = coef(reg)["treated1:I(month > start)TRUE"]
}


# get actual estimate
actual_lm = lm(prop ~ treated1*I(month > as.Date("2017-07-01")), filter(all_treated, treated_team=="liverpool")) 
summary(actual_lm)
actual_est = coef(actual_lm)[4]
actual_est

# get one-sided p-value 
pval = mean(store_betas$coef < actual_est)


# plot results ------------------------------------------------------------

ggplot(store_betas) + 
  aes(x = coef) + 
  geom_histogram(fill = "#A0A0A0", aes(y = ..ncount../sum(..ncount..)), bins = 40) + 
  geom_segment(x = actual_est, xend = actual_est, y = 0, yend = .07, size = 1) + 
  annotate(geom = "text", x = 0.01, y = .065, label = paste0("Observed estimate\nOne-sided p = ", round(pval, 3))) + 
  labs(x = "Placebo coefficient estimate", y = "Density") +
  scale_x_continuous(breaks = seq(-.1, .1, .05)) + 
  theme_minimal() + 
  theme(panel.grid = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=.85),
        panel.spacing = unit(0, "in"))
ggsave("figs/twitter_placebo_analysis_monthly.pdf", width=11, height = 7)





# Mane twitter analysis ---------------------------------------------------

# truncate outcome data to before Salah joined and see what the p-value on 
# the treatment starting at June 2016 is
# run simulations ---------------------------------------------------------


#Each Team Treated Separately 

# proportion of tweets about muslims that are negative
liverpool_treated = all_teams %>% 
  group_by(month=floor_date(date, "month"), club) %>%
  summarise(prop = sum(anti_muslim_predicted_class)/sum(muslim),
            count_anti_muslim = sum(anti_muslim_predicted_class)) %>% 
  filter(month >= ymd("2014-01-01")& month < ymd("2017-07-01"))%>% 
  mutate(post_salah = ifelse(month > ymd("2017-07-01"), 1, 0))%>% 
  mutate(treated1 = ifelse(club=="liverpool", 1, 0))%>% 
  mutate(treated2 = ifelse(treated1==1&post_salah==1, 1, 0))
liverpool_treated$treated_team<-"liverpool"

chelsea_treated = all_teams %>% 
  group_by(month=floor_date(date, "month"), club) %>%
  summarise(prop = sum(anti_muslim_predicted_class)/sum(muslim),
            count_anti_muslim = sum(anti_muslim_predicted_class)) %>% 
  filter(month >= ymd("2014-01-01")& month < ymd("2017-07-01"))%>% 
  mutate(post_salah = ifelse(month > ymd("2017-07-01"), 1, 0))%>% 
  mutate(treated1 = ifelse(club=="chelsea", 1, 0))%>% 
  mutate(treated2 = ifelse(treated1==1&post_salah==1, 1, 0))
chelsea_treated$treated_team<-"chelsea"

arsenal_treated = all_teams %>% 
  group_by(month=floor_date(date, "month"), club) %>%
  summarise(prop = sum(anti_muslim_predicted_class)/sum(muslim),
            count_anti_muslim = sum(anti_muslim_predicted_class)) %>% 
  filter(month >= ymd("2014-01-01")& month < ymd("2017-07-01"))%>% 
  mutate(post_salah = ifelse(month > ymd("2017-07-01"), 1, 0))%>% 
  mutate(treated1 = ifelse(club=="arsenal", 1, 0))%>% 
  mutate(treated2 = ifelse(treated1==1&post_salah==1, 1, 0))
arsenal_treated$treated_team<-"arsenal"

city_treated = all_teams %>% 
  group_by(month=floor_date(date, "month"), club) %>%
  summarise(prop = sum(anti_muslim_predicted_class)/sum(muslim),
            count_anti_muslim = sum(anti_muslim_predicted_class)) %>% 
  filter(month >= ymd("2014-01-01")& month < ymd("2017-07-01"))%>% 
  mutate(post_salah = ifelse(month > ymd("2017-07-01"), 1, 0))%>% 
  mutate(treated1 = ifelse(club=="city", 1, 0))%>% 
  mutate(treated2 = ifelse(treated1==1&post_salah==1, 1, 0))
city_treated$treated_team<-"city"

manutd_treated = all_teams %>% 
  group_by(month=floor_date(date, "month"), club) %>%
  summarise(prop = sum(anti_muslim_predicted_class)/sum(muslim),
            count_anti_muslim = sum(anti_muslim_predicted_class)) %>% 
  filter(month >= ymd("2014-01-01")& month < ymd("2017-07-01"))%>% 
  mutate(post_salah = ifelse(month > ymd("2017-07-01"), 1, 0))%>% 
  mutate(treated1 = ifelse(club=="manutd", 1, 0))%>% 
  mutate(treated2 = ifelse(treated1==1&post_salah==1, 1, 0))
manutd_treated$treated_team<-"manutd"

everton_treated = all_teams %>% 
  group_by(month=floor_date(date, "month"), club) %>%
  summarise(prop = sum(anti_muslim_predicted_class)/sum(muslim),
            count_anti_muslim = sum(anti_muslim_predicted_class)) %>% 
  filter(month >= ymd("2014-01-01")& month < ymd("2017-07-01"))%>% 
  mutate(post_salah = ifelse(month > ymd("2017-07-01"), 1, 0))%>% 
  mutate(treated1 = ifelse(club=="everton", 1, 0))%>% 
  mutate(treated2= ifelse(treated1==1&post_salah==1, 1, 0))
everton_treated$treated_team<-"everton"


all_treated<-rbind(arsenal_treated, chelsea_treated, city_treated, liverpool_treated, manutd_treated, everton_treated)


nsims = 1e4
store_betas = data.frame(start = NA, treat = NA, coef = numeric(nsims))
teams = c("arsenal", "chelsea", "city", "liverpool", "everton", "manutd")

# random select months between 4 months after the start of the data and the
# actual observed treatment date (June 1, 2016)
minmonth = as.Date("2014-05-01")
maxmonth = as.Date("2016-06-01")
dates = seq.Date(minmonth, maxmonth, by = "month")

# do the simulations
for (i in 1:nsims){
  
  # select one team to be "treated"
  team_name = sample(teams,1)
  d = subset(all_treated, treated_team==team_name)
  
  # select treatment date to kick in
  start = sample(dates, 1)
  
  # run regression
  reg = lm(prop ~ treated1*I(month > start), d)
  
  store_betas$start[i] = start
  store_betas$treat[i] = team_name
  store_betas$coef[i] = coef(reg)["treated1:I(month > start)TRUE"]
}


# get actual estimate
actual_lm = lm(prop ~ treated1*I(month > as.Date("2016-06-01")), filter(all_treated, treated_team=="liverpool")) 
summary(actual_lm)
actual_est = coef(actual_lm)[4]

# get one-sided p-value 
pval = mean(store_betas$coef < actual_est)

# make plot
ggplot(store_betas) + 
  aes(x = coef) + 
  geom_histogram(fill = "#A0A0A0", aes(y = ..ncount../sum(..ncount..)), bins = 40) + 
  geom_segment(x = actual_est, xend = actual_est, y = 0, yend = .07, size = 1) + 
  annotate(geom = "text", x = .01, y = .065, label = paste0("Observed estimate for Mané\nOne-sided p = ", round(pval, 3))) + 
  labs(x = "Placebo coefficient estimate", y = "Density") +
  scale_x_continuous(breaks = seq(-.1, .1, .05)) + 
  theme_minimal() + 
  theme(panel.grid = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=.85),
        panel.spacing = unit(0, "in")) 
ggsave("figs/twitter_placebo_analysis_mane_monthly.pdf", width=5.5, height = 4)
