## Replication file for "Can Celebrities Reduce Prejudice" ##
# Create Figure 1 -- trends in Islamophobia. Data from YouGov.


# set wd to replication project directory
# setwd("~/Dropbox/Mo_Salah/replication_files/")


library(tidyverse)
library(survey)
library(ggthemes)
library(stats)

df = read.csv("data/yougov.csv", stringsAsFactors = F)

#Table the question of interest
df$Cam_Q10 %>% table(useNA = "always")

#Fix date; recode for the clash variable
df = df %>% mutate(date = lubridate::mdy(date),
                   clash = ifelse(Cam_Q10 == "There is a fundamental clash between Islam and the values of British society", 1, 0),
                   compatible = ifelse(Cam_Q10 == "Islam is generally compatible with the values of British society", 1, 0))

df2 = df %>% mutate(w = weight) %>% select(date, wave, clash, compatible, w)

# Obtaining waves to input into survey design
waves = df %>% select(wave, date) %>% group_by(wave, date) %>% filter(row_number()==1)

# Preparing matrix for survey design outputs 
store = matrix(ncol = 3, nrow = nrow(waves))
colnames(store) = c("mean","low","up")
rownames(store) = unique(waves$wave)

for(i in 1:nrow(waves)){
  #subset to wave i
  df_temp = df2[df2$wave == waves$wave[i],]
  
  #make svydesign object
  des = svydesign(id=~1, weights=~w, data=df_temp)
  
  #get weighted mean
  store[i,1] = svymean(df_temp$clash, design = des)
  
  #get confidence intervals
  out = svyciprop(~clash,design=des)
  
  #store
  store[i,2:3] <- c(confint(out))
}
store = as.data.frame(store)

store$wave = row.names(store) %>% as.numeric
store = store %>% left_join(waves)

store = store %>% as_tibble()

store %>% 
  ggplot(aes(x = date, y = mean)) + 
  geom_line()


store %>% 
  ggplot(aes(x = date, y = mean)) + 
  geom_line(aes(colour = "line")) + 
  geom_smooth(aes(colour = "smooth")) + 
  scale_y_continuous(labels = scales::percent) +
  labs(y = "Percent claiming Islam clashes\n with British values", x = NULL) + 
  #geom_vline(xintercept = lubridate::ymd("2017-07-01"), linetype = 2) + 
  theme_bw() + 
  scale_color_hc() + 
  theme(legend.position = "none")
ggsave("figs/yougov_data.pdf", width = 6, height = 4)



## FIGURE 1 IN MAIN TEXT ##
## just smooth out the underlying data using geom_smooth() rather
## than summarising first
ggplot() + 
  geom_point(data = store, aes(x = date, y = mean)) + 
  stat_smooth(data = df2, mapping = aes(x = date, y = clash, weight = w), 
              geom = "smooth", colour = "black", method = "gam", formula = y~s(x, k = 8, bs = "cr")) +
  scale_y_continuous(labels = function(x) paste0(x*100, "%"), breaks = seq(.35, .65, .05)) + 
  coord_cartesian(ylim = c(.39, .61)) + 
  labs(x = NULL, y = "Percent saying Islam clashes w/ British values") + 
  theme_bw() 
ggsave("figs/yougov_data2.pdf", width=6,height=4)  
ggsave("../04drafts/Final_Submission/Fig1.pdf", width=6,height=4)


  




