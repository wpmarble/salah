## Makes Appendix Figure A-7

library(tidyverse)
library(lubridate)
library(ggthemes)

df = readRDS("data/liverpool_echo.rds")


# analysis ----------------------------------------------------------------
df$salah_title = df$title %>% str_detect("Salah|salah")

df$mane_title = df$title %>% str_detect("Mane|mane")


df = df %>% mutate(month = floor_date(date, "month"))

df2 = df %>% 
  group_by(month) %>% 
  summarise(salah_title = sum(salah_title, na.rm = T),
            mane_title = sum(mane_title, na.rm = T),
            total = n()) %>% 
  mutate(salah_title = salah_title / total,
         mane_title = mane_title / total)

df_plot = df2 %>% 
  select(-total) %>% 
  pivot_longer(-c(month), names_to = "data_type", values_to = "pct")

df_plot %>% 
  filter(str_detect(data_type, "title")) %>% 
  mutate(data_type = recode(data_type, "salah_title" = "Salah", "mane_title" = "Mane")) %>% 
  ggplot(aes(x = month, y = pct, colour = data_type)) + 
  geom_line() +
  geom_vline(xintercept = ymd("2016-06-01"), linetype = 2) + 
  annotate(geom = "text", x = ymd("2016-02-25"), y = 0.135, label = "Mane\nJoins", size = 3) + 
  geom_vline(xintercept = ymd("2017-06-01"), linetype = 2) + 
  annotate(geom = "text", x = ymd("2017-02-25"), y = 0.135, label = "Salah\nJoins", size = 3) + 
  labs(x = "Date", y = "Percent of articles per month", subtitle = "Percent of Headlines Mentioning Player") + 
  theme_bw() + 
  coord_cartesian(xlim = c(ymd("2013-07-01"), ymd("2019-04-01"))) +
  scale_x_date(date_breaks = "3 months", date_labels = "%Y-%m") + 
  theme(legend.position = "bottom", legend.title = element_blank(), axis.text.x = element_text(angle = 90)) +
  scale_color_hc()
ggsave("figs/liverpool_echo_titles.pdf", width = 6, height = 4)

