# Makes Figure 1 (share saying Islam clashes with British values) from YouGov's SPSS file.
# October 2026: Corrects error in original published version that assigned dates in reverse order.

library(tidyverse)
library(haven)
library(here)



# ---- Load ----
df_yougov <- read_sav(here("data", "data-from-yougov.sav"))

# ---- Drop Northern Ireland ----
# YouGov's published series covers Great Britain. In this file the Feb 2017 and June 2018
# waves each include 50 Northern Ireland respondents. Without them, wave sizes match YouGov's 
# published tables.
is_ni <- as_factor(df_yougov$profile_GOR) %in% "Northern Ireland"
stopifnot(sum(is_ni) == 100, identical(is_ni, is.na(df_yougov$newgor)))
df_yougov <- df_yougov[!is_ni, ]

# ---- Weight and survey month of each respondent ----
# Each wave has its own weight column, labelled with the survey month ("Jan 15 w8").
# A respondent has exactly one non-missing weight; that column gives both their
# weight and their survey month. 
weight_cols <- str_subset(names(df_yougov), "^W8_")
has_weight <- !is.na(as.matrix(df_yougov[, weight_cols]))
stopifnot(length(weight_cols) == 20, rowSums(has_weight) == 1)

weight_key <- tibble(weight_col = weight_cols) %>%
  mutate(
    label = map_chr(weight_col, ~ attr(df_yougov[[.x]], "label")),
    month = match(str_sub(label, 1, 3), month.abb),
    year = 2000L + as.integer(str_extract(label, "\\d{2}")),
    date = as.Date(sprintf("%d-%02d-01", year, month))
  )
stopifnot(!anyNA(weight_key$date), !anyDuplicated(weight_key$date))

df_yougov$weight_col <- weight_cols[max.col(has_weight)]
df_yougov$weight <- do.call(coalesce, unname(as.list(df_yougov[, weight_cols])))

n_before_join <- nrow(df_yougov)
df_yougov <- df_yougov %>%
  left_join(weight_key %>% select(weight_col, date), by = "weight_col")

# Check: the join adds no rows, every row has a date, and each of the 20 waves has one date.
stopifnot(
  nrow(df_yougov) == n_before_join,
  !anyNA(df_yougov$date),
  nrow(distinct(df_yougov, wave)) == 20,
  nrow(distinct(df_yougov, wave, date)) == 20
)

# ---- Outcome ----
# 1 = chose the clash statement, 0 = compatible, neither, or don't know.
df_yougov$response <- as.character(as_factor(df_yougov$Cam_Q10))
df_yougov$clash <- as.integer(
  df_yougov$response == "There is a fundamental clash between Islam and the values of British society"
)

# ---- Share per wave ----
df_wave_share <- df_yougov %>%
  group_by(date) %>%
  summarise(mean = weighted.mean(clash, weight), .groups = "drop")

# ---- Figure 1 ----
# Trend line: weighted GAM smooth of respondent-level data, default 95% band.
plot_clash_trend <- ggplot() +
  geom_point(data = df_wave_share, aes(x = date, y = mean)) +
  stat_smooth(data = df_yougov, mapping = aes(x = date, y = clash, weight = weight),
              geom = "smooth", colour = "black", method = "gam",
              formula = y ~ s(x, k = 8, bs = "cr")) +
  scale_y_continuous(labels = function(x) paste0(x * 100, "%"),
                     breaks = seq(.35, .65, .05)) +
  coord_cartesian(ylim = c(.39, .61)) +
  labs(x = NULL, y = "Percent saying Islam clashes w/ British values") +
  theme_bw()

ggsave(here("figs", "Fig1_corrected.pdf"), plot_clash_trend, width = 6, height = 4)
