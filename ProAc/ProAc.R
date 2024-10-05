# libraries
library(REdaS)
library(tidyverse)

df <- read.csv("ProAc/publications.csv")

current_yr <- as.numeric(format(Sys.Date(), "%Y"))
max_yr <- min(df$Year)
active_yrs <- current_yr - max_yr

c_prp <- sum(subset(subset(df, Year < current_yr), Kind == "Peer-reviewed paper")[, "Current.number.of.citations"]) / active_yrs
o_prp <- nrow(subset(df, Year < current_yr))

c_total <- sum(df$Current.number.of.citations)

c_fa <- sum(subset(df, First.authored == 1)[, "Current.number.of.citations"])
c_aut <- sum(subset(df, Including.PhD.supervisor == 0)[, "Current.number.of.citations"])
c_nprp <- sum(subset(df, Kind != "Peer-reviewed paper")[, "Current.number.of.citations"])

o_oa <- nrow(subset(subset(df, Open.access == 1), Year > current_yr - 5))
o_t5 <- nrow(subset(df, Year > current_yr - 5))

df %>% pull(Current.number.of.citations)

a <- sort(df$Current.number.of.citations, decreasing = TRUE)
h <- tail(which(a >= seq_along(a)), 1)

arc_df <- tribble(
  ~metric, ~min, ~value,
  "impact", 0, ifelse(c_prp / o_prp > 20, 100, ((c_prp / o_prp) / 20) * 100),
  "self_reliance", 0, (c_fa / c_total) * 100,
  "autonomy", 0, ifelse(c_aut / c_total >= 0.50, 100, (c_prp / c_total) * 100),
  "contribution", 0, ifelse(c_nprp / c_total >= 0.25, 100, (c_prp / c_total) * 100),
  "openness", 0, (o_oa / o_t5) * 100,
  "achievements", 0, ifelse(h >= 30, 100, (h / 30) * 100)) %>%
  mutate(x = - (value * sin(deg2rad(60 * (row_number(.) - 1)))),
         y = (value * cos(deg2rad(60 * (row_number(.) - 1)))),
         ymax = (100 * cos(deg2rad(60 * (row_number(.) - 1)))))

ggplot(arc_df) +
    geom_point(aes(x = x,
                   y = ymax)) +
    geom_point(aes(x = x,
                   y = y),
               shape = 4)