# libraries
library(colorspace)
library(REdaS)
library(tidyverse)

df <- read.csv("../ProAc/publications.csv")

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

a <- sort(df$Current.number.of.citations, decreasing = TRUE)
h <- tail(which(a >= seq_along(a)), 1)

arc_df <- tribble(
  ~metric, ~min, ~value, ~place, ~text,
  "Impact", 0, ifelse(c_prp / o_prp > 20, 100, ((c_prp / o_prp) / 20) * 100), 0, "Annual citations per peer-reiewed publication",
  "Achievements", 0, ifelse(h >= 30, 100, (h / 30) * 100), 5, "Publication output and impact",
  "Autonomy", 0, ifelse(c_aut / c_total >= 0.50, 100, (c_prp / c_total) * 100), 4, "% of publications exluding PhD supervisor",
  "Self-reliance", 0, (c_fa / c_total) * 100, 3, "% of first-authored citations",
  "Wider contribution", 0, ifelse(c_nprp / c_total >= 0.25, 100, (c_prp / c_total) * 100), 2, "% on non-peer-reviewed publication citations",
  "Openness", 0, (o_oa / o_t5) * 100, 1, "% of open-access output during past 5 years") %>%
  mutate(angle = deg2rad(60 * place)) %>%
  mutate(x = (value * sin(angle)),
         xmin = (100 * sin(angle)),
         y = (value * cos(angle)),
         ymax = (100 * cos(angle)),
         ysegment = (105 * cos(angle)),
         xsegment = (105 * sin(angle)),
         ytext = (110 * cos(angle)),
         xtext = (110 * sin(angle))) %>% 
  arrange(place)  %>% 
  mutate(hjust = c("middle", "left", "left", "middle", "right", "right"),
         ynudge = case_when(metric %in% c("Impact", "Achievements", "Openness") ~ ytext+10,
                            .default = ytext-10))

ggplot(arc_df) +
  geom_segment(aes(x = 0, xend = xsegment, y = 0, yend = ysegment),
               linewidth = 2) +
  geom_polygon(aes(x = xmin, y = ymax),
               fill = NA,
               colour = 'black') +
  geom_polygon(aes(x = x,
                   y = y),
               fill = 'pink',
               colour = 'pink',
               alpha = 0.5) +
  geom_point(aes(x = x, y = y),
             size = 12,
             colour = 'pink') +
  geom_text(aes(x = x, y = y,
                label = round(value, 0))) +
  geom_text(aes(x = xtext,
                y = ytext,
                label = metric,
                hjust = hjust),
            size = 9,
            fontface = "bold",
            colour = "#04ADBF") +
  geom_text(aes(x = xtext,
                y = ynudge,
                label = text,
                hjust = hjust),
            vjust = "outward",
            colour = lighten("#04ADBF", 0.3)) +
  coord_cartesian(xlim = c(-200, 200),
                  ylim = c(-120, 120)) +
  labs(caption = "Crameri, Fabio (2023), Multi-metric academic profiling with ProAc (1.0.0),: https://doi.org/10.5281/zenodo.4899015") +
  theme_void()

ggsave("../assets/proac.png",
       width = 4200,
       height = 2300,
       units = "px")
