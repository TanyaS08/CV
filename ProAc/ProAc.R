# libraries
library(colorspace)
library(extrafont)
library(REdaS)
library(sysfonts)
library(showtext)
library(tidyverse)

# import dataset
df <- read.csv("../ProAc/publications.csv")

# import fonts
font_add_google("Roboto",
                "Roboto")
font_paths()  
font_files()
font_families()
trace(grDevices::png, exit = quote({
  showtext::showtext_begin()
}), print = FALSE)

# year intervals/values
current_yr <- as.numeric(format(Sys.Date(), "%Y"))
max_yr <- min(df$Year)
active_yrs <- current_yr - max_yr

# get interim metric/terms (notation ~ used in manuscript)
# the annual mean peer-reviewed publication citations 
# over all academic years older than 1
c_prp <- sum(subset(subset(df, Year < current_yr), Kind == "Peer-reviewed paper")[, "Current.number.of.citations"]) / active_yrs
o_prp <- nrow(subset(df, Year < current_yr))
# total citations
c_total <- sum(df$Current.number.of.citations)
# citation first author
c_fa <- sum(subset(df, First.authored == 1)[, "Current.number.of.citations"])
# citations exclude PhD supervisor
c_aut <- sum(subset(df, Including.PhD.supervisor == 0)[, "Current.number.of.citations"])
# non-peer-reviewed citations
c_nprp <- sum(subset(df, Kind != "Peer-reviewed paper")[, "Current.number.of.citations"])
# OA publications (last 5 years)
o_oa <- nrow(subset(subset(df, Open.access == 1), Year > current_yr - 5))
# total publications (last 5 years)
o_t5 <- nrow(subset(df, Year > current_yr - 5))
# h index
a <- sort(df$Current.number.of.citations, decreasing = TRUE)
h <- tail(which(a >= seq_along(a)), 1)

# get metrics
impact <- c_prp / o_prp
achievement <- h
autonomy <- c_aut / c_total
self <- c_fa / c_total * 100
wider <- c_nprp / c_total
openness <- o_oa / o_t5  * 100

# collate all metrics and apply rotation function
arc_df <- tribble(
  ~metric, ~score, ~value, ~place, ~text,
  "Impact", impact, ifelse(impact > 20, 100, (impact / 20) * 100), 0, "Annual citations per peer-reiewed publication",
  "Achievements", h, ifelse(h >= 30, 100, (h / 30) * 100), 5, "Publication output and impact",
  "Autonomy", autonomy, ifelse(autonomy >= 0.50, 100, autonomy * 100), 4, "% of publications exluding PhD supervisor",
  "Self-reliance", self, self, 3, "% of first-authored citations",
  "Wider contribution", wider, ifelse(wider >= 0.25, 100, (wider) * 100), 2, "% on non-peer-reviewed publication citations",
  "Openness", openness, openness, 1, "% of open-access output during past 5 years") %>%
  mutate(angle = deg2rad(60 * place)) %>%
  mutate(x = (value * sin(angle)),
         xmin = (100 * sin(angle)),
         y = (value * cos(angle)),
         ymax = (100 * cos(angle)),
         ysegment = (105 * cos(angle)),
         xsegment = (105 * sin(angle)),
         ytext = (112 * cos(angle)),
         xtext = (112 * sin(angle))) %>% 
  arrange(place)  %>% 
  mutate(hjust = c("middle", "left", "left", "middle", "right", "right"),
         ynudge = case_when(metric %in% c("Impact", "Achievements", "Openness") ~ ytext+10,
                            .default = ytext-10),
         score = ifelse(value == 100, "max", round(score, 0)))

# get cords for paths
path_df <- tibble(place = 0:5, 
                  metric = c("Impact", "Openness", "Wider contribution", "Self-reliance", "Autonomy", "Achievements")) %>% 
  mutate(angle = deg2rad(60 * place),
         y1 = (20 * cos(angle)),
         y2 = (40 * cos(angle)),
         y3 = (60 * cos(angle)),
         y4 = (80 * cos(angle)),
         y5 = (100 * cos(angle)),
         x1 = (20 * sin(angle)),
         x2 = (40 * sin(angle)),
         x3 = (60 * sin(angle)),
         x4 = (80 * sin(angle)),
         x5 = (100 * sin(angle))) %>% 
  pivot_longer(
    cols = y1:x5,
    names_to = c("plane", "group"),
    names_pattern = "(.)(.)",
    values_to = "value") %>% 
  pivot_wider(names_from = plane, values_from = value) %>% 
  mutate(group = as.numeric(group),
         text = case_when(metric == "Impact" ~ group * 4,
                          metric == "Achievements" ~ group * 6,
                          metric == "Autonomy" ~ group * 10,
                          metric == "Self-reliance" ~ group * 20,
                          metric == "Wider contribution" ~ group * 5,
                          metric == "Openness" ~ group * 20))
plot <-
  ggplot(arc_df) +
  geom_polygon(data = path_df,
               aes(x = x, y = y, group = group),
               fill = NA,
               colour = "#4B2473",
               linewidth = 1) +
  geom_segment(aes(x = 0, xend = xsegment, y = 0, yend = ysegment),
               linewidth = 5, colour = "#E1EF77") +
  geom_text(data = path_df,
            aes(x = x, y = y, label = text),
            colour = "#4B2473",
            size = 3,
            family = "Roboto") +
  geom_polygon(aes(x = x,
                   y = y),
               fill = '#F263A6',
               colour = '#F263A6',
               alpha = 0.6,
               linewidth = 3) +
  geom_point(aes(x = x, y = y),
             size = 14,
             colour = '#F263A6') +
  geom_text(aes(x = x, y = y,
                label = score),
            size = 5,
            fontface = "bold",
            colour = "#4B2473",
            family = "Roboto") +
  geom_text(aes(x = xtext,
                y = ytext,
                label = metric,
                hjust = hjust),
            size = 8,
            fontface = "bold",
            colour = "#04ADBF",
            family = "Roboto") +
  geom_text(aes(x = xtext,
                y = ynudge,
                label = text,
                hjust = hjust),
            size = 4,
            vjust = "outward",
            colour = "#4B2473",
            family = "Roboto") +
  coord_cartesian(xlim = c(-200, 200),
                  ylim = c(-120, 120)) +
  labs(caption = "Source: Multi-metric academic profiling with ProAc (1.0.0): https://doi.org/10.5281/zenodo.4899015") +
  theme_void() +
  theme(plot.caption = element_text(size = 11, face = "italic", family = "Roboto"))

ggsave("../assets/proac.png",
       plot,
       width = 4000,
       height = 2250,
       units = "px")
