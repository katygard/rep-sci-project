library(tidyverse) # data manipulation and graphics
library(lubridate) # manipulation of date data
library(lme4) # fitting general(ized) linear mixed models
library(car) # Anova function for Type III tests  !!!NB!!! car masks dplyr::recode
library(emmeans) # estimate means and slopes, rough interaction plots
library(DHARMa) # checking assumptions of mixed model
library(cAIC4) # conditional AIC for model comparison


# import data----
growth.wide <- read.csv("../processed_data/specific-growth-rates.csv")
# note the flat and parsimonious structure of this file

# restructure from wide to long----
growth.long <- gather(growth.wide, days, sp_growth, na.rm = F, X14:X154)

# recode sdate to sdate2 as character class----
growth.long <- growth.long %>%
  mutate(days2 = dplyr::recode(days,                     # note use of dplyr::
                                X14  = 14,
                                X28 = 28,
                                X42 = 42,
                                X56 = 56,
                                X70 = 70,
                                X84 = 84,
                                X98 = 98,
                                X112 = 112,
                                X126 = 126,
                                X140 = 140,
                                X154 = 154))

# check recoding----
with(growth.long, table(days, days2))

names(growth.long)[names(growth.long) == "ï..TEMP"] <- "TEMP"
names(growth.long)[names(growth.long) == "ID."] <- "ID"

colnames(growth.long)

# plot profile for each individual in each treatment----
ggplot(growth.long, aes(x = days2, y = sp_growth, color=ID)) +
  geom_line() +
  geom_point() +
  facet_wrap(~ TRTMENT + TEMP, ncol = 6, labeller = "label_both") +
  theme(legend.position = "none")

# use smoother to get a sense of the shape of the relationship----
ggplot(growth.long, aes(x = days2, y = sp_growth, color=ID)) +
  geom_smooth(se = F, span=2) +
  geom_point() +
  facet_wrap(~ TRTMENT + TEMP, ncol = 6, labeller = "label_both") +
  theme(legend.position = "none") # turns legend off


## generate the same plots, but truncated data, eliminating the first 2 weeks of data (problematic bc error)

# import data----
growth.wide <- read.csv("../processed_data/specific-growth-rates_truncated14.csv")
# note the flat and parsimonious structure of this file

# restructure from wide to long----
growth.long <- gather(growth.wide, days, sp_growth, na.rm = F, X14:X140)

# recode sdate to sdate2 as character class----
growth.long <- growth.long %>%
  mutate(days2 = dplyr::recode(days,                     # note use of dplyr::
                               X14  = 14,
                               X28 = 28,
                               X42 = 42,
                               X56 = 56,
                               X70 = 70,
                               X84 = 84,
                               X98 = 98,
                               X112 = 112,
                               X126 = 126,
                               X140 = 140))

# check recoding----
with(growth.long, table(days, days2))

names(growth.long)[names(growth.long) == "ï..TEMP"] <- "TEMP"
names(growth.long)[names(growth.long) == "ID."] <- "ID"

colnames(growth.long)

means <- aggregate(growth.long[, 6], list(growth.long$TANK, growth.long$TEMP, growth.long$TRTMENT), 
                   mean, na.rm = TRUE)

names(means)[names(means) == "x"] <- "avg_growth_rate"
names(means)[names(means) == "Group.1"] <- "TANK"
names(means)[names(means) == "Group.2"] <- "TEMP"
names(means)[names(means) == "Group.3"] <- "TRTMENT"

write.csv(means,"C:/Users/kmg31/OneDrive/Desktop/means_trunc.csv", row.names = FALSE)

# plot profile for each individual in each treatment----
ggplot(growth.long, aes(x = days2, y = sp_growth, color=ID)) +
  geom_line() +
  geom_point() +
  facet_wrap(~ TRTMENT + TEMP, ncol = 6, labeller = "label_both") +
  theme(legend.position = "none")

# use smoother to get a sense of the shape of the relationship----
ggplot(growth.long, aes(x = days2, y = sp_growth, color=ID)) +
  geom_smooth(se = F, span=2) +
  geom_point() +
  facet_wrap(~ TRTMENT + TEMP, ncol = 6, labeller = "label_both") +
  theme(legend.position = "none") # turns legend off



## generate the same plots, but with data that eliminates the first 2 weeks, and also calculates 
## specific growth rate at each 2 week time step

# import data----
growth.wide <- read.csv("../processed_data/specific-growth-rates_biweekly.csv")
# note the flat and parsimonious structure of this file

# restructure from wide to long----
growth.long <- gather(growth.wide, days, sp_growth, na.rm = F, X14:X140)

# recode sdate to sdate2 as character class----
growth.long <- growth.long %>%
  mutate(days2 = dplyr::recode(days,                     # note use of dplyr::
                               X14  = 14,
                               X28 = 28,
                               X42 = 42,
                               X56 = 56,
                               X70 = 70,
                               X84 = 84,
                               X98 = 98,
                               X112 = 112,
                               X126 = 126,
                               X140 = 140))

# check recoding----
with(growth.long, table(days, days2))

names(growth.long)[names(growth.long) == "ï..TEMP"] <- "TEMP"
names(growth.long)[names(growth.long) == "ID."] <- "ID"

colnames(growth.long)

means <- aggregate(growth.long[, 6], list(growth.long$TANK, growth.long$TEMP, growth.long$TRTMENT), 
                   mean, na.rm = TRUE)

names(means)[names(means) == "x"] <- "avg_growth_rate"
names(means)[names(means) == "Group.1"] <- "TANK"
names(means)[names(means) == "Group.2"] <- "TEMP"
names(means)[names(means) == "Group.3"] <- "TRTMENT"

write.csv(means,"C:/Users/kmg31/OneDrive/Desktop/means.csv", row.names = FALSE)


# plot profile for each individual in each treatment----
ggplot(growth.long, aes(x = days2, y = sp_growth, color=ID)) +
  geom_line() +
  geom_point() +
  facet_wrap(~ TRTMENT + TEMP, ncol = 6, labeller = "label_both") +
  theme(legend.position = "none")

# use smoother to get a sense of the shape of the relationship----
ggplot(growth.long, aes(x = days2, y = sp_growth, color=ID)) +
  geom_smooth(se = F, span=2) +
  geom_point() +
  facet_wrap(~ TRTMENT + TEMP, ncol = 6, labeller = "label_both") +
  theme(legend.position = "none") # turns legend off
