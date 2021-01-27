library(tidyverse) # data manipulation and graphics
library(lubridate) # manipulation of date data
library(lme4) # fitting general(ized) linear mixed models
library(car) # Anova function for Type III tests  !!!NB!!! car masks dplyr::recode
library(emmeans) # estimate means and slopes, rough interaction plots
library(DHARMa) # checking assumptions of mixed model
library(cAIC4) # conditional AIC for model comparison

setwd("C:/Users/kmg31/Box/Katy/Research/Experiments/Pteronarcys_Sulfate") #laptop working directory

# import data----
head.wide <- read.csv("Ptery_Sulfate Growth.csv")
# note the flat and parsimonious structure of this file

# restructure from wide to long----
head.long <- gather(head.wide, sdate, head_width, na.rm = F, X28.Jul:X30.Nov)

# recode sdate to sdate2 as character class----
head.long <- head.long %>%
  mutate(sdate2 = dplyr::recode(sdate,                     # note use of dplyr::
                                X28.Jul  = "2020-07-28",
                                X10.Aug = "2020-08-10",
                                X24.Aug = "2020-08-24",
                                X8.Sep = "2020-09-08",
                                X21.Sep = "2020-09-21",
                                X5.Oct = "2020-10-05",
                                X19.Oct = "2020-10-19",
                                X2.Nov = "2020-11-02",
                                X16.Nov = "2020-11-16",
                                X30.Nov = "2020-11-30"))

# check recoding----
with(head.long, table(sdate2, sdate))

# create sampling_date (date class) from sdate2 (character class)----
head.long <- head.long %>%
  mutate(sampling_date = lubridate::ymd(sdate2))

# date data are stored as integers: the number of days since 1960-01-01 minus 1
# see https://stats.idre.ucla.edu/r/faq/how-does-r-handle-date-values/ 

# create rescaled date, such that 2019-07-01 is equivalent to zero, to use in regression----
# for a nice discussion of this use of rescaling, see 
#   https://stats.stackexchange.com/questions/65900/does-it-make-sense-to-use-a-date-variable-in-a-regression

# sampling_date_scaled is number of days since 2020-07-28
head.long<- head.long %>%
  mutate(sampling_date_scaled = sampling_date - as.Date("2020-07-28"))

# plot profile for each individual in each treatment----
ggplot(head.long, aes(x = sampling_date, y = head_width, color=ID.)) +
  geom_line() +
  geom_point() +
  facet_wrap(~ TANK) +
  theme(legend.position = "none")

# use smoother to get a sense of the shape of the relationship----
ggplot(head.long, aes(x = sampling_date, y = head_width, color=ID.)) +
  geom_smooth(se = F, span=2) +
  geom_point() +
  facet_wrap(~ TANK) +
  theme(legend.position = "none") # turns legend off


## random coefficients model
# make a copy of the data frame used in the model, to make life easier
df <- filter(head.long, sampling_date <= as.Date("2020-11-30") & !is.na(head_width))

# convert TANK to factor to work in model below

df['TANK'] <- lapply(df['TANK'], factor)

# fit random intercepts only model----
# slopes for all ID. within a TANK will be equal
m1 <- lmer(head_width ~ TANK * sampling_date_scaled + (1 | ID.),
           data = df,
           contrasts = list(TANK = contr.sum)) #need to convert TANK to factor for this code to work
summary(m1)
car::Anova(m1)

simulationOutput <- simulateResiduals(fittedModel = m1)

simulationOutput
