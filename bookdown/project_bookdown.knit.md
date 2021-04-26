--- 
title: "WILD6900: Reproducible Science Project"
author: "Katy Gardner"
date: "2021-04-26"
site: bookdown::bookdown_site
documentclass: book
bibliography: [book.bib, packages.bib]
biblio-style: apalike
link-citations: yes
description: "This is the bookdown file desrcibing the process I am taking in constructing my final project for WILD6900: Reproducible Science, a graduate course at Utah State University."
---

# Introduction

For my final project, I will use data collected during long-term laboratory experiments I am running for my dissertation. Data collected during these experiments include: survivorship data, growth data, water chemistry, and water quality measurements. Overall, these experiments are designed to investigate the effects of salinity and temperature on aquatic insect growth and survival. The data used in this project are from my first experiment, in which I exposed *Pteronarcys californica* nymphs (the giant salmonfly) to 6 sulfate concentration treatments across 6 temperature treatments, for a total of 36 exposure treatments.

## Project Goals
In this project, I will:

1. clean and manage the data I have collected for this experiment
2. create a database structure that can also be used in future experiments
3. generate a fitness index
4. visually inspect collected data for treatment effects on 
    i) survival and 
    ii) fitness index

## Project Outline 

Chapter \@ref(database) details the process of creating an RSQLite database in RMarkdown and importing all project data (mentioned above: survivorship, growth, water quality and water chemistry) into this database.

Chapter \@ref(survival) visually inspects the survival data for trends in % survival in response to either the sulfate or temperature treatments.

Chapter \@ref(fitness) generates a fitness index using the survival and growth data from our database, and uses the fitness index data in plots to visualize the effects of temperature and sulfate treatments on *Pteronarcys* fitness.

<!--chapter:end:index.Rmd-->


# Building my SQLite database {#database}


```r
#load packages
library(DBI)
library(RSQLite)
library(dplyr)
library(tidyverse)
library(lessR)
library(tibble)
```

For my project, I have 4 main .csv files that I want to incorporate into one database. The files pertain to two main entities: the tanks and the bugs. Tanks contain bugs in certain treatment conditions (temperature and added sulfate). There are 3 sampling processes going on: tanks, bugs, and chemical analyses. Chemical analyses are conducted prior to the start of the experiment and at the end of the experiment. Tanks are measured approximately twice a week (with some variability) for dissolved oxygen, conductivity, and temperature levels as a sort of maintenance measurement. The number of bugs alive per tank is measured once a week and recorded as that tank's survival. Finally, bugs are measured once every two weeks for head capsule width, which can then be converted to a specific growth measurement. The database structure I intend to follow is displayed below.

<div class="figure" style="text-align: center">
<img src="assignment4.jpg" alt="Database structure. Italics denote primary key, bold denotes foreign keys." width="100%" />
<p class="caption">(\#fig:image)Database structure. Italics denote primary key, bold denotes foreign keys.</p>
</div>

## USUAL water data

First, I will create a database connection in the "project" folder of my Reproducible Science class folder.

```r
#establish a database connection
ptery_sulf_db <- dbConnect(drv = RSQLite::SQLite(), 
                        "../raw_data/ptery_sulf.db")
```

Next, I want to load in all of the .csv files I intend to add to the database. I'll start with USUAL water measurement data.

```r
USUAL_start <- read.csv("../raw_data/2020-07_USUAL_results.csv")
USUAL_end <- read.csv("../raw_data/2021-01_USUAL_results.csv")
#original USUAL docs had "<" when measurement values were below detection limits (typically <0.001)
#I replaced "<" with empty cells for R purposes. Not sure if empty/blank cells are more appropriate than 0's?
```

We want to combine these two USUAL .csv files into one for the database. Some cleaning needs to be done before merging the two files. Additionally, carbonate and bicarbonate were only measured in the second round of tests ("end"), so these columns will need to be added to the first document.


```r
USUAL_start <- USUAL_start %>%
  rename(USU_ID = ï..USU_ID,
         Sulfate = Sulfate.Sulfur,
         arsenic = As) #renaming columns that imported into R poorly

USUAL_start <- USUAL_start[1:37, ] #deleting empty rows that were for some reason included in the csv import

USUAL_start <- add_column(USUAL_start, date = rep(as.Date("2020-07-27"),37), .after = 1) #add in date of water sample to differentiate between start and end

USUAL_end <- USUAL_end %>%
  rename(USU_ID = ï..USU_ID,
         Sulfate = Sulfate.Sulfur,
         arsenic = As, #for some reason RSQLite won't create table with this column name as As
         carbonate = Carbonate_mmolc.L,
         bicarbonate = Bicarbonate_mmolc.L) #the "." in the original name not SQL friendly

USUAL_end <- USUAL_end[1:36, ] #USUAL_end has one fewer rows because we did not test the well water a second time

USUAL_end <- add_column(USUAL_end, date = rep(as.Date("2021-01-07"),36), .after = 1)

empty_cols <- c("carbonate", "bicarbonate") #adding empty columns before vertically merging
USUAL_start[, empty_cols] <- NA

#merge files vertically
usual <- Merge(USUAL_start, USUAL_end, by="rows")
```

Let's add the USUAL dataframe into our database.


```r
#write table in database
dbExecute(ptery_sulf_db, "CREATE TABLE usual (
          USU_ID float NOT NULL PRIMARY KEY,
          date text,
          Trough integer,
          Tank integer,
          Chloride float, 
          Al float,
          arsenic float,
          B float,
          Ba float,
          Ca float,
          Cd float,
          Co float,
          Cr float,
          Cu float,
          Fe float,
          K float,
          Mg float,
          Mn float,
          Mo float,
          Na float,
          Ni float,
          P float,
          Pb float,
          S float,
          Se float,
          Si float,
          Sr float,
          Zn float,
          Sulfate float,
          carbonate float,
          bicarbonate float);")

dbWriteTable(ptery_sulf_db, "usual", usual, append = TRUE)
dbListTables(ptery_sulf_db) 
```

With the table added, let's check to see that it worked before moving on to the other 3 tables.


```r
dbGetQuery(ptery_sulf_db, "SELECT * FROM usual LIMIT 10;")
```

```
##    USU_ID    date Trough Tank Chloride Al arsenic     B Ba     Ca Cd    Co
## 1    2333 18470.0      1    1     3.42 NA   0.006 0.004 NA   6.29 NA    NA
## 2    2334 18470.0      1    2     2.37 NA   0.005 0.004 NA 104.00 NA 0.001
## 3    2335 18470.0      1    3     2.68 NA   0.004 0.005 NA 242.00 NA 0.001
## 4    2336 18470.0      1    4     2.40 NA      NA 0.003 NA  54.80 NA    NA
## 5    2337 18470.0      1    5     2.45 NA   0.003 0.004 NA 195.00 NA 0.001
## 6    2338 18470.0      1    6     2.57 NA   0.002 0.003 NA 151.00 NA    NA
## 7    2339 18470.0      2    7     2.36 NA   0.004 0.004 NA 148.00 NA    NA
## 8    2340 18470.0      2    8     2.42 NA   0.006 0.002 NA  56.10 NA    NA
## 9    2341 18470.0      2    9     2.38 NA   0.004 0.002 NA   6.70 NA    NA
## 10   2342 18470.0      2   10     2.33 NA   0.002 0.004 NA 101.00 NA 0.001
##       Cr    Cu Fe    K     Mg    Mn Mo   Na    Ni  P    Pb      S Se   Si   Sr
## 1     NA 0.003 NA 0.17   2.13    NA NA 0.93    NA NA 0.001   0.61 NA 0.76 0.02
## 2     NA 0.002 NA 0.18  44.10    NA NA 0.88    NA NA    NA 138.00 NA 0.86 0.03
## 3     NA    NA NA 0.30 105.00 0.002 NA 0.89    NA NA    NA 338.00 NA 0.82 0.06
## 4     NA 0.001  0 0.22  23.00    NA NA 0.84    NA NA 0.001  69.10 NA 0.80 0.03
## 5  0.001    NA NA 0.14  83.40    NA NA 0.77    NA NA    NA 268.00 NA 0.75 0.05
## 6     NA 0.003 NA 0.32  64.10    NA NA 0.99    NA NA 0.002 204.00 NA 0.84 0.04
## 7     NA 0.003 NA 0.11  63.40    NA NA 0.91 0.001 NA    NA 202.00 NA 0.88 0.05
## 8     NA 0.002 NA 0.13  23.20    NA NA 0.92 0.001 NA 0.002  69.70 NA 0.97 0.03
## 9     NA 0.001 NA 0.11   2.20    NA NA 0.85 0.002 NA    NA   1.05 NA 0.76 0.02
## 10    NA 0.004 NA 0.12  42.40    NA NA 0.94    NA NA    NA 134.00 NA 0.81 0.04
##      Zn Sulfate carbonate bicarbonate
## 1  0.01    1.83        NA          NA
## 2  0.02  413.00        NA          NA
## 3  0.01 1013.00        NA          NA
## 4  0.01  207.00        NA          NA
## 5  0.01  803.00        NA          NA
## 6  0.02  613.00        NA          NA
## 7  0.02  607.00        NA          NA
## 8  0.02  209.00        NA          NA
## 9  0.01    3.16        NA          NA
## 10 0.02  403.00        NA          NA
```

Sweet! It worked! Adding the other .csv files next.

## Survival data


```r
survival.wide <- read.csv("../raw_data/weekly_survival.csv")

#this file is in wide form, let's switch it to a more r-friendly long-form

survival <- gather(survival.wide, sdate, survival, na.rm = F, X28.Jul:X28.Dec)



survival$Sulfate.mg.L <- as.character(survival$Sulfate.mg.L)

survival <- survival %>%
  mutate(sdate2 = dplyr::recode(sdate,                     # note use of dplyr::
                                X28.Jul  = "2020-07-28",
                                X3.Aug = "2020-08-03",
                                X10.Aug = "2020-08-10",
                                X17.Aug = "2020-08-17",
                                X24.Aug = "2020-08-24",
                                X31.Aug = "2020-08-31",
                                X8.Sep = "2020-09-08",
                                X14.Sep = "2020-09-14",
                                X21.Sep = "2020-09-21",
                                X28.Sep = "2020-09-28",
                                X5.Oct = "2020-10-05",
                                X12.Oct = "2020-10-12",
                                X19.Oct = "2020-10-19",
                                X26.Oct = "2020-10-26",
                                X2.Nov = "2020-11-02",
                                X9.Nov = "2020-11-09",
                                X16.Nov = "2020-11-16",
                                X23.Nov = "2020-11-23",
                                X30.Nov = "2020-11-30",
                                X7.Dec = "2020-12-07",
                                X14.Dec = "2020-12-14",
                                X21.Dec = "2020-12-21",
                                X28.Dec = "2020-12-28"))


with(survival, table(sdate2, sdate)) # a check to make sure therer are 36 values at each old date and recoded date
```

```
##             sdate
## sdate2       X10.Aug X12.Oct X14.Dec X14.Sep X16.Nov X17.Aug X19.Oct X2.Nov
##   2020-07-28       0       0       0       0       0       0       0      0
##   2020-08-03       0       0       0       0       0       0       0      0
##   2020-08-10      36       0       0       0       0       0       0      0
##   2020-08-17       0       0       0       0       0      36       0      0
##   2020-08-24       0       0       0       0       0       0       0      0
##   2020-08-31       0       0       0       0       0       0       0      0
##   2020-09-08       0       0       0       0       0       0       0      0
##   2020-09-14       0       0       0      36       0       0       0      0
##   2020-09-21       0       0       0       0       0       0       0      0
##   2020-09-28       0       0       0       0       0       0       0      0
##   2020-10-05       0       0       0       0       0       0       0      0
##   2020-10-12       0      36       0       0       0       0       0      0
##   2020-10-19       0       0       0       0       0       0      36      0
##   2020-10-26       0       0       0       0       0       0       0      0
##   2020-11-02       0       0       0       0       0       0       0     36
##   2020-11-09       0       0       0       0       0       0       0      0
##   2020-11-16       0       0       0       0      36       0       0      0
##   2020-11-23       0       0       0       0       0       0       0      0
##   2020-11-30       0       0       0       0       0       0       0      0
##   2020-12-07       0       0       0       0       0       0       0      0
##   2020-12-14       0       0      36       0       0       0       0      0
##   2020-12-21       0       0       0       0       0       0       0      0
##   2020-12-28       0       0       0       0       0       0       0      0
##             sdate
## sdate2       X21.Dec X21.Sep X23.Nov X24.Aug X26.Oct X28.Dec X28.Jul X28.Sep
##   2020-07-28       0       0       0       0       0       0      36       0
##   2020-08-03       0       0       0       0       0       0       0       0
##   2020-08-10       0       0       0       0       0       0       0       0
##   2020-08-17       0       0       0       0       0       0       0       0
##   2020-08-24       0       0       0      36       0       0       0       0
##   2020-08-31       0       0       0       0       0       0       0       0
##   2020-09-08       0       0       0       0       0       0       0       0
##   2020-09-14       0       0       0       0       0       0       0       0
##   2020-09-21       0      36       0       0       0       0       0       0
##   2020-09-28       0       0       0       0       0       0       0      36
##   2020-10-05       0       0       0       0       0       0       0       0
##   2020-10-12       0       0       0       0       0       0       0       0
##   2020-10-19       0       0       0       0       0       0       0       0
##   2020-10-26       0       0       0       0      36       0       0       0
##   2020-11-02       0       0       0       0       0       0       0       0
##   2020-11-09       0       0       0       0       0       0       0       0
##   2020-11-16       0       0       0       0       0       0       0       0
##   2020-11-23       0       0      36       0       0       0       0       0
##   2020-11-30       0       0       0       0       0       0       0       0
##   2020-12-07       0       0       0       0       0       0       0       0
##   2020-12-14       0       0       0       0       0       0       0       0
##   2020-12-21      36       0       0       0       0       0       0       0
##   2020-12-28       0       0       0       0       0      36       0       0
##             sdate
## sdate2       X3.Aug X30.Nov X31.Aug X5.Oct X7.Dec X8.Sep X9.Nov
##   2020-07-28      0       0       0      0      0      0      0
##   2020-08-03     36       0       0      0      0      0      0
##   2020-08-10      0       0       0      0      0      0      0
##   2020-08-17      0       0       0      0      0      0      0
##   2020-08-24      0       0       0      0      0      0      0
##   2020-08-31      0       0      36      0      0      0      0
##   2020-09-08      0       0       0      0      0     36      0
##   2020-09-14      0       0       0      0      0      0      0
##   2020-09-21      0       0       0      0      0      0      0
##   2020-09-28      0       0       0      0      0      0      0
##   2020-10-05      0       0       0     36      0      0      0
##   2020-10-12      0       0       0      0      0      0      0
##   2020-10-19      0       0       0      0      0      0      0
##   2020-10-26      0       0       0      0      0      0      0
##   2020-11-02      0       0       0      0      0      0      0
##   2020-11-09      0       0       0      0      0      0     36
##   2020-11-16      0       0       0      0      0      0      0
##   2020-11-23      0       0       0      0      0      0      0
##   2020-11-30      0      36       0      0      0      0      0
##   2020-12-07      0       0       0      0     36      0      0
##   2020-12-14      0       0       0      0      0      0      0
##   2020-12-21      0       0       0      0      0      0      0
##   2020-12-28      0       0       0      0      0      0      0
```

```r
survival <- survival %>%
  rename(sulfate_trtment = Sulfate.mg.L,
         date = sdate2,
         set_temp = Temperature,
         tank = Tank) #renaming columns that imported into R poorly

survival <- survival[,c(2:4,6:7)]

survival$survival_ID <- 1:nrow(survival) #add a serial no. column to act as unique primary key

survival <- survival[, c("survival_ID", "date", "tank", "set_temp", "sulfate_trtment", "survival")] #reordering columns
```

Write the survival table into database.

```r
dbExecute(ptery_sulf_db, "CREATE TABLE survival (
          survival_ID integer NOT NULL PRIMARY KEY,
          date text,
          tank integer,
          set_temp integer,
          sulfate_trtment integer, 
          survival integer,
          FOREIGN KEY(tank) REFERENCES usual(tank));")

dbWriteTable(ptery_sulf_db, "survival", survival, append = TRUE)
dbListTables(ptery_sulf_db)
```


```r
dbGetQuery(ptery_sulf_db, "SELECT * FROM survival LIMIT 10;")
```

```
##    survival_ID       date tank set_temp sulfate_trtment survival
## 1            1 2020-07-28    1       14               0       10
## 2            2 2020-07-28    2       14             400       10
## 3            3 2020-07-28    3       14            1000       10
## 4            4 2020-07-28    4       14             200       10
## 5            5 2020-07-28    5       14             800       10
## 6            6 2020-07-28    6       14             600       10
## 7            7 2020-07-28    7       22             600       10
## 8            8 2020-07-28    8       22             200       10
## 9            9 2020-07-28    9       22               0       10
## 10          10 2020-07-28   10       22             400       10
```


## Weekly water measurements
Add weekly water measurement data. NOTE: This data has not all been entered yet, so this data file is really incomplete. Will need to go back and re-add to database when all data has been entered.


```r
water_meas <- read.csv("../raw_data/weekly_water_measurements.csv")

#rename some columns
water_meas <- water_meas %>%
  rename(date = ï..DATE,
         measured_temp = TEMP..C.,
         DO_percent = DO.,
         DO_mgL = DO..mg.L.,
         conductivity = CONDUCTIVITY,
         tank = TANK)

#add a serial no. column to act as unique primary key, put it as first column
water_meas <- add_column(water_meas, water_ID = 1:nrow(water_meas), .after = 0)
```

Create water_measures table in SQLite.


```r
dbExecute(ptery_sulf_db, "CREATE TABLE water_measures (
          water_ID integer NOT NULL PRIMARY KEY,
          date text,
          tank integer,
          measured_temp float,
          DO_percent float, 
          DO_mgL float,
          conductivity integer,
          FOREIGN KEY(tank) REFERENCES survival(tank));")

dbWriteTable(ptery_sulf_db, "water_measures", water_meas, append = TRUE)
dbListTables(ptery_sulf_db) 
```


```r
dbGetQuery(ptery_sulf_db, "SELECT * FROM water_measures LIMIT 10;")
```

```
##    water_ID      date tank measured_temp DO_percent DO_mgL conductivity
## 1         1 7/29/2020    1          15.4       77.7   7.77           60
## 2         2 7/29/2020    2          15.4       82.6   8.26          815
## 3         3 7/29/2020    3          15.5       82.5   8.24         1685
## 4         4 7/29/2020    4          15.4       83.8   8.38          485
## 5         5 7/29/2020    5          15.3       83.5   8.37         1395
## 6         6 7/29/2020    6          14.8       84.0   8.50         1100
## 7         7 7/29/2020    7          21.4       83.8   7.46         1165
## 8         8 7/29/2020    8          21.3       82.2   7.38          510
## 9         9 7/29/2020    9          21.4       83.1   7.39           70
## 10       10 7/29/2020   10          21.5       84.6   7.47          850
```


## Head widths
Import head width data.


```r
head_width <- read.csv("../raw_data/head-width_measurements.csv")

#rename some columns
head_width <- head_width %>%
  rename(meas_date = DATE,
         week = WEEK,
         observer = MEASURED_BY,
         bug_ID = ID,
         tank = TANK,
         head_width = HEAD_WIDTH)

#add a serial no. column to act as unique primary key, put it as first column
head_width <- add_column(head_width, meas_ID = 1:nrow(head_width), .after = 0)

#eliminate the "ZOOM" and "MICROMETER UNITS" columns (not needed for db)
head_width <- head_width[,c(1:4, 6:7,10)]
```

Create head_width table in SQLite.


```r
dbExecute(ptery_sulf_db, "CREATE TABLE head_width (
          meas_ID integer NOT NULL PRIMARY KEY,
          week integer,
          meas_date text,
          observer text,
          tank integer,
          bug_ID varchar(4),
          head_width float,
          FOREIGN KEY(tank) REFERENCES survival(tank));")

dbWriteTable(ptery_sulf_db, "head_width", head_width, append = TRUE)
dbListTables(ptery_sulf_db) 
```


```r
dbGetQuery(ptery_sulf_db, "SELECT * FROM head_width LIMIT 10;")
```

```
##    meas_ID week meas_date observer tank bug_ID head_width
## 1        1    0 7/28/2020       KG    1    65F       1.67
## 2        2    0 7/28/2020       KG    1    44F       1.91
## 3        3    0 7/28/2020       KG    1    85D       2.11
## 4        4    0 7/28/2020       KG    1    25A       1.93
## 5        5    0 7/28/2020       KG    1    16E       1.93
## 6        6    0 7/28/2020       KG    1    22B       1.49
## 7        7    0 7/28/2020       KG    1    20G       1.82
## 8        8    0 7/28/2020       KG    1    98D       1.69
## 9        9    0 7/28/2020       KG    1    97A       1.96
## 10      10    0 7/28/2020       KG    1   38D1       1.76
```

<!--chapter:end:01-database.Rmd-->

# Survival {#survival}

## Load in data
Now that the data are loaded into the database, let's look at how survival in each group was affected by the treatments.



Load in survival data from database and add column to mutate dates into days for plot simplicity.

```r
# get data from ptery_sulf_db database
surv <- dbGetQuery(ptery_sulf_db, "SELECT * FROM survival;")  

# create sampling_date (date class) from date (character class)----
surv <- surv %>%
  mutate(sampling_date = lubridate::ymd(date)) 

#create days column
surv <- surv %>%
  mutate(days = sampling_date - as.Date("2020-07-28"))

surv <- surv[,c(3:6, 8)]
```

## Plots
Plot survival over time (days) by sulfate treatment.

```r
#total by treatment, regardless of temperature
surv_trtment <- surv[,c(3:5)]
surv_trtment <- aggregate(. ~sulfate_trtment+days, data = surv_trtment, sum)
surv_trtment$survival <- (((surv_trtment$survival)/60)*100)

surv_trtment$sulfate_trtment <- as.factor(surv_trtment$sulfate_trtment)

#survival plot by sulfate trtment over time
ggplot(surv_trtment, aes(x=days, y=survival, color=sulfate_trtment)) +
  geom_line(lwd = 1.3) +
  theme_bw() +
  labs(x = "Sampling Date (days)", y = "% Survival", color = "Sulfate (mg/L)") +
  theme(legend.position = c(0.01, 0.01),
        legend.justification = c("left", "bottom"),
        legend.background = element_blank(),
        legend.title = element_text(size=11),
        legend.text = element_text(size=11),
        legend.key.size = unit(1, "cm")) +
  scale_y_continuous(limits = c(0,100)) +
  scale_color_viridis_d() 
```

```
## Don't know how to automatically pick scale for object of type difftime. Defaulting to continuous.
```

<img src="project_bookdown_files/figure-html/unnamed-chunk-18-1.png" width="672" />


Plot survival over time (days) by temperature treatment.

```r
#total by temperature, regardless of treatment
surv_temp <- surv[,c(2,4:5)]
surv_temp <- aggregate(. ~set_temp+days, data = surv_temp, sum)
surv_temp$survival <- (((surv_temp$survival)/60)*100)

surv_temp$set_temp <- as.factor(surv_temp$set_temp)

#survival plot by temperature over time
ggplot(surv_temp, aes(x=days, y=survival, color=set_temp)) +
  geom_line(lwd = 1.3) +
  theme_bw() +
  labs(x = "Sampling Date (days)", y = "% Survival", color = "Temperature (°C)") +
  theme(legend.position = c(0.01, 0.01),
        legend.justification = c("left", "bottom"),
        legend.background = element_blank(),
        legend.title = element_text(size=11),
        legend.text = element_text(size=11),
        legend.key.size = unit(1, "cm")) +
  scale_y_continuous(limits = c(0,100)) +
  scale_color_viridis_d() 
```

```
## Don't know how to automatically pick scale for object of type difftime. Defaulting to continuous.
```

<img src="project_bookdown_files/figure-html/unnamed-chunk-19-1.png" width="672" />

Generate barplots of survival at end of experiment per group.

```r
####barplots of final survival
#sulfate
sulf_final <- subset(surv_trtment, days == 153)
ggplot(sulf_final, aes(x=sulfate_trtment, y=survival, color=sulfate_trtment, fill=sulfate_trtment)) +
  geom_bar(position="dodge", stat="identity") +
  theme_bw() +
  scale_color_viridis_d() +
  scale_fill_viridis_d() +
  labs(x = "Sulfate (mg/L)", y = "% Survival", title = "% Survival Grouped by Sulfate Treatment") +
  theme(legend.position = "none") +
  scale_y_continuous(limits = c(0,100))
```

<img src="project_bookdown_files/figure-html/unnamed-chunk-20-1.png" width="672" />

```r
#temp
temp_final <- subset(surv_temp, days == 153)
ggplot(temp_final, aes(x=set_temp, y=survival, color=set_temp, fill=set_temp)) +
  geom_bar(position="dodge", stat="identity") +
  theme_bw() +
  scale_color_viridis_d() +
  scale_fill_viridis_d() +
  labs(x = "Temperature (°C)", y = "% Survival", title = "% Survival Grouped by Temperature Treatment") +
  theme(legend.position = "none") +
  scale_y_continuous(limits = c(0,100))
```

<img src="project_bookdown_files/figure-html/unnamed-chunk-20-2.png" width="672" />

There don't seem to be any visible trends across treatments for % survival, whether that be over time or final. However, survival is only one piece of the overall fitness puzzle we're looking at. In the next section of this project, we will generate a fitness index combining the survival data and growth data to see how overall *Pteronarcys* fitness was affected by these treatements. 

<!--chapter:end:02-survival.Rmd-->


# Fitness index {#fitness}

We are ultimately interested in how overall organism fitness is impacted by sulfate levels. We can better visualize that through a fitness index that combines survival and growth measurements. The fitness index is calculated by multiplying percent survival by the average growth rate (G) for each treatment.



## Generate fitness index
To generate our fitness index, we first need to source the data we're interested in from our database `ptery_sulf_db` (Chapter \@ref(database)). We will be using data from the `head_width` and `survival` tables, but only grabbing data for bugs who survived until the end of the experiment (week 22). We will also be omitting the first week of measurement data in `head_width` due to issues with observer error.


```r
# only select head width information of individuals that were alive on 2nd measurement day and last
hw_survivors <- dbGetQuery(ptery_sulf_db, "SELECT *
                                            FROM (SELECT *          
                                                  FROM head_width
                                                  WHERE bug_id IN(
                                                      SELECT bug_id
                                                      FROM head_width
                                                      GROUP BY bug_id
                                                      HAVING COUNT(DISTINCT meas_id) = 12))
                                            WHERE (week = 2) OR (week = 22);") 
#nested inquiry isolates individuals alive on all dates
#WHERE statement selects only the data from last first and last measurement dates


end_survival <- dbGetQuery(ptery_sulf_db, "SELECT *
                                           FROM survival
                                           WHERE date = '2020-12-28';")
sum(end_survival$survival)  
```

```
## [1] 240
```
Output should read 240, which is the number of individuals alive in the last measurement date out of 360.

Next, we will calculate the growth rate for each individual using the specific growth rate (G) equation.

```r
hw_survivors <- hw_survivors[,c(2,5:7)]
hw_survivors <- spread(hw_survivors, week, head_width)

hw_survivors <- hw_survivors %>% 
  dplyr:: rename(init = 3, final = 4) %>% 
  mutate(ln = log(final/init), G = (ln/140)*100 ) #140 days between 08/10 and 12/28

#average growth rate for each tank
tank_fit <- aggregate(hw_survivors[,6], list(hw_survivors$tank), mean)
tank_fit <- tank_fit %>%
  dplyr::rename(tank = Group.1, avg_G = x)
```

To calculate the fitness index, we want survival to be represented as the proportion of individuals alive out of 10.

```r
#change survival numbers to proportions
end_survival <- end_survival %>% 
  mutate(surv_prop = survival/10)
```

Finally, we will use the specific growth rate (G) data and the survival data to create our fitness index for each tank.

```r
#create fitness index 
tank_fit <- tank_fit %>% 
  mutate(surv_prop = end_survival$surv_prop,
         fit_index = (surv_prop*avg_G),
         temp = end_survival$set_temp,
         trtment = end_survival$sulfate_trtment)
###NOTE: first, third, fourth line of mutate code only worked well bc tanks were in order in both df's
###would need to recode to include some "by group" argument to get to work otherwise
```


## Data visualization
### 3D Surface plot


```r
prep <- tank_fit[order(tank_fit$trtment),] #reordering survival.wide by sulfate level
prep1 <- prep[order(prep$temp),] #now reordering by temperature

#you want the reordering steps so that you can predictably label the z matrix w/ temp and sulfate 
#column/row names. the original survival.wide had treatments in no particular order
temp <- seq(12, 22, 2) #setting up matrix colnames
sulf <- seq(0, 1000, 200) #rownames

z <- matrix(prep1$fit_index, 6, 6) #reformatting 36 survival data points into matrix, by treatment

rownames(z) <- sulf
colnames(z) <- temp

surf_fit <- plot_ly(type = "surface", z=z, x=temp, y=sulf)
surf_fit <- surf_fit %>% layout(scene = list(xaxis = list(title = 'Temperature'),
                                             yaxis = list(title = 'Sulfate mg/L'),
                                             zaxis = list(title = 'Fitness Index')))

surf_fit
```

```{=html}
<div id="htmlwidget-b0900fae7554a531f1a1" style="width:672px;height:480px;" class="plotly html-widget"></div>
<script type="application/json" data-for="htmlwidget-b0900fae7554a531f1a1">{"x":{"visdat":{"3d785a56d2f":["function () ","plotlyVisDat"]},"cur_data":"3d785a56d2f","attrs":{"3d785a56d2f":{"z":[[0.140685143412217,0.143220950434744,0.176697243439627,0.109929108894883,0.118895798454461,0.108444125629431],[0.081867758815438,0.21668532245432,0.161802198716668,0.177295994291742,0.122468594904893,0.100359487782655],[0.114387836057187,0.191557804736859,0.206256853936537,0.186164270764454,0.0730318247364414,0.0923689928860526],[0.121601175372995,0.103830797166539,0.109931478140631,0.125601968954413,0.0879222850884593,0.085094968421349],[0.0537208318218097,0.0224041113467887,0.141439890436286,0.107377682625295,0.0139102911994651,0.113852619572023],[0.0890226758206065,0.0226753021989608,0.244732063437085,0.188301331869503,0.0731876349440555,0.194455762352886]],"x":[12,14,16,18,20,22],"y":[0,200,400,600,800,1000],"alpha_stroke":1,"sizes":[10,100],"spans":[1,20],"type":"surface"}},"layout":{"margin":{"b":40,"l":60,"t":25,"r":10},"scene":{"xaxis":{"title":"Temperature"},"yaxis":{"title":"Sulfate mg/L"},"zaxis":{"title":"Fitness Index"}},"hovermode":"closest","showlegend":false,"legend":{"yanchor":"top","y":0.5}},"source":"A","config":{"showSendToCloud":false},"data":[{"colorbar":{"title":"","ticklen":2,"len":0.5,"lenmode":"fraction","y":1,"yanchor":"top"},"colorscale":[["0","rgba(68,1,84,1)"],["0.0416666666666667","rgba(70,19,97,1)"],["0.0833333333333333","rgba(72,32,111,1)"],["0.125","rgba(71,45,122,1)"],["0.166666666666667","rgba(68,58,128,1)"],["0.208333333333333","rgba(64,70,135,1)"],["0.25","rgba(60,82,138,1)"],["0.291666666666667","rgba(56,93,140,1)"],["0.333333333333333","rgba(49,104,142,1)"],["0.375","rgba(46,114,142,1)"],["0.416666666666667","rgba(42,123,142,1)"],["0.458333333333333","rgba(38,133,141,1)"],["0.5","rgba(37,144,140,1)"],["0.541666666666667","rgba(33,154,138,1)"],["0.583333333333333","rgba(39,164,133,1)"],["0.625","rgba(47,174,127,1)"],["0.666666666666667","rgba(53,183,121,1)"],["0.708333333333333","rgba(79,191,110,1)"],["0.75","rgba(98,199,98,1)"],["0.791666666666667","rgba(119,207,85,1)"],["0.833333333333333","rgba(147,214,70,1)"],["0.875","rgba(172,220,52,1)"],["0.916666666666667","rgba(199,225,42,1)"],["0.958333333333333","rgba(226,228,40,1)"],["1","rgba(253,231,37,1)"]],"showscale":true,"z":[[0.140685143412217,0.143220950434744,0.176697243439627,0.109929108894883,0.118895798454461,0.108444125629431],[0.081867758815438,0.21668532245432,0.161802198716668,0.177295994291742,0.122468594904893,0.100359487782655],[0.114387836057187,0.191557804736859,0.206256853936537,0.186164270764454,0.0730318247364414,0.0923689928860526],[0.121601175372995,0.103830797166539,0.109931478140631,0.125601968954413,0.0879222850884593,0.085094968421349],[0.0537208318218097,0.0224041113467887,0.141439890436286,0.107377682625295,0.0139102911994651,0.113852619572023],[0.0890226758206065,0.0226753021989608,0.244732063437085,0.188301331869503,0.0731876349440555,0.194455762352886]],"x":[12,14,16,18,20,22],"y":[0,200,400,600,800,1000],"type":"surface","frame":null}],"highlight":{"on":"plotly_click","persistent":false,"dynamic":false,"selectize":false,"opacityDim":0.2,"selected":{"opacity":1},"debounce":0},"shinyEvents":["plotly_hover","plotly_click","plotly_selected","plotly_relayout","plotly_brushed","plotly_brushing","plotly_clickannotation","plotly_doubleclick","plotly_deselect","plotly_afterplot","plotly_sunburstclick"],"base_url":"https://plot.ly"},"evals":[],"jsHooks":[]}</script>
```

Not seeing a clear trend from the 3D surface plot, whether it be to temperature, sulfate, or an interaction of the two.
(hint: it moves! drag it around to view from different angles)

### Grouped bar plots
Let's visualize the data by one treatment at a time (temperature first, then sulfate).
The method we will use here still shows individual tanks and what their treatment level was of the other treatment, but we group bars by the primary treatment we are visualizing.

```r
tank_fit$temp <- as.factor(tank_fit$temp)
tank_fit$trtment <- as.factor(tank_fit$trtment)

#grouped by temp
ggplot(tank_fit, aes(fill=trtment, y=fit_index, x=temp)) + 
  geom_bar(position="dodge", stat="identity") +
  scale_fill_viridis_d()+
  xlab("Temperature") +
  ylab("Fitness Index") +
  theme_bw()
```

<img src="project_bookdown_files/figure-html/unnamed-chunk-27-1.png" width="672" />

```r
#grouped by sulfate treatment
ggplot(tank_fit, aes(fill=temp, y=fit_index, x=trtment)) + 
  geom_bar(position="dodge", stat="identity")+
  scale_fill_viridis_d()+
  xlab("Sulfate (mg/L)") +
  ylab("Fitness Index") +
  theme_bw()
```

<img src="project_bookdown_files/figure-html/unnamed-chunk-27-2.png" width="672" />

Hmm... again, no clear patterns. Perhaps this is because grouped bar plots like this are a bit messy. Let's average fitness index values across single treatments to simplify the plots.

### Fitness index averaged bar plots

```r
#fitness index averaged by temp
avg <- tank_fit %>%
  group_by(temp) %>%
  summarise(fit_index = mean(fit_index))

ggplot(tank_fit, aes(x=temp, y=fit_index, color = temp, fill = temp)) +
  geom_point() +
  geom_bar(data=avg, stat = "identity", alpha = 0.7) +
  ggrepel::geom_text_repel(aes(label = tank), color = "black", size = 2.5, segment.color = "grey") +
  scale_color_viridis_d() +
  scale_fill_viridis_d() +
  xlab("Temperature") +
  ylab("Fitness Index") +
  theme_bw()
```

<img src="project_bookdown_files/figure-html/unnamed-chunk-28-1.png" width="672" />

```r
#fitness index averaged by sulfate treatment
avg <- tank_fit %>%
  group_by(trtment) %>%
  summarise(fit_index = mean(fit_index))

ggplot(tank_fit, aes(x=trtment, y=fit_index, color = trtment, fill = trtment)) +
  geom_point() +
  geom_bar(data=avg, stat = "identity", alpha = 0.7) +
  ggrepel::geom_text_repel(aes(label = tank), color = "black", size = 2.5, segment.color = "grey") +
  scale_color_viridis_d() +
  scale_fill_viridis_d() +
  xlab("Sulfate mg/L") +
  ylab("Fitness Index") +
  theme_bw()
```

<img src="project_bookdown_files/figure-html/unnamed-chunk-28-2.png" width="672" />

In the plots, the points represent individual tanks, while the bars represent the average fitness index of those tanks.

There is *almost* a unimodal patterned response to temperature, but fitness is a bit higher in the highest temp. This is surprising, because other research from my lab has shown *Pteronarcys californica* has an optimal temperature of about 14-16, and really marginal to no survival at high temperatures like 22C. No clear trends in response to sulfate levels. 


<!--chapter:end:03-fit_index.Rmd-->

