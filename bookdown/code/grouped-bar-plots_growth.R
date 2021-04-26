library(ggplot2)
library(viridis)
library(dplyr)
library(plotly)
library(ggpubr)
library(tidyr)


# import head capsule width growth data
head.wide <- read.csv("Ptery_Sulfate Growth.csv")
##file above renamed and moved, fix

###OCTOBER 19
#create grouped bar plots for oct 19
head.wide$j_e <- (head.wide$X19.Oct - head.wide$X28.Jul)
head.wide$growth <- (head.wide$j_e/head.wide$X28.Jul)

growth <- aggregate(head.wide[,c(1,2,17)], list(head.wide$TANK), mean, na.rm = TRUE) #third column # c() increases with each new week of data
growth

growth$TEMP <- as.factor(growth$TEMP)
growth$TRTMENT <- as.factor(growth$TRTMENT)

# Grouped by treatment, oct 19
ggplot(growth, aes(fill=TEMP, y=growth, x=TRTMENT)) + 
  geom_bar(position="dodge", stat="identity")

# grouped by temperature, oct 19
ggplot(growth, aes(fill=TRTMENT, y=growth, x=TEMP)) + 
  geom_bar(position="dodge", stat="identity")

###NOVEMBER 2
#create grouped bar plots for nov 2
head.wide$j_e <- (head.wide$X2.Nov - head.wide$X28.Jul)
head.wide$growth <- (head.wide$j_e/head.wide$X28.Jul)

growth <- aggregate(head.wide[,c(1,2,17)], list(head.wide$TANK), mean, na.rm = TRUE) #third column # c() increases with each new week of data
growth

growth$TEMP <- as.factor(growth$TEMP)
growth$TRTMENT <- as.factor(growth$TRTMENT)

# Grouped by treatment, nov 2
ggplot(growth, aes(fill=TEMP, y=growth, x=TRTMENT)) + 
  geom_bar(position="dodge", stat="identity")

# grouped by temperature, nov 2
ggplot(growth, aes(fill=TRTMENT, y=growth, x=TEMP)) + 
  geom_bar(position="dodge", stat="identity")


###NOVEMBER 16
#create grouped bar plots for nov 16
head.wide$j_e <- (head.wide$X16.Nov - head.wide$X28.Jul)
head.wide$growth <- (head.wide$j_e/head.wide$X28.Jul)

growth <- aggregate(head.wide[,c(1,2,17)], list(head.wide$TANK), mean, na.rm = TRUE) #third column # c() increases with each new week of data
growth

growth$TEMP <- as.factor(growth$TEMP)
growth$TRTMENT <- as.factor(growth$TRTMENT)

# Grouped by treatment, nov 16
ggplot(growth, aes(fill=TEMP, y=growth, x=TRTMENT)) + 
  geom_bar(position="dodge", stat="identity")

# grouped by temperature, nov 16
ggplot(growth, aes(fill=TRTMENT, y=growth, x=TEMP)) + 
  geom_bar(position="dodge", stat="identity")


###NOVEMBER 30
#create grouped bar plots for nov 30
head.wide$j_e <- (head.wide$X30.Nov - head.wide$X28.Jul)
head.wide$growth <- (head.wide$j_e/head.wide$X28.Jul)

growth <- aggregate(head.wide[,c(1,2,17)], list(head.wide$TANK), mean, na.rm = TRUE) #third column # c() increases with each new week of data
growth

growth$TEMP <- as.factor(growth$TEMP)
growth$TRTMENT <- as.factor(growth$TRTMENT)

# Grouped by treatment, nov 30
ggplot(growth, aes(fill=TEMP, y=growth, x=TRTMENT)) + 
  geom_bar(position="dodge", stat="identity")

# grouped by temperature, nov 30
ggplot(growth, aes(fill=TRTMENT, y=growth, x=TEMP)) + 
  geom_bar(position="dodge", stat="identity")


###december 14
#create grouped bar plots for dec 14
head.wide$j_e <- (head.wide$X14.Dec - head.wide$X28.Jul)
head.wide$growth <- (head.wide$j_e/head.wide$X28.Jul)

growth <- aggregate(head.wide[,c(1,2,17)], list(head.wide$TANK), mean, na.rm = TRUE) #third column # c() increases with each new week of data
growth

growth$TEMP <- as.factor(growth$TEMP)
growth$TRTMENT <- as.factor(growth$TRTMENT)

# Grouped by treatment, dec 14
ggplot(growth, aes(fill=TEMP, y=growth, x=TRTMENT)) + 
  geom_bar(position="dodge", stat="identity")

# grouped by temperature, dec 14
ggplot(growth, aes(fill=TRTMENT, y=growth, x=TEMP)) + 
  geom_bar(position="dodge", stat="identity")


###----averaged bar plots, by temp

###OCTOBER 19
#create grouped bar plots for oct 19
head.wide$j_e <- (head.wide$X19.Oct - head.wide$X28.Jul)
head.wide$growth <- (head.wide$j_e/head.wide$X28.Jul)

growth <- aggregate(head.wide[,c(1,2,17)], list(head.wide$TEMP), mean, na.rm = TRUE) #third column # c() increases with each new week of data
growth

growth$TEMP <- as.factor(growth$TEMP)
growth$TRTMENT <- as.factor(growth$TRTMENT)

ggplot(growth, aes(y=growth, x=TEMP, fill=TEMP)) +
    geom_bar(position="dodge", stat = "identity") +
    scale_fill_viridis_d()


###Nov 2
#create grouped bar plots for oct 2
head.wide$j_e <- (head.wide$X2.Nov - head.wide$X28.Jul)
head.wide$growth <- (head.wide$j_e/head.wide$X28.Jul)

growth <- aggregate(head.wide[,c(1,2,17)], list(head.wide$TEMP), mean, na.rm = TRUE) #third column # c() increases with each new week of data
growth

growth$TEMP <- as.factor(growth$TEMP)
growth$TRTMENT <- as.factor(growth$TRTMENT)

ggplot(growth, aes(y=growth, x=TEMP, fill=TEMP)) +
  geom_bar(position="dodge", stat = "identity") +
  scale_fill_viridis_d()


###Nov 16
#create grouped bar plots for nov 16
head.wide$j_e <- (head.wide$X16.Nov - head.wide$X28.Jul)
head.wide$growth <- (head.wide$j_e/head.wide$X28.Jul)

growth <- aggregate(head.wide[,c(1,2,17)], list(head.wide$TEMP), mean, na.rm = TRUE) #third column # c() increases with each new week of data
growth

growth$TEMP <- as.factor(growth$TEMP)
growth$TRTMENT <- as.factor(growth$TRTMENT)

ggplot(growth, aes(y=growth, x=TEMP, fill=TEMP)) +
  geom_bar(position="dodge", stat = "identity") +
  scale_fill_viridis_d()


###Nov 30
#create grouped bar plots for nov 30
head.wide$j_e <- (head.wide$X30.Nov - head.wide$X28.Jul)
head.wide$growth <- (head.wide$j_e/head.wide$X28.Jul)

growth <- aggregate(head.wide[,c(1,2,17)], list(head.wide$TEMP), mean, na.rm = TRUE) #third column # c() increases with each new week of data
growth

growth$TEMP <- as.factor(growth$TEMP)
growth$TRTMENT <- as.factor(growth$TRTMENT)

ggplot(growth, aes(y=growth, x=TEMP, fill=TEMP)) +
  geom_bar(position="dodge", stat = "identity") +
  scale_fill_viridis_d()


###Dec 14
#create grouped bar plots for dec 14
head.wide$j_e <- (head.wide$X14.Dec - head.wide$X28.Jul)
head.wide$growth <- (head.wide$j_e/head.wide$X28.Jul)

growth <- aggregate(head.wide[,c(1,2,17)], list(head.wide$TEMP), mean, na.rm = TRUE) #third column # c() increases with each new week of data
growth

growth$TEMP <- as.factor(growth$TEMP)
growth$TRTMENT <- as.factor(growth$TRTMENT)

ggplot(growth, aes(y=growth, x=TEMP, fill=TEMP)) +
  geom_bar(position="dodge", stat = "identity") +
  scale_fill_viridis_d()




###----averaged bar plots, by treatment

###OCTOBER 19
#create grouped bar plots for oct 19
head.wide$j_e <- (head.wide$X19.Oct - head.wide$X28.Jul)
head.wide$growth <- (head.wide$j_e/head.wide$X28.Jul)

growth <- aggregate(head.wide[,c(1,2,17)], list(head.wide$TRTMENT), mean, na.rm = TRUE) #third column # c() increases with each new week of data
growth

growth$TEMP <- as.factor(growth$TEMP)
growth$TRTMENT <- as.factor(growth$TRTMENT)

ggplot(growth, aes(y=growth, x=TRTMENT, fill=TRTMENT)) +
  geom_bar(position="dodge", stat = "identity") +
  scale_fill_viridis_d()


###Nov 2
#create grouped bar plots for oct 2
head.wide$j_e <- (head.wide$X2.Nov - head.wide$X28.Jul)
head.wide$growth <- (head.wide$j_e/head.wide$X28.Jul)

growth <- aggregate(head.wide[,c(1,2,17)], list(head.wide$TRTMENT), mean, na.rm = TRUE) #third column # c() increases with each new week of data
growth

growth$TEMP <- as.factor(growth$TEMP)
growth$TRTMENT <- as.factor(growth$TRTMENT)

ggplot(growth, aes(y=growth, x=TRTMENT, fill=TRTMENT)) +
  geom_bar(position="dodge", stat = "identity") +
  scale_fill_viridis_d()


###Nov 16
#create grouped bar plots for nov 16
head.wide$j_e <- (head.wide$X16.Nov - head.wide$X28.Jul)
head.wide$growth <- (head.wide$j_e/head.wide$X28.Jul)

growth <- aggregate(head.wide[,c(1,2,17)], list(head.wide$TRTMENT), mean, na.rm = TRUE) #third column # c() increases with each new week of data
growth

growth$TEMP <- as.factor(growth$TEMP)
growth$TRTMENT <- as.factor(growth$TRTMENT)

ggplot(growth, aes(y=growth, x=TRTMENT, fill=TRTMENT)) +
  geom_bar(position="dodge", stat = "identity") +
  scale_fill_viridis_d()


###Nov 30
#create grouped bar plots for nov 30
head.wide$j_e <- (head.wide$X30.Nov - head.wide$X28.Jul)
head.wide$growth <- (head.wide$j_e/head.wide$X28.Jul)

growth <- aggregate(head.wide[,c(1,2,17)], list(head.wide$TRTMENT), mean, na.rm = TRUE) #third column # c() increases with each new week of data
growth

growth$TEMP <- as.factor(growth$TEMP)
growth$TRTMENT <- as.factor(growth$TRTMENT)

ggplot(growth, aes(y=growth, x=TRTMENT, fill=TRTMENT)) +
  geom_bar(position="dodge", stat = "identity") +
  scale_fill_viridis_d()


###Dec 14
#create grouped bar plots for dec 14
head.wide$j_e <- (head.wide$X14.Dec - head.wide$X28.Jul)
head.wide$growth <- (head.wide$j_e/head.wide$X28.Jul)

growth <- aggregate(head.wide[,c(1,2,17)], list(head.wide$TRTMENT), mean, na.rm = TRUE) #third column # c() increases with each new week of data
growth

growth$TEMP <- as.factor(growth$TEMP)
growth$TRTMENT <- as.factor(growth$TRTMENT)

ggplot(growth, aes(y=growth, x=TRTMENT, fill=TRTMENT)) +
  geom_bar(position="dodge", stat = "identity") +
  scale_fill_viridis_d()


#------MEAN WEEKLY GROWTH - TEMP
library(tidyr)
library(dplyr)
head.long <- gather(head.wide, sdate, headsize, na.rm = T, X28.Jul:X14.Dec)


head.wide$j_e <- (head.wide$X19.Oct - head.wide$X28.Jul)
head.wide$growth19Oct <- (head.wide$j_e/head.wide$X28.Jul)
head.wide$j_e <- (head.wide$X2.Nov - head.wide$X28.Jul)
head.wide$growth2Nov <- (head.wide$j_e/head.wide$X28.Jul)
head.wide$j_e <- (head.wide$X16.Nov - head.wide$X28.Jul)
head.wide$growth16Nov <- (head.wide$j_e/head.wide$X28.Jul)
head.wide$j_e <- (head.wide$X30.Nov - head.wide$X28.Jul)
head.wide$growth30Nov <- (head.wide$j_e/head.wide$X28.Jul)
head.wide$j_e <- (head.wide$X14.Dec - head.wide$X28.Jul)
head.wide$growth14Dec <- (head.wide$j_e/head.wide$X28.Jul)

growth.wide <- head.wide[,c(1,17,18,19,20,21)]
colnames(growth.wide)[2] <- "Oct.19"
colnames(growth.wide)[3] <- "Nov.2"
colnames(growth.wide)[4] <- "Nov.16"
colnames(growth.wide)[5] <- "Nov.30"
colnames(growth.wide)[6] <- "Dec.14"

growth.long <- gather(growth.wide, sdate, growth, na.rm = T, Oct.19:Dec.14)

growth.long$TEMP <- as.factor(growth.long$TEMP)

se <- function(x) sqrt(var(x)/length(x))

group <- growth.long %>% group_by(TEMP, sdate)
group
group_means <- summarize(group, means = mean(growth), se = se(growth))

group_means

growths<- group_means %>% group_by(TEMP)
growths
growths <- summarize(growths, mean = mean(means), se = se(means))
growths
