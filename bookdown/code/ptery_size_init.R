setwd("C:/Users/kmg31/Box/Katy/Research/Experiments") #laptop working directory

ptery_measure <- read.csv("C:/Users/kmg31/Box/Katy/Research/Experiments/Pteronarcys Sulfate Initial Measurements.csv")

hist(ptery_measure$Head_width..mm., breaks = c(0.5, 0.75, 1.0, 1.25, 1.5, 1.75, 2.0, 2.25,
                                               2.5, 2.75, 3.0, 3.25, 3.5, 3.75, 4.0),
     main = NULL,
     xlab = "Head Width (mm)")

hist(ptery_measure$Length..mm., breaks = 10, main = NULL,
     xlab = "Length (mm)")

table(ptery_measure$Head_width..mm. < 2.25)
table(ptery_measure$Head_width..mm. < 1.5)

plot(ptery_measure$Head_width..mm. ~ ptery_measure$Length..mm.)


groups <- read.csv("C:/Users/kmg31/Box/Katy/Research/Experiments/Ptery_Sulfate Head Growth.csv")

stripchart(groups$TANK ~ groups$HEAD_WIDTH, pch = 16, ylab = "Head Width (mm)",
           las = 1, xlab = "Group" )

sd(groups$Head_width..mm.[1:10])/mean(groups$Head_width..mm.[1:10])*100
sd(groups$Head_width..mm.[11:20])/mean(groups$Head_width..mm.[11:20])*100
sd(groups$Head_width..mm.[21:30])/mean(groups$Head_width..mm.[21:30])*100
sd(groups$Head_width..mm.[31:40])/mean(groups$Head_width..mm.[31:40])*100
sd(groups$Head_width..mm.[41:50])/mean(groups$Head_width..mm.[41:50])*100
sd(groups$Head_width..mm.[51:60])/mean(groups$Head_width..mm.[51:60])*100

