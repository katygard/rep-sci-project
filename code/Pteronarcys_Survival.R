setwd("C:/Users/kmg31/Box/Katy/Research/Experiments/Pteronarcys_Sulfate") #laptop working directory

# import data----
head.wide <- read.csv("Ptery_Sulfate Growth.csv")
survival.wide <- read.csv("Pteronarcys Sulfate Survival.csv")

survival.long <- gather(survival.wide, sdate, survival, na.rm = F, X28.Jul:X5.Oct)

library(ggplot2)
library(viridis)
library(dplyr)
library(plotly)
library(ggpubr)


survival.long$Sulfate.mg.L <- as.character(survival.long$Sulfate.mg.L)

survival.long <- survival.long %>%
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
                                X19.Oct = "2020-10-19"))


with(survival.long, table(sdate2, sdate))


colors <- viridis(10)

#aug31 survival surface plot

prep <- survival.wide[order(survival.wide$Sulfate.mg.L),]
prep1 <- prep[order(prep$Temperature),]

temp <- seq(12, 22, 2)
sulf <- seq(0, 1000, 200)

z <- matrix(prep1$X31.Aug, 6, 6)

rownames(z) <- sulf
colnames(z) <- temp

surf_aug31 <- plot_ly(type = "surface", z=z, x=temp, y=sulf)
surf_aug31 <- surf_aug31 %>% layout(scene = list(xaxis = list(title = 'Temperature'),
                                                 yaxis = list(title = 'Sulfate mg/L'),
                                                 zaxis = list(title = '# Alive (Aug 31)')))

surf_aug31 

#save as html so interactive functionality is preserved
htmlwidgets::saveWidget(as_widget(surf_aug31), "surv_surf_aug31.html")

####oct19 survival surface plot

z <- matrix(prep1$X19.Oct, 6, 6)
rownames(z) <- sulf
colnames(z) <- temp

surf_oct19 <- plot_ly(type = "surface", z=z, x=temp, y=sulf)
surf_oct19 <- surf_oct19 %>% layout(scene= list(xaxis = list(title = 'Temperature'),
                                                yaxis = list(title = 'Sulfate mg/L'),
                                                zaxis = list(title = '# Alive (Oct 19)')))

surf_oct19

htmlwidgets::saveWidget(as_widget(surf_oct19), "surv_surf_oct19.html")


####  initial size surface plot

meansize <- aggregate(head.wide[,c(1,2,5)], list(head.wide$TANK), mean)
meansize

prep <- meansize[order(meansize$TRTMENT),]
prep1 <- prep[order(prep$TEMP),]



z <- matrix(prep1$X28.Jul, 6, 6)
rownames(z) <- sulf
colnames(z) <- temp

surf_initsize <- plot_ly(type = "surface", z=z, x=temp, y=sulf)
surf_initsize <- surf_initsize %>% layout(scene= list(xaxis = list(title = 'Temperature'),
                                                yaxis = list(title = 'Sulfate mg/L'),
                                                zaxis = list(title = 'Head Width (mm)')))

surf_initsize

htmlwidgets::saveWidget(as_widget(surf_initsize), "size_surf_28Jul.html")

#####surface plot of mean head size 19 Oct

meansize <- aggregate(head.wide[,c(1,2,11)], list(head.wide$TANK), mean, na.rm = TRUE)
meansize

prep <- meansize[order(meansize$TRTMENT),]
prep1 <- prep[order(prep$TEMP),]



z <- matrix(prep1$X19.Oct, 6, 6)
rownames(z) <- sulf
colnames(z) <- temp

size_surf_oct19 <- plot_ly(type = "surface", z=z, x=temp, y=sulf)
size_surf_oct19 <- size_surf_oct19 %>% layout(scene= list(xaxis = list(title = 'Temperature'),
                                                      yaxis = list(title = 'Sulfate mg/L'),
                                                      zaxis = list(title = 'Head Width (mm)')))

size_surf_oct19

htmlwidgets::saveWidget(as_widget(size_surf_oct19), "size_surf_19Oct.html")

#####surface plot of mean head size 16 Nov

meansize <- aggregate(head.wide[,c(1,2,13)], list(head.wide$TANK), mean, na.rm = TRUE)
meansize

prep <- meansize[order(meansize$TRTMENT),]
prep1 <- prep[order(prep$TEMP),]



z <- matrix(prep1$X16.Nov, 6, 6)
rownames(z) <- sulf
colnames(z) <- temp

size_surf_nov16 <- plot_ly(type = "surface", z=z, x=temp, y=sulf)
size_surf_nov16 <- size_surf_nov16 %>% layout(scene= list(xaxis = list(title = 'Temperature'),
                                                          yaxis = list(title = 'Sulfate mg/L'),
                                                          zaxis = list(title = 'Head Width (mm)')))

size_surf_nov16

htmlwidgets::saveWidget(as_widget(size_surf_nov16), "size_surf_16Nov.html")

######growth plots

#oct5

head.wide$j_e <- (head.wide$X5.Oct - head.wide$X28.Jul)
head.wide$growth <- (head.wide$j_e/head.wide$X28.Jul)

growth <- aggregate(head.wide[,c(1,2,16)], list(head.wide$TANK), mean, na.rm = TRUE) #third number in c() will change with time, should be growth
growth

prep <- growth[order(growth$TRTMENT),]
prep1 <- prep[order(prep$TEMP),]

z <- matrix(prep1$growth, 6, 6)
rownames(z) <- sulf
colnames(z) <- temp

growth_surf_oct5 <- plot_ly(type = "surface", z=z, x=temp, y=sulf)
growth_surf_oct5 <- growth_surf_oct5 %>% layout(scene= list(xaxis = list(title = 'Temperature'),
                                                          yaxis = list(title = 'Sulfate mg/L'),
                                                          zaxis = list(title = 'Growth (mm)')))

growth_surf_oct5

htmlwidgets::saveWidget(as_widget(growth_surf_oct5), "growth_surf_5Oct.html")

#oct19

head.wide$j_e <- (head.wide$X19.Oct - head.wide$X28.Jul)
head.wide$growth <- (head.wide$j_e/head.wide$X28.Jul)

growth <- aggregate(head.wide[,c(1,2,16)], list(head.wide$TANK), mean, na.rm = TRUE) #third number in c() will change with time, should be growth
growth

prep <- growth[order(growth$TRTMENT),]
prep1 <- prep[order(prep$TEMP),]

z <- matrix(prep1$growth, 6, 6)
rownames(z) <- sulf
colnames(z) <- temp

growth_surf_oct19 <- plot_ly(type = "surface", z=z, x=temp, y=sulf)
growth_surf_oct19 <- growth_surf_oct19 %>% layout(scene= list(xaxis = list(title = 'Temperature'),
                                                            yaxis = list(title = 'Sulfate mg/L'),
                                                            zaxis = list(title = 'Growth (mm)')))

growth_surf_oct19

htmlwidgets::saveWidget(as_widget(growth_surf_oct19), "growth_surf_19Oct.html")


#nov2

head.wide$j_e <- (head.wide$X2.Nov - head.wide$X28.Jul)
head.wide$growth <- (head.wide$j_e/head.wide$X28.Jul)

growth <- aggregate(head.wide[,c(1,2,16)], list(head.wide$TANK), mean, na.rm = TRUE)
growth

prep <- growth[order(growth$TRTMENT),]
prep1 <- prep[order(prep$TEMP),]

z <- matrix(prep1$growth, 6, 6)
rownames(z) <- sulf
colnames(z) <- temp

growth_surf_nov2 <- plot_ly(type = "surface", z=z, x=temp, y=sulf)
growth_surf_nov2 <- growth_surf_nov2 %>% layout(scene= list(xaxis = list(title = 'Temperature'),
                                                              yaxis = list(title = 'Sulfate mg/L'),
                                                              zaxis = list(title = 'Growth (mm)')))

growth_surf_nov2

htmlwidgets::saveWidget(as_widget(growth_surf_nov2), "growth_surf_nov2.html")


#nov16

head.wide$j_e <- (head.wide$X16.Nov - head.wide$X28.Jul)
head.wide$growth <- (head.wide$j_e/head.wide$X28.Jul)

growth <- aggregate(head.wide[,c(1,2,16)], list(head.wide$TANK), mean, na.rm = TRUE)
growth

prep <- growth[order(growth$TRTMENT),]
prep1 <- prep[order(prep$TEMP),]

z <- matrix(prep1$growth, 6, 6)
rownames(z) <- sulf
colnames(z) <- temp

growth_surf_nov16 <- plot_ly(type = "surface", z=z, x=temp, y=sulf)
growth_surf_nov16 <- growth_surf_nov16 %>% layout(scene= list(xaxis = list(title = 'Temperature'),
                                                              yaxis = list(title = 'Sulfate mg/L'),
                                                              zaxis = list(title = 'Growth (mm)')))

growth_surf_nov16

htmlwidgets::saveWidget(as_widget(growth_surf_nov16), "growth_surf_nov16.html")

#nov30

head.wide$j_e <- (head.wide$X30.Nov - head.wide$X28.Jul)
head.wide$growth <- (head.wide$j_e/head.wide$X28.Jul)

growth <- aggregate(head.wide[,c(1,2,16)], list(head.wide$TANK), mean, na.rm = TRUE)
growth

prep <- growth[order(growth$TRTMENT),]
prep1 <- prep[order(prep$TEMP),]

z <- matrix(prep1$growth, 6, 6)
rownames(z) <- sulf
colnames(z) <- temp

growth_surf_nov30 <- plot_ly(type = "surface", z=z, x=temp, y=sulf)
growth_surf_nov30 <- growth_surf_nov30 %>% layout(scene= list(xaxis = list(title = 'Temperature'),
                                                              yaxis = list(title = 'Sulfate mg/L'),
                                                              zaxis = list(title = 'Growth (mm)')))

growth_surf_nov30

htmlwidgets::saveWidget(as_widget(growth_surf_nov30), "growth_surf_nov30.html")



##------ 3D SCATTER PLOT CODE BELOW -------------------------
#3d scatter plot of Aug31 survival against temp and treatment

survival.wide$X31.Aug <- as.numeric(survival.wide)
fig.aug31 <- plot_ly(x=survival.wide$Temperature, y=survival.wide$Sulfate.mg.L, z=survival.wide$X31.Aug, 
                     type="scatter3d", mode="markers", color=survival.wide$X31.Aug)
fig.aug31 <- fig.aug31 %>% add_markers()
fig.aug31 <- fig.aug31 %>% add_surface(fig.aug31, z = survival.wide$X31.Aug)
fig.aug31 <- fig.aug31 %>% layout(scene = list(xaxis = list(title = 'Temperature'),
                                         yaxis = list(title = 'Sulfate mg/L'),
                                         zaxis = list(title = '# Alive (Aug 31)')))

fig.aug31

#save as html so interactive functionality is preserved
htmlwidgets::saveWidget(as_widget(fig.aug31), "aug31survival.html")

#3d plot of latest (19 Oct) survival against temp and treatment

fig.oct19 <- plot_ly(x=survival.wide$Temperature, y=survival.wide$Sulfate.mg.L, z=survival.wide$X19.Oct, 
               type="scatter3d", mode="markers", color=survival.wide$X19.Oct)
fig.oct19 <- fig.oct19 %>% add_markers()
fig.oct19 <- fig.oct19 %>% layout(scene = list(xaxis = list(title = 'Temperature'),
                                   yaxis = list(title = 'Sulfate mg/L'),
                                   zaxis = list(title = '# Alive (Oct 19)',
                                                range=c(1,10))))
fig.oct19

htmlwidgets::saveWidget(as_widget(fig.oct19), "oct19survival.html")


#run a two-way anova w/ temp and treatment as factors on initial size
head.wide$TRTMENT_f <- factor(head.wide$TRTMENT)
head.wide$TEMP_f <- factor(head.wide$TEMP)

table(head.wide$TRTMENT_f, head.wide$TEMP_f)

#data vis of initial size 

colors <- viridis(6)
init.size <- ggboxplot(head.wide, x = "TEMP_f", y = "X28.Jul", color = "TRTMENT_f",
          palette = colors, xlab = "Temperature", ylab = "Initial Head Size (mm)") 
ggpar(init.size, legend.title = "Sulfate mg/L")

#2-way anova w/ interaction
init.anova <- aov(X28.Jul ~ TRTMENT_f * TEMP_f, data = head.wide)
summary(init.anova)

#3d plot of initial mean head size vs temp and treatment
meansize <- aggregate(head.wide[,c(1,2,5)], list(head.wide$TANK), mean)
meansize

head.28jul <- plot_ly(x=meansize$TEMP, y=meansize$TRTMENT, z=meansize$X28.Jul, 
                     type="scatter3d", mode="markers", color = meansize$X28.Jul)
head.28jul <- head.28jul %>% layout(scene = list(xaxis = list(title = 'Temperature'),
                                               yaxis = list(title = 'Sulfate mg/L'),
                                               zaxis = list(title = 'Head Width (mm)')))

head.28jul
htmlwidgets::saveWidget(as_widget(head.28jul), "initialmeansize.html")



#two-way anova of mean head size now (19 Oct)
oct19.anova <- aov(X19.Oct ~ TRTMENT_f * TEMP_f, data = head.wide)
summary(oct19.anova)

#3d plot of mean head size now (19 Oct)

meansize_oct19 <- aggregate(head.wide[,c(1,2,11)], list(head.wide$TANK), mean, na.rm = TRUE)
meansize_oct19

head.19oct <- plot_ly(x=meansize_oct19$TEMP, y=meansize_oct19$TRTMENT, z=meansize_oct19$X19.Oct, 
                      type="scatter3d", mode="markers", color = meansize_oct19$X19.Oct)
head.19oct <- head.19oct %>% layout(scene = list(xaxis = list(title = 'Temperature'),
                                                 yaxis = list(title = 'Sulfate mg/L'),
                                                 zaxis = list(title = 'Head Width (mm)')))
head.19oct

htmlwidgets::saveWidget(as_widget(head.19oct), "oct19_meansize.html")

#growth plots

#oct5

head.wide$j_e <- (head.wide$X5.Oct - head.wide$X28.Jul)
head.wide$growth <- (head.wide$j_e/head.wide$X28.Jul)

growth <- aggregate(head.wide[,c(1,2,14)], list(head.wide$TANK), mean, na.rm = TRUE)
growth

growth.oct5 <- plot_ly(x=growth$TEMP, y=growth$TRTMENT, z=growth$growth,
                        type="scatter3d", mode="markers", color = growth$growth)
growth.oct5 <- growth.oct5 %>% layout(scene = list(xaxis = list(title = 'Temperature'),
                                                     yaxis = list(title = 'Sulfate mg/L'),
                                                     zaxis = list(title = 'Growth (mm)')))
growth.oct5

htmlwidgets::saveWidget(as_widget(growth.oct19), "oct5_growth.html")

#two-way anova of growth
growth.anova.5oct <- aov(growth ~ TRTMENT_f * TEMP_f, data = head.wide)
summary(growth.anova.5oct)


#oct19
head.wide$j_e <- (head.wide$X19.Oct - head.wide$X28.Jul)
head.wide$growth <- (head.wide$j_e/head.wide$X28.Jul)

growth <- aggregate(head.wide[,c(1,2,14)], list(head.wide$TANK), mean, na.rm = TRUE)
growth

growth.oct19 <- plot_ly(x=growth$TEMP, y=growth$TRTMENT, z=growth$growth,
                        type="scatter3d", mode="markers", color = growth$growth)
growth.oct19 <- growth.oct19 %>% layout(scene = list(xaxis = list(title = 'Temperature'),
                                                     yaxis = list(title = 'Sulfate mg/L'),
                                                     zaxis = list(title = 'Growth (mm)')))
growth.oct19

htmlwidgets::saveWidget(as_widget(growth.oct19), "oct19_growth.html")

#two-way anova of growth
growth.anova <- aov(growth ~ TRTMENT_f * TEMP_f, data = head.wide)
summary(growth.anova)
summary(growth.anova.5oct)



##----------------
#old code, not in use
###ggplot(survival.long, aes(x=sdate2, y=survival, group=Tank, color=Temperature, lty=Sulfate.mg.L)) + 
  ###geom_point() +
  ###geom_line() +
  ###scale_color_viridis(discrete = FALSE)

