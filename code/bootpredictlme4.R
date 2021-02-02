# http://www.remkoduursma.com/post/2017-06-15-bootpredictlme4/

# # package setup, only needed for first session
# install.packages("remotes")
# remotes::install_github("RemkoDuursma/bootpredictlme4")

# a model object (e.g., m2) and a data frame (e.g., df2) need to exist

library(bootpredictlme4)
library(ggplot2)

options(bootnsim = 100) # takes awhile to run
boot.m2 <- predict(m2, re.form = NA, se.fit = T)

bootFit <- boot.m2[1]$fit # fitted values
bootCI <- as.data.frame(matrix(boot.m2$ci.fit, ncol=2, byrow=TRUE)) # 95% CI
colnames(bootCI) <- c("bootLCL", "bootUCL")
# str(bootCI)

bootdf <- as.data.frame(cbind(df2, bootFit, bootCI))
# str(bootdf)

# plot observations, population-averaged regressions and cup-level regressions----
# it is OK to model using sampling_date_scaled and to plot using sampling_date
ggplot(data=bootdf) +
  geom_point(aes(x = sampling_date, y = length, color = Cup)) +
  geom_line(aes(x = sampling_date, y = yhat_pa)) +
  geom_line(aes(x = sampling_date, y = yhat, color = Cup), linetype = "dashed") +
  geom_ribbon(aes(x = sampling_date, ymin = bootLCL, ymax = bootUCL), alpha=0.1) +
  facet_wrap(~ Trt) +
  theme(legend.position = "none")

