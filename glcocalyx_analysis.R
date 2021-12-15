library(tidyverse)
library(readxl)
library(magrittr)
library(lmerTest)

#Read in height and coverage files
glyco_h <- read_excel("glycocalyx_clean(height).xlsx", na = "no good/none")
glyco_c <- read_excel("glycocalyx_clean(coverage).xlsx", na = c("no good/none", "no good /none"))

#Filter out null rows
glyco_h <- filter(glyco_h, !is.na(Eye))
glyco_c <- filter(glyco_c, !is.na(Eye))

#Tidy the datasets
glyco_h <- glyco_h %>%
  pivot_longer(cols = -c(Eye, Region), names_to = "Outflow_Location", values_to = "height") %>%
  mutate(L_or_R = substr(Eye,5,5), monkey = substr(Eye,3,3)) %>%
  mutate(left = ifelse(L_or_R == "S",1,0)) %>%
  mutate(monkey = as.factor(monkey), Region = as.factor(Region), Outflow_Location = as.factor(Outflow_Location)) %>%
  select(Region, Outflow_Location, monkey, left, height) %>%
  filter(!is.na(height))

glyco_c <- glyco_c %>%
  pivot_longer(cols = -c(Eye, Region), names_to = "Outflow_Location", values_to = "coverage") %>%
  mutate(L_or_R = substr(Eye,5,5), monkey = substr(Eye,3,3)) %>%
  mutate(left = ifelse(L_or_R == "S",1,0)) %>%
  mutate(monkey = as.factor(monkey), Region = as.factor(Region), Outflow_Location = as.factor(Outflow_Location)) %>%
  select(Region, Outflow_Location, monkey, left, coverage) %>%
  filter(!is.na(coverage))

#Write to files to export
# write_csv(glyco_h, file = "Glycocalyx_Heights_Cleaned.csv")
# write_csv(glyco_c, file = "Glycocalyx_Coverages_Cleaned.csv")

#First attempts at multilevel with region-location interaction
fit_h <- lmer(log(height) ~ 1 + Region * Outflow_Location + left + (1|monkey), data = glyco_h)
summary(fit_h)
plot(fit_h)

fit_c <- lmer(log(coverage) ~ 1 +  Region*Outflow_Location + left + (1|monkey), data = glyco_c)
summary(fit_c)
plot(fit_c)

#We could interpret coefficients from this, but it may be hard
sfit_c <- stan_lmer(log(coverage) ~ 1 +  Region*Outflow_Location + left + (1|monkey), data = glyco_c)

#Model to show results by plotting
sfit_c2 <- stan_lmer(coverage ~ 1 +Region+Outflow_Location+  (1|Region:Outflow_Location) + left + (1|monkey), data = glyco_c)
plot(sfit_c2)
postsample=as.matrix(sfit_c)

sfit_h2 <- stan_lmer(log(height) ~ 1 +Region+Outflow_Location+  (1|Region:Outflow_Location) + left + (1|monkey), data = glyco_h)
plot(sfit_h2)

#We could add together all the coeffs to get results, or we can give random intercepts to the interaction
#By doing the second version, we can plot to see coeff estimates. 
#We mainly see that Control High Flow and Control Low Flow regions seem to differ from Control and non-lasered
#Can also point out that Non-lasered SC is closest to significant when comparing within region, especially against Control SC.










#Deprecated: Don't use this code
# height_SC <- filter(glyco_h, Outflow_Location == "SC")
# fit <- lmer(height ~ Region + left + (1|monkey), data = height_SC)
# plot(fit)
# 
# locs <- unique(glyco_h$Outflow_Location)
# regs <- unique(glyco_h$Region)
# 
# #Heights-RegComp: CC Nonlasered, ISV HF, ISV LF
# #Coverages-RegComp: SC LF, SC Nonlasered
# #Heights-LocComp: Control Corneo, Control SC, Control TM, Control Uveal, Nonlasered ALL, HF All but ISV, LF All but ISV
# #Coverages-LocComp: Nonlasered ESV, Nonlasered SC, LF Corneo, LF ESV, LF SC
# 
# for(loc in locs){
#   data <- filter(glyco_h, Outflow_Location == loc)
#   fit <- lmer(log(height) ~ Region + left + (1|monkey), data = data)
#   print("------------------------------------------------------------")
#   print(loc)
#   print(summary(fit))
#   print(plot(fit, main = paste("Heights: Comparing Regions in Location",loc, sep = " ")))
# }
# 
# for(loc in locs){
#   data <- filter(glyco_c, Outflow_Location == loc)
#   fit <- lmer(log(coverage) ~ Region + left + (1|monkey), data = data)
#   print("------------------------------------------------------------")
#   print(loc)
#   print(summary(fit))
#   print(plot(fit, main = paste("Coverages: Comparing Regions in Location",loc, sep = " ")))
# }
# 
# for(reg in regs){
#   data <- filter(glyco_h, Region == reg)
#   fit <- lmer(log(height) ~ Outflow_Location + left + (1|monkey), data = data)
#   print("------------------------------------------------------------")
#   print(reg)
#   print(summary(fit))
#   print(plot(fit, main = paste("Heights: Comparing Locations in Region",reg, sep = " ")))
# }
# 
# 
# for(reg in regs){
#   data <- filter(glyco_c, Region == reg)
#   fit <- lmer(log(coverage) ~ Outflow_Location + left + (1|monkey), data = data)
#   print("------------------------------------------------------------")
#   print(reg)
#   print(summary(fit))
#   print(plot(fit, main = paste("Coverages: Comparing Locations in Region",reg, sep = " ")))
# }

