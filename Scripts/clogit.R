# Preliminaries
rm(list = ls(all = TRUE))             # Clear workspace
Sys.setenv(LANG = "en")               # Set language
setwd("C:/Users/Max/Documents/ThesisMax")                            # Set working directory 

# Loading R packages
library(survival)                       # enables the estimation of the conditional logit model (clogit)
library(haven)                          # allows to read stata data files
library(support.CEs)                    # for mwtp

data.clogit <- read_stata("CE_distance_urban_v2.dta") 

#generating id.var
data.clogit$case <- ((data.clogit$DLNMID-72000000)*10) + data.clogit$Q10_Pair

#converting price variable
data.clogit$Price_real<-0
data.clogit$Price_real[data.clogit$Price == 6] <- 120
data.clogit$Price_real[data.clogit$Price == 5] <- 80
data.clogit$Price_real[data.clogit$Price == 4] <- 50
data.clogit$Price_real[data.clogit$Price == 3] <- 20
data.clogit$Price_real[data.clogit$Price == 2] <- 10
data.clogit$Price_real[data.clogit$Price == 1] <- 2.50

#creating asc
data.clogit$asc1 <- 0
data.clogit$asc1[data.clogit$Q10_AorBorSQ == 1] <- 1
data.clogit$asc2 <- 0
data.clogit$asc2[data.clogit$Q10_AorBorSQ == 2] <- 1

#conditional logit estimation

results.clogit.basic <- clogit(Choicedummy ~ asc1 + asc2 + sandy_cycling_track + shelly_cycling_track + natural_forest + private_management + regional_management + local_management + Price_real +strata(case), 
                  data = data.clogit,
                  subset = data.clogit$Q10_Loc == 2
                  )

                  
summary(results.clogit.basic)

#WTP

mwtp(output = results.clogit.basic, monetary.variables = c("Price_real"), nonmonetary.variables = c("sandy_cycling_track", "shelly_cycling_track", "natural_forest", "private_management", "local_management" ), confidence.level = 0.95, seed = 234)

#linear distance

data.clogit$Price_distance <- data.clogit$Price_real*data.clogit$distance

#conditional logit with linear distance estimation

results.clogit.lineardistance <- clogit(Choicedummy ~ asc1 + asc2 + sandy_cycling_track + shelly_cycling_track + natural_forest + private_management + regional_management + local_management + Price_real + Price_distance +strata(case), 
                               data = data.clogit,
                               subset = data.clogit$Q10_Loc == 2
)


summary(results.clogit.lineardistance)

#quadratic distance

data.clogit$distance2 <- data.clogit$distance * data.clogit$distance
data.clogit$Price_distance2 <- data.clogit$Price_real*data.clogit$distance2

#conditional logit with quadratic distance estimation

results.clogit.quadratic_distance <- clogit(Choicedummy ~ asc1 + asc2 + sandy_cycling_track + shelly_cycling_track + natural_forest + private_management + regional_management + local_management + Price_real + Price_distance + Price_distance2 +strata(case), 
                                        data = data.clogit,
                                        subset = data.clogit$Q10_Loc == 2
)


summary(results.clogit.quadratic_distance)

#log distance

data.clogit$log_distance <- log(data.clogit$distance)
data.clogit$Price_log_distance <- data.clogit$Price_real*data.clogit$log_distance

#conditional logit with log distance estimation

results.clogit.log_distance <- clogit(Choicedummy ~ asc1 + asc2 + sandy_cycling_track + shelly_cycling_track + natural_forest + private_management + regional_management + local_management + Price_real + Price_log_distance +strata(case), 
                                        data = data.clogit,
                                        subset = data.clogit$Q10_Loc == 2
)


summary(results.clogit.log_distance)