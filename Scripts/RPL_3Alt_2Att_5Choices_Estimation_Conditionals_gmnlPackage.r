###############################################################################
#   
#   Script for RPL estimation 
#   using gmnl package
#   3 Alternatives
#   2 Attributes
#   1 Choice occasion per individual
#
#   Petr Mariel (petr.mariel@ehu.eus)
#   25th June 2017
#
###############################################################################

# Preliminaries
rm(list = ls(all = TRUE))             # Clear workspace
Sys.setenv(LANG = "en")               # Set language
setwd(".")                            # Set working directory             
set.seed(12345)

# Loading R packages
library(mlogit)                       # enables the estimation of the multinomial logit
# models with individual and/or alternative specific variables
library(gmnl)                         # maximum simulated likelihood method for the
# estimation of multinomial logit models with random coefficients

# Reading the database "RPL_3Alt_2Att_5Choices_Data_Conditionals.txt" created by "RPL_3Alt_2Att_5Choices_GenerateRespones_Conditionals.r"
data.RPL <- read.table("RPL_3Alt_2Att_5Choices_Data_Conditionals.txt", header=TRUE) 

colnames(data.RPL)[which(colnames(data.RPL[1,]) == "alt1.attr1")] <- "attr1.1"
colnames(data.RPL)[which(colnames(data.RPL[1,]) == "alt1.attr2")] <- "attr2.1"
colnames(data.RPL)[which(colnames(data.RPL[1,]) == "alt2.attr1")] <- "attr1.2"
colnames(data.RPL)[which(colnames(data.RPL[1,]) == "alt2.attr2")] <- "attr2.2"
colnames(data.RPL)[which(colnames(data.RPL[1,]) == "alt3.attr1")] <- "attr1.3"
colnames(data.RPL)[which(colnames(data.RPL[1,]) == "alt3.attr2")] <- "attr2.3"

# reshaping data from wide to long format using mlogit.data from mlogit package
data.RPL.mlogit <- mlogit.data(data.RPL, 
                               id.var = "id.individual",  # identification of the individual
                               choice = "choice",         # choice variable
                               varying = 2:7,             # columns containing attributes
                               shape = "wide",            # wide or long shape of the data
                               sep = ".")                 # how the number of alternative is separated from the attribut name

# creating variables for ASCs
data.RPL.mlogit$asc1 <- as.numeric(data.RPL.mlogit$alt == 1)
data.RPL.mlogit$asc2 <- as.numeric(data.RPL.mlogit$alt == 2)

# RPL model estimation
rpl.output <- gmnl(choice ~ asc1 + asc2 + attr1 + attr2 | 0 ,
                  data    = data.RPL.mlogit,
                  model = 'mixl', 
                  R = 10,                                 # number of Halton draws 
                  panel = TRUE,
                  print.level = 3,
                  ranp = c(attr1 = "n",attr2 = "n"))      # distribution of the random coefficients
                                                          # 1) Normal = "n" 
                                                          # 2) truncated normal = "cn"
                                                          # 3) uniform = "u" 
                                                          # 4) triangular = "t" 
                                                          # 5) and Johnson's Sb = "sb".

# printing out the results
summary(rpl.output)
names(rpl.output)

########################################################################
# Conditional distributions
#######################################################################

bi.attr1 <- effect.gmnl(rpl.output, par = "attr1")
bi.attr2 <- effect.gmnl(rpl.output, par = "attr2")

########################################################################
# Draws from unconditional distribution
#######################################################################

# Random draws N(estimation.beta,estimation.st.dev.) for each random parameter
# For each individual
data.RPL$uncond.attr1 <- rpl.output$coefficients[3] + rpl.output$coefficients[5] * rnorm(200)
data.RPL$uncond.attr2 <- rpl.output$coefficients[4] + rpl.output$coefficients[6] * rnorm(200)


plot(data.RPL$rand.attr1.beta[seq(1,1000, by=5)],bi.attr1$mean,
     main="attr1",
     ylim=range(data.RPL$rand.attr1.beta, bi.attr1$mean),
     xlab="True coefficients",
     ylab="Unconditionals and Conditionals")
points(data.RPL$rand.attr1.beta, data.RPL$uncond.attr1, col='red') 

plot(data.RPL$rand.attr2.beta[seq(1,1000, by=5)],bi.attr2$mean,
     main="attr2",
     ylim=range(data.RPL$rand.attr2.beta, bi.attr2$mean),
     xlab="True coefficients",
     ylab="Unconditionals and Conditionals")
points(data.RPL$rand.attr2.beta, data.RPL$uncond.attr2, col='red') 

######################################################################
# RPL model estimation with correlated coefficients
######################################################################

rpl.output <- gmnl(choice ~ asc1 + asc2 + attr1 + attr2 | 0 ,
                   data    = data.RPL.mlogit,
                   model = 'mixl', 
                   R = 10,                                 # number of Halton draws 
                   panel = TRUE,
                   print.level = 3,
                   correlation = TRUE,
                   ranp = c(attr1 = "n",attr2 = "n"))      # distribution of the random coefficients
# 1) Normal = "n" 
# 2) truncated normal = "cn"
# 3) uniform = "u" 
# 4) triangular = "t" 
# 5) and Johnson's Sb = "sb".

# printing out the results
summary(rpl.output)

# We can recover the full variance-covariance matrix
# using the command cov.gmnl
cov.gmnl(rpl.output)
# ... and correlation matrix
cor.gmnl(rpl.output)

