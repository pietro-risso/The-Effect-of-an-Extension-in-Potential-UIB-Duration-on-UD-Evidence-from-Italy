# COPMPLETED SPELLS ANALYSIS - NON-PARAMETRIC REGRESSION




################################################################################

################################################################################




# LIBRARIES

library(tidyverse)
library(vroom)
library(stargazer)
library(tidyverse)  
library(broom)  
library(rdrobust)  
library(rddensity)  
library(modelsummary)  
library(rdd)
library(ggthemes)



################################################################################

################################################################################




### LOADING THE DATA

ASpI_14 <- vroom("ASpI_2014_completed_spells") 

ASpI_15 <- vroom("ASpI_2015_completed_spells") 




################################################################################

################################################################################




### CALCULATIONS FOR DESCRIPTIVE STATISTICS




stargazer(data.frame(ASpI_14))

stargazer(data.frame(ASpI_15))




# YOUNGER THAN 50 2014

mean(filter(ASpI_14, ETAM < 50)$DURRIC)
median(filter(ASpI_14, ETAM < 50)$DURRIC)
sd(filter(ASpI_14, ETAM < 50)$DURRIC)
nrow(filter(ASpI_14, ETAM < 50))




# BETWEEN 50 AND 54 2014

mean(filter(ASpI_14, ETAM > 49, ETAM < 55)$DURRIC)
median(filter(ASpI_14, ETAM > 49, ETAM < 55)$DURRIC)
sd(filter(ASpI_14, ETAM > 49, ETAM < 55)$DURRIC)
nrow(filter(ASpI_14, ETAM > 49, ETAM < 55))




# OLDER THAN 54 2015

mean(filter(ASpI_14, ETAM > 54)$DURRIC)
median(filter(ASpI_14, ETAM > 54)$DURRIC)
sd(filter(ASpI_14, ETAM > 54)$DURRIC)
nrow(filter(ASpI_14, ETAM > 54))




# YOUNGER THAN 50 2015

mean(filter(ASpI_15, ETAM < 50)$DURRIC)
median(filter(ASpI_15, ETAM < 50)$DURRIC)
sd(filter(ASpI_15, ETAM < 50)$DURRIC)
nrow(filter(ASpI_15, ETAM < 50))




# BETWEEN 50 AND 54 2015

mean(filter(ASpI_15, ETAM > 49, ETAM < 55)$DURRIC)
median(filter(ASpI_15, ETAM > 49, ETAM < 55)$DURRIC)
sd(filter(ASpI_15, ETAM > 49, ETAM < 55)$DURRIC)
nrow(filter(ASpI_15, ETAM > 49, ETAM < 55))




# OLDER THAN 54 2015

mean(filter(ASpI_15, ETAM > 54)$DURRIC)
median(filter(ASpI_15, ETAM > 54)$DURRIC)
sd(filter(ASpI_15, ETAM > 54)$DURRIC)
nrow(filter(ASpI_15, ETAM > 54))




################################################################################

################################################################################




### REGRESSIONS NO DURATIONS NO SUBSETTING

## CREATING THE DFS

ASpI_14_1 <- ASpI_14 %>% filter(ETAM < 55)

ASpI_14_2 <- ASpI_14 %>% filter(ETAM >= 50)

ASpI_15_1 <- ASpI_15 %>% filter(ETAM < 55)

ASpI_15_2 <- ASpI_15 %>% filter(ETAM >= 50) 




## RUNNING THE RDDS

# FIRST DISCONTINUITY

bw14_1 <- IKbandwidth(ASpI_14_1$ETAM, ASpI_14_1$DURRIC, 
                      cutpoint = 49.9,
                      kernel = "triangular")

RDD14_1 <- RDestimate(DURRIC ~ ETAM, ASpI_14_1,
                      cutpoint = 49.9,
                      bw = bw14_1,
                      kernel = "triangular",
                      se.type = "HC")

summary(RDD14_1)




# SECOND DISCONTINUITY

bw14_2 <- IKbandwidth(ASpI_14_2$ETAM, ASpI_14_2$DURRIC, 
                      cutpoint = 54.9,
                      kernel = "triangular")

RDD14_2 <- RDestimate(DURRIC ~ ETAM, ASpI_14_2,
                      cutpoint = 54.9,
                      bw = bw14_2,
                      kernel = "triangular",
                      se.type = "HC")

summary(RDD14_2)




# THIRD DISCONTINUITY

bw15_1 <- IKbandwidth(ASpI_15_1$ETAM, ASpI_15_1$DURRIC, 
                      cutpoint = 49.9,
                      kernel = "triangular")

RDD15_1 <- RDestimate(DURRIC ~ ETAM, ASpI_15_1,
                      cutpoint = 49.9,
                      bw = bw15_1,
                      kernel = "triangular",
                      se.type = "HC")

summary(RDD15_1)




# FOURTH DISCONTINUITY

bw15_2 <- IKbandwidth(ASpI_15_2$ETAM, ASpI_15_2$DURRIC, 
                      cutpoint = 54.9,
                      kernel = "triangular")

RDD15_2 <- RDestimate(DURRIC ~ ETAM, ASpI_15_2,
                      cutpoint = 54.9,
                      bw = bw15_2,
                      kernel = "triangular",
                      se.type = "HC")

summary(RDD15_2)




################################################################################

################################################################################




### REGRESSIONS WITHOUT OUTLIERS

## CREATING THE DFS

ASpI_14_1_SUB <- ASpI_14 %>% filter(ETAM < 55, DURRIC <= 48)

ASpI_14_2_SUB <- ASpI_14 %>% filter(ETAM >= 50, DURRIC <= 48)

ASpI_15_1_SUB <- ASpI_15 %>% filter(ETAM < 55, DURRIC <= 48)

ASpI_15_2_SUB <- ASpI_15 %>% filter(ETAM >= 50, DURRIC <= 48)




## RUNNING THE RDDS

# FIRST DISCONTINUITY

bw14_1_SUB <- IKbandwidth(ASpI_14_1_SUB$ETAM, ASpI_14_1_SUB$DURRIC, 
                          cutpoint = 49.9,
                          kernel = "triangular")

RDD14_1_SUB <- RDestimate(DURRIC ~ ETAM, ASpI_14_1_SUB,
                          cutpoint = 49.9,
                          bw = bw14_1_SUB,
                          kernel = "triangular",
                          se.type = "HC")

summary(RDD14_1_SUB)




# SECOND DISCONTINUITY

bw14_2_SUB <- IKbandwidth(ASpI_14_2_SUB$ETAM, ASpI_14_2_SUB$DURRIC, 
                          cutpoint = 54.9,
                          kernel = "triangular")

RDD14_2_SUB <- RDestimate(DURRIC ~ ETAM, ASpI_14_2_SUB,
                          cutpoint = 54.9,
                          bw = bw14_2_SUB,
                          kernel = "triangular",
                          se.type = "HC")

summary(RDD14_2_SUB)




# THIRD DISCONTINUITY

bw15_1_SUB <- IKbandwidth(ASpI_15_1_SUB$ETAM, ASpI_15_1_SUB$DURRIC, 
                      cutpoint = 49.9,
                      kernel = "triangular")

RDD15_1_SUB <- RDestimate(DURRIC ~ ETAM, ASpI_15_1_SUB,
                      cutpoint = 49.9,
                      bw = bw15_1_SUB,
                      kernel = "triangular",
                      se.type = "HC")

summary(RDD15_1_SUB)




# FOURTH DISCONTINUITY

bw15_2_SUB <- IKbandwidth(ASpI_15_2_SUB$ETAM, ASpI_15_2_SUB$DURRIC, 
                      cutpoint = 54.9,
                      kernel = "triangular")

RDD15_2_SUB <- RDestimate(DURRIC ~ ETAM, ASpI_15_2_SUB,
                      cutpoint = 54.9,
                      bw = bw15_2_SUB,
                      kernel = "triangular",
                      se.type = "HC")

summary(RDD15_2_SUB)




################################################################################

################################################################################




#### CI PLOT OF ESTIMATES WITHOUT OUTLIERS

## CREATING THE DF COLUMNS
year <- c("2014", "2014", "2015", "2015")
discontinuity <- c("Age 49", "Age 54", "Age 49", "Age 54")
estimates <- c(0.656,3.524,0.017,0.88)
sds <- c(0.47,1.586,0.728,0.442)




# CREATING THE DF
CI_df <- data.frame(year,discontinuity,estimates,sds)



# CREATING THE PLOT
pdf(file = "<insert own path here>\\ESTIMATES_CI_PLOT.pdf",
    width = 7.5,
    height = 5)

CI_df %>%
  ggplot() +
  aes(
    y = estimates,
    x = discontinuity,
    color = year
  ) +
  geom_point(
    size = 1.5
  ) + 
  geom_errorbar(
    aes(
    ymax = estimates + sds * qnorm(0.975),
    ymin = estimates + sds * qnorm(0.025)
    ),
    linewidth = 1.25
  )+
  facet_wrap(~year) +
  labs(title = "95% Confidence Interval of Week-per-Week Estimates",
       y = "Week-per-Week Estimates",
       x = "Cutoff",
       color = "ASpI Year") +
  theme_linedraw()

dev.off()




################################################################################

################################################################################




##### ROBUSTNESS CHECKS




### CHECK CONTINUITY OF RUNNING VARIABLE


# FIRST DISCONTINUITY

DENSITY14_1 <- rddensity(ASpI_14_1_SUB$ETAM, c = 49)

summary(DENSITY14_1)

pdf(file = "<insert own path here>\\RCHECK_14_DISC1.pdf",
    width = 7.5,
    height = 5)

rdplotdensity(rdd = DENSITY14_1, 
              X = ASpI_14_1_SUB$ETAM, 
              type = "both",
              plotRange = c(45,54),
              title = "Checking Continuity of Forcing Variable - ASpI 2014",
              xlabel = "Density",
              ylabel = "Age")

dev.off()




# SECOND DISCONTINUITY

DENSITY14_2 <- rddensity(ASpI_14_2_SUB$ETAM, c = 54)

summary(DENSITY14_2)

pdf(file = "<insert own path here>\\RCHECK_14_DISC2.pdf",
    width = 7.5,
    height = 5)

rdplotdensity(rdd = DENSITY14_2, 
              X = ASpI_14_2_SUB$ETAM, 
              type = "both",
              plotRange = c(50,59),
              title = "Checking Continuity of Forcing Variable - ASpI 2014",
              xlabel = "Density",
              ylabel = "Age")


dev.off()




# THIRD DISCONTINUITY

DENSITY15_1 <- rddensity(ASpI_15_1_SUB$ETAM, c = 49)

summary(DENSITY15_1)

pdf(file = "<insert own path here>\\RCHECK_15_DISC1.pdf",
    width = 7.5,
    height = 5)

rdplotdensity(rdd = DENSITY15_1, 
              X = ASpI_15_1_SUB$ETAM, 
              type = "both",
              plotRange = c(45,54),
              title = "Checking Continuity of Forcing Variable  - ASpI 2015",
              xlabel = "Density",
              ylabel = "Age")

dev.off()




# FOURTH DISCONTINUITY

DENSITY15_2 <- rddensity(ASpI_15_2_SUB$ETAM, c = 54)

summary(DENSITY15_2)

pdf(file = "<insert own path here>\\RCHECK_15_DISC2.pdf",
    width = 7.5,
    height = 5)

rdplotdensity(rdd = DENSITY15_2, 
              X = ASpI_15_2_SUB$ETAM, 
              type = "both",
              plotRange = c(50,59),
              title = "Checking Continuity of Forcing Variable - ASpI 2015",
              xlabel = "Density",
              ylabel = "Age")

dev.off()








### CHECKING CONTINUITY OF COVARIATES

## PERMUTATION TEST FUNCTION

permutation.test <- function(group1, group2, FUN, level, repetitions){
  
  makePermutedStats <- function(group1, group2) {
    
    sampled <- sample(1:length(c(group1, group2)), size = length(group1), replace = FALSE)
    
    return(FUN(c(group1, group2)[sampled], c(group1, group2)[-sampled], level))
  }
  
  stat.obs <- FUN(group1, group2, level)
  
  stat.permute <- replicate(repetitions, makePermutedStats(group1, group2))
  
  p.value <- sum(stat.permute >= stat.obs) / repetitions
  
  return(list(p.value = p.value, observedStat = stat.obs, permutedStats = stat.permute))
}




## BINOMIAL PROPORTION FUNCTION

proportion.binomial <- function(group1, group2, level){
  
  prop1 <- sum(group1 == level) / length(group1)
  
  prop2 <- sum(group2 == level) / length(group2)
  
  return(as.numeric(prop1 - prop2))  # Convert the result to numeric
}




## FUNCTION TO CREATE THE ROWS OF THE DATA-FRAME

createRow <- function(name, group1, group2, level, repetitions) {
  
  result <- permutation.test(group1, group2, proportion.binomial, level, repetitions)
  
  row <- c(name, round(as.numeric(result$observedStat), 3), round(as.numeric(result$p.value), 3))
  
  return(row)
}




## LIST OF COVARIATES AND LEVELS

covariateInfo <- list(
  "SG11" = c(1, 2),
  "STACIM" = c(1, 2, 3, 6),
  "CITTAD" = c(1, 2, 3),
  "RIP3" = c(1, 2, 3),
  "TISTUD" = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
)




## FUNCTION TO CREATE THE DATAFRAME

createDF <- function(group1, group2){
  
  rows <- list()
  for (covariate in names(covariateInfo)) {
    
    for (level in covariateInfo[[covariate]]) {
      
      name <- paste0("Proportion of (", covariate, "=", level, ")")
      
      row <- createRow(name, 
                       unlist(group1[,grep(covariate, colnames(group1))]),
                       unlist(group2[,grep(covariate, colnames(group2))]),
                       level, 
                       1000)  
      
      rows[[name]] <- row
      
    }
  }
  
  permdf <- data.frame(matrix(unlist(rows), ncol = 3, byrow = TRUE))
  colnames(permdf) <- c("Covariate", "Observed Statistics", "P value")
  
  return(permdf)
  
}




# FIRST DISC

permdf1 <- createDF(
  ASpI_14_1_SUB[ASpI_14_1_SUB$ETAM >= 45 & ASpI_14_1_SUB$ETAM < 50,],
  ASpI_14_1_SUB[ASpI_14_1_SUB$ETAM >= 50,]
)

stargazer(permdf1,
          summary = FALSE,
          rownames = FALSE,
          title = "Permutation Test of Difference in Proportion Around the Cutoff (Age = 49) - ASpI 2014")




# SECOND DISC

permdf2 <- createDF(
  ASpI_14_2_SUB[ASpI_14_2_SUB$ETAM < 55,],
  ASpI_14_2_SUB[ASpI_14_2_SUB$ETAM >= 55,]
)

stargazer(permdf2,
          summary = FALSE,
          rownames = FALSE,
          title = "Permutation Test of Difference in Proportion Around the Cutoff (Age = 54) - ASpI 2014")




# THIRD DISC

permdf3 <- createDF(
  ASpI_15_1_SUB[ASpI_15_1_SUB$ETAM >= 45 & ASpI_15_1_SUB$ETAM < 50,],
  ASpI_15_1_SUB[ASpI_15_1_SUB$ETAM >= 50,]
)

stargazer(permdf3,
          summary = FALSE,
          rownames = FALSE,
          title = "Permutation Test of Difference in Proportion Around the Cutoff (Age = 49) - ASpI 2015")




# FOURTH DISC

permdf4 <- createDF(
  ASpI_15_2_SUB[ASpI_15_2_SUB$ETAM < 55,],
  ASpI_15_2_SUB[ASpI_15_2_SUB$ETAM >= 55,]
)

stargazer(permdf4,
          summary = FALSE,
          rownames = FALSE,
          title = "Permutation Test of Difference in Proportion Around the Cutoff (Age = 54) - ASpI 2014")




################################################################################

################################################################################




##### COMPUTING HAZARD RATES OF CONTROL AND TREATMENTS

## CREATING THE SUBSETS

ASpI_14_45_49 <- ASpI_14 %>% filter(ETAM >= 45 & ETAM < 50)
ASpI_14_50_54 <- ASpI_14 %>% filter(ETAM >= 50 & ETAM < 54)
ASpI_14_55_59 <- ASpI_14 %>% filter(ETAM >= 54 & ETAM < 60)

ASpI_15_45_49 <- ASpI_15 %>% filter(ETAM >= 45 & ETAM < 50)
ASpI_15_50_54 <- ASpI_15 %>% filter(ETAM >= 50 & ETAM < 54)
ASpI_15_55_59 <- ASpI_15 %>% filter(ETAM >= 54 & ETAM < 60)



## FUNCTION TO CREATE LIFETABLES

life_table <- function(df){
  
  N <- nrow(df)
  
  lt <- df %>% mutate(D = ifelse(DURRIC > 20, 0, 1))
  
  lt$DURRIC[lt$DURRIC > 20] = 20
  
  lt <- lt %>% 
    mutate(ONES = 1) %>% 
    arrange(DURRIC) %>% 
    group_by(DURRIC) %>% 
    summarise(
      Nz = sum(ONES),
      rempl = sum(D))
  
  prior_count <- cumsum(lt$Nz)
  prior_count <- c(0, prior_count[-length(prior_count)])
  
  lt$at_risk = N - prior_count
  
  
  lt$censored = lt$at_risk - c(lt$at_risk[2:nrow(lt)], 0) - lt$rempl
  
  
  lt$hazard = lt$rempl / lt$at_risk
  lt$survival = cumprod(1 - lt$hazard)
  
  
  lt$sigma2_hazard = (lt$hazard * (1 - lt$hazard)) / (lt$at_risk / N)
  lt$sigma2_survival = (lt$survival ^ 2) * cumsum((lt$hazard / (1 - lt$hazard)) * (1 / (lt$at_risk / N)))
  
  
  lt$UB_hazard = lt$hazard + qnorm(0.975, mean = 0, sd = 1) * sqrt((1/N) * lt$sigma2_hazard)
  lt$LB_hazard = lt$hazard - qnorm(0.975, mean = 0, sd = 1) * sqrt((1/N) * lt$sigma2_hazard)
  
  lt$CI_hazard = paste0("[",round(lt$LB_hazard, digits = 2),",",round(lt$UB_hazard, digits = 2),"]")
  
  
  lt$UB_survival = lt$survival + qnorm(0.975, mean = 0, sd = 1) * sqrt((1/N) * lt$sigma2_survival)
  lt$LB_survival = lt$survival - qnorm(0.975, mean = 0, sd = 1) * sqrt((1/N) * lt$sigma2_survival)
  
  lt$CI_survival = paste0("[",round(lt$LB_survival, digits = 2),",",round(lt$UB_survival, digits = 2),"]")
  
  lt <- lt %>% select(!(Nz))
  
  return(lt)
}




## FUNCTION TO EXPAND LIFETABLES


expand_lt <- function(df){
  
  comp <- c(1:max(df[, "DURRIC"]))
  durr <- df$DURRIC
  
  for (i in comp){
    
    if (i == durr[i]){
      
      i = i + 1
      
    } else { 
      
      new_row <- df[(i - 1),]
      
      new_row[,"DURRIC"] <- i
      
      df <- rbind(df[1:(i - 1),],
                  new_row,
                  df[-(1:(i - 1)),])
      
      durr <- df$DURRIC
      
      i = i + 1
      
    }
  }
  
  return(df)
  
}



# ASPI 14 CONTROL

lf_14_45_49 <- expand_lt(life_table(ASpI_14_45_49))




# ASPI 14 TREATMENT 1

lf_14_50_54 <- expand_lt(life_table(ASpI_14_50_54))




# ASPI 14 TREATMENT 2

lf_14_55_59 <- expand_lt(life_table(ASpI_14_55_59))




# 14 MERGED

colnames(lf_14_45_49)[2:ncol(lf_14_45_49)] <- 
  paste(colnames(lf_14_45_49)[2:ncol(lf_14_45_49)],"1",sep="_")
  
colnames(lf_14_50_54)[2:ncol(lf_14_50_54)] <- 
  paste(colnames(lf_14_50_54)[2:ncol(lf_14_50_54)],"2",sep="_")

colnames(lf_14_55_59)[2:ncol(lf_14_55_59)] <- 
  paste(colnames(lf_14_55_59)[2:ncol(lf_14_55_59)],"3",sep="_")



hz_14_graph <- merge(lf_14_45_49, lf_14_50_54)
hz_14_graph <- merge(hz_14_graph, lf_14_55_59)




# ASPI 15 CONTROL

lf_15_45_49 <- expand_lt(life_table(ASpI_15_45_49))





# ASPI 15 TREATMENT 1

lf_15_50_54 <- expand_lt(life_table(ASpI_15_50_54))




# ASPI 15 TREATMENT 2

lf_15_55_59 <- expand_lt(life_table(ASpI_15_55_59))




# ASPI 15 MERGED

colnames(lf_15_45_49)[2:ncol(lf_14_45_49)] <- 
  paste(colnames(lf_15_45_49)[2:ncol(lf_15_45_49)],"1",sep="_")

colnames(lf_15_50_54)[2:ncol(lf_14_50_54)] <- 
  paste(colnames(lf_15_50_54)[2:ncol(lf_15_50_54)],"2",sep="_")

colnames(lf_15_55_59)[2:ncol(lf_14_55_59)] <- 
  paste(colnames(lf_15_55_59)[2:ncol(lf_15_55_59)],"3",sep="_")



hz_15_graph <- merge(lf_15_45_49, lf_15_50_54)
hz_15_graph <- merge(hz_15_graph, lf_15_55_59)




################################################################################

################################################################################




### HAZARD PLOTS

pdf(file = "<insert own path here>\\hz_ASpI_14.pdf",
    width = 7.5, height = 5)

hz_14_graph %>% 
  ggplot() +
  geom_step(
    aes(
      x = DURRIC,
      y = hazard_1,
      color = "Group 1"
    )
  ) +
  geom_step(
    aes(
      x = DURRIC,
      y = hazard_2,
      color = "Group 2"
    )
  ) +
  geom_step(
    aes(
      x = DURRIC,
      y = hazard_3,
      color = "Group 3"
    )
  ) + 
  theme_classic() +
  labs(x = "Search Duration (Months)",
       y = "Hazard Rate",
       title = "Hazard Rates by Group \nASpI 2014",
       color = "Legend") +
  geom_vline(xintercept = 8, size = 0.5, linetype = "dashed") +
  geom_vline(xintercept = 12, size = 0.5, linetype = "dashed") +
  geom_vline(xintercept = 14, size = 0.5, linetype = "dashed") +
  scale_x_continuous(breaks=seq(1,20,1))

dev.off()

pdf(file = "<insert own path here>\\hz_ASpI_15.pdf",
    width = 7.5, height = 5)

hz_15_graph %>% 
  ggplot() +
  geom_step(
    aes(
      x = DURRIC,
      y = hazard_1,
      color = "Group 1"
    )
  ) +
  geom_step(
    aes(
      x = DURRIC,
      y = hazard_2,
      color = "Group 2"
    )
  ) +
  geom_step(
    aes(
      x = DURRIC,
      y = hazard_3,
      color = "Group 3"
    )
  ) + 
  theme_classic() +
  labs(x = "Search Duration (Months)",
       y = "Hazard Rate",
       title = "Hazard Rates by Group \nASpI 2015",
       color = "Legend") +
  geom_vline(xintercept = 10, size = 0.5, linetype = "dashed") +
  geom_vline(xintercept = 12, size = 0.5, linetype = "dashed") +
  geom_vline(xintercept = 16, size = 0.5, linetype = "dashed") +
  scale_x_continuous(breaks=seq(1,20,1))

dev.off()




################################################################################

################################################################################




# PLOTS OF SURVIVAL

pdf(file = "<insert own path here>\\sr_ASpI_14.pdf",
    width = 7.5, height = 5)

hz_14_graph %>% 
  ggplot() +
  geom_step(
    aes(
      x = DURRIC,
      y = survival_1,
      color = "Group 1"
    )
  ) +
  geom_step(
    aes(
      x = DURRIC,
      y = survival_2,
      color = "Group 2"
    )
  ) +
  geom_step(
    aes(
      x = DURRIC,
      y = survival_3,
      color = "Group 3"
    )
  ) + 
  theme_classic() +
  labs(x = "Search Duration (Months)",
       y = "Survival Rate",
       title = "Survival Rates by Group \nASpI 2014",
       color = "Legend") +
  geom_vline(xintercept = 8, size = 0.5, linetype = "dashed") +
  geom_vline(xintercept = 12, size = 0.5, linetype = "dashed") +
  geom_vline(xintercept = 14, size = 0.5, linetype = "dashed") +
  scale_x_continuous(breaks=seq(1,20,1))

dev.off()

pdf(file = "<insert own path here>\\sr_ASpI_15.pdf",
    width = 7.5, height = 5)

hz_15_graph %>% 
  ggplot() +
  geom_step(
    aes(
      x = DURRIC,
      y = survival_1,
      color = "Group 1"
    )
  ) +
  geom_step(
    aes(
      x = DURRIC,
      y = survival_2,
      color = "Group 2"
    )
  ) +
  geom_step(
    aes(
      x = DURRIC,
      y = survival_3,
      color = "Group 3"
    )
  ) + 
  theme_classic() +
  labs(x = "Search Duration (Months)",
       y = "Survival Rate",
       title = "Survival Rates by Group \nASpI 2015",
       color = "Legend") +
  geom_vline(xintercept = 10, size = 0.5, linetype = "dashed") +
  geom_vline(xintercept = 12, size = 0.5, linetype = "dashed") +
  geom_vline(xintercept = 16, size = 0.5, linetype = "dashed") +
  scale_x_continuous(breaks=seq(1,20,1))

dev.off()




################################################################################

################################################################################




### HAZARD DIFFERENCE PLOTS

## CREATING CIs FOR DIFFERENCES

hz_14_graph <- hz_14_graph %>% 
  mutate(UB_hz1_minus_hz2 = (hazard_1 - hazard_2) + 
           qnorm(0.975, mean = 0, sd = 1) * 
           sqrt(sigma2_hazard_1 + sigma2_hazard_1),
         LB_hz1_minus_hz2 = (hazard_1 - hazard_2) - 
           qnorm(0.975, mean = 0, sd = 1) * 
           sqrt(sigma2_hazard_1 + sigma2_hazard_1),
         UB_hz2_minus_hz3 = (hazard_2 - hazard_3) + 
           qnorm(0.975, mean = 0, sd = 1) * 
           sqrt(sigma2_hazard_1 + sigma2_hazard_1),
         LB_hz2_minus_hz3 = (hazard_2 - hazard_3) - 
           qnorm(0.975, mean = 0, sd = 1) * 
           sqrt(sigma2_hazard_1 + sigma2_hazard_1))


hz_15_graph <- hz_15_graph %>% 
  mutate(UB_hz1_minus_hz2 = (hazard_1 - hazard_2) + 
           qnorm(0.975, mean = 0, sd = 1) * 
           sqrt(sigma2_hazard_1 + sigma2_hazard_1),
         LB_hz1_minus_hz2 = (hazard_1 - hazard_2) - 
           qnorm(0.975, mean = 0, sd = 1) * 
           sqrt(sigma2_hazard_1 + sigma2_hazard_1),
         UB_hz2_minus_hz3 = (hazard_2 - hazard_3) + 
           qnorm(0.975, mean = 0, sd = 1) * 
           sqrt(sigma2_hazard_1 + sigma2_hazard_1),
         LB_hz2_minus_hz3 = (hazard_2 - hazard_3) - 
           qnorm(0.975, mean = 0, sd = 1) * 
           sqrt(sigma2_hazard_1 + sigma2_hazard_1))





pdf(file = "<insert own path here>\\hz_ASpI_14_2_2.pdf",
    width = 7.5, height = 5)

hz_14_graph %>% 
  ggplot() +
  geom_step(
    aes(
    x = DURRIC,
    y = (hazard_1 - hazard_2)
  )) +
  geom_step(
    aes(
      x = DURRIC,
      y = UB_hz1_minus_hz2
    ),
    color = "grey",
    linetype = 2) +
  geom_step(
    aes(
      x = DURRIC,
      y = LB_hz1_minus_hz2
    ),
    color = "grey",
    linetype = 2) +
  theme_classic() +
  labs(x = "Search Duration (Months)",
       y = "Hazard Rate",
       title = "Hazard Rates Difference \n(Group 1 - Group 2) \nASpI 2014") +
  guides(color = "none") +
  geom_vline(xintercept = 8, color = "red",
             size = 0.5, linetype = "dashed") +
  guides(color = "none") +
  geom_vline(xintercept = 12, color = "red",
             size = 0.5, linetype = "dashed") +
  guides(color = "none") +
  geom_vline(xintercept = 14, color = "red",
             size = 0.5, linetype = "dashed") +
  scale_x_continuous(breaks=seq(1,20,1))

dev.off()

pdf(file = "<insert own path here>\\hz_ASpI_14_2_3.pdf",
    width = 7.5, height = 5)

hz_14_graph %>% 
  ggplot() +
  geom_step(
    aes(
      x = DURRIC,
      y = (hazard_2 - hazard_3)
    )) +
  geom_step(
    aes(
      x = DURRIC,
      y = UB_hz2_minus_hz3
    ),
    color = "grey",
    linetype = 2) +
  geom_step(
    aes(
      x = DURRIC,
      y = LB_hz2_minus_hz3
    ),
    color = "grey",
    linetype = 2) +
  theme_classic() +
  labs(x = "Search Duration (Months)",
       y = "Hazard Rate",
       title = "Hazard Rates Difference \n(Group 2 - Group 3) \nASpI 2014") +
  guides(color = "none") +
  geom_vline(xintercept = 8, color = "red",
             size = 0.5, linetype = "dashed") +
  guides(color = "none") +
  geom_vline(xintercept = 12, color = "red",
             size = 0.5, linetype = "dashed") +
  guides(color = "none") +
  geom_vline(xintercept = 14, color = "red",
             size = 0.5, linetype = "dashed") +
  scale_x_continuous(breaks=seq(1,20,1))

dev.off()

pdf(file = "<insert own path here>\\hz_ASpI_15_1_2.pdf",
    width = 7.5, height = 5)

hz_15_graph %>% 
  ggplot() +
  geom_step(
    aes(
      x = DURRIC,
      y = (hazard_1 - hazard_2)
    )) +
  geom_step(
    aes(
      x = DURRIC,
      y = UB_hz1_minus_hz2
    ),
    color = "grey",
    linetype = 2) +
  geom_step(
    aes(
      x = DURRIC,
      y = LB_hz1_minus_hz2
    ),
    color = "grey",
    linetype = 2) +
  theme_classic() +
  labs(x = "Search Duration (Months)",
       y = "Hazard Rate",
       title = "Hazard Rates Difference \n(Group 1 - Group 2) \nASpI 2015") +
  guides(color = "none") +
  geom_vline(xintercept = 10, color = "red",
             size = 0.5, linetype = "dashed") +
  guides(color = "none") +
  geom_vline(xintercept = 12, color = "red",
             size = 0.5, linetype = "dashed") +
  guides(color = "none") +
  geom_vline(xintercept = 16, color = "red",
             size = 0.5, linetype = "dashed") +
  scale_x_continuous(breaks=seq(1,20,1))

dev.off()

pdf(file = "<insert own path here>\\hz_ASpI_15_2_3.pdf",
    width = 7.5, height = 5)

hz_15_graph %>% 
  ggplot() +
  geom_step(
    aes(
      x = DURRIC,
      y = (hazard_2 - hazard_3)
    )) +
  geom_step(
    aes(
      x = DURRIC,
      y = UB_hz2_minus_hz3
    ),
    color = "grey",
    linetype = 2) +
  geom_step(
    aes(
      x = DURRIC,
      y = LB_hz2_minus_hz3
    ),
    color = "grey",
    linetype = 2) +
  theme_classic() +
  labs(x = "Search Duration (Months)",
       y = "Hazard Rate",
       title = "Hazard Rates Difference \n(Group 2 - Group 3) \nASpI 2015") +
  guides(color = "none") +
  geom_vline(xintercept = 10, color = "red",
             size = 0.5, linetype = "dashed") +
  guides(color = "none") +
  geom_vline(xintercept = 12, color = "red",
             size = 0.5, linetype = "dashed") +
  guides(color = "none") +
  geom_vline(xintercept = 16, color = "red",
             size = 0.5, linetype = "dashed") +
  scale_x_continuous(breaks=seq(1,20,1))

dev.off()




##### EXPORTING LIFETABLES

# 2014 GROUP 1

lf_14_45_49 <- lf_14_45_49 %>% 
  select(at_risk_1,
         rempl_1,
         censored_1,
         hazard_1,
         sigma2_hazard_1,
         CI_hazard_1,
         survival_1,
         sigma2_survival_1,
         CI_survival_1) %>% 
  mutate(
    hazard_1 = round(hazard_1, digits = 2),
    survival_1 = round(survival_1, digits = 2),
    sigma2_hazard_1 = round(sigma2_hazard_1, digits = 2),
    sigma2_survival_1 = round(sigma2_survival_1, digits = 2)
  )

stargazer(lf_14_45_49,
          summary = FALSE,
          title = "Lifetable Group 1 - ASpI 2014")




# 2014 GROUP 2

lf_14_50_54 <- lf_14_50_54 %>% 
  select(at_risk_2,
         rempl_2,
         censored_2,
         hazard_2,
         sigma2_hazard_2,
         CI_hazard_2,
         survival_2,
         sigma2_survival_2,
         CI_survival_2) %>% 
  mutate(
    hazard_2 = round(hazard_2, digits = 2),
    survival_2 = round(survival_2, digits = 2),
    sigma2_hazard_2 = round(sigma2_hazard_2, digits = 2),
    sigma2_survival_2 = round(sigma2_survival_2, digits = 2)
  )

stargazer(lf_14_50_54,
          summary = FALSE,
          title = "Lifetable Group 2 - ASpI 2014",
          label = "table:14")




# 2014 GROUP 3

lf_14_55_59 <- lf_14_55_59 %>% 
  select(at_risk_3,
         rempl_3,
         censored_3,
         hazard_3,
         sigma2_hazard_3,
         CI_hazard_3,
         survival_3,
         sigma2_survival_3,
         CI_survival_3) %>% 
  mutate(
    hazard_3 = round(hazard_3, digits = 2),
    survival_3 = round(survival_3, digits = 2),
    sigma2_hazard_3 = round(sigma2_hazard_3, digits = 2),
    sigma2_survival_3 = round(sigma2_survival_3, digits = 2)
  )

stargazer(lf_14_55_59,
          summary = FALSE,
          title = "Lifetable Group 3 - ASpI 2014",
          label = "table:15")




# 2015 GROUP 1

lf_15_45_49 <- lf_15_45_49 %>% 
  select(at_risk_1,
         rempl_1,
         censored_1,
         hazard_1,
         sigma2_hazard_1,
         CI_hazard_1,
         survival_1,
         sigma2_survival_1,
         CI_survival_1) %>% 
  mutate(
    hazard_1 = round(hazard_1, digits = 2),
    survival_1 = round(survival_1, digits = 2),
    sigma2_hazard_1 = round(sigma2_hazard_1, digits = 2),
    sigma2_survival_1 = round(sigma2_survival_1, digits = 2)
  )

stargazer(lf_15_45_49,
          summary = FALSE,
          title = "Lifetable Group 1 - ASpI 2015",
          label = "table:16")




# 2015 GROUP 2

lf_15_50_54 <- lf_15_50_54 %>% 
  select(at_risk_2,
         rempl_2,
         censored_2,
         hazard_2,
         sigma2_hazard_2,
         CI_hazard_2,
         survival_2,
         sigma2_survival_2,
         CI_survival_2) %>% 
  mutate(
    hazard_2 = round(hazard_2, digits = 2),
    survival_2 = round(survival_2, digits = 2),
    sigma2_hazard_2 = round(sigma2_hazard_2, digits = 2),
    sigma2_survival_2 = round(sigma2_survival_2, digits = 2)
  )

stargazer(lf_15_50_54,
          summary = FALSE,
          title = "Lifetable Group 2 - ASpI 2015",
          label = "table:17")




# 2015 GROUP 3

lf_15_55_59 <- lf_15_55_59 %>% 
  select(at_risk_3,
         rempl_3,
         censored_3,
         hazard_3,
         sigma2_hazard_3,
         CI_hazard_3,
         survival_3,
         sigma2_survival_3,
         CI_survival_3) %>% 
  mutate(
    hazard_3 = round(hazard_3, digits = 2),
    survival_3 = round(survival_3, digits = 2),
    sigma2_hazard_3 = round(sigma2_hazard_3, digits = 2),
    sigma2_survival_3 = round(sigma2_survival_3, digits = 2)
  )

stargazer(lf_15_55_59,
          summary = FALSE,
          title = "Lifetable Group 3 - ASpI 2015",
          label = "table:18")