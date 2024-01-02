### Data cleaning


# Loading the libraries

################################################################################
library(tidyverse)
library(vroom)
library(stringr)
################################################################################



# Functions to subset and merge the waves data sets

################################################################################
RCFLtidying <- function(start_year, end_year){
  
  
  # Warnings
  if(nchar(start_year) != 4 | nchar(end_year) != 4) 
    warning("start_year and end_year are of the yyyy format")
  
  
  # Initializing RBinded data-set
  binded <- data.frame(matrix(NA, nrow = 1, ncol = 7))
  names(binded) <- c("SG11","CITTAD","ETAM","TISTUD","DURRIC","STACIM","RIP3")
  
  
  # Loop
  l <- 1
  for (i in c(seq(start_year, end_year))){
    
    j <- 1
    while (j < 5){
        
      df_char_name <- paste0("RCFL_",as.character(i),"_",as.character(j),".txt")
      tmp <- vroom(df_char_name, guess_max = 169484)
      tmp <- tmp %>% 
        mutate(DURRIC = as.numeric(DURRIC),
               ETAM = as.numeric(ETAM),
               C21 = as.numeric(C21),
               TISTUD = as.numeric(TISTUD),
               DURRIC_PLUS_C21 = DURRIC + C21) %>% 
        filter(!is.na(DURRIC),
               !(DURRIC %in% c(0,999)),
               WAVQUA == 1,
               DURRIC_PLUS_C21 < (3*l - 3),
               DURRIC_PLUS_C21 > (3*l - 12)) %>% 
        select(SG11,
               CITTAD,
               ETAM,
               TISTUD,
               DURRIC,
               STACIM,
               RIP3)
        
        binded <- rbind(binded, tmp)
        
        j <- j + 1
        l <- l + 1
        
        gc()
        
      
    }
  }
  
  binded <- binded[-1,]
  return(binded)
  
}
################################################################################


# Creating the data set of ASpI recipients in 2014

################################################################################
ASpI_2014_completed_spells <- RCFLtidying(2014,2020)

write.csv(ASpI_2014_completed_spells,
          "<insert own path here>\\ASpI_2014_completed_spells")
################################################################################


# Creating the data set of ASpI recipients in 2015

################################################################################
ASpI_2015_completed_spells <- RCFLtidying(2015,2020)

write.csv(ASpI_2015_completed_spells,
          "<insert own path here>\\ASpI_2015_completed_spells")
################################################################################


# Creating the data set of ASpI recipients from 2016 on wards

################################################################################
ASpI_2016_completed_spells <- RCFLtidying(2016,2020)

write.csv(ASpI_2016_completed_spells,
          "<insert own path here>\\ASpI_2016_completed_spells")
################################################################################


