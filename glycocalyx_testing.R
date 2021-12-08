library(tidyverse)
library(readxl)

#Read in height and coverage files
glyco_h <- read_excel("glycocalyx_clean(height).xlsx", na = "no good/none")
glyco_c <- read_excel("glycocalyx_clean(coverage).xlsx", na = "no good/none")

#Filter out null rows
glyco_h <- filter(glyco_h, !is.na(Eye))
glyco_c <- filter(glyco_c, !is.na(Eye))

#example testing
ex1 <- filter(glyco_h, Region == "Non-Lasered") %>% select("SC")
ex2 <- filter(glyco_h, Region == "Control High Flow") %>% select("SC")
t.test(ex1,ex2)


#returns p-value of region comparison when isolating outflow location
region_comp <- function(df, region1, region2, col){
  ex1 <- filter(df, Region == region1) %>% select(all_of(col))
  ex2 <- filter(df, Region == region2) %>% select(all_of(col))
  p_val <- t.test(ex1,ex2)$p.value
  return(p_val)
}

#returns p-value of outflow location comparison when isolating region
col_comp <- function(df, col1, col2, region){
  ex1 <- filter(df, Region == region) %>% select(all_of(col1))
  ex2 <- filter(df, Region == region) %>% select(all_of(col2))
  p_val <- t.test(ex1,ex2)$p.value
  return(p_val)
}

#function testing
region_comp(glyco_h, "Non-Lasered", "Control High Flow", "SC")

#Create lists of region/locations names, as well as the combination matrices
regions <- unique(glyco_h$Region)
reg_combs <- combn(regions, 2)
cols <- colnames(glyco_h)[5:length(glyco_h)]
col_combs <- combn(cols,2)

#Region comparison over all possible combinations
for (i in 1:6){
  for (col in cols){
    print("------------------------------")
    print(reg_combs[1,i])
    print(reg_combs[2,i])
    print(col)
    print(region_comp(glyco_h, reg_combs[1,i], reg_combs[2,i], col))
  }
}

#Outflow location comparison over all possible combinations
for (i in 1:10){
  for (region in regions){
    print("-------------------------------")
    print(col_combs[1,i])
    print(col_combs[2,i])
    print(region)
    print(col_comp(glyco_h, col_combs[1,i], col_combs[2,i], region))
  }
}

#Dataframes of means over each region
means_by_region_h <- glyco_h %>%
  group_by(Region) %>%
  summarise(TM = mean(TM, na.rm = TRUE), 
          SC = mean(SC, na.rm = TRUE),
          CC = mean(CC, na.rm = TRUE),
          ISV = mean(ISV, na.rm = TRUE),
          ESV = mean(ESV, na.rm = TRUE))

means_by_region_c <- glyco_c %>%
  group_by(Region) %>%
  summarise(TM = mean(TM, na.rm = TRUE), 
            SC = mean(SC, na.rm = TRUE),
            CC = mean(CC, na.rm = TRUE),
            ISV = mean(ISV, na.rm = TRUE),
            ESV = mean(ESV, na.rm = TRUE))


#Summarize data in terms of the eyes sampled from (height)
sum_eye_h <- glyco_h %>%
  group_by(Eye) %>%
  summarise(Region = Region,
            TM = mean(TM, na.rm = TRUE), 
            SC = mean(SC, na.rm = TRUE),
            CC = mean(CC, na.rm = TRUE),
            ISV = mean(ISV, na.rm = TRUE),
            ESV = mean(ESV, na.rm = TRUE))
sum_eye_h <- ungroup(sum_eye_h)
sum_eye_h <- sum_eye_h[!duplicated(sum_eye_h),]

#Summarize data in terms of the eyes sampled from (coverage)
sum_eye_c <- glyco_c %>%
  group_by(Eye) %>%
  summarise(Region = Region,
            TM = mean(TM, na.rm = TRUE), 
            SC = mean(SC, na.rm = TRUE),
            CC = mean(CC, na.rm = TRUE),
            ISV = mean(ISV, na.rm = TRUE),
            ESV = mean(ESV, na.rm = TRUE))
sum_eye_c <- ungroup(sum_eye_c)
sum_eye_c <- sum_eye_c[!duplicated(sum_eye_c),]

#Region comparison when data is summarized by eye
for (i in 1:6){
  for (col in cols){
    print("------------------------------")
    print(reg_combs[1,i])
    print(reg_combs[2,i])
    print(col)
    print(region_comp(sum_eye_c, reg_combs[1,i], reg_combs[2,i], col))
  }
}

#Outflow location comparison when data is summarized by eye
for (i in 1:10){
  for (region in regions){
    print("-------------------------------")
    print(col_combs[1,i])
    print(col_combs[2,i])
    print(region)
    print(col_comp(sum_eye_c, col_combs[1,i], col_combs[2,i], region))
  }
}
