library(dplyr)
library(tidyr)
library(readr)

# read.csv("podatki/N_06.csv",sep=";", na=c("#","*")) %>% filter(h_mean=="18.84")  
# read.csv("podatki/N_06.csv",sep=";", na=c("#","*"))  %>% filter(h_mean > 18.84) 
# col.names = c("OCC koda","OCC naziv","grupa","celotna zaposlenost","5","6","7","8","9","10","11","12",
# 13","14","15","16","17","18","19"))


# NATIONAL DATA

nat6 <- read.csv2("podatki/N_06.csv", fileEncoding = "UTF-8" , na=c("#","*","**","***",""),) %>%
  select(2, 4,6,7,11,16)

nat8 <- read.csv2("podatki/N_08.csv", fileEncoding = "UTF-8" , na=c("#","*","**","***",""),) %>%
  select(1:2, 4,6,7,11,16) 

nat10 <- read.csv2("podatki/N_10.csv", fileEncoding = "UTF-8" , na=c("#","*","**","","***"),) %>%
  select(1:2, 4,6,7,11,16) 

nat12 <- read.csv2("podatki/N_12.csv", fileEncoding = "UTF-8" , na=c("#","*","**","","***"),) %>%
  select(1:2, 4,6,7,11,16) 

nat14 <- read.csv2("podatki/N_14.csv", fileEncoding = "UTF-8" , na=c("#","*","**","","***"),) %>%
  select(1:2, 4,6,7,11,16) 

nat16 <- read.csv2("podatki/N_16.csv", fileEncoding = "UTF-8" , na=c("#","*","**","","***"),) %>%
  select(1:2, 4,6,7,11,16) 

nat18 <- read.csv2("podatki/N_18.csv", fileEncoding = "UTF-8" , na=c("#","*","**","","***"),) %>%
  select(1:2, 4,6,7,11,16) 

# total employment

total_employment1 <- nat6 %>% 
  select(occ_title,tot_emp) %>% 
  rename(emp=tot_emp) %>% 
  mutate(leto=c(2006)) %>% 
  .[c(1,3,2)]  

total_employment2 <- nat8 %>% 
  select(occ_title,tot_emp) %>% 
  rename(emp=tot_emp) %>% 
  mutate(leto=c(2008)) %>% 
  .[c(1,3,2)] 

total_employment3 <- nat10 %>% 
  select(OCC_TITLE,TOT_EMP) %>% 
  rename(occ_title=OCC_TITLE, emp=TOT_EMP) %>% 
  mutate(leto=c(2010)) %>% 
  .[c(1,3,2)] 

total_employment4 <- nat12 %>% 
  select(OCC_TITLE,TOT_EMP) %>% 
  rename(occ_title=OCC_TITLE, emp=TOT_EMP) %>% 
  mutate(leto=c(2012)) %>% 
  .[c(1,3,2)] 

total_employment5 <- nat14 %>% 
  select(OCC_TITLE,TOT_EMP) %>% 
  rename(occ_title=OCC_TITLE, emp=TOT_EMP) %>% 
  mutate(leto=c(2014)) %>% 
  .[c(1,3,2)] 

total_employment6 <- nat16 %>% 
  select(OCC_TITLE,TOT_EMP) %>% 
  rename(occ_title=OCC_TITLE, emp=TOT_EMP) %>% 
  mutate(leto=c(2016)) %>% 
  .[c(1,3,2)] 

total_employment7 <- nat18 %>% 
  select(OCC_TITLE,TOT_EMP) %>% 
  rename(occ_title=OCC_TITLE, emp=TOT_EMP) %>% 
  mutate(leto=c(2018)) %>% 
  .[c(1,3,2)] 

total_employment <-  rbind(total_employment1,total_employment2, total_employment3, total_employment4, total_employment5, total_employment6, total_employment7)

# H_MEAN

h_mean1 <- nat6 %>% 
  select(occ_title,h_mean) %>% 
  rename(HM=h_mean) %>% 
  mutate(leto=c(2006)) %>% 
  .[c(1,3,2)] %>%
  drop_na(HM) 

h_mean2 <- nat8 %>% 
  select(occ_title,h_mean) %>% 
  rename(HM=h_mean) %>% 
  mutate(leto=c(2008)) %>% 
  .[c(1,3,2)] %>%
  drop_na(HM)

h_mean3 <- nat10 %>% 
  select(OCC_TITLE,H_MEAN) %>% 
  rename(occ_title=OCC_TITLE, HM=H_MEAN) %>% 
  mutate(leto=c(2010)) %>% 
  .[c(1,3,2)] %>%
  drop_na(HM)

h_mean4 <- nat12 %>% 
  select(OCC_TITLE,H_MEAN) %>% 
  rename(occ_title=OCC_TITLE, HM=H_MEAN) %>% 
  mutate(leto=c(2012)) %>% 
  .[c(1,3,2)] %>%
  drop_na(HM) 

h_mean5 <- nat14 %>% 
  select(OCC_TITLE,H_MEAN) %>% 
  rename(occ_title=OCC_TITLE, HM=H_MEAN) %>% 
  mutate(leto=c(2014)) %>% 
  .[c(1,3,2)] %>%
  drop_na(HM) 

h_mean6 <- nat16 %>% 
  select(OCC_TITLE,H_MEAN) %>% 
  rename(occ_title=OCC_TITLE, HM=H_MEAN) %>% 
  mutate(leto=c(2016)) %>% 
  .[c(1,3,2)] %>%
  drop_na(HM) 

h_mean7 <- nat18 %>% 
  select(OCC_TITLE,H_MEAN) %>% 
  rename(occ_title=OCC_TITLE, HM=H_MEAN) %>% 
  mutate(leto=c(2018)) %>% 
  .[c(1,3,2)] %>%
  drop_na(HM) 

h_mean <- rbind(h_mean1,h_mean2,h_mean3,h_mean4,h_mean5,h_mean6,h_mean7)

# A_MEAN

a_mean1 <- nat6 %>% 
  select(occ_title,a_mean) %>% 
  rename(AM=a_mean) %>% 
  mutate(leto=c(2006)) %>% 
  .[c(1,3,2)] %>%
  drop_na(AM) 

a_mean2 <- nat8 %>% 
  select(occ_title,a_mean) %>% 
  rename(AM=a_mean) %>% 
  mutate(leto=c(2008)) %>% 
  .[c(1,3,2)] %>%
  drop_na(AM)

a_mean3 <- nat10 %>% 
  select(OCC_TITLE,A_MEAN) %>% 
  rename(occ_title=OCC_TITLE, AM=A_MEAN) %>% 
  mutate(leto=c(2010)) %>% 
  .[c(1,3,2)] %>%
  drop_na(AM)

a_mean4 <- nat12 %>% 
  select(OCC_TITLE,A_MEAN) %>% 
  rename(occ_title=OCC_TITLE, AM=A_MEAN) %>% 
  mutate(leto=c(2012)) %>% 
  .[c(1,3,2)] %>%
  drop_na(AM) 

a_mean5 <- nat14 %>% 
  select(OCC_TITLE,A_MEAN) %>% 
  rename(occ_title=OCC_TITLE, AM=A_MEAN) %>% 
  mutate(leto=c(2014)) %>% 
  .[c(1,3,2)] %>%
  drop_na(AM) 

a_mean6 <- nat16 %>% 
  select(OCC_TITLE,A_MEAN) %>% 
  rename(occ_title=OCC_TITLE, AM=A_MEAN) %>% 
  mutate(leto=c(2016)) %>% 
  .[c(1,3,2)] %>%
  drop_na(AM) 

a_mean7 <- nat18 %>% 
  select(OCC_TITLE,A_MEAN) %>% 
  rename(occ_title=OCC_TITLE, AM=A_MEAN) %>% 
  mutate(leto=c(2018)) %>% 
  .[c(1,3,2)] %>%
  drop_na(AM) 

a_mean <- rbind(a_mean1,a_mean2,a_mean3,a_mean4,a_mean5,a_mean6,a_mean7)

# H_MEDIAN

h_median1 <- nat6 %>% 
  select(occ_title,h_median) %>% 
  rename(HME=h_median) %>% 
  mutate(leto=c(2006)) %>% 
  .[c(1,3,2)] %>%
  drop_na(HME) 

h_median2 <- nat8 %>% 
  select(occ_title,h_median) %>% 
  rename(HME=h_median) %>% 
  mutate(leto=c(2008)) %>% 
  .[c(1,3,2)] %>%
  drop_na(HME)

h_median3 <- nat10 %>% 
  select(OCC_TITLE,H_MEDIAN) %>% 
  rename(occ_title=OCC_TITLE, HME=H_MEDIAN) %>% 
  mutate(leto=c(2010)) %>% 
  .[c(1,3,2)] %>%
  drop_na(HME)

h_median4 <- nat12 %>% 
  select(OCC_TITLE,H_MEDIAN) %>% 
  rename(occ_title=OCC_TITLE, HME=H_MEDIAN) %>% 
  mutate(leto=c(2012)) %>% 
  .[c(1,3,2)] %>%
  drop_na(HME) 

h_median5 <- nat14 %>% 
  select(OCC_TITLE,H_MEDIAN) %>% 
  rename(occ_title=OCC_TITLE, HME=H_MEDIAN) %>% 
  mutate(leto=c(2014)) %>% 
  .[c(1,3,2)] %>%
  drop_na(HME) 

h_median6 <- nat16 %>% 
  select(OCC_TITLE,H_MEDIAN) %>% 
  rename(occ_title=OCC_TITLE, HME=H_MEDIAN) %>% 
  mutate(leto=c(2016)) %>% 
  .[c(1,3,2)] %>%
  drop_na(HME) 

h_median7 <- nat18 %>% 
  select(OCC_TITLE,H_MEDIAN) %>% 
  rename(occ_title=OCC_TITLE, HME=H_MEDIAN) %>% 
  mutate(leto=c(2018)) %>% 
  .[c(1,3,2)] %>%
  drop_na(HME) 

h_median <- rbind(h_median1,h_median2,h_median3,h_median4,h_median5,h_median6,h_median7) 

# A_MEDIAN
a_median1 <- nat6 %>% 
  select(occ_title,a_median) %>% 
  rename(AME=a_median) %>% 
  mutate(leto=c(2006)) %>% 
  .[c(1,3,2)] %>%
  drop_na(AME) 

a_median2 <- nat8 %>% 
  select(occ_title,a_median) %>% 
  rename(AME=a_median) %>% 
  mutate(leto=c(2008)) %>% 
  .[c(1,3,2)] %>%
  drop_na(AME)

a_median3 <- nat10 %>% 
  select(OCC_TITLE,A_MEDIAN) %>% 
  rename(occ_title=OCC_TITLE, AME=A_MEDIAN) %>% 
  mutate(leto=c(2010)) %>% 
  .[c(1,3,2)] %>%
  drop_na(AME)

a_median4 <- nat12 %>% 
  select(OCC_TITLE,A_MEDIAN) %>% 
  rename(occ_title=OCC_TITLE, AME=A_MEDIAN) %>% 
  mutate(leto=c(2012)) %>% 
  .[c(1,3,2)] %>%
  drop_na(AME) 

a_median5 <- nat14 %>% 
  select(OCC_TITLE,A_MEDIAN) %>% 
  rename(occ_title=OCC_TITLE, AME=A_MEDIAN) %>% 
  mutate(leto=c(2014)) %>% 
  .[c(1,3,2)] %>%
  drop_na(AME) 

a_median6 <- nat16 %>% 
  select(OCC_TITLE,A_MEDIAN) %>% 
  rename(occ_title=OCC_TITLE, AME=A_MEDIAN) %>% 
  mutate(leto=c(2016)) %>% 
  .[c(1,3,2)] %>%
  drop_na(AME) 

a_median7 <- nat18 %>% 
  select(OCC_TITLE,A_MEDIAN) %>% 
  rename(occ_title=OCC_TITLE, AME=A_MEDIAN) %>% 
  mutate(leto=c(2018)) %>% 
  .[c(1,3,2)] %>%
  drop_na(AME) 

a_median <- rbind(a_median1,a_median2,a_median3,a_median4,a_median5,a_median6,a_median7) 

# STATE DATA  

st6 <- read.csv2("podatki/S_06.csv", fileEncoding = "UTF-8" , na=c("#","*","**","***",""),) %>%
  select(3,5,7,9,10,14,19)  

st8 <- read.csv2("podatki/S_08.csv", fileEncoding = "UTF-8" , na=c("#","*","**","***",""),) %>%
  select(3,5,7,9,10,14)  

st10 <- read.csv2("podatki/S_10.csv", fileEncoding = "UTF-8" , na=c("#","*","**","","***"),) %>%
  select(3,5,7,11,12,16,21)  

st12 <- read.csv2("podatki/S_12.csv", fileEncoding = "UTF-8" , na=c("#","*","**","","***"),) %>%
  select(3,5,7,11,12,16,21) 

st14 <- read.csv2("podatki/S_14.csv", fileEncoding = "UTF-8" , na=c("#","*","**","","***"),) %>%
  select(3,5,7,11,12,16,21)  

st16 <- read.csv2("podatki/S_16.csv", fileEncoding = "UTF-8" , na=c("#","*","**","","***"),) %>%
  select(3,5,7,11,12,16,21) 

st18 <- read.csv2("podatki/S_18.csv", fileEncoding = "UTF-8" , na=c("#","*","**","","***"),) %>% 
  select(3,5,7,11,12,16,21) %>% View()

# total employment












