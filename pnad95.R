# Marcos Ribeiro

setwd("C:/Users/user/Downloads/Thesis_paper_1/calibration")

# libraries ----

library(readxl)
library(tidyverse)
library(dplyr)
library(fGarch)
library(magrittr)
library(haven)



d = function(x){                   # usar o in ao invés do ==
  k = ifelse(x==1, 1,
             ifelse(x %in% c(2, 3, 4), 2, 
                    ifelse(x %in% seq(4,9, 1), 3, 0)
             ))
  return(k)
}


# inpc ----


library(readxl)
inpc = read_excel("inpc.xlsx")
inpc = data.frame(inpc)


# Deflator ----

# minimo 1995 = 100 



defla = inpc[inpc$ano==1995, 'd10']


w_min = 100*defla


hr_trab = (252*8)/12  # 252 days  8 hours worked  12 months  hour worked per month


# rule 25%  - get s_hora > r25

r25 = (0.25*w_min)/hr_trab




# load pnad  1995 ----


pnad95 <- read_dta("D:/PNADs/PNAD_DATAZOOM/pnad1995pes.dta")



# add new column

pnad95 = pnad95 %>%
  add_column(new = 1)




pnad95 =  pnad95 %>%
  dplyr::mutate(s_hora = (v4718*defla)/hr_trab)




# change variable my code ----


pnad95 = pnad95 %>%
  dplyr::mutate(my_code = d(v4716))



# Filter ----


pnad95 = pnad95 %>%
  dplyr::filter(v4706 != 2 &                      # remove military
                  v4716 != 10 &                   # remove public administration
                  my_code !=0 &                   # remove NAs 
                  s_hora > r25 &                   # rule 25%
                  v4718 < 999999999999)           #            




# HC ----

anos_est =pnad95 %>%
  group_by(my_code) %>%
  dplyr::summarise( weighted.mean(v4703,
                                  w = v4729,
                                  na.rm = TRUE) )






# Average Wage

W_i = pnad95 %>%
  group_by(my_code) %>%
  summarise(weighted.mean(s_hora,
                          w = v4729,
                          na.rm=T))



# Proportion



soma = pnad95 %>%
  group_by(my_code) %>%
  count(new, wt = v4729) %>% 
  summarise(n)


p_i = soma %>%
  summarise(pi = n/sum(soma$n))



anos_est[1] = NULL

W_i[1] = NULL



colnames(anos_est) = c( 'anos_est')
colnames(W_i) = c('W_i')
colnames(p_i) = c('p_i')



data.frame(anos_est, W_i, p_i)



# Elasticity of time spent at school  (phi) ----


eta = 0.103
beta = 0.231
c = (1-eta)/beta

s = 0.24*(anos_est/25) 

phi = c*s/(1-s) 

colnames(phi) = 'phi'


# load pibs ----
sec = 3

library(readxl)

pibs = read_excel('pibs_set.xls', sheet = 's1')
pibs = data.frame(pibs)

head(pibs)



# get alfas ----

alfa95 = pibs[pibs$Data==1995, 1:sec+1]/pibs[pibs$Data==1995, sec+2 ]



library(data.table)

alfa95 = transpose(alfa95)
colnames(alfa95) = c('alfa95')



# create a data frame ----


ocp = c('AGR', 'IND', 'SEV')


df = data.frame(W_i, p_i, anos_est, phi, alfa95)


row.names(df) = ocp


library("xlsx")

write.xlsx(df, file='data_95.xlsx')







