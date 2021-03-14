# see http://asdfree.com/pesquisa-nacional-por-amostra-de-domicilios-nhts.html

# SEE http://rstudio-pubs-static.s3.amazonaws.com/433004_b68efaa351b94cbbb9839ea8f331a5c6.html#:~:text=A%20PNAD%202015%2C%20a%20%C3%BAltima,os%20moradores%20dos%20domic%C3%ADlios%20selecionados.&text=Para%20se%20ler%20os%20microdados,IBGE%20junto%20com%20os%20microdados.


################# Marcos Ribeiro



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
      ifelse(x %in% c(2, 3,4), 2, 
      ifelse(x %in% seq(4,12, 1), 3, 0)
             ))
  return(k)
}


# inpc ----


inpc = read_excel("inpc.xlsx")
inpc = data.frame(inpc)


# Deflator ----


# minimo 2015 = 788,00

defla = inpc[inpc$ano==2015, 'd10']

w_min = 788*defla


hr_trab = (252*8)/12  # 252 days  8 hours worked  12 months  hour worked per month


# rule 25%  - get s_hora > r25
 
r25 = (0.25*w_min)/hr_trab




# load pnad  2015 ----


#pnad15 = readRDS('pnad2015.rds')

#pnad15 = as_tibble(pnad15)   # convert to tibble

#pnad15 = pnad2015pes


pnad15 <- read_dta("D:/PNADs/PNAD_DATAZOOM/pnad2015pes.dta")



#basicStats(t$v4816)

# NAs na variável V4816 = 1.770730e+05


# add new column

pnad15 = pnad15 %>%
  add_column(new = 1)




pnad15 =  pnad15 %>%
  dplyr::mutate(s_hora = (v4718*defla)/hr_trab)



# change variable my code ----


pnad15 = pnad15 %>%
  dplyr::mutate(my_code = d(v4816))
  



pnad15 %>%
  count(is.na(my_code)==T & is.na(s_hora)==T )

basicStats(pnad15$s_hora)

# Filter ----


pnad15 = pnad15 %>%
        dplyr::filter(v4817 != 9 &            # remove military
        v4816 != 8 &                          # remove public administration
        my_code !=0 &                         # remove NAs 
        s_hora > r25 &                        # rule 25%
        v4718 < 999999995904)                        



# HC ----

hc =pnad15 %>%
  group_by(my_code) %>%
  summarise( weighted.mean(v4803,
                           w = v4729,
                           na.rm = TRUE) )






# Average Wage

W = pnad15 %>%
  group_by(my_code) %>%
  summarise(weighted.mean(s_hora,
                          w = v4729,
                          na.rm=T))





# Proportion






soma = pnad15 %>%
  group_by(my_code) %>%
  count(new, wt = v4729) %>% 
  summarise(n)


pi = soma %>%
  summarise(pi = n/sum(soma$n))






hc[1] = NULL

W[1] = NULL



colnames(hc) = c( 'hc')
colnames(W) = c('W')
colnames(pi) = c('pi')





data.frame(hc, W, pi)


