################ Marcos Ribeiro



setwd("C:/Users/user/Downloads/Thesis_paper_1/calibration")


library(readxl)
library(tidyverse)
library(dplyr)
library(fGarch)
library(magrittr)
library(haven)
library(xlsx)
library(data.table)




d = function(x){                   # usar o %in% ao invés do ==
  k = ifelse(x%in%c(1, 13), 1,
         ifelse(x %in% c(2, 3,4), 2, 
                ifelse(x %in% seq(5, 12, 1), 3, 0)
         ))
  return(k)
}


# 2006 para trás usar v4703
# 2007 adiante usar v4803

sy = 2010
xx = seq(2002, 2015, 1)
xx = xx[!xx%in%sy]  # remove o 2010

c = 0
for(k in xx){
  c = c + 1
  if(k<=2006){info(k, v4703)}
  else{info(k, v4803)}
  cat('\n Progresso:', paste(round(100*c/length(xx),4), '%', sep=''), '\n' )
  cat('Fim da Função \n')

}






info = function(year, t_est){
  cat(strrep("*",40) )
  cat('\n Ano:', year)
  t_est = enquo(t_est)                    # Create quosure
  
  inpc = read_excel("inpc.xlsx")
  inpc = data.frame(inpc)
  
  
  s_min = read_excel("salário_min.xlsx")
  s_min = data.frame(s_min)
  
  
  defla = inpc[inpc$ano==year, 'd10']
  
  w_min = s_min[s_min$ano==year, 'salário']*defla
  
  hr_trab = (252*8)/12       # 252 days  8 hours worked  12 months  hour worked per month
                             # rule 25%  - get s_hora > r25
  r25 = (0.25*w_min)/hr_trab
  
  
  pnad <- read_dta(paste('D:/PNADs/PNAD_DATAZOOM/pnad', year,'pes.dta', sep=''))
  pnad = pnad %>% add_column(new = 1)
  pnad =  pnad %>% dplyr::mutate(s_hora = (v4718*defla)/hr_trab)
  pnad = pnad %>% dplyr::mutate(my_code = d(v4816))
  pnad %>% count(is.na(my_code)==T & is.na(s_hora)==T )
  
  pnad = pnad %>%
    dplyr::filter(is.na(v4817)|v4817 != 9 &       # remove military
                    v4816 != 8 )                   # remove public administration
  
  
  pnad = pnad %>%
    dplyr::filter(
        my_code !=0 &                       # remove NAs 
        s_hora > r25 &                      # rule 25%
        v4718 < 999999995904) 
  cat('\n Dimensão da PNAD (L x C):', dim(pnad) )
  
  anos_est =pnad %>%
    group_by(my_code) %>%
    dplyr::summarise(weighted.mean(!!t_est,
                             w = v4729,
                             na.rm = TRUE), .groups = 'drop' );
  W_i = pnad %>%
    group_by(my_code) %>%
    summarise(weighted.mean(s_hora,
                            w = v4729,
                            na.rm=T), .groups = 'drop')
  soma = pnad %>%
    group_by(my_code) %>%
    count(new, wt = v4729) %>% 
    summarise(n, .groups = 'drop')
  
  p_i = soma %>%
    summarise(p_i = n/sum(soma$n), .groups = 'drop')
  
  anos_est[1] = NULL
  W_i[1] = NULL
  
  colnames(anos_est) = c('anos_est')
  colnames(W_i) = c('W_i')
  colnames(p_i) = c('p_i')
  
  data.frame(anos_est, W_i, p_i)
  
  
  eta = 0.103
  beta = 0.231
  c = (1-eta)/beta
  s = 0.24*(anos_est/25) 
  phi = c*s/(1-s) 
  colnames(phi) = 'phi'
  
  sec = 3
  
  pibs = read_excel('pibs_set.xls', sheet = 's1')
  pibs = data.frame(pibs)


  alfa = pibs[pibs$Data==year, 1:sec+1]/pibs[pibs$Data==year, sec+2 ]
  alfa = transpose(alfa)
  colnames(alfa) = 'alfa'
  

  ocp = c('AGR', 'IND', 'SEV')
  df = data.frame(W_i, p_i, anos_est, phi, alfa)
  row.names(df) = ocp
  write.xlsx(df, file=paste('data_',substr(year,ifelse((year>=2000 & year<=2009),4,3),4), '.xlsx', sep=''))

}








