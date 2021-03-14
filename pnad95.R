
setwd("C:/Users/user/Downloads/Thesis_paper_1/calibration")


# inpc ----

library(readxl)
inpc = read_excel("inpc.xlsx")
inpc = data.frame(inpc)


# load pnad95 


pnad95=readRDS('pnad95.rds')

attach(pnad95)

# my groups ----

sec = 3

pnad95$my_code = 0 


pnad95$my_code = ifelse(pnad95$V4716==1, 1, pnad95$my_code )   # AGR

pnad95$my_code = ifelse(pnad95$V4716==c(2, 3, 4), 2, pnad95$my_code )  # IND

pnad95$my_code = ifelse(pnad95$V4716==c(5, 6, 7, 8, 9), 3, pnad95$my_code ) # SERV


pnad95 = pnad95[pnad95$my_code!=0, ]


table(pnad95$my_code)


# V4718 - Rendimento mensal do trabalho principal ----


# minimo 1995 = 100 


defla = inpc[inpc$ano==1995, 'd10']


w_min = 100*defla

pnad95$V4718 = pnad95$V4718*defla  # deflate data


hr_trab = (252*8)/12

pnad95 = pnad95[pnad95$V4718!=999999999999, ]    # without declaration


pnad95 = pnad95[pnad95$V4718!= -1, ]             # without declaration

pnad95$rend_hr = pnad95$V4718/hr_trab           # earnings by hour

pnad95 = pnad95[pnad95$rend_hr<2.664851e+07, ]   # drop outliers

pnad95= pnad95[is.na(pnad95$rend_hr)==FALSE, ]   # drop NA

pnad95 = pnad95[pnad95$rend_hr>0.25*(w_min/hr_trab), ]   # drop earnings less than R$0.29/h



library(fGarch)

basicStats(pnad95$rend_hr)



W_i = by(pnad95$rend_hr, pnad95$my_code, mean)  # average earnings


W_i = W_i[1:sec]




# years of schooling  ----


anos_est  = by(pnad95$V4703, pnad95$my_code, mean)  # average years of schooling

anos_est = anos_est[1:sec]


# Elasticity of time spent at school  (phi) ----


eta = 0.25
beta = 0.69
c = (1-eta)/beta

s = 0.24*(anos_est/25) 

phi = c*s/(1-s) 

phi


# Proportion of individuals  ----

p_i = prop.table(table(pnad95$my_code))

p_i = p_i[1:sec]





# load pibs ----


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
df = df[,-2]

colnames(df)[2] = 'p_i'


row.names(df) = ocp


library("xlsx")

write.xlsx(df, file='data_95.xlsx')







