# -*- coding: utf-8 -*-
"""
Created on Tue Dec  8 10:28:12 2020

@author: Marcos J Ribeiro
"""


import numpy as np
from math import gamma
import matplotlib.pyplot as plt
from scipy.optimize import minimize
import pandas as pd

import os
os.chdir("C:\\Users\\user\\Downloads\\Thesis_paper_1\\calibration")

np.set_printoptions(suppress=True) # supress scientific notation



## parameteres ----


pd.read_excel('data_2.xlsx')


def get_data(year):
    w='data_' + str(year)
    alfa = np.array(pd.read_excel(f'{w}.xlsx').iloc[:,5])
    phi = np.array(pd.read_excel(f'{w}.xlsx').iloc[:,4])
    return alfa, phi



def pars(year):
    global alfa, rho, beta, theta, eta, psi, kappa, lamb, gamma1, G, phi
    rho = 0.19
    beta = 2
    theta = 3.44
    eta = 0.25
    psi = eta**eta*(1-eta)**(1-eta)
    kappa = 1/(1 - eta)

    lamb = (theta*(1 - eta) - 1 ) / (theta*(1- eta)) 
    gamma1 = eta*kappa
    
    G = gamma(1 - np.multiply( np.divide(1, np.multiply(theta, (1-rho)) ), np.divide(1, (1 - eta) ) ) )   
    
    alfa, phi = get_data(year)



# taus
st=8
def taus2(st):        
    tau = np.random.uniform(low=-0.989, high=5e4, size=(st)) 
    tau[7] = 100
    x1 = np.array(tau)    
    return x1


# Time spent at school

def sf( ):
    global s
    s = np.power( (1 + np.divide( (1-eta), ( np.multiply(beta, phi) ) ) ), -1 )
    #s = s.reshape(st, 1)
    return s

pars(2)



## N_i = H_i / w_i^gamma1


def N_if(x1):
    s = sf()
    T = np.multiply(np.power((1 + x1), (-1*theta*lamb-gamma1)), np.power(psi, theta*lamb) )    
    S = np.power(s, (phi*(theta*lamb + (1-gamma1)))  )
    S2 = np.multiply(np.power((1 - s), (((1 - eta)*theta*lamb)/beta)  ), np.power(eta, gamma1) )
    N_i = np.multiply(np.multiply(T, np.multiply(S, S2)), G)
    return N_i


#  linear system 


def lin_sys(x1):
#    global A, B, C, D, N_i
    A = np.repeat(alfa, st).reshape(st, st).T  # matriz de parâmetros    
    B = np.repeat(alfa, st).reshape(st, st).T        # Matriz de alfas 
    for i in range(st):
        for j in range(st):
            if i==j:
                A[i, j] = 1 - (A[i, j] - 1)*(theta*lamb + gamma1)
                B[i, j] = B[i, j] - 1
            if i != j:
                A[i, j] = -A[i, j]*(theta*lamb + gamma1) 
  
    N_i = np.log( N_if(x1) )          # Logaritmo de N_i
    C = np.log(alfa)                  # Logaritmo dos alfas
    D = np.dot(B, N_i) + C            # produto matricial      
    lin = np.linalg.solve(A, D)       # Resolvendo o sistema  A w = D
    res = np.exp(lin)
    return res


# w_til


def w_til_f(x1):
    A = np.multiply(np.power((1 + x1), -eta), w )
    S = np.multiply(np.power(s, phi), np.power((1 - s), ((1-eta)/beta) ) )    
    w_til = np.multiply(psi, np.multiply(A, S) )    
    return w_til





# p_i

def p_i_f(x1):
    w_til = w_til_f(x1)
    p_i =  np.power(w_til, theta)/ np.sum( np.power(w_til, theta) )   
    return p_i
    



# wages - model 

def W_i_f(x1):
    w_til = w_til_f(x1)
    S_wtil = np.power((np.sum( np.power(w_til, theta) ) ), (kappa/theta) )
    S = np.power((1-s), (-1/beta))
    W_i = 1/kappa*np.multiply(np.multiply(S, S_wtil), G )     
    return W_i

    
# PNAD - DATA


def get_p_i(year):
    global p_s2
    w='data_' + str(year)
    p_i = np.array(pd.read_excel(f'{w}.xlsx').iloc[:,2])
    p_s2 = p_i[1:st]
    return p_i



# Objective function

pars(15)
get_p_i(15)


def obj2(x1):    
    global w
    x1 = x1.reshape((st)) 
    x1[7] = 100    
    w = lin_sys(x1) 
    W_i = W_i_f(x1) 
    p_i = p_i_f(x1)   
    p_i = p_i[1:st]    
    D = np.sum(np.power( np.divide(( p_s2 - p_i ), p_s2 )  , 2 ) )
    D = np.log(D)
    return D


# bounds

Bd = ((-0.99, 5e4), )*st 
Bd = np.array(Bd, dtype=object)



x1=taus2(st)

obj2(x1) 



p_i_f(x1)
g_pif(x1)



# solver 1  



cc = 0
def callback(x):
    global cc
    cc += 1
    fobj = obj2(x)
    print(f'\033[1;033mObjetivo: {np.around(fobj, 5)}, iter: {cc}') 
    

cc=0
sol= minimize(obj2, x1,  method='L-BFGS-B', 
              callback=callback, bounds=Bd, 
              tol=1e-9000, options={'maxiter':1e3, 
                                    'maxfun':1e10000, 
                                    'maxcor': 10000,
                                    'maxls': 200,
                                    'eps': 1e-900,})       


g_pif(sol.x)


cc=0
sol= minimize(obj2, sol.x,  method='Nelder-Mead', 
              callback=callback, 
              tol=1e-9, options={'maxiter':2e4})


                                         
sol.x
sol


# get distortions


def get_dist(year):
    w ='dist_' + str(year)
    dist = np.array(pd.read_excel('dist.xlsx').loc[:,w])
    return dist



pars(2)
get_p_i(2)
obj2(get_dist(2))




def g_pif(x1):
    p_i = p_i_f(x1)
    p_s = get_p_i(2)       
    fig,ax = plt.subplots(figsize=(10,8))
    plt.scatter(p_i, p_s)
    plt.plot([0.0, 0.4], [0.0, 0.4], 'k-', lw=2, label='45° line', color='blue')
    plt.grid(True)
    plt.xticks(fontsize=20)
    plt.yticks(fontsize=20)
    plt.xlabel(r"$p_i$ - Model", fontsize=20)
    plt.ylabel(r"$p_i$ - Data", fontsize=20)
    plt.legend(loc="lower right", prop={'size': 20})
    plt.tight_layout()  





ocp = np.array(['AGR', 'IND_EXTR', 'IND_TRANSF', 'IND_CONST',
                'SERV_COMER', 'SERV_TRANSP_ARM',  'SERV_FIN_IMOB', 'ADM_SAUDE_EDUC'])



# human capital

def hc_plt(res):
    N_i = N_if(get_dist(15))
    H_i = np.multiply(N_i, np.power(w, gamma1) ) 

    plt.scatter(H_i, w, s=50)
    for tt, txt in enumerate(ocp):
        plt.annotate(txt, (H_i[tt], w[tt]), size=15)  


plt.scatter(H_i, np.arange(1, 9, 1))
 

pars(15)
get_p_i(15)


hc_plt(get_dist(15))        



# p_i


def pi_plot(res):
    plt.scatter( p_i_f(res), w)
    for tt, txt in enumerate(ocp):
        plt.annotate(txt, (p_i_f(sol.x)[tt], w[tt]), size=15)  



pars(2)
get_p_i(2)

pi_plot(get_dist(2))


# output

t_y = np.arange(0, 1, 0.01)
Y_tau = []

for i  in t_y:
    res = get_dist(15) + i    
    N_i = N_if(res)
    H_i = np.multiply(N_i, np.power(w, gamma1) ) 
        
    Y_tau.append( np.prod(np.power(H_i, alfa )) )
    

plt.plot(t_y, Y_tau)






