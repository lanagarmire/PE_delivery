#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Tue May  5 20:07:48 2020

@author: wangdi
"""

# -*- coding: utf-8 -*-
import os
os.chdir('./')

import sys
import time
from cox_nnet_v2 import *
#from cox_nnet_v1 import *
import numpy
import sklearn
from sklearn.model_selection import train_test_split

start = time.time()
# load data
index=str(sys.argv[1])

x_train = numpy.loadtxt(fname=("./x_train"+index+".csv"),delimiter=",",skiprows=1)
x_test = numpy.loadtxt(fname=("./x_test"+index+".csv"),delimiter=",",skiprows=1)
ytime_train = numpy.loadtxt(fname=("./ytime_train"+index+".csv"),delimiter=",",skiprows=1)
ytime_test = numpy.loadtxt(fname=("./ytime_test"+index+".csv"),delimiter=",",skiprows=1)
ystatus_train = numpy.loadtxt(fname=("./ystatus_train"+index+".csv"),delimiter=",",skiprows=1)
ystatus_test = numpy.loadtxt(fname=("./ystatus_test"+index+".csv"),delimiter=",",skiprows=1)

# parameters tunning
model_params = dict(node_map = None, input_split = None)

#search_params = dict(method = "nesterov", learning_rate=0.005, momentum=0.9,

#        max_iter=1500, stop_threshold=0.995, patience=1000, patience_incr=2, rand_seed = 100,

#        eval_step=23, lr_decay = 0.9, lr_growth = 1.0)

search_params = dict(method = "adam", learning_rate=1e-2, beta1 = 0.9, beta2 = 0.999, epsilon = 1e-8, max_iter=10000, stop_threshold=0.995, patience=1000, patience_incr=2, 
    rand_seed = 100, eval_step=23, lr_decay = 0.9, lr_growth = 1.0)

cv_params = dict(cv_seed=1, n_folds=10, cv_metric = "cindex", L2_range = numpy.arange(-5,1))
q1 = time.time()
#profile log likelihood to determine lambda parameter
cv_loglikelihoods, L2_reg_params, mean_cvpl = L2CVProfile(x_train,ytime_train,ystatus_train,
    model_params, search_params, cv_params, verbose=False)

numpy.savetxt(("./SRTR_cvpl"+index+".csv"), mean_cvpl, delimiter=",")
median = time.time()
#build model based on optimal lambda parameter
L2_reg = L2_reg_params[numpy.argmax(mean_cvpl)]
model_params = dict(node_map = None, input_split = None, L2_reg=numpy.exp(L2_reg))
model, cost_iter = trainCoxMlp(x_train, ytime_train, ystatus_train, model_params, search_params, verbose=True)
q3 = time.time()
theta_train = model.predictNewData(x_train)
theta = model.predictNewData(x_test)
#print(theta)
numpy.savetxt(("./SRTR_theta_train"+index+".csv"), theta_train, delimiter = ",")
numpy.savetxt(("./SRTR_theta_hcc_new"+index+".csv"), theta, delimiter=",")

stop = time.time()
print(q1, median, q3, stop)

# feature importance
features = varImportance(model, x_train, ytime_train, ystatus_train)
features2 = permutationImportance(model, 1, x_train, ytime_train, ystatus_train)
features3 = permutationImportance(model, 2, x_train, ytime_train, ystatus_train)
numpy.savetxt(("./SRTR_features"+index+".csv"), features, delimiter=",")
numpy.savetxt(("./SRTR_features2"+index+".csv"), features2, delimiter=",")
numpy.savetxt(("./SRTR_features3"+index+".csv"), features3, delimiter=",")

# feature directionality
sign = signOfBeta(model, x_test)
numpy.savetxt(("./SRTR_sign"+index+".csv"), sign, delimiter=",")
# get running time
run_time = [q1-start, median-q1, q3-median, stop- q3, stop-start]
numpy.savetxt("./running_time_adam"+index+".csv",run_time, delimiter=",")
print (('total running time: %f seconds') % (time.time() - start))


#saveModel(model, "savedModel2")

# get output embedding
b = map(lambda tvar : tvar.eval(), model.b)
W = map(lambda tvar : tvar.eval(), model.W)
b=b[0]
W=W[0]

b=numpy.asarray(b,dtype='float32')
W=numpy.asarray(W,dtype='float32')
numpy.savetxt("./Wr.csv", W, delimiter=",")
numpy.savetxt("./br.csv", b, delimiter=",")
