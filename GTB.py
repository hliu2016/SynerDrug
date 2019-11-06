# -*- coding: utf-8 -*-
"""
Created on Sat Oct 26 20:34:43 2019

@author: 12402
"""

from sklearn.ensemble import GradientBoostingClassifier
from sklearn.metrics import roc_auc_score
import numpy as np
import os
import pandas as pd
import csv
from sklearn.metrics import roc_curve, auc
import matplotlib.pyplot as plt

def loadDataSet(fileName):
    benchmark=[]
    with open(fileName, 'r') as f:
        f.readline()
        for line in f:
            items=line.strip().split(',')
            benchmark.append(items[1:])
    simpler = np.random.permutation(len(benchmark))
    benchmark = pd.DataFrame(benchmark)
    benchmark = benchmark.take(simpler)
    benchmark = benchmark.values        
    dataMat = []; labelMat = []
    for i in range(0,len(benchmark)):
        lineArr = np.zeros(6074)
        for j in range(0, len(benchmark[i,:])-1):
            lineArr[j] = float(benchmark[i,j])
        dataMat.append(lineArr[0:])
        labelMat.append(int(benchmark[i,-1]))
    return dataMat,labelMat

def performance(labelArr, predictArr):
    TP = 0.; TN = 0.; FP = 0.; FN = 0.   
    for i in range(len(labelArr)):
        if labelArr[i] == 1 and predictArr[i] == 1:
            TP += 1.
        if labelArr[i] == 1 and predictArr[i] == 0:
            FN += 1.
        if labelArr[i] == 0 and predictArr[i] == 1:
            FP += 1.
        if labelArr[i] == 0 and predictArr[i] == 0:
            TN += 1.
    ACC = (TP + TN) / (TP + FN + FP + TN)
    SN = TP/(TP + FN) 
    SP = TN/(FP + TN) 
    if TP + FP != 0:       
        Precision = TP/(TP + FP)
    else:
        Precision = 0
    Recall = SN
    if Precision + Recall != 0:        
        F1 = 2 * (Precision * Recall)/(Precision + Recall)
    else:
        F1 = 0
    fz = float(TP*TN - FP*FN)
    fm = float(np.math.sqrt((TP+FP)*(TP+FN)*(TN+FP)*(TN+FN)))
    if fm !=0:        
        MCC = fz/fm
    else:
        MCC = 0    
    return ACC, SN, SP, Precision, F1, MCC

def classifier(train_X1, train_y1, test_X, test_y,clf):
    print (" training begin..." )
    clf = clf.fit(train_X1,train_y1)
    print (" training end.")
    # test Classifier with testsets
    print (" test begin.")
    predict_ = clf.predict(test_X) #return type is float64
    proba = clf.predict_proba(test_X) #return type is float64
    print (" test end.") 
    ACC, SN, SP, Precision, F1, MCC = performance(test_y, predict_)
    pre_prob = []
    for i in range(len(proba)):
        pre_prob.append(proba[i][1])
    AUC = roc_auc_score(test_y, pre_prob)
    #save output 
    eval_output = []
    eval_output.append(ACC)
    eval_output.append(SN)
    eval_output.append(SP)
    eval_output.append(Precision)
    eval_output.append(F1)
    eval_output.append(MCC)
    #eval_output.append(score_);
    eval_output.append(AUC)
    eval_output = np.array(eval_output, dtype=float)
#    if not os.path.exists(outpath+'/result'):
#        os.makedirs(outpath+'/result')
#    np.savetxt(outpath+'/result'+"/test_y.data",test_y,fmt="%d",delimiter="\t")
#    np.savetxt(outpath+'/result'+"/predict.data",predict_,fmt="%d",delimiter="\t") 
#    np.savetxt(outpath+'/result'+"/eval_output.data",eval_output,fmt="%f",delimiter="\t")
    return ACC, SN, SP, Precision, F1, MCC, AUC

def mean_fun(onelist):
    count = 0
    for i in onelist:
        count += i
    return float(count/len(onelist))

if __name__ == '__main__':
    out = open(r'E:\zwh\education\graduated\1导师布置\小论文5（GTB）\code\code2\GTB results\GTBresults.csv','a',newline='')
    csv_write = csv.writer(out,dialect='excel') 
#    csv_write.writerow(['n_estimators','Precision','Recall','F-measure','MCC','AUC'])
    #replace your output path here
    fold = 10
    p = 1.0/fold
    benchmarkX,benchmarky = loadDataSet(r'E:\zwh\education\graduated\1导师布置\小论文5（GTB）\code\code2\rwr results\t_m_0.9.csv')
    benchmarkX = np.array(benchmarkX)
    benchmarky = np.array(benchmarky)
    assert len(benchmarkX) == len(benchmarky)
    size = len(benchmarkX)
####tuning
#    n_est_num = [300,350,400,450,500,550,600,650,700,750,800]
#    for item in enumerate(n_est_num):
#        e_num = item[1]
#        params = { 'n_estimators': 300, 'max_depth': 13, 'subsample': 0.7, 'learning_rate': 0.3, 'min_samples_leaf': 2, 'random_state': 7}
#        clf = GradientBoostingClassifier(**params)
#        matric=[]
#        ACCs = [];SNs = []; SPs =[]; MCCs = []; Precisions = []; F1s = [];AUCs=[]
#        for i in range(fold):
#            trainX = np.row_stack((benchmarkX[:int(size*(p*i)), :], benchmarkX[int(size*(p*(i+1))):, :]))
#            trainY = benchmarky[ :int(size*(p*i))].tolist() + benchmarky[int(size*(p*(i+1))):].tolist()
#            testX = benchmarkX[int(size*(p*i)):int(size*(p*(i+1))), :]
#            testY = benchmarky[int(size*(p*i)):int(size*(p*(i+1)))]
#            ACC, SN, SP, Precision, F1, MCC, AUC = classifier(trainX,trainY,testX,testY,clf)
#            ACCs.append(ACC) 
#            SNs.append(SN)
#            SPs.append(SP)
#            MCCs.append(MCC)
#            Precisions.append(Precision)
#            F1s.append(F1)
#            AUCs.append(AUC)
#        matric.append(e_num)
#        matric.append(mean_fun(Precisions))
#        matric.append(mean_fun(SNs))
#        matric.append(mean_fun(F1s))
#        matric.append(mean_fun(MCCs))
#        matric.append(mean_fun(AUCs))
#        csv_write.writerow(matric)
#    out.close()
    params = { 'n_estimators': 300, 'max_depth': 13, 'subsample': 0.7, 'learning_rate': 0.2, 'min_samples_leaf': 2, 'random_state': 7}
    clf = GradientBoostingClassifier(**params)
    matric=[]
    ACCs = [];SNs = []; SPs =[]; MCCs = []; Precisions = []; F1s = [];AUCs=[]
    for i in range(fold):
        i = 1
        trainX = np.row_stack((benchmarkX[:int(size*(p*i)), :], benchmarkX[int(size*(p*(i+1))):, :]))
        trainY = benchmarky[ :int(size*(p*i))].tolist() + benchmarky[int(size*(p*(i+1))):].tolist()
        testX = benchmarkX[int(size*(p*i)):int(size*(p*(i+1))), :]
        testY = benchmarky[int(size*(p*i)):int(size*(p*(i+1)))]
        ACC, SN, SP, Precision, F1, MCC, AUC = classifier(trainX,trainY,testX,testY,clf)
        ACCs.append(ACC) 
        SNs.append(SN)
        SPs.append(SP)
        MCCs.append(MCC)
        Precisions.append(Precision)
        F1s.append(F1)
        AUCs.append(AUC)
    matric.append(mean_fun(Precisions))
    matric.append(mean_fun(SNs))
    matric.append(mean_fun(F1s))
    matric.append(mean_fun(MCCs))
    matric.append(mean_fun(AUCs))
    csv_write.writerow(matric)
    out.close()


    

