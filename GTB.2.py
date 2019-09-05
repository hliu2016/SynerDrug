# -*- coding: utf-8 -*-
"""
Created on Mon Apr  2 10:00:22 2018

"""
from sklearn.ensemble import GradientBoostingClassifier
from sklearn.metrics import roc_auc_score
import numpy as np
import os

def loadDataSet(fileName):
    fr = open(fileName)
    dataMat = []; labelMat = []
    for eachline in fr:
        lineArr = np.zeros(6075)
        curLine = eachline.strip().split() 
        for i in range(1, len(curLine)):
            n = int(curLine[i].split(':')[0])
            lineArr[n-1] = float(curLine[i].split(':')[1])
        dataMat.append(lineArr[1:])
        labelMat.append(int(float(curLine[0])))
    fr.close()
    return dataMat,labelMat

def splitDataSet(fileName, split_size, outdir):
    if not os.path.exists(outdir): 
        os.makedirs(outdir)
    fr = open(fileName,'r') 
    num_line = 0
    onefile = fr.readlines()
    num_line = len(onefile)        
    arr = np.arange(num_line)
    np.random.shuffle(arr) 
    list_all = arr.tolist()
    remain = num_line%split_size
    if remain != 0:
        for i in range(remain):
            list_all.pop()
    each_size = num_line // split_size 
    split_all = []; each_split = []
    count_num = 0; count_split = 0  
    for i in range(len(list_all)): 
        each_split.append(onefile[int(list_all[i])].strip()) 
        count_num += 1
        if count_num == each_size:
            count_split += 1 
            array_ = np.array(each_split)
            np.savetxt(outdir + "/split_" + str(count_split) + '.txt',\
                        array_,fmt="%s", delimiter='\t')  
            split_all.append(each_split) 
            each_split = []
            count_num = 0
    return split_all

def generateDataSet(filename,fold,outdir):
    if not os.path.exists(outdir +'/train'):
        os.makedirs(outdir +'/train')
    if not os.path.exists(outdir +'/test'):
        os.makedirs(outdir +'/test')
    splitDataSet(filename,fold,outdir+'/'+ 'split')
    listfile = os.listdir(outdir+'/'+'split')
    train_all = []; test_all = [];cross_now = 0
    for filename1 in listfile:
        train_sets = []; test_sets = []; 
        cross_now += 1 
        for filename2 in listfile:
            if filename2 !=filename1:
                fr = open(outdir+'/'+'split'+'/'+filename2, 'r')
                for i in fr:
                    train_sets.append(i)
                fr.close()
        with open(outdir +"/train/train_"+str(cross_now)+".txt",'w') as fw_train:
            for oneline_test in train_sets:
                fw_train.write(oneline_test)
        train_all.append(train_sets)
        fr = open(outdir+'/'+'split'+'/'+filename1, 'r')
        for i in fr:
            test_sets.append(i)
        fr.close()
        with open(outdir +"/test/test_"+str(cross_now)+".txt",'w') as fw_test:
            for oneline_test in test_sets:
                fw_test.write(oneline_test)
        test_all.append(test_sets)
    return train_all,test_all  

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

def mean_fun(onelist):
    count = 0
    for i in onelist:
        count += i
    return float(count/len(onelist))

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
    if not os.path.exists(outpath+'/result'):
        os.makedirs(outpath+'/result')
    np.savetxt(outpath+'/result'+"/test_y.data",test_y,fmt="%d",delimiter="\t")
    np.savetxt(outpath+'/result'+"/predict.data",predict_,fmt="%d",delimiter="\t") 
    np.savetxt(outpath+'/result'+"/eval_output.data",eval_output,fmt="%f",delimiter="\t")
    return ACC, SN, SP, Precision, F1, MCC, AUC

def cross(filename,fold,outpath,clf):
    generateDataSet(filename,fold,outpath)
    ACCs = [];SNs = []; SPs =[]; MCCs = []; Precisions = []; F1s = [];AUCs=[]
    for i in range(fold):
        trainX = [];trainy = []
        testX = [];testy = []
        testX,testy = loadDataSet(outpath + '/test/' + 'test_' + str(i+1) + '.txt')
        trainX,trainy = loadDataSet(outpath + '/train/' + 'train_' + str(i+1) + '.txt')
        ACC, SN, SP, Precision, F1, MCC, AUC = classifier(trainX,trainy,testX,testy,clf)
        ACCs.append(ACC) 
        SNs.append(SN)
        SPs.append(SP)
        MCCs.append(MCC)
        Precisions.append(Precision)
        F1s.append(F1)
        AUCs.append(AUC)
        print('Step %d result: ACC:%.3f SN:%.3f SP:%.3f Precision:%.3f F1:%.3f MCC:%.3f AUC:%.3f' 
          %(i,ACC,SN,SP,Precision,F1, MCC, AUC) )
    ACC_mean = mean_fun(ACCs)
    SN_mean = mean_fun(SNs)
    SP_mean = mean_fun(SPs)
    MCC_mean = mean_fun(MCCs)
    Precision_mean = mean_fun(Precisions)
    F1_mean = mean_fun(F1s)
    AUC_mean = mean_fun(AUCs)
    print('Cross validation result:')
    print('ACC:%.3f SN:%.3f SP:%.3f Precision:%.3f F1:%.3f MCC:%.3f AUC:%.3f' 
          %(ACC_mean,SN_mean,SP_mean,Precision_mean,F1_mean, MCC_mean, AUC_mean) )
    return ACC_mean,SN_mean,SP_mean,Precision_mean,F1_mean, MCC_mean, AUC_mean 

def test(trainfile,testfile,outpath):
    train_X = [];train_y = []
    test_X = [];test_y = []
    test_X,test_y = loadDataSet(testfile)
    train_X,train_y = loadDataSet(trainfile)
    ACC, SN, SP, Precision, F1, MCC, AUC = classifier(train_X,train_y,test_X,test_y,clf)
    print('Independent result:')
    print('ACC:%.3f SN:%.3f SP:%.3f Precision:%.3f F1:%.3f MCC:%.3f AUC:%.3f' %(ACC, SN, SP, Precision, F1, MCC, AUC))
    return ACC, SN, SP, Precision, F1, MCC, AUC

if __name__ == '__main__':
    outpath =r'C:/Users/Administrator/Desktop/GTB/'  #replace your output path here,for example outpath =r'c:\experiment\result'
    fold = 10     #n-fold cross validation
    trainfile=r'C:/Users/Administrator/Desktop/GTB/0.7T.libsvm' #your training data,for example trainfile = r'c:\experiment\train.txt'
    testfile=r''  #your testing data
    params = { 'n_estimators': 600, 'max_depth': 13, 'subsample': 0.7,
        'learning_rate': 0.3, 'min_samples_leaf': 2, 'random_state': 7}
    clf = GradientBoostingClassifier(**params)
    cross(trainfile,fold,outpath,clf) #this method is used to implement n-fold cross validation
#    test(trainfile,testfile,outpath) #this method is used to implement independent test
