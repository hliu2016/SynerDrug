# -*- coding: utf-8 -*-
"""
Created on Mon Nov  4 23:43:01 2019

@author: 12402
"""

#############construct initial probability vector for each combination
import csv
with open(r'E:\zwh\education\graduated\1导师布置\小论文5（GTB）\code\SynerDrug-master\data\DCDBccguiihua.csv','r') as f:
    items=f.readline().strip().split(',')
druglist=[]
for i in range(1, len(items)):
    druglist.append(int(items[i]))
len(druglist)
##load drug nodes

with open(r'E:\zwh\education\graduated\1导师布置\小论文5（GTB）\code\SynerDrug-master\data\DCDBppguiihua.csv','r') as f:
    items=f.readline().strip().split(',')
items[0]
proteinlist=[]
for i in range(1, len(items)):
    proteinlist.append(items[i])
len(proteinlist)
#load protein nodes

cp=[]
with open(r'E:\zwh\education\graduated\1导师布置\小论文5（GTB）\code\code2\STITCHcpmatrixnormalized.csv','r') as f:
    items=f.readline()
    for line in f:
        cp.append(line.strip().split(',')[1:])
##load cp links   
    
#import numpy as np
#m0=np.zeros((2718,len(druglist)+len(proteinlist)))

def seekprotein(drug):
    index=druglist.index(drug)
    protein=cp[index]
    proteinindex=[]
    for i in range(0,len(protein)):
        if protein[i]!= '0.0':
            proteinindex.append(i)
    return proteinindex

def countdrugs(listcurrentdrug):
    i=0
    for item in listcurrentdrug:
        if item in druglist:
            i=i+1
    return i
            
out = open(r'E:\zwh\education\graduated\1导师布置\小论文5（GTB）\code\code2\m.0_0.6.csv','a',newline='')
csv_write = csv.writer(out,dialect='excel')
csv_write.writerow(druglist+proteinlist)
eta=0.6       
with open(r'E:\zwh\education\graduated\1导师布置\小论文5（GTB）\code\code2\benchmarkset.csv','r') as f:
    for line in f:
        probabilityvector=[0]*6074 #3266+2808
        drugs=line.strip().split(',')[7].split(';')
        drugs=[int(i) for i in drugs ]
        drugs=list(set(drugs))
        drugnode=countdrugs(drugs)
        if drugnode==0:
            csv_write.writerow(probabilityvector)
        else:   
            drugvalue=1.0/drugnode
            proteinset=[]
            for i in range(0,len(drugs)):
                if drugs[i] in druglist:
                    index=druglist.index(drugs[i])
                    probabilityvector[index]=eta*drugvalue
                    proteinset=proteinset+seekprotein(drugs[i])
                else:
                    continue
            proteinset=list(set(proteinset))
            proteinnode=len(proteinset)
            if proteinnode!= 0:
                proteinvalue=1.0/proteinnode
                for item in proteinset:
                    probabilityvector[item+3266]=(1-eta)*proteinvalue # protein allocating
            csv_write.writerow(probabilityvector)    
out.close()

#eta=0.5        
#with open(r'E:\zwh\education\graduated\1导师布置\小论文5（GTB）\code\code2\benchmarkset.csv','r') as f:
#    
#    probabilityvector=[0]*6074 #3266+2808
#    drugs=f.readline().strip().split(',')[7].split(';')
#    drugs=[int(i) for i in drugs ]
#    drugnode=countdrugs(drugs)
#    drugvalue=1.0/drugnode
#    proteinset=[]
#    for i in range(0,len(drugs)):
#        if drugs[i] in druglist:
#            index=druglist.index(drugs[i])
#            probabilityvector[index]=eta*drugvalue
#            proteinset=proteinset+seekprotein(drugs[i])
#        else:
#            continue
#    proteinset=list(set(proteinset))
#    proteinnode=len(proteinset)
#    if proteinnode!= 0:
#        proteinvalue=1.0/proteinnode
#        for item in proteinset:
#            probabilityvector[item+3266]=(1-eta)*proteinvalue # protein allocating
#    csv_write.writerow(probabilityvector)    
#out.close()
#        
