# -*- coding: utf-8 -*-
"""
Created on Wed Dec 30 14:51:09 2015

Script to rename trialtypes in csv-file
According to previous collaborators E-prime script deviant trials are
called 'novel' in the data file. Furthermore, this script is also coding
in post-deviant trials (trial subsequenet to deviant)

Used Python since the R-script I made was inefficient (i.e., slower)

@author: erik

"""

import pandas as pd

df = pd.read_csv('../Data/_STUDY1_raw_data.csv')    

#First we replace 'Novel' with 'Deviant':
df.TrialType = df.TrialType.replace(to_replace="Novel", 
                                    value="Deviant", inplace=False, 
                                    limit=None, regex=False,
                                    method='pad', axis=None)

#Then we loop through the column and code the Post-deviant standards
for  i,row in df['TrialType'].iteritems():
    if df.loc[i,'TrialType']== "Deviant":
        df.loc[i+1,'TrialType'] = "Post-deviant-standard" 
        

#Last, we save the new csv
df.to_csv(path_or_buf="../Data/_STUDY1_post-dev-std.csv", sep=';',
                 index=False)