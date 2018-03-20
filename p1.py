# -*- coding: utf-8 -*-
"""
Created on Tue Dec 05 00:59:02 2017

@author: Sushidhar
"""

import pandas as pd
import numpy as np

import matplotlib as plt
import seaborn as sn
hr_df = pd.read_csv( 'HR_comma_sep.csv' )
corrmat = hr_df.corr()
f, ax = plt.pyplot.subplots(figsize=(8, 8))
sn.heatmap(corrmat, vmax=.8, square=True)
plt.pyplot.show()