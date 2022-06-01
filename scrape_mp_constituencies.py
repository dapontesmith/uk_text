# -*- coding: utf-8 -*-
"""
Created on Thu May 26 11:57:42 2022

@author: dapon
"""
from bs4 import BeautifulSoup
import requests
import pandas as pd
import re

#set url 
url = "https://en.wikipedia.org/wiki/List_of_MPs_elected_in_the_2017_United_Kingdom_general_election"

# get the html and read the tables 
html = requests.get(url, verify = False).content
out = pd.read_html(html)
# this is the mps table
mps = out[6]
# clean up the table and save as csv
table = mps[["Constituency","Member returned (2017)[9]","Party.1"]]
table.columns = ["constituency",'mp',"party"]
table.to_csv("C:/Users/dapon/Dropbox/Harvard/dissertation/data/uk_geography/pcon_data/mps_by_constituency_2017.csv")


# #######################
# 2015 ELECTION
######

url = "https://en.wikipedia.org/wiki/List_of_MPs_elected_in_the_2015_United_Kingdom_general_election"
# get the html and read the tables 
html = requests.get(url, verify = False).content
out = pd.read_html(html)
out2015 = out[5][["Constituency","Member returned (2015)[3].1"]]
out2015.columns = ["constituency","mp"]
mps = out2015["mp"].values.tolist()
#mps_out = [mps[i].split("(")[0] for i in range(len(mps))]
mps_out = [mps[i].replace("The Hon. ","") for i in range(len(mps))]
out2015["mp"] = mps_out

# write to csv
out2015.to_csv("C:/Users/dapon/Dropbox/Harvard/dissertation/data/uk_geography/pcon_data/mps_by_constituency_2015.csv")


###############################################
## 2010 ELECTION
#####################################
url = "https://en.wikipedia.org/wiki/List_of_MPs_elected_in_the_2010_United_Kingdom_general_election"
allmps = pd.read_html(requests.get(url, verify = False).content)
out2010 = allmps[5][["Constituency","Member returned (2010).1"]]
out2010.columns = ["constituency","mp"]

mps = out2010["mp"].values.tolist()
mps_out = [mps[i].split(" (")[0] for i in range(len(mps))]
out2010["mp"] = mps_out
out2010.to_csv("C:/Users/dapon/Dropbox/Harvard/dissertation/data/uk_geography/pcon_data/mps_by_constituency_2010.csv")


#### ##################################
## 2005 ELECTION
###############################

url = "https://en.wikipedia.org/wiki/List_of_MPs_elected_in_the_2005_United_Kingdom_general_election"
html = requests.get(url, verify = False).content
tables = pd.read_html(html)
out5 = tables[4].iloc[:, 0:2]
out5.columns = ["constituency","mp"]
out5.to_csv("C:/Users/dapon/Dropbox/Harvard/dissertation/data/uk_geography/pcon_data/mps_by_constituency_2005_dirty.csv")


#2001 election 
url = "https://en.wikipedia.org/wiki/List_of_MPs_elected_in_the_2001_United_Kingdom_general_election"
html = requests.get(url, verify = False).content
tables = pd.read_html(html)
out1 = tables[4].iloc[:, 0:2]
out1.to_csv("C:/Users/dapon/Dropbox/Harvard/dissertation/data/uk_geography/pcon_data/mps_by_constituency_2001_dirty.csv")


