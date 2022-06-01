# -*- coding: utf-8 -*-
"""
Created on Thu Oct 14 19:44:46 2021

@author: dapon
"""

from bs4 import BeautifulSoup
import requests
import pandas as pd
import re

#set url for all place-names
url = "https://en.wikipedia.org/wiki/List_of_United_Kingdom_locations"
r = requests.get(url, verify = False)
soup = BeautifulSoup(r.text, "html.parser")
hrefs = soup.find_all("a")

#collect all links to place-name lists on page 
links = []
for a in soup.find_all('a', href=True):
    if "List_of_United_Kingdom_locations:_" in a["href"]:
        links.append(a["href"])
       
#set base url
base_url = "https://en.wikipedia.org"

coord_holder, longitude, latitude = [], [], []
df = pd.DataFrame()
#iterate over linsk to get all place names 
for link in links:
    print(link)
    #make full url
    full_url = base_url + link
    html = requests.get(full_url).content
    #get the coordinates out 
    
    #read html to get place names 
    df_list = pd.read_html(html)
    #get rid of extraoenous first and last elements of df_list 
    #and concat all dfs together
    df_list = pd.concat(df_list[1:len(df_list)])
    #df = df_list.iloc[ : , 0:2 ]
    df = df.append(df_list)

#eliminate header rows  
df.columns = ["location","locality","coordinates","grid_reference"]
df = df[df["location"].str.contains("Location")==False]
df_clean = df.dropna()
#df = df[["location","locality"]]

#coordinates are not in the right form, but i have all the place_names
df_names = df_clean[["location","locality"]]

#
coord_list = []
coordinates = df_clean[["coordinates"]].values.tolist()
#get list of coordinates
for i in range(len(coordinates)):
    print(i)
    coord_list.append(coordinates[i][0].replace(u"\ufeff","").split(" / ")[1])

df_names["coordinates"] = coord_list

#clean the remaining messy coordinate entries
for i in range(len(df_names)):
    if "parser" in df_names['coordinates'].iloc[i]:
        df_names["coordinates"].iloc[i] = df_names["coordinates"].iloc[i].split(":nowrap}")[1].split(u"\ufeff")[0]




#put coordinates int oa list 
coords = df_names["coordinates"].values.tolist()

#this deals with cases that have both coordinates (incorrect and correct) in a different format
test = []
for i in range(len(coords)):
    print(i)
    if coords[i].count("N") == 2:
        test.append(coords[i].split(" ", 2)[2])
    else:
        test.append(coords[i])



#initialize empty lists
full_lat, full_long = [], []
#loop over lists 
for i in range(len(test)):
    print(i)
    #get rid of degrees and minutes signs
    new = test[i].replace(u'°',' ').replace('\'','').replace('"','')
    #get rid of the coordinates' seconds - this sacrificies some precision,
    #leaving us with only degrees and minutes 
    new = re.sub(r'′.+?″', '', new)
    
    #we have to do this to deal with one aberrant case (index 8750)
    if "." in new:
    #put lat andlong into separate lists 
        lat = new.split(' N')[0]
        long = new.split(" N ")[1]
        #do some string replacememnt of cardinal directions in lat and long
        #for j in range(len(lat)):
            #   lat[j] = lat[j].replace("N","").replace("′","")
        if "W" in long:
            long = long.replace(" W","").replace("′","")
            #do this to get W coordinates to be negative
            long_right = float(long) * -1
        if "E" in long:
            long_right = long.replace("E","").replace("′","")
    #if the coordinates are in a bad format, just skip them (only happens on index 8750)
    else: 
        lat = []
        long = []
    #simply add them together to get coordinates, making east ones negative
    full_lat.append(lat)
    full_long.append(long_right)
   
#put lists as columns of dataframe    
df_names["latitude"] = full_lat
df_names['longitude'] = full_long
        
        

#write to csv
df_names.to_csv("C:/Users/dapon/Dropbox/Harvard/dissertation/data/uk_geography/uk_placenames_wiki.csv")
        


####################################################################
####################################################################
###################################################################

