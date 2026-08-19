from mpl_toolkits.basemap import Basemap
import numpy as np
import matplotlib.pyplot as plt
import pandas as pd
import os
import pydytuesday

# Download files from the week, which you can then read in locally
pydytuesday.get_date('2026-08-04')

wool = pd.read_csv("basotho_wool.csv")

os.chdir("C:\\Users\\rpeploe\\Documents\\TidyTuesday\\2026-31")

# Set the plot size for this notebook:
plt.rcParams["figure.figsize"]=15,12

'''
# A basic map
m = Basemap(llcrnrlon=-100,
            llcrnrlat=20,
            urcrnrlon=30,
            urcrnrlat=70,
            projection='merc')
m.drawmapboundary(fill_color='#A6CAE0', linewidth=0)
m.fillcontinents(color='grey', alpha=0.7, lake_color='grey')
m.drawcoastlines(linewidth=0.1, color="white")

m = Basemap(llcrnrlon=-100,
            llcrnrlat=20,
            urcrnrlon=30,
            urcrnrlat=70,
            projection='merc')
m.drawmapboundary(fill_color='#A6CAE0', linewidth=0)
m.fillcontinents(color='grey', alpha=0.7, lake_color='grey')
m.drawcoastlines(linewidth=0.1, color="white")

# Add a connection between new york and London
startlat = 40.78; startlon = -73.98
arrlat = 51.53; arrlon = 0.08
m.drawgreatcircle(startlon, startlat, arrlon, arrlat, linewidth=2, color='orange')
'''

'''
cities = {
    'city': ["Paris", "Melbourne", "Saint.Petersburg",
            "Abidjan", "Montreal", "Nairobi", "Salvador"],
    'lon': [2, 145, 30.32, -4.03, -73.57, 36.82, -38.5],
    'lat': [49, -38, 59.93, 5.33, 45.52, -1.29, -12.97]
    }
'''
cities = {
    'city': ["LSO", "ZAF"],
    'lon': [28.24475, 24.75253],
    'lat': [-29.60168, -28.55362]
    }

countries ={
    'country_source' : "LSO",
    'source_lon' : pd.NA,
    'source_lat' : pd.NA,
    'country_target' : wool["reporter_iso"],
    'target_lon' : pd.NA,
    'target_lat' : pd.NA
    }

countries_df = pd.DataFrame(countries) 

df = pd.DataFrame(cities, columns = ['city', 'lon', 'lat'])

m = Basemap(llcrnrlon=-179,
            llcrnrlat=-60,
            urcrnrlon=179,
            urcrnrlat=70, 
            projection='merc')
m.drawmapboundary(fill_color='white', linewidth=0)
m.fillcontinents(color='#f2f2f2', alpha=0.7)
m.drawcoastlines(linewidth=0.5, color="grey")

m.drawgreatcircle(28.24475, -29.60168, 24.75253, -28.55362,
                  linewidth=1, color='#69b3a2')
plt.annotate("LSO", xy=m(28.24475 +3, -29.60168), verticalalignment='center')
plt.annotate("ZAF", xy=m(24.75253 -6, -28.55362 + 2), verticalalignment='center')

m.drawgreatcircle(28.24475, -29.60168, -56.0192, -32.78195,
                  linewidth=1, color='#69b3a2')
plt.annotate("URY", xy=m(-56.0192 +3, -32.78195), verticalalignment='center')

m.drawgreatcircle(28.24475, -29.60168, 104.6911, 38.07325,
                  linewidth=1, color='#69b3a2')
plt.annotate("CHN", xy=m(104.6911 +3, 38.07325), verticalalignment='center')

m.drawgreatcircle(28.24475, -29.60168, 81.173, 23.5863,
                  linewidth=1, color='#69b3a2')
plt.annotate("IND", xy=m(81.173 + 3, 23.5863), verticalalignment='center')


plt.savefig("Wool Trade Map.png")
'''
# Loop on every pair of cities to add the connection
for startIndex, startRow in df.iterrows():
    for endIndex in range(startIndex, len(df.index)):
        endRow = df.iloc[endIndex]

        # if the same city, skip to avoid a GeodError
        if startRow.city == endRow.city:
            pass
        else:
            m.drawgreatcircle(startRow.lon, startRow.lat, endRow.lon, endRow.lat,
                              linewidth=1, color='#69b3a2')

# Add city names
for i, row in df.iterrows():
    plt.annotate(row.city, xy=m(row.lon+3, row.lat), verticalalignment='center')
'''