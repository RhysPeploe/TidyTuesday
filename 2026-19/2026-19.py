import pydytuesday
import pandas as pd
import geopandas as gpd
import matplotlib.pyplot as plt

pydytuesday.get_date('2026-05-12')

cities = pd.read_csv("cities.csv")
links = pd.read_csv('links.csv')

print(cities.shape)

print(cities.groupby("country").size().reset_index(name = "n"))

print(cities["country"].str.contains("eswatini"))
print(cities.index[cities["country"].str.contains(r"[Ee][Ss]watini")])
print(cities.iloc[2083])

cities["country"] = cities["country"].str.title()
print(cities.iloc[2083])

print(cities.groupby("country").size().reset_index(name = "n"))

uk = cities[cities["country"] == "United Kingdom"]

uk_links = links[(links['source'].isin(uk['id'])) |
      (links['target'].isin(uk['id']))]


uk_merge = uk_links.merge(
    cities[["id", "country"]], 
    how = "left", 
    left_on = "source", 
    right_on = "id"
    ).rename(
        columns = {"country": "source_country"}
    ).drop(
        columns = "id"
    ).merge(
        cities[["id", "country"]],                  
        how = "left", 
        left_on = "target", 
        right_on = "id"
    ).rename(
        columns = {"country": "target_country"}
    ).drop(
        columns = "id"
    )

uk_merge["twin"] = pd.NA
uk_merge["twinee"] = "United Kingdom"

print(uk_merge)

for i in range(len(uk_merge)):
    if uk_merge.loc[i, "source_country"] == "United Kingdom":
        uk_merge.loc[i, "twin"] = uk_merge.loc[i, "target_country"]
    
    elif uk_merge.loc[i, "target_country"] == "United Kingdom":
        uk_merge.loc[i, "twin"] = uk_merge.loc[i, "source_country"]

print(uk_merge)

print(uk_merge.groupby("twin").size().reset_index(name = "n").sort_values("n"))

plot_df = uk_merge.groupby(
    "twin"
    ).size(
    ).reset_index(
        name = "n"
    ).sort_values(
        "n"
    ).merge(
        cities.groupby("country").head(1).reset_index(drop=True)[["country", "lng", "lat"]],
        left_on = "twin",
        right_on = "country"
        )



world = gpd.read_file(
    "https://naturalearth.s3.amazonaws.com/110m_cultural/ne_110m_admin_0_countries.zip"
)

# Convert points to GeoDataFrame
gdf = gpd.GeoDataFrame(
    plot_df,
    geometry=gpd.points_from_xy(plot_df.lng, plot_df.lat),
    crs="EPSG:4326"
)

# Plot
fig, ax = plt.subplots(figsize=(14, 8))

world.plot(ax=ax, color="lightgrey", edgecolor="white")
gdf.plot(
    ax=ax,
    markersize=plot_df["n"] * 2,
    alpha=0.7
)

plt.axis("off")
plt.tight_layout()
plt.show()