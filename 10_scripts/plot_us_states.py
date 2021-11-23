#%%
import geopandas as gpd
import matplotlib.pyplot as plt
from pandas.core.reshape.merge import merge
import seaborn as sns
import pandas as pd
import numpy as np

#%%
BBOX = (-125.104, 24, -65.039, 51.5)

df = gpd.read_file("../00_source_data/us-state-boundaries.geojson", bbox=BBOX)
df.head()

# %%
randints = pd.read_parquet("../20_intermediate_files/ranef_randint_bystate.parquet")
randints.head()

#%%
def is_sig(pointestimate, err):
    if pointestimate < 0 and pointestimate + err * 1.96 >= 0:
        return np.nan
    elif pointestimate > 0 and pointestimate - err * 1.96 <= 0:
        return np.nan
    else:
        return pointestimate


# %%

randints = randints.rename({"state": "name"}, axis=1)
randints["name"] = randints["name"].astype("string")
df["name"] = df["name"].astype("string")

df, randints = df.reset_index(drop=True), randints.reset_index(drop=True)


merged = pd.merge(df, randints, on="name", how="left")

merged = merged.assign(
    pointestimate_sig=merged.apply(
        lambda row: is_sig(row["pointestimate"], row["err"]), axis=1
    )
)


# %%
fig, ax = plt.subplots(1, figsize=(15, 15))
merged = merged.to_crs(2163)
merged.plot(
    ax=ax,
    column="pointestimate_sig",
    ec="0.6",
    # legend=True,
    cmap="viridis",
    missing_kwds={"color": "0.95", "hatch": "//", "edgecolor":"0.6"},
)

# ax.set_xlim(-125.104, -65.039)
# ax.set_ylim(24, 51.5)
sns.despine(bottom=True, left=True)
fig.colorbar(ax.collections[0], ax=ax, label="Random Intercept", shrink=0.5)
ax.set_xticklabels([])
ax.set_yticklabels([])
ax.set_xticks([])
ax.set_yticks([])
# %%