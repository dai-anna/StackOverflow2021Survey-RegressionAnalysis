#%%
import geopandas as gpd
import matplotlib.pyplot as plt
from pandas.core.reshape.merge import merge
import seaborn as sns
import pandas as pd
import numpy as np

#################### Random Intercepts by State ####################

#%%
############################ Map of US #############################

DARKBLUE = "#012169"
BBOX = (-125.104, 24, -65.039, 51.5)

df = gpd.read_file("../00_source_data/us-state-boundaries.geojson", bbox=BBOX)
df.head()

randints = pd.read_parquet("../20_intermediate_files/ranef_randint_bystate.parquet")
randints.head()

def is_sig(pointestimate, err):
    if pointestimate < 0 and pointestimate + err * 1.96 >= 0:
        return np.nan
    elif pointestimate > 0 and pointestimate - err * 1.96 <= 0:
        return np.nan
    else:
        return pointestimate


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


fig, ax = plt.subplots(1, figsize=(60, 15))
merged = merged.to_crs(2163)
merged.plot(
    ax=ax,
    column="pointestimate_sig",
    ec="0.6",
    # legend=True,
    cmap="viridis",
    missing_kwds={"color": "0.95", "hatch": "//", "edgecolor": "0.6"},
)

# ax.set_xlim(-125.104, -65.039)
# ax.set_ylim(24, 51.5)
sns.despine(bottom=True, left=True)
fig.colorbar(ax.collections[0], ax=ax, label="Random Intercept", shrink=0.5)
ax.set_xticklabels([])
ax.set_yticklabels([])
ax.set_xticks([])
ax.set_yticks([])
plt.tight_layout()
# plt.savefig(
#     "../20_intermediate_files/plots/map_bystate.png", facecolor="white", dpi=300
# )
# %%

############################ Dot Plot #############################

df_dotplot_state = pd.read_parquet(
    "../20_intermediate_files/ranef_randint_bystate.parquet"
).sort_values(by="pointestimate")
df_dotplot_state["state"] = df_dotplot_state["state"].astype("category")

fig, ax = plt.subplots(figsize=(8, 10))

sns.pointplot(
    data=df_dotplot_state,
    y="state",
    x="pointestimate",
    xerr=df_dotplot_state["err"],
    order=df_dotplot_state["state"],
    ax=ax,
    color=DARKBLUE,
    zorder=10,
)

highlight_states = [
    "New Mexico",
    "Alabama",
    "Indiana",
    "Iowa",
    "Kansas",
    "Ohio",
    "Kentucky",
    "Michigan",
    "Missouri",
    "Tennessee",
    "Illinois",
    "Colorado",
    "Massachusetts",
    "New Jersey",
    "New York",
    "Nevada",
    "California",
    "Washington",
]
ax.set_yticklabels(ax.get_yticklabels(), size=11)
for label in ax.get_yticklabels():
    label.set_color("k" if label.get_text() in highlight_states else "0.6")
    label.set_weight("bold" if label.get_text() in highlight_states else "normal")
    label.set_size(11.5 if label.get_text() in highlight_states else 10)

ax.errorbar(
    df_dotplot_state["pointestimate"],
    df_dotplot_state["state"],
    xerr=1.96 * df_dotplot_state["err"],
    zorder=0,
    color=DARKBLUE,
    ecolor=[
        DARKBLUE if label.get_text() in highlight_states else "0.7"
        for label in ax.get_yticklabels()
    ],
)
sns.despine()
ax.axvline(0, zorder=-1, color="0.6", linestyle="--")
ax.set_xlabel("(Intercept)")
ax.set_ylabel("State")
ax.set_title("Random Intercepts for States", weight="bold")
ax.spines["left"].set_visible(False)
ax.yaxis.set_tick_params(which="both", length=0)
plt.tight_layout()
plt.savefig(
    "../20_intermediate_files/plots/dotplot_bystate.png", facecolor="white", dpi=300
)

# %%
############## Calculate R^2 ###################

preddf = pd.read_parquet("../20_intermediate_files/predictions.parquet")
preddf.head()

from sklearn.metrics import r2_score

print(r2_score(preddf["df.logConvertedCompYearly"], preddf["predict.final_model."]))
# %%
