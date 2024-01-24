import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns
from shapely.geometry import Point
import geopandas as gpd
import string
from matplotlib.gridspec import GridSpec

def pd_to_gpd(data):
    geometry = [Point(xy) for xy in zip(data['Longitude'], data['Latitude'])]
    gdf = gpd.GeoDataFrame(data, geometry=geometry, crs="EPSG:4326")
    return gdf

def main():
    "plot sample number over 1 degree bin (x axis)"
    pi = pd.read_csv("tidy/forcens_fg_r_wsst.csv")
    lgm = pd.read_csv("tidy/lgm_fg_r_wsst.csv")
    lgm = lgm[lgm['Data_Source'] == 'margo']

    gdf_pi = pd_to_gpd(pi)
    gdf_lgm = pd_to_gpd(lgm)

    # Create a figure with a 2x3 grid of subplots
    sns.set_style("ticks")
    pi_color = "#e86254"
    lgm_color = "#1e466e"

    fig = plt.figure(figsize=(10.5, 8), tight_layout=True)

    # Create a GridSpec with 4 rows and 7 columns
    ## 5:2 for map to histogram width ratio
    gs = GridSpec(4, 7, figure=fig)

    # Define the subplots
    ax_map1 = plt.subplot(gs[0:2, :5])
    ax_map2 = plt.subplot(gs[2:4, :5])
    ax_hs1 = plt.subplot(gs[0, 5:])
    ax_hs2 = plt.subplot(gs[1, 5:])
    ax_hs3 = plt.subplot(gs[2, 5:])
    ax_hs4 = plt.subplot(gs[3, 5:])


    ax_hs = [ax_hs1, ax_hs2, ax_hs3, ax_hs4]
    all_axs = [ax_map1, ax_map2, ax_hs1, ax_hs2, ax_hs3, ax_hs4]

    # Add the world map to the first subplot
    world = gpd.read_file(gpd.datasets.get_path('naturalearth_lowres'))
    world.plot(ax=ax_map1, color='lightgray', edgecolor='lightgray')
    world.plot(ax=ax_map2, color='lightgray', edgecolor='lightgray')

    ## annotate the total number of samples
    n_pi = len(pi)
    n_lgm = len(lgm)
    
    ax_map1.annotate('n= '+str(n_lgm), xy=(0.05, 0.925), xycoords='axes fraction', fontsize=10)
    ax_map2.annotate('n='+str(n_pi), xy=(0.05, 0.925), xycoords='axes fraction', fontsize=10)

    gdf_lgm.plot(ax=ax_map1, markersize=5, label='LGM', color=lgm_color)
    gdf_pi.plot(ax=ax_map2, markersize=5, label='PI', color=pi_color)

    # Add histograms to the four subplots on the right
    cols = ['symbiont-barren non-spinose',
            'symbiont-barren spinose',
            'symbiont-obligate spinose',
            'SST']

    for i, col in enumerate(cols):
        #ax_hs[i].hist(pi[col], bins=30, label='PI', color=pi_color)
        #ax_hs[i].hist(lgm[col], bins=30, label='LGM', color=lgm_color)
        sns.kdeplot(pi[col], label='PI', ax=ax_hs[i], color=pi_color)
        sns.kdeplot(lgm[col], label='LGM', ax=ax_hs[i], color=lgm_color)

        if col != 'SST':
            ax_hs[i].set_xlabel('relative abundance')
        else:
            ax_hs[i].set_xlabel(col+' (°C)')
        ax_hs[i].set_title(col)
        ax_hs[i].set_ylabel('Density')
        ax_hs[i].legend()

    # Customize the legends for the histogram subplots
    labels = string.ascii_lowercase
    for i, ax in enumerate(all_axs):
        ax.legend()
        if i == 0:
            ax.set_title(labels[i]+" LGM", loc='left', fontweight='bold', fontsize=14)
        elif i == 1:
            ax.set_title(labels[i]+" PI", loc='left', fontweight='bold', fontsize=14)
        else:
            ax.set_title(labels[i], loc='left', fontweight='bold', fontsize=14)

    # Save the figure to a PNG file
    fig.savefig("example/sample_effort.png", dpi=300, bbox_inches='tight')

    print(">>> [DONE] sample efforts plotted in example/sample_effort.png")

if __name__ == "__main__":
    main()
