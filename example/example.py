import pandas as pd
import matplotlib.pyplot as plt
import cartopy.crs as ccrs
from cgeniepy.plot import scatter_map
from cgeniepy.plot import genie_cmap

def main():
	## initiate a figure
	fig = plt.figure(figsize=(10, 5))
	ax = plt.axes(projection=ccrs.Robinson())
	cmap = genie_cmap("Zissou1", reverse=False)

	df = pd.read_csv('tidy/lgm_fg_r_tidy.csv')
	p = scatter_map(df=df, ax=ax, var= "Symbiont-obligate Spinose", x="Longitude", y ="Latitude", interpolate="linear", cmap=cmap, vmax=1)
	cbar = plt.colorbar(p, orientation='horizontal', fraction=0.05, pad=0.05)
	cbar.set_label('Relative Abundance of symbiont-obligate foraminifera', fontsize=12)
	fig.savefig('example_map.png', dpi=300, bbox_inches='tight')

if __name__ == '__main__':
	main()
