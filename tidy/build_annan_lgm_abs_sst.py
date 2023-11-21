import xarray as xr

sst_lgm = xr.open_dataset("~/Downloads/cp-18-1883-2022-supplement/output/SST.mean.lgm.nc")
sst_pi = xr.open_dataset("~/Downloads/cp-18-1883-2022-supplement/data/modern/ERSST.tos.1854_1883.ann.nc")
sst_pi = sst_pi.isel(time=-1, lev=0)

## convert east longitude to normal longitude
def east_to_normal_lon(east_longitude):
    "convert longitude from 0-360 to -180-180"
    normal_longitude = (east_longitude + 180) % 360 - 180
    return normal_longitude

sst_lgm = sst_lgm.assign_coords({"longitude": list(map(east_to_normal_lon, sst_lgm.longitude.values))}).sortby(
    "longitude"
)
sst_pi = sst_pi.rename({'lat':'latitude', 'lon':'longitude'})

if (sst_pi.longitude  == sst_lgm.longitude).all():
    print("All longitude values are the same")

if (sst_pi.latitude  == sst_lgm.latitude).all():
    print("All latitude values are the same")

## make LGM absolute SST field
lgm_sst_abs_field = sst_lgm['SST.mean'] + sst_pi['tos']
lgm_sst_abs_field.plot()
## add name
lgm_sst_abs_field.name = 'sst'

## export to netcdf
lgm_sst_abs_field.to_netcdf('~/Downloads/lgm_annan2022_sst_field.nc')    