import xarray as xr
import numpy as np
import pandas as pd

def normal_to_east(x):
    "from -180~180 to 0~360, to eastern degrees"
    x[x<0] = x[x<0] + 360
    return x

def east_to_normal(x):
    "from 0~360 to -180~180"
    x[x>180] = x[x>180] - 360
    return x

def add_tierney_sst(data, lookup_data, column_name="SST"):
    lst = []
    latitude = data['Latitude'].copy()
    longitude = data['Longitude'].copy()
    longitude = normal_to_east(longitude)

    for i in range(len(longitude)):
        lat = latitude[i]
        lon = longitude[i]
        x = lookup_data.sel(lat=lat,
                         lon=lon,
                         method="nearest",
                         tolerance=1).values

        if np.isnan(x):
            x = lookup_data.sel(lat=lat,
                             lon=lon,
                             method="pad").values
        if np.isnan(x):
            x = lookup_data.sel(lat=lat,
                             lon=lon,
                             method="backfill").values
        lst.append(x)
    data[column_name] = np.array(lst)

def add_Had_data(data, lookup_data, column_name="SST"):
    lst = []
    latitude = data['Latitude'].copy()
    longitude = data['Longitude'].copy()

    for i in range(len(longitude)):
        lat = latitude[i]
        lon = longitude[i]
        x = lookup_data.sel(latitude=lat,
                         longitude=lon,
                         tolerance=1,
                         method="nearest").values
        lst.append(x)
    data[column_name] = np.array(lst)

lgm_data = xr.open_dataset("tidy/Tierney2020_DA_ocn_regrid.nc")
pi_data = xr.open_dataset("tidy/HadISST_PI.nc")

## longitude checked, regular but in eastern format
SST_LGM = lgm_data['SSTLGM']
SST_PI = pi_data['sst']

lgm_fg_a = pd.read_csv("~/foram_core/tidy/lgm_fg_a_tidy.csv")
lgm_fg_r = pd.read_csv("~/foram_core/tidy/lgm_fg_r_tidy.csv")
lgm_sp_a = pd.read_csv("~/foram_core/tidy/lgm_sp_a_tidy.csv")
lgm_sp_r = pd.read_csv("~/foram_core/tidy/lgm_sp_r_tidy.csv")

add_tierney_sst(lgm_fg_a, SST_LGM, "SST")
add_tierney_sst(lgm_fg_r, SST_LGM, "SST")
add_tierney_sst(lgm_sp_a, SST_LGM, "SST")
add_tierney_sst(lgm_sp_r, SST_LGM, "SST")

lgm_fg_a.to_csv("~/foram_core/tidy/lgm_fg_a_wsst.csv",index=False)
lgm_fg_r.to_csv("~/foram_core/tidy/lgm_fg_r_wsst.csv",index=False)
lgm_sp_a.to_csv("~/foram_core/tidy/lgm_sp_a_wsst.csv",index=False)
lgm_sp_r.to_csv("~/foram_core/tidy/lgm_sp_r_wsst.csv",index=False)

## ------------------------------

pi_fg_a = pd.read_csv("~/foram_core/tidy/forcens_fg_a_tidy.csv")
pi_fg_r = pd.read_csv("~/foram_core/tidy/forcens_fg_r_tidy.csv")
pi_sp_a = pd.read_csv("~/foram_core/tidy/forcens_sp_a_tidy.csv")
pi_sp_r = pd.read_csv("~/foram_core/tidy/forcens_sp_r_tidy.csv")

add_Had_data(pi_fg_a, SST_PI, "SST")
add_Had_data(pi_fg_r, SST_PI, "SST")
add_Had_data(pi_sp_a, SST_PI, "SST")
add_Had_data(pi_sp_r, SST_PI, "SST")

pi_fg_a.to_csv("~/foram_core/tidy/forcens_fg_a_wsst.csv",index=False)
pi_fg_r.to_csv("~/foram_core/tidy/forcens_fg_r_wsst.csv",index=False)
pi_sp_a.to_csv("~/foram_core/tidy/forcens_sp_a_wsst.csv",index=False)
pi_sp_r.to_csv("~/foram_core/tidy/forcens_sp_r_wsst.csv",index=False)

print(">>> [DONE] SST Added")
