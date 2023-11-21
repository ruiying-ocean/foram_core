import xarray as xr
import numpy as np
import pandas as pd
class DataProduct:

    def __init__(self, data_array, lon='lon', lat='lat', lon_eastern=False):
        """
        The class to find the nearest value in the data array to the target longitude and latitude.
        ------
        Parameters
        ------
        data_array: a xarray Data Array with longitude and latitude as dimensions
        lon: the name of the longitude dimension
        lat: the name of the latitude dimension
        lon_eastern: whether the longitude is in eastern format (0, 360) or not (-180, 180)
        """
        self.data = data_array
        self.lon_name = lon
        self.lat_name = lat
        self.lon_eastern = lon_eastern

        self.stacked_data = self.data.stack(x=[self.lon_name, self.lat_name])
        self.stacked_ocn_mask =~np.isnan(self.stacked_data)
        self.ocn_only_data = self.stacked_data[self.stacked_ocn_mask]

    def convert_to_longitude_east(self, normal_longitude):
        """
        normal longitude (-180, 180) to longitude east(0,360)
        """
        if normal_longitude < 0:
            return 360 + normal_longitude
        else:
            return normal_longitude

    @staticmethod
    def haversine_distance(point1, point2):
        """
        Calculate the Haversine distance between two points.
        """
        lon1, lat1 = point1
        lon2, lat2 = point2

        # Convert latitude and longitude from degrees to radians
        lat1, lon1, lat2, lon2 = np.radians([lat1, lon1, lat2, lon2])

        # Haversine formula
        dlon = lon2 - lon1
        dlat = lat2 - lat1
        a = np.sin(dlat / 2) ** 2 + np.cos(lat1) * np.cos(lat2) * np.sin(dlon / 2) ** 2
        c = 2 * np.arctan2(np.sqrt(a), np.sqrt(1 - a))

        # Radius of the Earth in kilometers
        earth_radius = 6371.0
        distance = earth_radius * c

        return distance

    def find_nearest(self, lon, lat, tolerate_na = True):
        """
        The API for users to find the nearest value and allow ignorance of the NA values

        The strategy is to mask the NA values and then use the nearest value
        source: https://github.com/pydata/xarray/issues/644

        Find the nearest value in the data array to the target longitude and latitude.
        ------
        Parameters
        ------
        ds: a stacked xarray data array with x
        lon: target longitude
        lat: target latitude
        tolerate_na: whether to tolerate NA values or not, this could greately slow down the process
        """

        if tolerate_na:
            kw_args = {self.lon_name: lon, self.lat_name: lat, 'method': 'nearest'}
            nearest_value =self.data.sel(**kw_args).values.item()
        else:                    
            index_pool = self.ocn_only_data.x.values
            point1 = (lon, lat)

            distances = np.array([DataProduct.haversine_distance(point1, point2) for point2 in index_pool])
            idx_min = np.argmin(distances)
            nearest_value = self.ocn_only_data.values[idx_min]
        return nearest_value

    def match_dataframe(self, df, lat_col, lon_col, column_name, tolerate_na = True):
        lst = []        
            
        for i in range(len(df)):
            lat = df[lat_col].iloc[i,]
            lon = df[lon_col].iloc[i,]

            ## if the model uses eastern longitude format
            ## convert the dataframe's longitude to match the model
            if self.lon_eastern:
                lon = self.convert_to_longitude_east(lon)
                ## find the value
            lst.append(self.find_nearest(lon, lat))
        df[column_name] = np.array(lst)
        return df

    
def main():    

    lgm_fg_a = pd.read_csv("tidy/lgm_fg_a_tidy.csv")
    lgm_fg_r = pd.read_csv("tidy/lgm_fg_r_tidy.csv")
    lgm_sp_a = pd.read_csv("tidy/lgm_sp_a_tidy.csv")
    lgm_sp_r = pd.read_csv("tidy/lgm_sp_r_tidy.csv")

    pi_fg_a = pd.read_csv("tidy/forcens_fg_a_tidy.csv")
    pi_fg_r = pd.read_csv("tidy/forcens_fg_r_tidy.csv")
    pi_sp_a = pd.read_csv("tidy/forcens_sp_a_tidy.csv")
    pi_sp_r = pd.read_csv("tidy/forcens_sp_r_tidy.csv")

    ## data frame column names
    df_lon = 'Longitude'
    df_lat = 'Latitude'
    df_sst = 'SST'

    tierney_data = xr.open_dataset("tidy/Tierney2020_DA_ocn_regrid.nc")['SSTLGM']
    SST_LGM = DataProduct(tierney_data, lon='lon', lat='lat', lon_eastern=True)
    SST_LGM.match_dataframe(lgm_fg_a, df_lat, df_lon, df_sst).to_csv("tidy/lgm_fg_a_wsst.csv", index=False)
    SST_LGM.match_dataframe(lgm_fg_r, df_lat, df_lon, df_sst).to_csv("tidy/lgm_fg_r_wsst.csv", index=False)
    SST_LGM.match_dataframe(lgm_sp_a, df_lat, df_lon, df_sst).to_csv("tidy/lgm_sp_a_wsst.csv", index=False)
    SST_LGM.match_dataframe(lgm_sp_r, df_lat, df_lon, df_sst).to_csv("tidy/lgm_sp_r_wsst.csv", index=False)

    ## if add the annan sst, use the following code
    ## the SST is built from PI + LGM anomaly, and PI field uses ERSSTv5
    # annan_sst = xr.open_dataset("tidy/Annan2022_sst_field.nc")['sst']
    # SST_LGM_ALT = DataProduct(annan_sst, lon='longitude', lat='latitude', lon_eastern=False)
    
    # SST_LGM_ALT.match_dataframe(lgm_fg_a, df_lat, df_lon, df_sst).to_csv("tidy/lgm_fg_a_wsst.csv", index=False)
    # SST_LGM_ALT.match_dataframe(lgm_fg_r, df_lat, df_lon, df_sst).to_csv("tidy/lgm_fg_r_wsst.csv", index=False)
    # SST_LGM_ALT.match_dataframe(lgm_sp_a, df_lat, df_lon, df_sst).to_csv("tidy/lgm_sp_a_wsst.csv", index=False)
    # SST_LGM_ALT.match_dataframe(lgm_sp_r, df_lat, df_lon, df_sst).to_csv("tidy/lgm_sp_r_wsst.csv", index=False)
    
    hadi_sst = xr.open_dataset("tidy/HadISST_PI.nc")['sst']
    PI_SST = DataProduct(hadi_sst, lon='longitude', lat='latitude', lon_eastern=False)
    PI_SST.match_dataframe(pi_fg_a, df_lat, df_lon, df_sst).to_csv("tidy/forcens_fg_a_wsst.csv", index=False)
    PI_SST.match_dataframe(pi_fg_r, df_lat, df_lon, df_sst).to_csv("tidy/forcens_fg_r_wsst.csv", index=False)
    PI_SST.match_dataframe(pi_sp_a, df_lat, df_lon, df_sst).to_csv("tidy/forcens_sp_a_wsst.csv", index=False)
    PI_SST.match_dataframe(pi_sp_r, df_lat, df_lon, df_sst).to_csv("tidy/forcens_sp_r_wsst.csv", index=False)

    # ersst = xr.open_dataset("tidy/ersst_1854_1900.nc")['sst']
    # PI_SST_ALT = DataProduct(ersst, lon='lon', lat='lat', lon_eastern=False)
    # PI_SST_ALT.match_dataframe(pi_fg_a, df_lat, df_lon, df_sst).to_csv("tidy/forcens_fg_a_wsst.csv", index=False)
    # PI_SST_ALT.match_dataframe(pi_fg_r, df_lat, df_lon, df_sst).to_csv("tidy/forcens_fg_r_wsst.csv", index=False)
    # PI_SST_ALT.match_dataframe(pi_sp_a, df_lat, df_lon, df_sst).to_csv("tidy/forcens_sp_a_wsst.csv", index=False)
    # PI_SST_ALT.match_dataframe(pi_sp_r, df_lat, df_lon, df_sst).to_csv("tidy/forcens_sp_r_wsst.csv", index=False)
    
    print(">>> [DONE] SST Added")

if __name__ == "__main__":
    main()
