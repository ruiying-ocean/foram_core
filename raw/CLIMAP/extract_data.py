import re
import pathlib
import pandas as pd

def extract_dat(file):
    with open(file, 'r') as f:
        for line in f:
            if "Core/Site" in line:
                site = re.search(r"(\w+\d+-\d+)", line).group(1)

            if "Latitude" in line:
                lat = re.search(r"\(\s+([0-9\.-]+)\)", line).group(1)

            if "Longitude" in line:
                long = re.search(r"\(\s+([0-9\.-]+)\)", line).group(1)

            if 'Water Depth(m)' in line:
                water_depth = re.search(r"(\d+)", line).group(1)

    return (site, lat, long, water_depth)

def main():
    # Apply to all files in the 'formatted_files' directory
    files = pathlib.Path('formatted_files').glob('*.txt')

    # Collect results in a list
    data_list = []

    for file_path in files:
        result = extract_dat(file_path)
        data_list.append(result)

    # Create a DataFrame from the list
    data = pd.DataFrame(data_list, columns=['CoreID', 'Latitude', 'Longitude', 'Water Depth (m)'])

    ## convert lat/long/water_depth to numeric
    data['Latitude'] = pd.to_numeric(data['Latitude'])
    data['Longitude'] = pd.to_numeric(data['Longitude'])
    data['Water Depth (m)'] = pd.to_numeric(data['Water Depth (m)'])

    data.to_csv('extracted_site_info.csv', index=False)

if __name__ == '__main__':
    main()
