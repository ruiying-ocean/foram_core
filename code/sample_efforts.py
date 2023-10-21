import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns

def main():
    "plot sample number over 1 degree bin (x axis)"
    pi = pd.read_csv("tidy/forcens_fg_a_wsst.csv")
    lgm = pd.read_csv("tidy/lgm_fg_a_wsst.csv")

    # Define the temperature bin size and range
    bin_res = 1.0  # Adjust the bin size as needed
    temperature_min = -1.0  # Minimum temperature
    temperature_max = 30.0  # Maximum temperature
    data_column = 'SST'  # Change to the actual column name in your data

    # Create the temperature bins
    temperature_bins = range(int(temperature_min), int(temperature_max) + 1, int(bin_res))

    # Create a figure and axis with Seaborn
    fig, ax = plt.subplots(figsize=(6, 4))
    sns.set(style="ticks")  # Set Seaborn style (white background with grid lines)
    sns.set_palette("Set1")  # Set color palette

    # Plot the histograms using Seaborn
    sns.histplot(data=pi, x=data_column, bins=temperature_bins, kde=False, label='PI')
    sns.histplot(data=lgm, x=data_column, bins=temperature_bins, kde=False, label='LGM', zorder=2)

    # Set labels and a title
    ax.set_xlabel('Temperature')
    ax.set_ylabel('Sample Number')
    ax.set_title('Sample number over 1 degree bin')

    # Add a legend
    ax.legend()

    ## save the plot
    fig.savefig("example/sample_effort.png", dpi=300, bbox_inches='tight')

    print(">>> [DONE] sample efforts plotted in example/sample_effort.png")

if __name__ == "__main__":
    main()
