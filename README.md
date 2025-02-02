# Generate Customer Plots

This repository contains an R script for processing customer data and generating visual plots. The script reads data from a CSV file, processes it, and creates interactive plots using Plotly.

## Files

- `generate_plots.R`: R script to process data and generate a plot.
- `run_analysis.bat`: Batch file to run the R script on Windows.

## Requirements

- R (version 4.4.0 or later)
- The following R packages:
  - ggplot2
  - dplyr
  - lubridate
  - zoo
  - plotly
  - ggdark
  - htmlwidgets
  - gridExtra
  - readr

## Usage

1. Clone this repository:
   ```bash
   git clone https://github.com/ProVerde-Labs/Generate-Customer-Plots.git
   cd Generate-Customer-Plots
   ```

2. Place your data file (`DataQueryExport_CN_240701.csv`) in the repository directory.

3. Run the script:
   - On Windows, run the batch file:
     ```bash
     ./run_analysis.bat
     ```
   - On other operating systems, run the R script directly:
     ```bash
     Rscript generate_plots.R
     ```
