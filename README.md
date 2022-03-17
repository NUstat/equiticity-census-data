# Equiticity Census Data

Scripts, and data that were used to analyze census data in relation to micro-transit for a Chicago non-profit.

# Introduction

Hello!

This is a GitHub for a project we did for a non-profit interested in Chicago’s Divvy bikes. We worked to create scripts focusing on
Census data and other related data for Chicago neighborhoods.

We also created a visualization tool using kepler.gl which can be accessed with this link:

https://kepler.gl/demo/map?mapUrl=https://dl.dropboxusercontent.com/s/53m1fdu8lkh9ezx/keplergl_ud79hnq.json

We hope some of these scripts or data can be helpful in exploring demographics in Chicago.

To give you a sense of how to look into our repository, here’s a brief description of most of the files.

# *Table of Contents*

# Folders

## data

Contains raw data files we used for our visualizations.
Most of the data is in .csv format, for easy importing into R
or viewing using spreadsheet software.

The files in the "Bike Routes" (Source: https://data.cityofchicago.org/Transportation/Bike-Routes/3w5d-sru8) and "tl_2019" folders are shape
files, which must be used with an appropriate package to be
added onto a visualization.

### ACS_Chicago_Agg.csv
The aggregated American Community Survey (ACS) from 2015-2019.
Source: https://datahub.cmap.illinois.gov/dataset/community-data-snapshots-raw-data

### CensusTractsTiger2010.csv
Geographic information of Census Tracts from the 2010 Census.
Source: https://data.cityofchicago.org/Facilities-Geographic-Boundaries/Boundaries-Census-Tracts-2010/5jrd-6zik

### ChicagoWalkscore.csv
A table containing walk scores of Chicago neighborhoods form walkscore.com.
Source:  https://www.walkscore.com/IL/Chicago

### Divvy_Bicycle_Stations.csv
A more detailed dataset with information on Divvy bike stations, not included in the other datasets.
Source: https://data.cityofchicago.org/Transportation/Divvy-Bicycle-Stations-Historical/eq45-8inv

### Divvy_Trips_2020_Q1_data.csv
A sliver of Divvy trip info from Divvy itself, which we used to test some of our visualizations.
Source: https://divvy-tripdata.s3.amazonaws.com/index.html

### census_team_2015_2019.dat.csv
A combined dataset that we made by cleaning and collecting census tract-level data.
Source: https://usa.ipums.org/usa/

### comm_density_2021.csv
Community Areas in Chicago with the average amount of Divvy stations in a 2 mile radius (which was calculated by the Divvy Core Data Team).

### commute.csv
Specific data on methods of commuting by Chicago residents from the ACS
Source: https://usa.ipums.org/usa/

### population_fixed.csv
Community populations for Chicago.
Source: https://www.cmap.illinois.gov/documents/10180/126764/_Combined_AllCCAs.pdf/

### poverty_data.csv
More specific poverty data from the ACS 5-year 2015-2019.
Source: https://data2.nhgis.org/main

### poverty_data_codebook.txt

## curated_data

This folder contains data that we have processed in R
friendly .rds format, and in Excel .xlsx format, along with 
a codebook, in .pdf format.

### codebook_for_viz_and_comm_data.pdf
A codebook for the data files meant for the Visualization and Communication Team.

### community_index.xlsx
Our marginalization & socioeconomic hardship index, with one row for every community in Chicago.  See index_creation.Rmd for more details.

### demographic_data_community_codebook.pdf
A codebook for our final demographic data files.

### final_demoographic_data_no_geometry_with_connection…
Chicago demographic and divvy data on the community area-level, cleaned and combined data from the ACS, the CMAP Community Data Snapshots, the Divvy Core Data Team's datasets, and the Divvy open data portal, without mapping information.

### final_demographic_data_with_geometry_and_connection…
Same data as in the previous file but with mapping information. The .csv version of this file can be used to add out index to the Kepler tool.

### for_viz_no_geometry.xlsx
Divvy and demographic data on the community area-level used the by Visualization and Communication team without mapping information.

### selected_data_for_viz_com.rds
Same data as in the previous file but with mapping information.

### snapshots_with_connectivity.xlsx
CMAP Community Data Snapshots with Divvy connectivity (from Divvy Core Data Team) added.

## census_data_automation

A census data automation tool, made in Python. Contains the needed “data”, code,
and an “output” folder. Documentation on how to use the automation tool is in Census Data Automation.docx
visualize_data.ipynb is the script used to create kepler.gl visualization. If you would like to add additional data to the visualization, it must be in a .csv or .json format.

# Scripts

## Main Page

### visualizations.R

Script that contains all of our visualizations from our presentation, as well as some additional ones.
The additional visualizations are helpful to understand the dataset.

### creating_datasets_and_index.R

Brings in ACS data, and outputs a combined dataset with other sources of data in the folder.
Latter half of the script creates our marginalization index.  This script outputs all the data
files in the curated_data folder.

### generic_matching_script.R

Script that allows you to switch between geographical areas, census tracts, and other types
of geographic areas and map points (like Divvy stations) within those areas using shape files.

### variable_distributions.R

A script that creates several histograms and scatter plots with certain variable distributions.
Mainly to get a grasp of the data.  Some explorations of variable correlations.

### bikeroutesandroads.R

Creates plots including bike route and roads in Chicago. 


# R Markdown files

### index_creation.Rmd

A detailed breakdown of our Marginalization and Economic Hardship index.


## How to cite

IEEE:
Jung, B, Chalas Cuevas, E, Braud, I, Chandler-Holtz, L, Coble, M (2022) Equiticity Census Data [Source code]. https://github.com/NUstat/equiticity-census-data

ACM:
Benedict Jung, Edwin Chalas Cuevas, Ian Braud, Lauren Chandler-Holtz, Mat Coble. 2022. Equiticity Census Data. https://github.com/NUstat/equiticity-census-data

CSE:
Jung, B, Chalas Cuevas, E, Braud, I, Chandler-Holtz, L, Coble, M. Equiticity Census Data. San Francisco (CA); GitHub; https://github.com/NUstat/equiticity-census-data
