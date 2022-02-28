import pandas as pd
import sys

# Returning DataFrame of Census Data
def return_dataframe(fileName):
    dat_name = pd.read_csv(f"data/{fileName}", encoding='latin-1')
    return dat_name

# Returning DataFrame of Chicago's Census Tract Data
def return_tract_dataframe(fileName):
    dat_name = pd.read_csv(f"data/background_dat/{fileName}")
    return dat_name

# Filtering initial Census Data with data points related to Chicago
def get_chicago_data(dataF):
    tract_dat = return_tract_dataframe("CensusTractsTIGER2010.csv")
    tract_list = []
    for i in tract_dat.TRACTCE10.unique():
        tract_list.append(i)
    dataF.dropna(axis=1, how='all', inplace=True)
    dataF = dataF[dataF["STATE"] == "Illinois"]
    dataF = dataF.reset_index(drop=True)
    for i in range(dataF.shape[0]):
        if dataF.loc[i,"TRACTA"] not in tract_list:
            dataF = dataF.drop(i)
    dataF = dataF.reset_index(drop=True)
    return dataF

# Creating a Column Dictionary to manipulate column names
def get_column_dict(field_name, topicList):
    di={}
    total_check = 0
    with open(f'data/{field_name}') as f:
        for line in f:
            lh,sep,rh=line.rstrip().partition(":")
            if rh.lstrip() == "Total":
                di[lh.rstrip().lstrip()]=topic_list[total_check] + " " + rh.lstrip()
                total_check += 1
            else:
                di[lh.rstrip().lstrip()]= rh.lstrip()
    return di

# Modify columns according to Column Dictionary
def modify_columns(dataF, colDict):
    col_names = []
    for i in dataF:
        col_names.append(i)
    for i in range(len(col_names)):
        col_names[i] = colDict[col_names[i]]
    dataF.columns = col_names
    return dataF

# Append Census Tract GEOM and Community Area number Column
def get_geom_data(dataF):
    tracts_dat = return_tract_dataframe("CensusTractsTIGER2010.csv")
    geom_list = []
    comm_area = []
    for i in range(dataF.shape[0]):
        for j in range(tracts_dat.shape[0]):
            if dataF.loc[i,"Census Tract Code"] == tracts_dat.loc[j,"TRACTCE10"]:
                geom_list.append(tracts_dat.loc[j,"the_geom"])
                comm_area.append(tracts_dat.loc[j,"COMMAREA"])
    dataF["geom"] = geom_list
    dataF["comm area number"] = comm_area
    return dataF

# Creating Codebook with column names of the DataFrame
def generate_source_file(dataF,field_name):
    with open(f'output/{field_name}', 'w') as f:
        f.write("Codebook for NHGIS data file")
        f.write("\n")
        f.write("Additional documentation on NHGIS data sources is available at:")
        f.write("\n")
        f.write("https://www.nhgis.org/documentation/tabular-data")
        f.write("\n\n")
        f.write("Column Names : \n\n")
        for i in dataF.columns:
            f.write(f"{i}")
            f.write("\n")
        f.write("\n\n")
        with open(f'data/background_dat/agreement.txt') as g:
            for line in g:
                f.write(line)
                f.write("\n")
        return
        

if __name__ == "__main__":
    data_name = input("Enter Census File Name : ")
    field_name = input("Enter Field Names .txt File Name : ")
    census_dat = return_dataframe(data_name)
    census_dat = get_chicago_data(census_dat)
    table_number = input("How many tables? : ")
    table_number = int(table_number)
    topic_list = []
    for i in range(table_number):
        topic = input(f"Table {i+1} title? : ")
        topic_list.append(topic)
        i += 1
    di = get_column_dict(field_name, topic_list)
    census_dat = modify_columns(census_dat, di)
    census_dat = get_geom_data(census_dat)
    generate_source_file(census_dat, field_name)
    print(census_dat)
    census_dat.to_csv(f"output/cleaned_{data_name}")


    
        
