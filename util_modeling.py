#!/usr/bin/env python
# coding: utf-8

# imports 
import utm
import geopandas as gpd
import shapely
import pysal as ps
from multiprocessing.pool import Pool
import concurrent
from splot.esda import lisa_cluster
import sklearn
import esda
import pandas as pd
x_range = None 
y_range = None 
mat_contig = None 
total_matrix = None 
matrix_transaction = None 
matrix_percentages = None 
matrix_median_prices = None 
matrix_cells = None 
cell = None 
w = None 

import os 
lists_transactions = os.listdir('celltransactions')

def weights_formation(cell,indexes, kmax):
    global mat_contig
    global w 
    w = ps.lib.weights.contiguity.Queen.from_dataframe(cell,ids = indexes,)
    w5 = ps.lib.weights.order(w, kmax=5)
    mat_contig = pd.DataFrame(w5,index = w5.keys())
    return mat_contig, w


# In[16]:


def total_matrix_allocation(matrix_cells,gdf_unique_houses):
    global total_matrix
    global x_range
    global y_range 
    timer = 0
    
    total_matrix = [[[] for x in range(len(x_range))] for y in range(len(y_range))] 
    
    for x in range(len(x_range)):
        for y in range(len(y_range)):
            polygon = matrix_cells[y][x]
            number_of_transactions = gdf_unique_houses.within(polygon).sum()
            print("Running total matrix allocation" )
            total_matrix[y][x]  = number_of_transactions 
            timer += 1 
            print(timer / (len(x_range) * len(y_range))) 
            clear_output(wait = True)
    return total_matrix 


def to_csvs_cells(matrix_cells,gdf, n_cells):
    timer = 0
    global lists_transactions 
    to_add_string = str(n_cells) + 'x' + str(n_cells)
    if to_add_string not in lists_transactions:
        os.mkdir('celltransactions/' + to_add_string)
    global x_range 
    global y_range 
    for x in range(len(x_range)):
        for y in range(len(y_range)):
            polygon = matrix_cells[y][x]
            slice_poly = gdf[gdf.within(polygon)]
            if len(slice_poly) == 0:
                continue 
            cell_name = str(y) + ',' + str(x)
            timer += 1 
            print(n_cells)
            print(timer / (len(x_range) * len(y_range)))
            clear_output()
            slice_poly.to_csv('celltransactions/' + to_add_string + '/'+ cell_name + '.csv', index = False)

def matrix_transaction_allocation(time_list,total_matrix, months = False):
    global matrix_transaction
    global x_range 
    global y_range 
    list_years = []
    matrix_transaction = [[[] for x in range(len(x_range))] for y in range(len(y_range))] 
    matrix_non_transactions = [[[] for x in range(len(x_range))] for y in range(len(y_range))] 
    list_to_iter = reversed(sorted(time_list, key = lambda element: (element[0], element[1]) )) if months else sorted(time_list,reverse = True)
    for gdf_year in list_to_iter:
        gdf_sample = time_list[gdf_year].drop_duplicates(subset = 'sbl')
        timer = 0
        year = gdf_year if months else gdf_sample['year'].unique()[0] 
        list_years.append(year)
        if months == False and float(year) == 2002:
            break 
        g_sample = gdf_sample['geometry']
        for x in range(len(x_range)):
            for y in range(len(y_range)):
                polygon = matrix_cells[y][x]
                number_of_transactions = g_sample.within(polygon).sum()
                number_of_transactions_opposite = total_matrix[y][x] - number_of_transactions

                matrix_transaction[y][x].append(number_of_transactions)
                matrix_non_transactions[y][x].append(number_of_transactions_opposite)
                clear_output(wait = True)
                print("running matrix_transaction_allocation")
                print(year)
                timer += 1 
                print(timer / (len(x_range) * len(y_range)))
    return matrix_transaction, matrix_non_transactions,list_years

def matrix_percentages_allocation(matrix_transaction,total_matrix):
    # percentages bought matrix 
    global matrix_percentages
    global x_range 
    global y_range 
    timer = 0
    matrix_percentages = [[[] for x in range(len(x_range))] for y in range(len(y_range))] 
    for x in range(len(x_range)):
        for y in range(len(y_range)):
            transactions_list = matrix_transaction[y][x]
            for idx,z in enumerate(transactions_list):
                try:
                    val = z / total_matrix[y][x]
                    if val == val:
                        matrix_percentages[y][x].append(val)
                    else:
                        matrix_percentages[y][x].append(0) 
                except:
                    matrix_percentages[y][x].append(0)
                print("running matrix percentages allocation")
                clear_output(wait = True)
                timer +=1 
                print(timer / (len(x_range) * len(y_range)))
    return matrix_percentages 

def matrix_median_prices_allocation(time_list, months = False):
    global matrix_median_prices
    global x_range 
    global y_range 
    global matrix_cells 
    list_years_in_z = []
    list_to_iter = reversed(sorted(time_list, key = lambda element: (element[0], element[1]) )) if months else sorted(time_list,reverse = True)
    matrix_median_prices = [[[] for x in range(len(x_range))] for y in range(len(y_range))] 
    for gdf_time in list_to_iter:
        mean_consideration = time_list[gdf_time].groupby('sbl')['consideration'].mean().to_dict()
        gdf_sample = time_list[gdf_time].drop_duplicates(subset = 'sbl')
        gdf_sample['mean consideration'] = gdf_sample['sbl'].map(mean_consideration)
        year = gdf_time if months else gdf_sample['year'].unique()[0]
        if months == False and float(year) == 2002:
            break 
        list_years_in_z.append(year)
        g_sample = gdf_sample['geometry']
        timer = 0
        # if gdf_sample['year'].unique()[0] == 2005:
        #     break
        for x in range(len(x_range)):
            for y in range(len(y_range)):
                polygon = matrix_cells[y][x]
                median_houseprice = gdf_sample[g_sample.within(polygon)]['mean consideration'].median()
                if median_houseprice == median_houseprice:
                    matrix_median_prices[y][x].append(median_houseprice)
                else:
                    matrix_median_prices[y][x].append(-1)
                print("running matrix median prices allocation")
                print("year:" + year)
                timer += 1 
                print(timer / (len(x_range) * len(y_range)))
                clear_output(wait = True)
    return matrix_median_prices, list_years_in_z

# Features formulation
def highest_valued_priced_cell_distance(x,y,z, order_contiguity = 1, lag = False):
    indexes = mat_contig[str(y) + ',' + str(x)][mat_contig[str(y) + ',' + str(x)] <= order_contiguity].index
   
    highest_median = -1 
    location = None 
    if lag == True:
        lag_list = []
    for y_,x_ in indexes.str.split(','):
        median_price = matrix_median_prices[int(y_)][int(x_)][z] # it outputs -1 if empty cell or empty house prices 
        if lag == True:
            lag_list.append(median_price)
        if median_price > highest_median:
            highest_median  = median_price 
            location = str(y_) + "," + str(x_)
    if highest_median == -1:
        return -1
    return mat_contig[str(y) + ',' + str(x)].loc[location] if lag == False else np.array(lag_list).mean()
   
def highest_transactions_cell_distance(x,y,z, order_contiguity = 1, lag = False):
    indexes = mat_contig[str(y) + ',' + str(x)][mat_contig[str(y) + ',' + str(x)] <= order_contiguity].index
    try:
        highest_transaction = -1 
        location = None 
        if lag:
            lag_list = []
        for y_,x_ in indexes.str.split(','):
            transaction = matrix_transaction[int(y_)][int(x_)][z]
            if lag:
                lag_list.append(transaction)
            if transaction > highest_transaction:
                highest_transaction  = transaction 
                location = str(y_) + "," + str(x_)
        if highest_transaction == 0 or highest_transaction == -1:
            return -1 
        return mat_contig[str(y) + ',' + str(x)].loc[location] if lag == False else np.array(lag_list).mean()
    except:
        return -1 

def highest_percentage_cell_distance(x,y,z, order_contiguity = 1, lag = False):
    indexes = mat_contig[str(y) + ',' + str(x)][mat_contig[str(y) + ',' + str(x)] <= order_contiguity].index

    highest_percentage = -1 
    location = None 
    if lag:
        lag_list = []
    for y_,x_ in indexes.str.split(','):
        percentage = matrix_percentages[int(y_)][int(x_)][z]
        if lag:
            lag_list.append(percentage)
        if percentage > highest_percentage:
            highest_percentage  = percentage 
            location = str(y_) + "," + str(x_)
    if highest_percentage == 0 or highest_percentage == -1:
        return -1 
    return mat_contig[str(y) + ',' + str(x)].loc[location] if lag == False else np.array(lag_list).mean() 
    
def distance_from_nearest_park(x,y):
    polygon = matrix_cells[y][x]
    return parks_df['geometry'].apply(lambda g:  g.distance(polygon) ).min()

def addition_spatial_dependencies(z, prices = False, transactions = False, percentages = False):
    cell_copy = cell.copy()
    variables_to_add = []
    if percentages:
        matrix_call = matrix_percentages
    elif prices: 
        matrix_call = matrix_median_prices
    else: # then its transactions 
        matrix_call = matrix_transaction
    for y,x in cell_copy.index.str.split(','):
        variables_to_add.append(matrix_call[int(y)][int(x)][z])
    cell_copy['val'] = variables_to_add 
    li = esda.Moran_Local(cell_copy['val'],w)
    cell_copy['lisa category'] = li.q
    cell_copy['local moran expected value'] = li.EI_sim
    cell_copy['local moran p-value'] = li.p_sim
    return cell_copy
def find_lisa_val(cell_copy,x,y,column):
    return cell_copy.loc[str(y) + ',' + str(x)][column]
def lisa_highest_positive_distance(x,y,lisa_cells, order_contiguity = 1):
    # buggy code 
    indexes = mat_contig[str(y) + ',' + str(x)][mat_contig[str(y) + ',' + str(x)] <= order_contiguity].index
    highest_moran_i = -9999999
    location = None 
    for y_,x_ in indexes.str.split(','):
        lisaval = find_lisa_val(lisa_cells,x_,y_,'local moran expected value')
        if lisaval > highest_moran_i:
            highest_moran_i = lisaval
            location = str(y_) + ',' + str(x_)
    if highest_moran_i < 0: # this is because we want clustering of high values of transactions and high prices
        return -1 
    return mat_contig[str(y) + ',' + str(x)].loc[location] # returns distance in terms of order of contiguity
    
def distance_temporallag_addition(x,y,z,dictionary , number_of_years): # adds (X year ago ) column name
    columns_check_list= []
    # 'distance from highest valued cell a year ago'
    for i in range(1,number_of_years+1):
        result_price = highest_valued_priced_cell_distance(x,y,z+i)
        result_transactions = highest_transactions_cell_distance(x,y,z+i)
        column_name_highest_value = 'distance from highest valued cell ' + str(i) + ' years ago'
        column_name_highest_trans = 'distance from highest transaction cell ' + str(i) + ' years ago'
        if  column_name_highest_value in dictionary: 
            dictionary[column_name_highest_value].append(result_price)
        else:
            dictionary[column_name_highest_value] = []
            dictionary[column_name_highest_value].append(result_price)
        if column_name_highest_trans in dictionary: 
            dictionary[column_name_highest_trans].append(result_transactions)
        else:
            dictionary[column_name_highest_trans] = []
            dictionary[column_name_highest_trans].append(result_transactions)
        if (column_name_highest_value in columns_check_list) or (column_name_highest_trans in columns_check_list): # raise error
            raise ValueError("buggy code")
        else:
            columns_check_list.append(column_name_highest_value)
            columns_check_list.append(column_name_highest_trans)
            
            
def spatial_lag_time_price_lag_addition(x,y,z,dictionary, number_of_years, order_contiguity):
    columns_check_list = []
    for i in range(1, number_of_years +1):
        for num_order in range(1, order_contiguity +1,3):
            column_name = 'spatial lag of house price cells ('+ str(num_order) + ' order) ' + str(i) + ' year(s) ago'
            if column_name in dictionary:
                dictionary[column_name].append(highest_valued_priced_cell_distance(x,y,z+i,order_contiguity = num_order, lag = True ))
            else:
                dictionary[column_name] = []
                dictionary[column_name].append(highest_valued_priced_cell_distance(x,y,z+i,order_contiguity = num_order, lag = True ))
            if column_name in columns_check_list: # raise 
                raise ValueError("buggy code")
            else:
                columns_check_list.append(column_name)
                
def spatial_lag_time_transaction_lag_addition(x,y,z,dictionary, number_of_years, order_contiguity):
    columns_check_list = []
    for i in range(1, number_of_years +1):
        for num_order in range(1, order_contiguity +1,3):
            column_name = 'spatial lag of number of transaction cells ('+ str(num_order) + ' order) ' + str(i) + ' year(s) ago'
            if column_name in dictionary:
                dictionary[column_name].append(highest_transactions_cell_distance(x,y,z+i,order_contiguity = num_order, lag = True ))
            else:
                dictionary[column_name] = []
                dictionary[column_name].append(highest_transactions_cell_distance(x,y,z+i,order_contiguity = num_order, lag = True ))
            if column_name in columns_check_list: # raise error
                raise ValueError("buggy code")
            else:
                columns_check_list.append(column_name)
                
def spatial_lag_time_percentages_lag_addition(x,y,z,dictionary, number_of_years, order_contiguity):
    columns_check_list = []
    # highest_percentage_cell_distance
    for i in range(1, number_of_years +1):
        for num_order in range(1, order_contiguity +1,3):
            column_name = 'spatial lag of percentage of homes sold cells ('+ str(num_order) + ' order) ' + str(i) + ' year(s) ago'
            if column_name in dictionary:
                dictionary[column_name].append(highest_percentage_cell_distance(x,y,z+i,order_contiguity = num_order, lag = True ))
            else:
                dictionary[column_name] = []
                dictionary[column_name].append(highest_percentage_cell_distance(x,y,z+i,order_contiguity = num_order, lag = True ))
            if column_name in columns_check_list: # raise error
                raise ValueError("buggy code")
            else:
                columns_check_list.append(column_name) # you only want it to be appended once during this function call

                
                
                
def temporal_lag_lisa(x,y, dictionary, lisa_cells, column_category ):
    columns_check_list = []
    for idx,cell_matrix in enumerate(lisa_cells):
         # lisa additions + temporal lag 
          
        lc,col_name_lc = find_lisa_val(cell_matrix,x,y,'lisa category'), 'lisa category (' + column_category + ') ' + str(idx + 1) + ' year(s) ago'
        le,col_name_le  = find_lisa_val(cell_matrix,x,y, 'local moran expected value'), 'lisa moran expected value (' + column_category + ') ' + str(idx + 1) + ' year(s) ago'
        lp,col_name_lp = find_lisa_val(cell_matrix,x,y,'local moran p-value'), 'lisa p-value (' + column_category + ') ' + str(idx + 1) + ' year(s) ago'
        
        if col_name_lc in dictionary:
            dictionary[col_name_lc].append(lc)
        else: 
            dictionary[col_name_lc] = []
            dictionary[col_name_lc].append(lc)
        if col_name_le in dictionary:
            dictionary[col_name_le].append(le)
        else: 
            dictionary[col_name_le] = []
            dictionary[col_name_le].append(le)
        if col_name_lp in dictionary:
            dictionary[col_name_lp].append(le)
        else:
            dictionary[col_name_lp] = []
            dictionary[col_name_lp].append(lp)
            
        if (col_name_lc in columns_check_list) or (col_name_le in columns_check_list) or (col_name_lp in columns_check_list):
            raise ValueError("buggy code")
        else:
            columns_check_list.append(col_name_lc)
            columns_check_list.append(col_name_le)
            columns_check_list.append(col_name_lp)
            
            
def constant_variable_time_lags(matrix,x,y,z, dictionary,number_of_years,column_category):
    columns_check_list = []
    for i in range(1, number_of_years +1):
        string_column = column_category + ' ' + str(i) + ' year(s) ago'
        if string_column in dictionary:
            dictionary[string_column].append(matrix[y][x][z+i])
        else:
            dictionary[string_column] = []
            dictionary[string_column].append(matrix[y][x][z+i])
        if string_column in columns_check_list:
            raise ValueError("buggy code")
        else:
            columns_check_list.append(string_column)
            
            
def lisa_distance_cluster_lags(lisa_cells,x,y,dictionary, order_contiguity, column_category):
    columns_check_list = []
    for idx,cell_matrix in enumerate(lisa_cells):
        for num_order in range(1, order_contiguity +1):
            distance = lisa_highest_positive_distance(x,y,cell_matrix,order_contiguity= num_order)
            column_name = 'distance from highest positive value clustering (lisa) ' + column_category + ' ' + str(idx+ 1) +' year(s) ago with ' + str(num_order) + ' orders of contiguity'
            if column_name in dictionary:
                dictionary[column_name].append(distance)
            else:
                dictionary[column_name] = []
                dictionary[column_name].append(distance)
            if column_name in columns_check_list:
                raise ValueError("buggy code")
            else:
                columns_check_list.append(column_name)
    

def calculate_features(z_yr,number_of_years_time_lag = 6, months = False):
    global total_matrix
    global matrix_transaction
    global matrix_percentages
    global matrix_median_prices 
    global x_range 
    global y_range 
    z,yr = z_yr[0],z_yr[1]
    total_length = len(x_range) * len(y_range)
    to_append_dct = {'target': [], 'year': [],'cell name': [],'distance from nearest park':[],'total number of houses':[]}
    lisa_cells_prices = [ addition_spatial_dependencies(z+i, prices = True) for i in range(1,number_of_years_time_lag+1)]
    lisa_cells_transactions =   [ addition_spatial_dependencies(z+i, transactions = True) for i in range(1,number_of_years_time_lag+1)]
    lisa_cells_percentages = [addition_spatial_dependencies(z+i, percentages = True) for i in range(1,number_of_years_time_lag+1)]
    counter = 0
    for x in range(len(x_range)):
        for y in range(len(y_range)):
            transactions =  matrix_transaction[y][x][z] # goes through 2017 first 
            percentage = matrix_percentages[y][x][z]
            # if no homes then skip 
            to_append_dct['target'].append(percentage)
            to_append_dct['year'].append(yr)
            distance_temporallag_addition(x,y,z,to_append_dct, number_of_years = number_of_years_time_lag)
            to_append_dct['distance from nearest park'].append(distance_from_nearest_park(x,y))
            spatial_lag_time_price_lag_addition(x,y,z,to_append_dct, number_of_years = number_of_years_time_lag, order_contiguity = 5)
            spatial_lag_time_transaction_lag_addition(x,y,z,to_append_dct, number_of_years = number_of_years_time_lag, order_contiguity = 5)
            spatial_lag_time_percentages_lag_addition(x,y,z,to_append_dct, number_of_years = number_of_years_time_lag, order_contiguity = 5)
          
            # 
            temporal_lag_lisa(x,y,to_append_dct, lisa_cells_prices, 'median home prices')
            temporal_lag_lisa(x,y,to_append_dct, lisa_cells_transactions, '# transactions')
            temporal_lag_lisa(x,y,to_append_dct, lisa_cells_percentages, 'percentage house sold')
            
            # temporal constant time lags
         
            constant_variable_time_lags(matrix_median_prices, x,y,z,to_append_dct, number_of_years_time_lag, 'median home prices')
            constant_variable_time_lags(matrix_transaction, x,y,z,to_append_dct, number_of_years_time_lag, 'number of transactions')
            constant_variable_time_lags(matrix_percentages, x,y,z,to_append_dct, number_of_years_time_lag, 'percentage of homes sold')
            
            # distance from clustering lisas 
         
            lisa_distance_cluster_lags(lisa_cells_prices,x,y,to_append_dct, order_contiguity=5, column_category='median home prices' )
            lisa_distance_cluster_lags(lisa_cells_transactions,x,y,to_append_dct, order_contiguity=5, column_category='# transactions' )
            lisa_distance_cluster_lags(lisa_cells_percentages,x,y,to_append_dct, order_contiguity=5, column_category='percentage house sold')
            to_append_dct['total number of houses'].append(total_matrix[y][x])
            to_append_dct['cell name'].append(str(y) + ',' + str(x))
            counter += 1 
            print(yr)
            print(counter / total_length )
            clear_output(wait = True)
    return pd.DataFrame(to_append_dct)


def multiprocessing_feature_engineering_years(time_list):
    lists_pds = []
   
    with concurrent.futures.ProcessPoolExecutor() as executor:
        results = []
        for z,yr in enumerate(range(2020,2003,-1)):
            args = (z,yr)
            f1 = executor.submit(calculate_features, args)
            results.append(f1)

        for f in concurrent.futures.as_completed(results):
            lists_pds.append(f.result())
    return pd.concat(lists_pds)


def multiprocessing_feature_engineering_months(months_list):
    lists_pds = []
    months_list = sorted(time_list,reverse = True)
    with concurrent.futures.ProcessPoolExecutor() as executor:
        results = []
        
        for z,month in enumerate(months_list):
            args = (z,yr)
            f1 = executor.submit(calculate_features, args)
            results.append(f1)

        for f in concurrent.futures.as_completed(results):
            lists_pds.append(f.result())
    return pd.concat(lists_pds)


def binary_prediction_task(data_frame_model,percent_greater_than = 10):
    data_frame_model = data_frame_model[data_frame_model['total number of houses'] > 0]
    data_frame_model['target'] = data_frame_model['target'] *  100
    data_frame_model['new_target'] = data_frame_model['target'] > percent_greater_than
    target = data_frame_model[data_frame_model['year'] == 2018]
    train = data_frame_model[data_frame_model['year'] < 2018]
    X_train,y_train = train.drop(['target','year','new_target', 'cell name'], axis =1), train['new_target']
    X_test, y_test = target.drop(['target','year','new_target', 'cell name'], axis =1 ), target['new_target']
    clf = sklearn.ensemble.RandomForestClassifier(n_estimators= 200, verbose = 2,n_jobs = -1)
    clf.fit(X_train,y_train)
    print(sklearn.metrics.classification_report(y_test, clf.predict(X_test)))

def baseline_prediction_task_yearprior(to_model):
    to_model = to_model[to_model['total number of houses'] > 0]
    predict_ = []
    truth_  = [] 
    target = to_model[to_model['year'] == 2018]
    train = to_model[to_model['year'] == 2017]
    merged = target.merge(train, on  = 'cell name')
    y_before,y_after = merged['new_target_y'], merged['new_target_x']
    print(sklearn.metrics.classification_report(y_after, y_before))


