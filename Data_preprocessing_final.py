import numpy as np
import pandas as pd
from datetime import datetime, timedelta

home_dir = 'C:/Users/v_vet/OneDrive/Dokumente/Seminar Topics in Econometrics/Code'
data_dir = 'C:/Users/v_vet/OneDrive/Dokumente/Seminar Topics in Econometrics/Code'

start_date = datetime(2017, 6, 14)
result_date = start_date + timedelta(days=100)

# -------------------------------Intraday Volumes and Prices-----------------------------------------------------
volumesintra = np.zeros((837, 24, 14))
for n_hour in range(24):
    with open(home_dir + '/ID_DATA/' + f'volumes_hourly_{("0" + str(n_hour))[-2:]}') as f:
        lines = f.readlines()
        date_hour = [str(line.split(',')[0].strip()) for line in lines[1:]]
        data = np.array([[float(e) for e in line.strip().split(',')[1:]] for line in lines[1:]])
    date_3hour = data[:,:12]
    date_str = [datetime.strptime(date_string, '%Y-%m-%dT%H:%M:%SZ').strftime('%Y%m%d')
                for date_string in date_hour]
    date_diff = [(datetime.strptime(date_string, '%Y%m%d') - start_date).days
                 for date_string in date_str]
    hour = np.zeros(837) + n_hour
    
    volumesintra[:, n_hour, 0] = np.array(date_diff) + 1 # date
    volumesintra[:, n_hour, 1] = hour
    volumesintra[:, n_hour, 2:] = date_3hour
    
volumesintra_flat = volumesintra.reshape(-1, volumesintra.shape[-1])
   
pricesintra = np.zeros((837, 24, 15))
for n_hour in range(24):
    with open(home_dir + '/ID_DATA/' + f'prices_hourly_{("0" + str(n_hour))[-2:]}') as f:
        lines = f.readlines()
        date_hour = [str(line.split(',')[0].strip()) for line in lines[1:]]
        data = np.array([[float(e) for e in line.strip().split(',')[1:]] for line in lines[1:]])
    date_3hour = data[:,:13]
    date_str = [datetime.strptime(date_string, '%Y-%m-%dT%H:%M:%SZ').strftime('%Y%m%d')
                for date_string in date_hour]
    date_diff = [(datetime.strptime(date_string, '%Y%m%d') - start_date).days
                 for date_string in date_str]
    hour = np.zeros(837) + n_hour
    
    pricesintra[:, n_hour, 0] = np.array(date_diff) + 1 # date
    pricesintra[:, n_hour, 1] = hour
    pricesintra[:, n_hour, 2:] = date_3hour
    
pricesintra_flat = pricesintra.reshape(-1, pricesintra.shape[-1])   
   
#-------------------------------------Calculation of VWA Price--------------------------------------------------------
subprices = pricesintra_flat[:, 2:14]
subvolumes = volumesintra_flat[:, 2:]

id3_volume_sum = np.sum(subvolumes, axis=1)
id3_pricevolume_sum = np.sum((subprices * subvolumes), axis=1)
mean_subprice = np.mean(subprices, axis=1)

zero_volume_index = np.where(id3_volume_sum == 0)
nonzero_volume_index = ~np.isin(np.array(list(range(837*24))), zero_volume_index)
    
id3_vwa_price = np.zeros(837*24)
id3_vwa_price[nonzero_volume_index] = id3_pricevolume_sum[nonzero_volume_index] / id3_volume_sum[nonzero_volume_index]
id3_vwa_price[zero_volume_index] = mean_subprice[zero_volume_index]

id_price_true = subprices[:,2:]
last_vwa_price = pricesintra_flat[:, 14]

id_pred = np.zeros((837*24, 14))
id_pred[:, :2] = pricesintra_flat[:, :2]
id_pred[:, 2] = last_vwa_price # Last VWA ID price (15min before delivery)
id_pred[:, 3] = id3_vwa_price # ID3 price
id_pred[:, 4:] = id_price_true

#--------------------------------------Exogenous Data Loading--------------------------------------------------------
exog_pred = np.zeros((837*24, 7))

with open(home_dir + '/EXOG_DATA/' + 'Day_Ahead_Epex.csv') as f:
    lines = f.readlines()
    date_str = [str(line.split(';')[0].strip()) for line in lines]
    hour = [int(line.split(';')[1].strip()) for line in lines] #1~24
    data = [float(line.split(';')[2].strip()) for line in lines]
    date_diff = [(datetime.strptime(date_string, '%Y%m%d') - start_date).days
                 for date_string in date_str]
     
exog_pred[:, 0] = np.array(date_diff) # date
exog_pred[:, 1] = np.array(hour) - 1 # hour
exog_pred[:, 2] = np.array(data) # day ahead price

with open(home_dir + '/EXOG_DATA/' + 'final_wind_offshore.csv') as f:
    lines = f.readlines()
    data = [float(line.strip()) for line in lines]
woff = np.array(data)

with open(home_dir + '/EXOG_DATA/' + 'final_wind_onshore.csv') as f:
    lines = f.readlines()
    data = [float(line.strip()) for line in lines]
won = np.array(data)

with open(home_dir + '/EXOG_DATA/' + 'final_wind_offshore_real.csv') as f:
    lines = f.readlines()
    data = [float(line.strip()) for line in lines]
woffreal = np.array(data)

with open(home_dir + '/EXOG_DATA/' + 'final_wind_onshore_real.csv') as f:
    lines = f.readlines()
    data = [float(line.strip()) for line in lines]
wonreal = np.array(data)

exog_pred[:, 3] = won + woff # wind sum forecast
exog_pred[:, 4] = wonreal + woffreal # wind sum real

with open(home_dir + '/EXOG_DATA/' + 'final_load_da.csv') as f:
    lines = f.readlines()
    data = [float(line.strip()) for line in lines]

exog_pred[:, 5] = np.array(data)

loadreal = np.zeros((837, 24))
with open(home_dir + '/EXOG_DATA/' + 'final_load_real.csv') as f:
    lines = f.readlines()
    data = [float(line.strip()) for line in lines]

exog_pred[:, 6] = np.array(data)

id_pred_df = pd.DataFrame(id_pred)
id_pred_df.columns = ['day', 'hour', 'last_p', 'id3_p', 'id_2', 'id_3', 'id_4', 
                      'id_5', 'id_6', 'id_7', 'id_8', 'id_9', 'id_10', 'id_11']

exog_pred_df = pd.DataFrame(exog_pred)
exog_pred_df.columns = ['day', 'hour', 'da_p', 'w_pred', 'w_real', 'l_pred', 'l_real']

#---------------------------------Combining Data into DataFrame----------------------------------------------------------------
pred_combine_df = pd.merge(exog_pred_df, id_pred_df, how="outer", on=["day", "hour"])
pred_combine_arr = pred_combine_df.values

#----------------------------------------Data Normalization---------------------------------------------------------------
cal_len_norm = 364 + 91 + 91

pred_combine_norm = pred_combine_arr.copy()
for i in range(2, 7):
    data_i = pred_combine_arr[:, i]
    data_cal = pred_combine_arr[:cal_len_norm*24, i]
    mu = data_cal.mean()
    sigma = data_cal.std()
    data_norm = (data_i - mu) / sigma
    pred_combine_norm[:,i] = data_norm
    
data_id_cal = pred_combine_arr[:cal_len_norm*24, 9:]
data_id_mu = data_id_cal.mean()
data_id_sigma = data_id_cal.std()
  
pred_combine_norm[:, 7:] = (pred_combine_arr[:, 7:] - data_id_mu) / data_id_sigma

with open('output_text.txt', 'w') as output_text:  # or 'a' for append mode
    print('ID price mu: ', data_id_mu, 'ID price sigma: ', data_id_sigma, file=output_text)
    ## Prints data_id_mu and data_id_sigma to output_text for reference, useful for tracking normalization parameters.

pred_combine_norm_df = pd.DataFrame(pred_combine_norm)
pred_combine_norm_df.columns = pred_combine_df.columns

#---------------------------------------Feature Selection for Model-------------------------------------------------
# Create Data_all, to hold the feature data required for prediction, including lagged values of various predictors like prices, wind, and load:
Nfeatures = 101
LEAD = 4 # predict h with info until h-LEAD
data_len = pred_combine_df.shape[0]
index_all = np.array(list(range(data_len)))
data_all = np.zeros((data_len, Nfeatures+12))
data_all[:, :2] = pred_combine_norm[:, :2]

for i in range(21):
    data_all[:, i+2] = pred_combine_norm[(index_all-LEAD-i), 8]
    
for i in range(25):
    data_all[:, i+2+21] = pred_combine_norm[(index_all-i), 2]
    
for i in range(25):
    data_all[:, i+2+21+25] = pred_combine_norm[(index_all-i), 3]
    
data_all[:, 2+21+25+25] = pred_combine_norm[(index_all-LEAD), 4]
data_all[:, 2+21+25+26] = pred_combine_norm[(index_all-24), 4]
    
for i in range(25):
    data_all[:, i+2+21+25+27] = pred_combine_norm[(index_all-i), 5]
    
data_all[:, 2+21+25+27+25] = pred_combine_norm[(index_all-LEAD), 6]
data_all[:, 2+21+25+27+26] = pred_combine_norm[(index_all-24), 6]
    
data_all[:, 2+21+25+27+27] = pred_combine_norm[index_all, 7]

data_all[:, (2+101):] = pred_combine_norm[index_all, 9:]

data_used = data_all[24:, :]

pd.DataFrame(data_used).to_csv("data_used.csv", index=False, header=False)
