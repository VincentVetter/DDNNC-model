import numpy as np
import pandas as pd
from scipy.stats import norm, multivariate_normal, johnsonsu
from scipy.special import erfinv
from scipy.stats import johnsonsu
import matplotlib.pyplot as plt

# Constants
cal_len = 382 # 91 + 91 + 200, period after 12.09.2018
sigma_cal = 91
n_ens = 10000

#### input
# read 2 datasets
# real: shape (382, 24, 10) (day, hour, sub-period) real ID price after 12.09.2018
# qra_99q: (the name was from quantile regression averaging of the LQC method in the paper, you can change the name)
#      shape (382, 24, 10, 100) (day, hour, sub-period, percentiles)
#      in the last dimension, draw 100 percentiles from the univariate distribution obtained from DDNN

# Import data and reshape
data_used = pd.read_csv("data_used.csv", header=None).to_numpy()


real = np.zeros((382, 24, 10))
for d in range(0,382):
    for h in range(0,24):
        for sub in range (0,10):
            real[d,h,sub] = data_used[(454*24)+(d*24)+h,103+sub]

# Import quantiles and reshape
percentiles = pd.read_csv("percentiles.csv", sep=",").to_numpy()
percentiles = percentiles[:, 1:]
qra_99q = np.zeros((382, 24, 10, 100))


for d in range(382):
    for h in range(24):
        for sub in range(10):
            qra_99q[d, h, sub, :] = percentiles[d*24+h,sub*100:(sub+1)*100]


Y = np.zeros((382, 24, 10)) # quantile matrix for later covariance 

for day in range(cal_len):
    for hour in range(24):
        for traj in range(10):
            real_values = real[day, hour, traj]
            qra_values = qra_99q[day, hour, traj, :]
            
            greater_count = np.sum(real_values > qra_values)
            
            if greater_count / 99 == 0:
                Y[day, hour, traj] = 0.01
            elif greater_count / 99 == 1:
                Y[day, hour, traj] = 0.99
            else:
                #Y[day, hour, traj] = greater_count / 99
                Y[day, hour, traj] = np.clip(greater_count / 99, 0.01, 0.99)



def calc_quants(tr, quantiles):
    # tr (10000,); quantiles (101,)
    quants = np.zeros(tr.shape)
    for i in range(len(tr)):
        idxs = tr[i] > np.arange(101)
        idx = np.max(np.where(idxs)[0])
        up = quantiles[idx + 1]
        down = quantiles[idx]
        quants[i] = down + (up - down) * (tr[i] - idx)
    return quants

X = np.sqrt(2) * erfinv(2 * Y - 1) 

# Files where to large for further processing so I had to divide
trajectories1 = np.zeros((37, 24, 10, n_ens))
trajectories2 = np.zeros((37, 24, 10, n_ens))
trajectories3 = np.zeros((37, 24, 10, n_ens))
trajectories4 = np.zeros((36, 24, 10, n_ens))
trajectories5 = np.zeros((36, 24, 10, n_ens))
trajectories6 = np.zeros((36, 24, 10, n_ens))
trajectories7 = np.zeros((36, 24, 10, n_ens))
trajectories8 = np.zeros((36, 24, 10, n_ens))


for i in range(sigma_cal, cal_len):
    iteration = i - 91 + 1
    for h in range(24):
        maxes = np.max(real[(i-sigma_cal):i, h, :], axis=0)
        mins = np.min(real[(i-sigma_cal):i, h, :], axis=0)
        C = np.corrcoef(X[(i-sigma_cal):i, h, :], rowvar=False)
        samples = np.random.multivariate_normal(np.zeros(10), C, n_ens)
        tr = norm.cdf(samples)
        tr = tr * 100
        
        for quar in range(10):
            quantiles = qra_99q[i, h, quar, :]
            min0 = np.min([quantiles[0], mins[quar]])
            max0 = np.max([quantiles[98], maxes[quar]])
            # approximation of quantiles at the edges
            quantiles_edge0 = np.append(min0, quantiles)
            quantiles_edge = np.append(quantiles_edge0, max0)
            if iteration <= 37:
                trajectories1[i-sigma_cal, h, quar, :] = calc_quants(tr[:, quar], quantiles_edge)
            if 37 < iteration <= 74:
                trajectories2[i-sigma_cal-37, h, quar, :] = calc_quants(tr[:, quar], quantiles_edge)
            if 74 < iteration <= 111:
                trajectories3[i-sigma_cal-74, h, quar, :] = calc_quants(tr[:, quar], quantiles_edge)
            if 111 < iteration <= 147:
                trajectories4[i-sigma_cal-111, h, quar, :] = calc_quants(tr[:, quar], quantiles_edge)
            if 147 < iteration <= 183:
                trajectories5[i-sigma_cal-147, h, quar, :] = calc_quants(tr[:, quar], quantiles_edge)
            if 183 < iteration <= 219:
                trajectories6[i-sigma_cal-183, h, quar, :] = calc_quants(tr[:, quar], quantiles_edge)
            if 219 < iteration <= 255:
                trajectories7[i-sigma_cal-219, h, quar, :] = calc_quants(tr[:, quar], quantiles_edge)
            if 255 < iteration <= 291:
                trajectories8[i-sigma_cal-255, h, quar, :] = calc_quants(tr[:, quar], quantiles_edge)

path_forecast1 = np.zeros((37*24*10,n_ens))
path_forecast2 = np.zeros((37*24*10,n_ens))
path_forecast3 = np.zeros((37*24*10,n_ens))
path_forecast4 = np.zeros((36*24*10,n_ens))
path_forecast5 = np.zeros((36*24*10,n_ens))
path_forecast6 = np.zeros((36*24*10,n_ens))
path_forecast7 = np.zeros((36*24*10,n_ens))
path_forecast8 = np.zeros((36*24*10,n_ens))


for d in range(37):
    for h in range(24):
        for sub in range(10):
            path_forecast1[d*24*10+h*10+sub,:] = trajectories1[d,h,sub,:]
        
for d in range(37):
    for h in range(24):
        for sub in range(10):
            path_forecast2[d*24*10+h*10+sub,:] = trajectories2[d,h,sub,:]

for d in range(37):
    for h in range(24):
        for sub in range(10):
            path_forecast3[d*24*10+h*10+sub,:] = trajectories3[d,h,sub,:]

for d in range(36):
    for h in range(24):
        for sub in range(10):
            path_forecast4[d*24*10+h*10+sub,:] = trajectories4[d,h,sub,:]

for d in range(36):
    for h in range(24):
        for sub in range(10):
            path_forecast5[d*24*10+h*10+sub,:] = trajectories5[d,h,sub,:]

for d in range(36):
    for h in range(24):
        for sub in range(10):
            path_forecast6[d*24*10+h*10+sub,:] = trajectories6[d,h,sub,:]

for d in range(36):
    for h in range(24):
        for sub in range(10):
            path_forecast7[d*24*10+h*10+sub,:] = trajectories7[d,h,sub,:]

for d in range(36):
    for h in range(24):
        for sub in range(10):
            path_forecast8[d*24*10+h*10+sub,:] = trajectories8[d,h,sub,:]


pd.DataFrame(path_forecast1).to_csv('path_forecast1.csv', index=False, header=False)
pd.DataFrame(path_forecast2).to_csv('path_forecast2.csv', index=False, header=False)
pd.DataFrame(path_forecast3).to_csv('path_forecast3.csv', index=False, header=False)
pd.DataFrame(path_forecast4).to_csv('path_forecast4.csv', index=False, header=False)
pd.DataFrame(path_forecast5).to_csv('path_forecast5.csv', index=False, header=False)
pd.DataFrame(path_forecast6).to_csv('path_forecast6.csv', index=False, header=False)
pd.DataFrame(path_forecast7).to_csv('path_forecast7.csv', index=False, header=False)
pd.DataFrame(path_forecast8).to_csv('path_forecast8.csv', index=False, header=False)


