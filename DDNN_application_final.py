import os 
os.environ["TF_CPP_MIN_LOG_LEVEL"] = "2"
import pandas as pd
import numpy as np
import tensorflow as tf 
from tensorflow import keras
import tensorflow_probability as tfp

# Setting Directory Paths
home_dir = 'C:/Users/v_vet/OneDrive/Dokumente/Seminar Topics in Econometrics/Code'
data_dir = 'C:/Users/v_vet/OneDrive/Dokumente/Seminar Topics in Econometrics/Code'

# Read in data_used:
data_used = pd.read_csv("data_used.csv", header=None).to_numpy()


# Constants and Variables:
cal_len = 364 + 91 - 1
# -1 because we left out the first day, thus our traing period is shorter (by 1)
val_len = 91 + 91 + 200
window_size = 7
Nfeatures = 102

# Train function
def train(day):
    X = np.zeros((24 * (cal_len - 1), Nfeatures))
    Yall = np.zeros((24 * (cal_len - 1), 10))

    X = data_used[day * 24: (day + cal_len - 1) * 24, 1:103]
    Yall = data_used[day * 24: (day + cal_len - 1) * 24, 103:113]
    
    # Data preperation: Accout for 10 subperiods:
    Y0 = np.zeros((Yall.shape[0] * 10, 1))
    X0 = np.zeros((X.shape[0] * 10, X.shape[1] + 1))
    for sub in range(10):
        Y = Yall[:, sub]
        Y0[sub::10, 0] = Y
        X0[sub::10, :-1] = X
        X0[sub::10, -1] = sub

    x = keras.layers.Input(shape=(Nfeatures+1,))
    y1 = keras.layers.Dense(128, activation='elu')(x)
    y2 = keras.layers.Dense(128, activation='elu')(y1)
    z1 = keras.layers.Dense(2, activation='linear')(y2)
    z2 = keras.layers.Dense(2, activation='softplus')(y2)
    z = keras.layers.Concatenate(axis=1)([z1, z2])
    model = keras.Model(inputs=x, outputs=z)


    optim = keras.optimizers.Adam(learning_rate = 0.0001)

    def negative_log_likelihood(y_true, y_pred):
        """
        Custom loss function to minimize the negative log-likelihood
        of the Johnson's SU distribution.
        Parameters:
        - y_true: Ground truth values.
        - y_pred: Predicted values (loc, scale, skewness, tailweight).
        """

        # Extract the predicted parameters
        loc = y_pred[:, 0]         # Location
        skewness = y_pred[:, 1]    # Skewness
        scale = y_pred[:, 2]       # Scale (enforced positive)
        tailweight = y_pred[:, 3]  # Tailweight (enforced positive)
            
        # Create a Johnson's SU distribution with the predicted parameters
        johnson_su = tfp.distributions.JohnsonSU(
            skewness=skewness,
            tailweight=tailweight,
            loc=loc,
            scale=scale
        )
            # Compute the log probability density for the true values
        log_likelihood = johnson_su.log_prob(y_true)

        # Return the negative log likelihood
        return -tf.reduce_mean(log_likelihood)
    
    model.compile(optimizer = optim, loss = negative_log_likelihood,
              metrics=[negative_log_likelihood])

    # Training and Validation Setup:
    callbacks = [keras.callbacks.EarlyStopping(patience=50, restore_best_weights=True)]
    perm = np.random.permutation(np.arange(X0.shape[0]))
    VAL_DATA = .2
    trainsubset = perm[:int((1 - VAL_DATA)*len(perm))]
    valsubset = perm[int((1 - VAL_DATA)*len(perm)):]

    # Model training:
    model.fit(X0[trainsubset], Y0[trainsubset], epochs=1500, validation_data=(X0[valsubset], Y0[valsubset]),
                  callbacks=callbacks, batch_size=512, verbose=True)
    return model
    

def forecast(day, model1):
    Xfut = np.zeros((24, Nfeatures))
    Xfut = data_used[(day + cal_len) * 24: (day + cal_len + 1) * 24, 1:103]

    predictions = np.zeros((24, 10 * 4))

    Xf0 = np.zeros((Xfut.shape[0] * 10, Xfut.shape[1] + 1))

    for sub in range(10):
        Xf0[sub::10, :-1] = Xfut
        Xf0[sub::10, -1] = sub

    for h in range(24):
        for sub in range(10):
            pred1 = model1.predict(Xf0[h*10+sub,:].reshape(1, -1))
            """
            pred2 = model2.predict(Xf0[h*10+sub,:].reshape(1, -1))
            pred3 = model3.predict(Xf0[h*10+sub,:].reshape(1, -1))
            pred4 = model4.predict(Xf0[h*10+sub,:].reshape(1, -1))
            pred5 = model5.predict(Xf0[h*10+sub,:].reshape(1, -1))
            stacked_preds = np.stack([pred1, pred2, pred3, pred4, pred5], axis=0)
            pred = np.mean(stacked_preds, axis=0)
            """

            predictions[h, sub*4:(sub+1)*4] = pred1

    return predictions

preds_test = np.zeros((24, 10 * 4))
model1 = train(0)

preds_test = forecast(0, model1)
pd.DataFrame(preds_test).to_csv('preds_test.csv', index=False, header=False)

"""
preds = np.zeros((val_len * 24, 10 * 4))

for window in range((val_len // window_size) + 1):
    model1 = train(window * window_size)
    model2 = train(window * window_size)
    model3 = train(window * window_size)
    model4 = train(window * window_size)
    model5 = train(window * window_size)
    for day in range(window_size):
        if (window * window_size + day + cal_len) <= (val_len + cal_len - 1):
            preds[(window * window_size + day) * 24:(window * window_size + day + 1) * 24,:] = forecast(window * window_size + day, model1, model2, model3, model4, model5)
pd.DataFrame(preds).to_csv('preds.csv', index=False, header=False)

"""