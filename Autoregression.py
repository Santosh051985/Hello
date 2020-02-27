# import required Library statsmodels for statistical Model
from statsmodels.tsa.ar_model import AR
# import Random for random values
from random import random
# contrived dataset
data = [x + random() for x in range(1, 100)]
# fit model
model = AR(data)
model_fit = model.fit()
# make prediction
yhat = model_fit.predict(len(data), len(data))
print(yhat)
