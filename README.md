# Prophet-forecasting

- Uses Quasy Newton Method for finding gradient and uses L-BFGS which uses current values to approximate/predict values 
- Addition of regressors like holidays can be used to make prediciton more accurate
- Changepoint helps to deal with lag
- Handles outliers by itself

## Backtesting
I have also added the part of back testing for predicting 1st,2nd & 3rd month forecast on a rolling period
For eg: 
1) Actual till - Jan      Forecast - Feb (1st), March (2nd) , April (3rd)
2) Actual till - Feb      Forecast - March (1st), April (2nd), May (3rd)
3) and soon
