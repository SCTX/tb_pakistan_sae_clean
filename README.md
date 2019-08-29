# tb_pakistan_sae_clean

Scripts to reproduce the results for the IDM entry into the TB Hackathon 2019

## Repository structure

The `data` folder holds all the cleaned/preprocessed data ready for input of the model. Inside it, the `training` folder holds the dataset used for training the model (and cross-validation), the `prediction` folder holds all the raster needed to make predictions from a trained model, and `shapefiles` holds all the shapefiles necessary throughout the analysis.

There are three scripts:
- `cross-validation-main.R` fits the model using leave-one-out cross-validation, computes the MSE and RSE values from the fits and produces a plot summarizing the results.
- `prediction-main.R` uses the whole 95 clusters to fit the model, and then make predictions at a 5km raster level using the covariates in `prediction`.
- `prediction-utils.R` holds utility functions to lighten the `prediction-main.R` script.
