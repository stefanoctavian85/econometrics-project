# Sustainable Energy Regression Model
The project analyzes sustainable energy consumption and CO2 emissions using regression and econometric models. It explores the relationship between energy variables like renewable energy share, electricity access, and energy intensity, as well as their impact on CO2 emissions across different countries from 2010 to 2020.

## Data
1. Entity
2. Year
3. Primary energy consumption per capita (kWh/person)
4. Access to electricity (% of population)
5. Renewable energy share in total final energy consumption (%)
6. Energy intensity (MJ per 2017 PPP GDP)
7. CO2 Emissions
8. Renewable Energy Share
9. Electricity from Fossil Fuels
10. Primary Energy Consumption
The dataset can be loaded into R from the .csv file from the repo.

## Hypothesis Testing:
1. OLS vs Fixed Effects (FE): Tested to determine if the fixed effects model is preferred over the OLS model.
2. Fixed Effects (FE) vs Random Effects (RE): Tested to decide between fixed effects and random effects models.
3. Fixed Effects in Time: Tested whether fixed effects are necessary over time.
4. Breusch-Pagan Lagrange Multiplier Test: Checked for variations over time to assess the suitability of random effects.
5. Cross-sectional Dependence: Tested for correlation between entities using the PCD test.
6. Autocorrelation: Checked for autocorrelation in the residuals with the Breusch-Godfrey/Wooldridge test.
7. Heteroscedasticity: Applied the Breusch-Pagan test to verify homoscedasticity or the presence of heteroscedasticity.
8. Multicollinearity: Tested using the Variance Inflation Factor (VIF) to detect if there is high correlation among predictor variables.
