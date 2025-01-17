# Sustainable Energy Regression Model
Aplicatia1 uses a regression model to analyze the relationship between sustainable energy consumption and various factors such as GDP per capita, access to electricity, renewable energy share, and energy intensity. The dataset used for this analysis includes data on primary energy consumption, renewable energy share, and other economic variables.

Aplicatia2 performs an econometric analysis of sustainable energy trends and CO2 emissions using a panel data regression model. The dataset includes information on CO2 emissions, renewable energy share, electricity from fossil fuels, primary energy consumption per capita, and energy intensity for different countries between 2010 and 2020.

## Data
The data used in this project comes from a global dataset on sustainable energy, which includes variables like:

Entity: Country or region.
Year: Year of the observation.
Primary energy consumption per capita (kWh/person)
Access to electricity (% of population)
Renewable energy share in total final energy consumption (%)
Energy intensity (MJ per 2017 PPP GDP)
CO2 Emissions: Carbon dioxide emissions (kt) by country.
Renewable Energy Share: Percentage of renewable energy in total energy consumption.
Electricity from Fossil Fuels: Electricity generated from fossil fuels (TWh).
Primary Energy Consumption: Primary energy consumption per capita (kWh).
The dataset can be loaded into R from the .csv file from the repo.

## Hypothesis Testing:
OLS vs Fixed Effects (FE): Tested to determine if the fixed effects model is preferred over the OLS model.
Fixed Effects (FE) vs Random Effects (RE): Tested to decide between fixed effects and random effects models.
Fixed Effects in Time: Tested whether fixed effects are necessary over time.
Breusch-Pagan Lagrange Multiplier Test: Checked for variations over time to assess the suitability of random effects.
Cross-sectional Dependence: Tested for correlation between entities using the PCD test.
Autocorrelation: Checked for autocorrelation in the residuals with the Breusch-Godfrey/Wooldridge test.
Heteroscedasticity: Applied the Breusch-Pagan test to verify homoscedasticity or the presence of heteroscedasticity.
