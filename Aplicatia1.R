#Aplicatia 1: Model de regresie

# Instalarea pachetelor
# Instalare si activare pachete
PackageNames <- c("tidyverse", "stargazer", "magrittr", "lmtest", "sandwich", 
                  "olsrr", "moments","whitestrap","ggplot2","DataCombine","car","tseries","readxl",
                  "foreign","caret","glmnet","corrplot","RColorBrewer","dplyr", "mltools", "MLmetrics")
for(i in PackageNames){
  if(!require(i, character.only = T)){
    install.packages(i, dependencies = T)
    require(i, character.only = T)
  }
}


library("dplyr")  
library("tidyverse")
library("stargazer")
library("magrittr")
library("lmtest")
library("olsrr")
library("moments")
library("whitestrap")
library("sandwich")
library("tseries")
library("readxl")
library("foreign")
library("tidyverse")
library("glmnet")
library("corrplot")
library("RColorBrewer")
library("caret")
library("caTools")
library(dplyr)
library(DataCombine)
library(mltools)
library(MLmetrics)

#clean environment
rm(list = ls())

#sustainable_energy <-read.csv("/Users/mateiciobanu/Documents/econometrie/proiect_econometrie/global-data-on-sustainable-energy.csv")
sustainable_energy <- read.csv("C:/Aplicatii/Facultate/Programe/Econometrie/Proiect/global-data-on-sustainable-energy.csv")

#2. Data cleaning

#Verificare valori repetate
nrow(sustainable_energy)
## [1] 3649
nrow(unique(sustainable_energy))
## [1] 3649

#Functie pentru inlocuirea valorilor lipsa
handle_missing_values <- function(df) {
  df <- df %>% group_by(Entity) %>% 
    mutate(across(where(is.numeric), ~ ifelse(is.na(.), mean(., na.rm = TRUE), .))) %>% 
    ungroup()
  
  return(df)
}

#Aplicarea functiei asupra setului de date
sustainable_energy_cleaned <- handle_missing_values(sustainable_energy)

#Verificare rezultate
summary(sustainable_energy_cleaned)

# Filtrare pentru anul 2019
sustainable_energy_2019 <- sustainable_energy_cleaned %>% filter(Year == 2019) %>% filter(!is.na(Primary.energy.consumption.per.capita..kWh.person.) & !is.na(Access.to.electricity....of.population.) &
                                                               !is.na(Renewable.energy.share.in.the.total.final.energy.consumption....) & !is.na(Energy.intensity.level.of.primary.energy..MJ..2017.PPP.GDP.)
                                                               & !is.na(gdp_per_capita))

# Verificare a setului de date filtrat
head(sustainable_energy_2019)

#Am modificat numele coloanelor pentru a avea un nume mai clar
sustainable_energy_2019 <- sustainable_energy_2019 %>%
  rename(
    primary_energy = `Primary.energy.consumption.per.capita..kWh.person.`,
    access_to_electricity = `Access.to.electricity....of.population.`,
    renewable_energy_share = `Renewable.energy.share.in.the.total.final.energy.consumption....`,
    energy_intensity = `Energy.intensity.level.of.primary.energy..MJ..2017.PPP.GDP.`
  )
names(sustainable_energy_2019)
#Model de regresie multifactorial
model <- lm(primary_energy ~ gdp_per_capita + access_to_electricity + renewable_energy_share + energy_intensity , data=sustainable_energy_2019)
#Modelul reprezinta consumul de energie per cap de locuitor in kWh in functie de PIB/locuitor, accesul populatiei la electricitate(exprimat in procente),
#cantitatea de energie necesara pentru produce o unitate de PIB, ajustata la paritatea puterii de cumparare a dolarului in 2017 si procentul de
#energie primara care este derivata din energia regenerabila
summary(model)
#Bonitatea: 62.51%
#Modelul este semnificativ statistic pentru 95%
#Variabilele independente explica 62% din variabilitatea consumului de energie
#Varibilele independente gdp_per_capita, renewable_energy_share, energy_intensity sunt semnificative statistic pentru un nivel de semnificatie de 1%, iar
#variabila independenta access_to_electricity este semnificativa statistic pentru un nivel de semnificatie de 5%.
#La o crestere a GDP/capita cu 1 dolar, consumul de energie primara creste cu 0.9655 (kWh/persoana)
#La o crestere a accesului la electricitate cu 1 procent, consumul de energie primara creste cu 194.1 (kWh/persoana)
#La o crestere a energiei regenerabile cu 1 procent, consumul de energie primara scade cu 281.3 (kWh/persoana)
#La o crestere a cantitatii de energie necesare cu 1 unitate, consumul de energie primara creste cu 5468 (kWh/persoana)
#Extragem reziduurile din model
sustainable_energy_2019 %<>% mutate(uhat = resid(model))

#Testarea de ipoteze
#1. Forma functionala este liniara
#Da, deoarece poate fi scris ca o functie liniara
#primary_energy = -2.323e+04 + 9.694e-01 * gdp_per_capita + 2.067e+02 * access_to_electricity - 2.758e+02 * renewable_energy_share + 5.520e+03 * energy_intensity

#2. Nr. de observatii > nr. variabile independente
nobs(model) > (model$rank - 1) # adevarat

#3. Modelul de regresie este corect specificat
# Presupune ca daca variabilele X si Y au o relatie inversa, ecuatia modelului
# sa fie specificata in mod corespunzator => nu este cazul nostru

#4. Variabilitatea in x este pozitiva
var(sustainable_energy_2019$gdp_per_capita)
var(sustainable_energy_2019$access_to_electricity)
var(sustainable_energy_2019$renewable_energy_share)
var(sustainable_energy_2019$energy_intensity) # toate valorile > 0 => ipoteza acceptata

#5. Media reziduurilor este 0
mean(model$residuals) # medie aproape de 0 => ipoteza acceptata

#6. Multicoliniaritate
vif(model) # nu avem valori > 10 => ipoteza acceptata

#7. Reziduurile nu sunt corelate cu variabilele independente
cor.test(sustainable_energy_2019$gdp_per_capita, model$residuals)
cor.test(sustainable_energy_2019$access_to_electricity, model$residuals)
cor.test(sustainable_energy_2019$renewable_energy_share, model$residuals)
cor.test(sustainable_energy_2019$energy_intensity, model$residuals)
# p-value pentru fiecare > 0.1 => nu sunt corelate => ipoteza acceptata

#8. Reziduurile sunt homoscedastice
bptest(model) # heteroscedastice
white_test(model) # heteroscedastice
#ipoteza incalcata => corectie prin WLS
model_WLS <- lm(primary_energy ~ gdp_per_capita + access_to_electricity + renewable_energy_share + energy_intensity, data=sustainable_energy_2019,
                weights = 1/fitted(model)^2)
summary(model_WLS)
bptest(model_WLS) 
white_test(model_WLS)
# heteroscedasticitate in continuare => ipoteza incalcata => corectie prin forma functionala log - log
# vom logaritma doar acele variabile independente care sunt numerice, nu si cele care sunt reprezentate procentual
sustainable_energy_2019 <- sustainable_energy_2019 %>%
  mutate(
    lprimary_energy = ifelse(primary_energy == 0, 0, log(primary_energy)),
    lgdp_per_capita = ifelse(gdp_per_capita == 0, 0, log(gdp_per_capita)),
    lenergy_intensity = ifelse(energy_intensity == 0, 0, log(energy_intensity)),
  )
model_log <- lm(lprimary_energy ~ lgdp_per_capita + access_to_electricity + renewable_energy_share + lenergy_intensity, data=sustainable_energy_2019)
summary(model_log)
bptest(model_log)
white_test(model_log) # ambele teste au valori > 0.05 => homoscedasticitate => ipoteza acceptata

#9. Reziduurile nu sunt autocorelate
acf(model_log$residuals) # nu sunt autocorelate
dwtest(model_log) # p-value > 0.1 => reziduuri nonautocorelate
bgtest(model_log) # p-value > 0.1 => reziduuri nonautocorelate
#ipoteza acceptata

#10. Reziduurile sunt normal distribuite
jarque.bera.test(model_log$residuals) # p-value < 0.05 => reziduurile nu sunt normal distribuite
ols_plot_cooksd_bar(model_log) # eliminam valorile outlier
sustainable_energy_2019_cook <- sustainable_energy_2019[-c(135, 137, 152), ]
model_log2 <- lm(lprimary_energy ~ lgdp_per_capita + access_to_electricity + renewable_energy_share + lenergy_intensity, data=sustainable_energy_2019_cook)
jarque.bera.test(model_log2$residuals) # p-value < 0.05 => reziduurile nu sunt normal distribuite
ols_plot_cooksd_bar(model_log2) # eliminam valorile outlier din nou
sustainable_energy_2019_cook2 <- sustainable_energy_2019_cook[-c(1, 18, 19, 30, 67, 83, 84, 100, 111, 116, 121, 138, 142), ]
model_log3 <- lm(lprimary_energy ~ lgdp_per_capita + access_to_electricity + renewable_energy_share + lenergy_intensity, data=sustainable_energy_2019_cook2)
jarque.bera.test(model_log3$residuals) # p-value > 0.05 => reziduurile sunt normal distribuite
ols_test_normality(model_log3) # 3/4 teste au p-value > 0.05 => reziduurile sunt normal distribuite

#Vom imbunatati modelul adaugand variabile dummy si termeni de interactiune
summary(sustainable_energy_2019_cook2$gdp_per_capita)
#Vom crea variabila dummy over50_access_to_electricity, care va avea valoarea 1 daca procentul accesului la electricitate este peste 50% in acea tara
#si 0 daca procentul este sub 50.
#Vom folosi termen de interactiune intre variabila dummy creata anterior, over50_access_to_electricity, si gdp_per_capita
sustainable_energy_2019_cook2 <- sustainable_energy_2019_cook2 %>%
  mutate(over50percent_access_to_electricity = ifelse(access_to_electricity > 50, 1, 0))
model_optim <- lm(lprimary_energy ~ lgdp_per_capita + access_to_electricity + renewable_energy_share + lenergy_intensity
                                    + over50percent_access_to_electricity + over50percent_access_to_electricity * lgdp_per_capita, data=sustainable_energy_2019_cook2)
summary(model_log3)
#Modelul imbunatatit cu variabila dummy si termenul de interactiune dintre variabila dummy over50percent_access_to_electricity si gdp_per_capita:
summary(model_optim)
#Bonitatea: 95%
#Modelul este semnificativ statistic pentru 95%
#Variabilele independente explica 95% din variabilitatea consumului de energie
#Varibilele independente lgdp_per_capita, access_to_electricity, renewable_energy_share, lenergy_intensity sunt semnificative statistic
#pentru un nivel de semnificatie de 1%.
#variabila independenta dummy over50percent_access_to_electricity si termenul de interactiune dintre variabila dummy si PIB-ul/locuitor logaritmat este
#semnificativ statistic pentru un nivel de semnificatie de 5%.
#La o crestere a GDP/capita logaritmat cu 1 punct procentual, consumul de energie primara creste cu 1.0231%
#La o crestere a accesului la electricitate cu 1 procent, consumul de energie primara creste cu 0.009%
#La o crestere a energiei regenerabile cu 1 procent, consumul de energie primara scade cu 0.014%
#La o crestere a cantitatii de energie necesare cu 1 procent, consumul de energie primara creste cu 0.7%
#Daca in tara respectiva accesul la electricitate este peste 50% pentru populatia tarii, atunci consumul de energie primara creste cu 1.93%.
#Pentru tarile cu accesul la electricitate peste 50%, efectul PIB-ul/locuitor asupra consumului de energie scade cu 0.28%

#Prognoza pentru modelul imbunatatit
#Impartim setul de date in setul de antrenare(80% din observatii) si cel de testare(20% din observatii)
set.seed(123)
training.samples <- sustainable_energy_2019_cook2$primary_energy %>%
  createDataPartition(p = 0.8, list = FALSE)
train.data <- sustainable_energy_2019_cook2[training.samples, ]
test.data <- sustainable_energy_2019_cook2[-training.samples, ]

#Modelul pentru setul de antrenare
model_optim_train <- lm(lprimary_energy ~ lgdp_per_capita + access_to_electricity + renewable_energy_share + lenergy_intensity
                  + over50percent_access_to_electricity + over50percent_access_to_electricity * lgdp_per_capita, data=train.data)
summary(model_optim_train)

#Predictia modelului pe setul de testare
y_pred <- predict(model_optim_train, newdata = test.data)
y_pred

#RMSE - Root Mean Squared Error
RMSE(y_pred, test.data$lprimary_energy) # RMSE < 1 => predictie buna

#MAE - Mean Absolute Error
MAE(y_pred, test.data$lprimary_energy) # MAE < 1 => predictie buna

#MSE - Mean Squared Error
mse(y_pred, test.data$lprimary_energy) # MSE < 1 => predictie buna

#MAPE - Mean Absolute Percentage Error
MAPE(y_pred, test.data$lprimary_energy) # MAPE < 1 => predictie buna 

#Predictie out of sample
out_of_sample <- data.frame(gdp_per_capita = c(5000, 3500, 2000),
                            access_to_electricity = c(80, 70, 30),
                            renewable_energy_share = c(20, 15, 30),
                            energy_intensity = c(9, 7, 5),
                            over50percent_access_to_electricity = c(1, 1, 0),
                            over50percent_access_to_electricityXlgdp_per_capita = c(5000, 3500, 0))
#Logaritmam
out_of_sample_log <- out_of_sample %>%
  mutate(lgdp_per_capita = log(gdp_per_capita),
         lenergy_intensity = log(energy_intensity),
         over50percent_access_to_electricityXlgdp_per_capita = log(over50percent_access_to_electricityXlgdp_per_capita))

#Scoatem variabilele de care nu avem nevoie
out_of_sample_log <- out_of_sample_log %>% select(-gdp_per_capita, -energy_intensity)

#Prognoza
y_pred_outofsample <- predict(model_optim_train, newdata = out_of_sample_log)
y_pred_outofsample
exp(y_pred_outofsample)
y_pred_outsample_ci <- predict(model_optim_train, 
                               newdata = out_of_sample_log, 
                               se.fit=TRUE,
                               interval = "confidence")
y_pred_outsample_ci
exp(y_pred_outsample_ci$fit)
#summary(model_optim_train)
#Valoarea estimata a consumului de energie primara este de 18635.308 kWh/persoana pentru un PIB/locuitor de 5000 dolari, accesul la electricitate
#fiind de 80% si pentru o cantitatea de energie regenerabila de 20%.
#Valoarea estimata a consumului de energie primara se va incadra in intervalul de incredere [16253.875, 21365.656] dolari pentru un interval
#de incredere de 99%.

#Tehnici de regularizare (Lasso, Ridge, Elastic Net) + comparatii cu modelul optim identificat anterior

#Regresia Ridge
model <- lm(primary_energy ~ gdp_per_capita + access_to_electricity + renewable_energy_share + energy_intensity , data=sustainable_energy_2019)
summary(model)
prognoza <- data.frame(
  gdp_per_capita = c(5000),
  access_to_electricity = c(80),
  renewable_energy_share = c(15),
  energy_intensity = c(3)
)
y_pred_scenariu <- predict(model, newdata = prognoza)
y_pred_scenariu

#Definim variabila raspuns
y <- sustainable_energy_2019$primary_energy

#Definim predictorii
x <- data.matrix(sustainable_energy_2019[, c('gdp_per_capita', 'access_to_electricity', 'renewable_energy_share', 'energy_intensity')])

#Estimam modelul ridge
model0 <- glmnet(x, y, alpha = 0)
summary(model0)

#Valoarea lui lambda pentru MSE minimalizat (cross-validation)
cv_model <- cv.glmnet(x, y, alpha = 0)
best_lambda <- cv_model$lambda.min
best_lambda # 2293.32

plot(cv_model)

#Reimplementam modelul cu valoarea lambda optima
best_model <- glmnet(x, y, alpha = 0, lambda = best_lambda)
coef(best_model)

#Prognoze
y_predicted <- predict(best_model, s = best_lambda, newx = x)

new <- matrix(c(5000, 80, 15, 3), nrow=1, ncol = 4)
predict(best_model, s = best_lambda, newx = new)

#calculam R2
sst <- sum((y - mean(y))^2)
sse <- sum((y_predicted - y)^2)
rsq <- 1 - sse/sst
rsq # 63.15%

#Regresia LASSO
model0 <- glmnet(x, y, alpha = 1)
cv_model <- cv.glmnet(x, y, alpha = 1)
best_lambda <- cv_model$lambda.min
best_lambda # 94.76
plot(cv_model)

best_model <- glmnet(x, y, alpha = 1, lambda = best_lambda)
coef(best_model)

#Prognoze
y_predicted <- predict(best_model, s = best_lambda, newx = x)

new <- matrix(c(5000, 80, 15, 3), nrow=1, ncol = 4)
predict(best_model, s = best_lambda, newx = new)

#calculam R2
sst <- sum((y - mean(y))^2)
sse <- sum((y_predicted - y)^2)
rsq <- 1 - sse/sst
rsq # 63.43846%

#Elastic Net
model0 <- glmnet(x, y, alpha = 0.5)
cv_model <- cv.glmnet(x, y, alpha = 0.5)
best_lambda <- cv_model$lambda.min
best_lambda # 130.62
plot(cv_model)

best_model <- glmnet(x, y, alpha = 0.5, lambda = best_lambda)
coef(best_model)

#Prognoze
y_predicted <- predict(best_model, s = best_lambda, newx = x)

new <- matrix(c(5000, 80, 15, 3), nrow=1, ncol = 4)
predict(best_model, s = best_lambda, newx = new)

#calculam R2
sst <- sum((y - mean(y))^2)
sse <- sum((y_predicted - y)^2)
rsq <- 1 - sse/sst
rsq # 63.43%
#Conform tehnicilor de regularizare:
#Ridge - RSQ = 63.15%
#Lasso - RSQ = 63.43846%
#Elastic Net - RSQ = 63.43867%
# => Modelul cu cea mai mare bonitate este Elastic Net
# Prin comparatie cu modelul multifactorial
summary(model)
# Bonitatea pentru acest model este 62.51% => Modelul cel mai potrivit este modelul cu tehnica de regularizare Elastic Net

