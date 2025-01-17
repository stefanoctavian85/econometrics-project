#Aplicatia 2. Model cu date de tip panel

# Instalarea pachetelor
# Instalare si activare pachete
PackageNames <- c("tidyverse","gplots","plm","readxl","foreign","lmtest")
for(i in PackageNames){
  if(!require(i, character.only = T)){
    install.packages(i, dependencies = T)
    require(i, character.only = T)
  }
}

library("tidyverse")
library("gplots")
library("plm")
library("readxl")
library("foreign")
library("lmtest")

rm(list = ls())

sustainable_energy <- read.csv("C:/Aplicatii/Facultate/Programe/Econometrie/Proiect/global-data-on-sustainable-energy.csv")

#Functie pentru inlocuirea valorilor lipsa
handle_missing_values <- function(df) {
  df <- df %>% group_by(Entity) %>% 
    mutate(across(where(is.numeric), ~ ifelse(is.na(.), mean(., na.rm = TRUE), .))) %>% 
    ungroup()
  
  return(df)
}

#Analiza evolutiei emisiilor de carbon pentru tarile din toata lumea in perioada 2010-2020 in functie de electricitatea bazata pe combustibilul fosil,
#procentul de energie regenerabila din totalul energiei consumate, consumul energiei primare per cap de locuitor, dar si nivelul de intensitate
#al energiei primare

#Filtrarea datelor
#Selectez doar datele din anii 2010-2020
sustainable_energy_cleaned <- sustainable_energy %>% filter(Year >= 2010 & Year <= 2020)

#Selectez doar coloanele de care voi avea nevoie in model
sustainable_energy_cleaned <- sustainable_energy_cleaned %>% select("Entity", "Year", "Value_co2_emissions_kt_by_country",
                                                                    "Renewable.energy.share.in.the.total.final.energy.consumption....",
                                                                    "Electricity.from.fossil.fuels..TWh.", "Primary.energy.consumption.per.capita..kWh.person.",
                                                                    "Energy.intensity.level.of.primary.energy..MJ..2017.PPP.GDP.")

#Redenumesc coloanele
sustainable_energy_cleaned <- sustainable_energy_cleaned %>%
  rename(
    renewable_energy_share = `Renewable.energy.share.in.the.total.final.energy.consumption....`,
    electricity_from_fossil_fuels = `Electricity.from.fossil.fuels..TWh.`,
    primary_energy_consumption = `Primary.energy.consumption.per.capita..kWh.person.`,
    energy_intensity = `Energy.intensity.level.of.primary.energy..MJ..2017.PPP.GDP.`,
    co2_emissions = `Value_co2_emissions_kt_by_country`
  )

#Aplicarea functiei de inlocuire a valorilor NA asupra setului de date
sustainable_energy_cleaned <- handle_missing_values(sustainable_energy_cleaned)

#Stergem toate observatiile care au cel putin o valoare NA
sustainable_energy_cleaned <- sustainable_energy_cleaned %>% drop_na()

summary(sustainable_energy_cleaned)

#Dataframe-ul pentru setul de date
pdata <- pdata.frame(sustainable_energy_cleaned, index = c("Entity", "Year"), drop.index = TRUE)

#Corelatia dintre emisiile de CO2/spatiu/timp
dev.off()
coplot(co2_emissions ~ Year|Entity, type="l", data=sustainable_energy_cleaned)

#Heterogeneitatea in sectiunea transversala
plotmeans(co2_emissions ~ Entity, main = 'Heterogeneitate in randul tarilor', data = sustainable_energy_cleaned)
#Exista tari cu rata foarte mare si tari cu rata foarte mica => heterogeneitate transversala

#Heterogeneitatea in sectiunea temporala
plotmeans(co2_emissions ~ Year, main = 'Heterogeneitate in timp', data = sustainable_energy_cleaned)
#Ani cu rata foarte mare si ani cu rata mica => heterogeneitate temporala, dar mai mica decat in cazul heterogeneitatii transversale

#Modelul OLS - model clasic de regresie liniara
ols <- lm(co2_emissions ~ renewable_energy_share + electricity_from_fossil_fuels + primary_energy_consumption + energy_intensity,
          sustainable_energy_cleaned)
summary(ols)
yhat <- ols$fitted

#Modelul FE(cu efecte fixe)
fe <- plm(co2_emissions ~ renewable_energy_share + electricity_from_fossil_fuels + primary_energy_consumption + energy_intensity,
          pdata, model="within")
summary(fe)
#Observam ca doar renewable_energy_share si electricity_from_fossil_fuels sunt semnificative, vom estima modelul din nou
#luand in considerare doar aceste 2 variabile
fe <- plm(co2_emissions ~ renewable_energy_share + electricity_from_fossil_fuels, pdata, model="within")
summary(fe)

#Cele 2 variabile renewable_energy_share si electricity_from_fossil_fuels sunt semnificative pentru un nivel de semnificatie
#de 95%

#Alegerea celei mai adecvate variante de model 
#OLS vs FE panel model
# H0: OLS
# H1: FE
pFtest(fe, ols) # p-value < 0.05 => se recomanda model de panel data FE

#Model cu efecte aleatorii RE(random effects)
re <- plm(co2_emissions ~ renewable_energy_share + electricity_from_fossil_fuels, pdata, model="between")
summary(re)
#Observam ca doar electricity_from_fossil_fuels este semnificativa statistic, vom estima modelul din nou tinand cont doar de
#aceasta variabila
re <- plm(co2_emissions ~ electricity_from_fossil_fuels, pdata, model="between")
summary(re)

# Testul Hausmann il aplicam pentru a decide intre FE si RE
# H0: model cu efecte random
# H1: model cu efecte fixe
phtest(fe, re) # p-value < 0.05 => se recomanda modelul FE

# Testare ipoteze
# In urma aplicarii testului Hausmann => aplicam modelul FE

#Testarea efectelor fixe in timp
fixed.time <- plm(co2_emissions ~ renewable_energy_share + electricity_from_fossil_fuels + factor(Year), sustainable_energy_cleaned,
                  index = c("Entity", "Year"), model="within")

#H0: nu sunt necesare efectele fixe in timp
#H1: sunt necesare efectele fixe in timp

pFtest(fixed.time, fe) #p-value > 0.05 => nu sunt necesare efectele fixe in timp
plmtest(fe, c("time"), type = "bp") #p-value > 0.05 => nu sunt necesare efectele fixe in timp

#Testarea efectelor aleatorii cu Breusch-Pagam Lagrange Multiplier
pool <- plm(co2_emissions ~ electricity_from_fossil_fuels, pdata, model="pooling")
summary(pool)

# H0: variatiile in timp sunt 0
# H1: variatiile in timp sunt diferite de 0
plmtest(pool, type = c("bp")) # p-value < 0.05 => respingem ipoteza nula
# variatiile in timp sunt diferite de 0 => efectele aleatorii sunt adecvate astfel incat exista diferente semnificative intre tari

#Testarea dependentei transversale folosind testul Breusch-Pagan LM si testul Parasan CD

#Ipoteze
# H1: reziduurile intre entitati nu sunt corelate
# H1: reziduurile intre entitati sunt corelate
pcdtest(fe, test = "lm") # p-value < 0.05 => dependenta transversala
pcdtest(fe, test = "cd")  # p-value > 0.05 => nu avem dependenta transversala
#nu corectam deoarece avem un panel cu perioade de timp < 40

#Testarea autocorelarii - Breusch-Godfrey/Wooldridge test
#Avem setul de date pe o perioada de 10 ani

# H0: nu exista autocorelare
# H1: exista autocorelare
pbgtest(fe) # p-value < 0.05 => autocorelare prezenta, insa setul de date este pe o perioada mica, asa ca o vom ignora

#Testarea heteroscedasticitatii - testul Breusch-Pagan
# H0: homoscedasticitate
# H1: heteroscedasticitate
bptest(co2_emissions ~ renewable_energy_share + electricity_from_fossil_fuels + factor(Entity), data = sustainable_energy_cleaned,
       studentize = F) # p-value < 0.05 => heteroscedasticitate => nu vom corecta

#Testarea efectelor random
pFtest(re, ols) # p-value < 0.05 => se recomanda efecte random
plmtest(re, c("time"), type = "bp") # p-value > 0.05 => nu se recomanda efecte random
#Cele 2 teste ofera raspunsuri contradictorii, asa ca vom testa totusi ipotezele
pbgtest(re) # p-value > 0.05 => nu exista autocorelare
bptest(co2_emissions ~ electricity_from_fossil_fuels + factor(Year), data = sustainable_energy_cleaned, studentize = F)
# p-value < 0.05 => heteroscedasticitate
