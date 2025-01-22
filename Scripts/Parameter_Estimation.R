
Ikeda_2007_Respiration_Data_original = read.csv("Data/Ikeda_2007_Respiration_Data.csv")


length(which(Ikeda_2007_Respiration_Data_original[,4] == 2))
length(which(Ikeda_2007_Respiration_Data_original[,4] == 3))
length(which(Ikeda_2007_Respiration_Data_original[,4] == 1.5))

Ikeda_2007_Respiration_Data_2_C = Ikeda_2007_Respiration_Data_original[which(Ikeda_2007_Respiration_Data_original[,4] == 2),] #only use data measured at 2 C


Temp_Ikeda_K_all = 1/Ikeda_2007_Respiration_Data_original[,4]+273.15
Ikeda_Respiration_all = (Ikeda_2007_Respiration_Data_original[,5]*24)*10^(-6)*(22.4)*15.998*10^6 #convert to micrograms of O2
Ikeda_Mass_all = Ikeda_2007_Respiration_Data_original[,6]*1000
Ikeda_Respiration_all_mass_specific = Ikeda_Respiration_all/Ikeda_Mass_all

lm_for_determining_EM = lm(log(Ikeda_Respiration_all_mass_specific)~Temp_Ikeda_K_all)
plot(log(Ikeda_Respiration_all_mass_specific)~Temp_Ikeda_K_all)

E_M = -0.09891 *8.617e-5

Ikeda_2007_Respiration_2C = (Ikeda_2007_Respiration_Data_2_C[,5]*24)*10^(-6)*(22.4)*15.998*10^6 #convert to micrograms of O2
Ikeda_2007_Dry_Mass_2C = (Ikeda_2007_Respiration_Data_2_C[,6]*1000)
Ikeda_2007_Temp_2C = (Ikeda_2007_Respiration_Data_2_C[,4])

t = lm(log(Ikeda_Respiration_all)~log(Ikeda_Mass_all)) #All data, not just restricted to 2 C
summary(t)

t2 = lm(log(Ikeda_2007_Respiration_2C)~log(Ikeda_2007_Dry_Mass_2C)) #Respiration data at 2 C
summary(t2)
exp(2.45033)




Saiz_Calbert_2007_DF_original = read.csv("Data/Saiz_Calbert_2007_Calanus_Ingestion.csv")

Saiz_Calbert_2007_DF_10_15_C = Saiz_Calbert_2007_DF_original[which(Saiz_Calbert_2007_DF_original[,2] >= 10 & Saiz_Calbert_2007_DF_original[,2] <=15),]

Saiz_Calbert_2007_DF = Saiz_Calbert_2007_DF_original

mean(Saiz_Calbert_2007_DF[,2])

Dry_Weight = Saiz_Calbert_2007_DF[,3] #Leave in micrograms of carbon per liter. Conversion to zooplankton dryweight happens after ingestion in stage. structured model
Ingestion = Saiz_Calbert_2007_DF[,5]
Ingestion_per_unit_body_weight = Ingestion/Dry_Weight
Food_Concentration = Saiz_Calbert_2007_DF[,4]
Temp = Saiz_Calbert_2007_DF[,2]+273.15
Temp_Saiz_Calbert_2007_DF_10_15_C = Saiz_Calbert_2007_DF_10_15_C[,2]+273.15 
Dry_Weight_Thousands = Dry_Weight/1000



Dry_Weight_DF_10_15_C = Saiz_Calbert_2007_DF_10_15_C[,3] #Leave in micrograms of carbon per liter. Conversion to zooplankton dryweight happens after ingestion in stage. structured model
Ingestion_DF_10_15_C = Saiz_Calbert_2007_DF_10_15_C[,5] #Leave in micrograms of carbon per liter
Food_Concentration_10_15_C = Saiz_Calbert_2007_DF_10_15_C[,4] #Leave in micrograms of carbon per liter

mean(Saiz_Calbert_2007_DF_10_15_C[,2])

plot(log(Ingestion_DF_10_15_C) ~ log(Dry_Weight_DF_10_15_C))
plot((Ingestion_DF_10_15_C) ~ Temp_Saiz_Calbert_2007_DF_10_15_C)

lm_calanoid_copepods_log_Ingestion_Log_Dry_Weight_DF_10_15_C = lm(log(Ingestion_DF_10_15_C)~log(Dry_Weight_DF_10_15_C) + log(Food_Concentration_10_15_C))

lm_calanoid_copepods_log_Ingestion_Log_Dry_Weight_DF_10_15_C[["coefficients"]]




plot(Ingestion ~ Food_Concentration)
plot(Ingestion ~ Temp)
plot(log(Ingestion) ~ (Temp))


#"temperature was not transformed because the logarithm of a biological rate is usually regarded as being a linear function of temperature (Q10 concept)" - Saiz and Calbert_2007 
Ingestion_lm = lm(log(Ingestion)~log(Dry_Weight) + log(Food_Concentration) + (Temp) )

t0 = 285.65
Temp = t0
A_hat = .096
alpha = .75
E_I = .6
k = 8.617e-5
cI = 0
# 
model = nls(Ingestion_DF_10_15_C  ~ (A_hat*Dry_Weight_DF_10_15_C^alpha*Food_Concentration_10_15_C)/(1+(A_hat*Dry_Weight_DF_10_15_C^alpha*Food_Concentration_10_15_C)/17.74286), start = list(alpha = .6)
)
model = nls(Ingestion_DF_10_15_C  ~ (A_hat*Food_Concentration_10_15_C)/(1+(A_hat*Food_Concentration_10_15_C)/Imax), start = list(Imax = .6)
)

model = nls(Ingestion_DF_10_15_C  ~ (Imax*(A_hat*Food_Concentration_10_15_C)/(A_hat*Food_Concentration_10_15_C+Imax)), start = list(Imax = .6)
)
# 
# 
n = exp(E_I*(Temp-t0)/((k)*Temp*t0))*(A_hat*(Dry_Weight_DF_10_15_C)^(alpha+cI*(Temp-t0)))*Food_Concentration_10_15_C
#

model2 = nls(Ingestion_DF_10_15_C ~ (n/(1+(n/Imax))), start = list(Imax = .1) )

model2 = nls(Ingestion_DF_10_15_C ~ ((A_hat*(Dry_Weight_DF_10_15_C)^(alpha))/(1+((A_hat*(Dry_Weight_DF_10_15_C)^(alpha))/Imax))), start = list(Imax = 10, alpha = .6 ) )

#model2 = nls(Ingestion_DF_10_15_C ~ ((A_hat*(Dry_Weight_DF_10_15_C)^(alpha))/(1+((A_hat*(Dry_Weight_DF_10_15_C)^(alpha))/(omega1*Dry_Weight_DF_10_15_C^omega2)))), start = list(Imax = 10, alpha = .6, omega1 = 20, omega2 = .7) )



model4 = nls(Ingestion_DF_10_15_C ~ (n/(1+(n/(omega1*Dry_Weight_DF_10_15_C^omega2)))), start = list(omega1 = .1, omega2 = .6) )


model4 = nls(Ingestion_DF_10_15_C ~ (omega1*Dry_Weight_DF_10_15_C^omega2)*(n/(n+(omega1*Dry_Weight_DF_10_15_C^omega2))), start = list(omega1 = .1, omega2 = .6) )


model4 = nls(Ingestion_DF_10_15_C ~ (Imax*(n/(n+Imax))), start = list(Imax = 1) )

model4 = nls(Ingestion_DF_10_15_C ~ (Imax*(((A_hat*(Dry_Weight_DF_10_15_C)^(alpha+cI*(Temp-t0)))*Food_Concentration_10_15_C)/(((A_hat*(Dry_Weight_DF_10_15_C)^(alpha+cI*(Temp-t0)))*Food_Concentration_10_15_C)+Imax))), start = list(Imax = 1) )

#model4 = nls(Ingestion_DF_10_15_C ~ ((omega1*Dry_Weight_DF_10_15_C^omega2)*(((A_hat*(Dry_Weight_DF_10_15_C)^(alpha+cI*(Temp-t0)))*Food_Concentration_10_15_C)/(((A_hat*(Dry_Weight_DF_10_15_C)^(alpha+cI*(Temp-t0)))*Food_Concentration_10_15_C)+(omega1*Dry_Weight_DF_10_15_C^omega2)))), start = list(Imax = 1, alpha = .01,
#                                                                                                                                                                                                                                                                                                        omega1 = .01, omega2 = .01) )


plot(Ingestion_DF_10_15_C~Dry_Weight_DF_10_15_C)

coefficients = Ingestion_lm[["coefficients"]]


summary(Ingestion_lm)

ANOVA_sum_sq = anova(Ingestion_lm)[,"Sum Sq"]

ANOVA_sum_sq/ sum(ANOVA_sum_sq)

options(buildtools.check = function(action) TRUE )


#Varying Rmax


Saiz_Calbert_2007_max_ingestion_vs_size_15_C = read.csv("Data/Saiz_Calbet_2007_Max_ingestion_Vs_Size_15_C.csv")
#max ingestion rate in µg C per day
#body weight in µg C
#Food Concentration in % body weight

plot(Saiz_Calbert_2007_max_ingestion_vs_size_15_C[,"Max.Ingestion"]~Saiz_Calbert_2007_max_ingestion_vs_size_15_C[,"Body.Size"]) 

plot(log(Saiz_Calbert_2007_max_ingestion_vs_size_15_C[,"Max.Ingestion"])~log(Saiz_Calbert_2007_max_ingestion_vs_size_15_C[,"Body.Size"]))

max_ingest_15_C = (Saiz_Calbert_2007_max_ingestion_vs_size_15_C[,"Max.Ingestion"])
body_size_15_C = (Saiz_Calbert_2007_max_ingestion_vs_size_15_C[,"Body.Size"])
Food_15_C = (Saiz_Calbert_2007_max_ingestion_vs_size_15_C[,"Body.Size"]*(Saiz_Calbert_2007_max_ingestion_vs_size_15_C[,"Food.Concentration"]/100)) 


log_max_ingest_15_C = log(Saiz_Calbert_2007_max_ingestion_vs_size_15_C[,"Max.Ingestion"]) #Leave in micrograms of carbon per liter. Conversion to zooplankton dryweight happens after ingestion in stage. structured model
log_body_size_15_C = log(Saiz_Calbert_2007_max_ingestion_vs_size_15_C[,"Body.Size"]) #Leave in micrograms of carbon per liter. Conversion to zooplankton dryweight happens after ingestion in stage. structured model
log_Food_15_C = log(Saiz_Calbert_2007_max_ingestion_vs_size_15_C[,"Body.Size"]*(Saiz_Calbert_2007_max_ingestion_vs_size_15_C[,"Food.Concentration"]/100)) 

lm0 = lm(log_max_ingest_15_C~log_body_size_15_C+log_Food_15_C) #Unclear if saiz and calbert 2007 controlled for food concentration.  Methods section is very vague
summary(lm0)

mean(Saiz_Calbert_2007_max_ingestion_vs_size_15_C[,"Max.Ingestion"])

#model3 = nls(max_ingest_15_C ~ omega1*body_size_15_C^omega2, start = list(omega1 = .01, omega2 = .5) )
plot(max_ingest_15_C ~ body_size_15_C)

#Control for food concentration
exp(0.0007407)


lm1 = lm(log_max_ingest_15_C~log_body_size_15_C)
summary(lm1)

lm2 = lm(log_max_ingest_15_C~log_Food_15_C)
summary(lm2)
