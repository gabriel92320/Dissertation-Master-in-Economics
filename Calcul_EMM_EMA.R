
# Calcul des EMM (Effectifs moyens mensuels) et de l'EMA (effectif moyen annuel)

# Etape 1: connexion aux différents fichiers des EMM des ent. regroupés par secteur (A6) de leurs établissements,
# au format Parquet
# Source: calculs à partir des fichiers Postes et Entreprises de la Base tous Salariés (2020-2021-2022).
# voir le script précédent: Calcul_EMM_sect.R

EMM_ent_etabBE_20 <- open_dataset(paste0(sauvegarde_EMA,"EMM_ent_etabBE_20.parquet"))
EMM_ent_etabFZ_20 <- open_dataset(paste0(sauvegarde_EMA,"EMM_ent_etabFZ_20.parquet"))
EMM_ent_etabGI_20 <- open_dataset(paste0(sauvegarde_EMA,"EMM_ent_etabGI_20.parquet"))
EMM_ent_etabJU_hors7820Z_20 <- open_dataset(paste0(sauvegarde_EMA,"EMM_ent_etabJU_hors7820Z_20.parquet"))
EMM_ent_etabJU_7820Z_20 <- open_dataset(paste0(sauvegarde_EMA,"EMM_ent_etabJU_7820Z_20.parquet"))

EMM_ent_etabBE_21 <- open_dataset(paste0(sauvegarde_EMA,"EMM_ent_etabBE_21.parquet"))
EMM_ent_etabFZ_21 <- open_dataset(paste0(sauvegarde_EMA,"EMM_ent_etabFZ_21.parquet"))
EMM_ent_etabGI_21 <- open_dataset(paste0(sauvegarde_EMA,"EMM_ent_etabGI_21.parquet"))
EMM_ent_etabJU_hors7820Z_21 <- open_dataset(paste0(sauvegarde_EMA,"EMM_ent_etabJU_hors7820Z_21.parquet"))
EMM_ent_etabJU_7820Z_21 <- open_dataset(paste0(sauvegarde_EMA,"EMM_ent_etabJU_7820Z_21.parquet"))

EMM_ent_etabBE_22 <- open_dataset(paste0(sauvegarde_EMA,"EMM_ent_etabBE_22.parquet"))
EMM_ent_etabFZ_22 <- open_dataset(paste0(sauvegarde_EMA,"EMM_ent_etabFZ_22.parquet"))
EMM_ent_etabGI_22 <- open_dataset(paste0(sauvegarde_EMA,"EMM_ent_etabGI_22.parquet"))
EMM_ent_etabJU_hors7820Z_22 <- open_dataset(paste0(sauvegarde_EMA,"EMM_ent_etabJU_hors7820Z_22.parquet"))
EMM_ent_etabJU_7820Z_22 <- open_dataset(paste0(sauvegarde_EMA,"EMM_ent_etabJU_7820Z_22.parquet"))


# Etape 2: restructuration du type "wide to long":

passe_wide_to_long<-function(data_wide,sect_A6){
  
  data_long <- data_wide %>%
    to_duckdb() %>%
    pivot_longer(cols = starts_with("EMM"),
                 names_to = "mois",
                 values_to = "EMM") %>%
    mutate(secteur=sect_A6,
           mois2=case_when(
             mois=="EMM_1" ~ "01",
             mois=="EMM_2" ~ "02",
             mois=="EMM_3" ~ "03",
             mois=="EMM_4" ~ "04",
             mois=="EMM_5" ~ "05",
             mois=="EMM_6" ~ "06",
             mois=="EMM_7" ~ "07",
             mois=="EMM_8" ~ "08",
             mois=="EMM_9" ~ "09",
             mois=="EMM_10" ~ "10",
             mois=="EMM_11" ~ "11",
             mois=="EMM_12" ~ "12"
           )) %>%
    select(-mois) %>%
    rename(mois=mois2) %>%
    to_arrow() %>% compute()
  
  return(data_long) 
}

# 2020:
EMM_ent_etabBE_20<-passe_wide_to_long(data_wide=EMM_ent_etabBE_20,sect_A6="BE")
#11 640 768 rows x 4 columns
EMM_ent_etabFZ_20<-passe_wide_to_long(data_wide=EMM_ent_etabFZ_20,sect_A6="FZ")
#11 640 768 rows x 4 columns
EMM_ent_etabGI_20<-passe_wide_to_long(data_wide=EMM_ent_etabGI_20,sect_A6="GI")
#11 640 768 rows x 4 columns
EMM_ent_etabJU_hors7820Z_20<-passe_wide_to_long(data_wide=EMM_ent_etabJU_hors7820Z_20,sect_A6="JU")
#11 640 768 rows x 4 columns
EMM_ent_etabJU_7820Z_20<-passe_wide_to_long(data_wide=EMM_ent_etabJU_7820Z_20,sect_A6="7820Z")
#11 640 768 rows x 4 columns

# 2021:
EMM_ent_etabBE_21<-passe_wide_to_long(data_wide=EMM_ent_etabBE_21,sect_A6="BE")
#11 640 768 rows x 4 columns
EMM_ent_etabFZ_21<-passe_wide_to_long(data_wide=EMM_ent_etabFZ_21,sect_A6="FZ")
#11 640 768 rows x 4 columns
EMM_ent_etabGI_21<-passe_wide_to_long(data_wide=EMM_ent_etabGI_21,sect_A6="GI")
#11 640 768 rows x 4 columns
EMM_ent_etabJU_hors7820Z_21<-passe_wide_to_long(data_wide=EMM_ent_etabJU_hors7820Z_21,sect_A6="JU")
#11 640 768 rows x 4 columns
EMM_ent_etabJU_7820Z_21<-passe_wide_to_long(data_wide=EMM_ent_etabJU_7820Z_21,sect_A6="7820Z")
#11 640 768 rows x 4 columns

#extrait2<-EMM_ent_etabBE_21 %>% slice_head(n=100) %>% collect()

# 2022:
EMM_ent_etabBE_22<-passe_wide_to_long(data_wide=EMM_ent_etabBE_22,sect_A6="BE")
#11 640 768 rows x 4 columns
EMM_ent_etabFZ_22<-passe_wide_to_long(data_wide=EMM_ent_etabFZ_22,sect_A6="FZ")
#11 640 768 rows x 4 columns
EMM_ent_etabGI_22<-passe_wide_to_long(data_wide=EMM_ent_etabGI_22,sect_A6="GI")
#11 640 768 rows x 4 columns
EMM_ent_etabJU_hors7820Z_22<-passe_wide_to_long(data_wide=EMM_ent_etabJU_hors7820Z_22,sect_A6="JU")
#11 640 768 rows x 4 columns
EMM_ent_etabJU_7820Z_22<-passe_wide_to_long(data_wide=EMM_ent_etabJU_7820Z_22,sect_A6="7820Z")
#11 640 768 rows x 4 columns


# Etape 3: Concaténation verticale des bases sectorielles au format long:

EMM_ent_sectetab_20 <- EMM_ent_etabBE_20 %>%
  union_all(EMM_ent_etabFZ_20) %>%
  union_all(EMM_ent_etabGI_20) %>% 
  union_all(EMM_ent_etabJU_hors7820Z_20) %>%
  union_all(EMM_ent_etabJU_7820Z_20) %>%
  arrange(secteur,mois) %>% compute()
# 58 203 840 rows !
# sauvegarde:
write_parquet(EMM_ent_sectetab_20,paste0(sauvegarde_EMA,"EMM_ent_sectetab_20.parquet"))

EMM_ent_sectetab_21 <- EMM_ent_etabBE_21 %>%
  union_all(EMM_ent_etabFZ_21) %>%
  union_all(EMM_ent_etabGI_21) %>% 
  union_all(EMM_ent_etabJU_hors7820Z_21) %>%
  union_all(EMM_ent_etabJU_7820Z_21) %>%
  arrange(secteur,mois) %>% compute()
# 58 203 840 rows !
# sauvegarde:
write_parquet(EMM_ent_sectetab_21,paste0(sauvegarde_EMA,"EMM_ent_sectetab_21.parquet"))


EMM_ent_sectetab_22 <- EMM_ent_etabBE_22 %>%
  union_all(EMM_ent_etabFZ_22) %>%
  union_all(EMM_ent_etabGI_22) %>% 
  union_all(EMM_ent_etabJU_hors7820Z_22) %>%
  union_all(EMM_ent_etabJU_7820Z_22) %>%
  arrange(secteur,mois) %>% compute()
# 58 203 840 rows !
# sauvegarde:
write_parquet(EMM_ent_sectetab_22,paste0(sauvegarde_EMA,"EMM_ent_sectetab_22.parquet"))


# Etape 4: Calcul de l'EMM de chaque entreprise (un EMM calculé pour chacun des 12 mois des années civiles 2020-2022):

calcul_EMM<-function(data_EMM_sectetab){
  
  EMM_ent <- data_EMM_sectetab %>%
    to_duckdb() %>%
    group_by(siren,mois) %>%
    summarise(EMM=sum(EMM,na.rm = T)) %>%
    to_arrow() %>% compute()
  
  return(EMM_ent) 
  
}

# connexion aux bases contenant les EMM des entreprises calculées par secteur de leurs établissements:
EMM_ent_sectetab_20 <- open_dataset(paste0(sauvegarde_EMA,"EMM_ent_sectetab_20.parquet"))
EMM_ent_sectetab_21 <- open_dataset(paste0(sauvegarde_EMA,"EMM_ent_sectetab_21.parquet"))
EMM_ent_sectetab_22 <- open_dataset(paste0(sauvegarde_EMA,"EMM_ent_sectetab_22.parquet"))

EMM_ent_20<-calcul_EMM(data_EMM_sectetab=EMM_ent_sectetab_20)
# sauvegarde:
write_parquet(EMM_ent_20,paste0(sauvegarde_EMA,"EMM_ent_20.parquet"))

EMM_ent_21<-calcul_EMM(data_EMM_sectetab=EMM_ent_sectetab_21)
# sauvegarde:
write_parquet(EMM_ent_21,paste0(sauvegarde_EMA,"EMM_ent_21.parquet"))

EMM_ent_22<-calcul_EMM(data_EMM_sectetab=EMM_ent_sectetab_22)
# sauvegarde:
write_parquet(EMM_ent_22,paste0(sauvegarde_EMA,"EMM_ent_22.parquet"))


# Etape 5: calcul de l'EMA de chaque entreprise:
# Moyenne arithmétique des EMM sur les 12 mois de l'année (si les EMM sont bien renseignés sur les 12 mois)
# sinon on ne compte que les mois pour lesquels un EMM est bien calculé (cas des entreprises avec une activité
# très saisonnière, création ou cessation d'entreprises...)

calcul_EMA<-function(data_EMM_ent,annee){
  
  EMA_ent <- data_EMM_ent %>%
    to_duckdb() %>%
    group_by(siren) %>%
    summarise(EMA=mean(EMM,na.rm = T)) %>%
    rename(!!paste("EMA",as.character(annee),sep="_"):= EMA) %>% 
    to_arrow() %>% compute()
  
  return(EMA_ent) 
  
}

# connexion aux bases contanant les EMM des entreprises:
EMM_ent_20 <- open_dataset("C:/Users/POLEMOB_G_SKLENAR/Documents/M?moire MiE/Data/Calculs_EMA/EMM_ent_20.parquet")
EMM_ent_21 <- open_dataset("C:/Users/POLEMOB_G_SKLENAR/Documents/M?moire MiE/Data/Calculs_EMA/EMM_ent_21.parquet")
EMM_ent_22 <- open_dataset("C:/Users/POLEMOB_G_SKLENAR/Documents/M?moire MiE/Data/Calculs_EMA/EMM_ent_22.parquet")

EMA_ent_20<-calcul_EMA(data_EMM_ent=EMM_ent_20,annee=2020)
# sauvegarde:
write_parquet(EMA_ent_20,paste0(sauvegarde_EMA,"EMA_ent_20.parquet"))

EMA_ent_21<-calcul_EMA(data_EMM_ent=EMM_ent_21,annee=2021)
# sauvegarde:
write_parquet(EMA_ent_21,paste0(sauvegarde_EMA,"EMA_ent_21.parquet"))

EMA_ent_22<-calcul_EMA(data_EMM_ent=EMM_ent_22,annee=2022)
# sauvegarde:
write_parquet(EMA_ent_22,paste0(sauvegarde_EMA,"EMA_ent_22.parquet"))

# Calcul de l'EMA sur la première période d'observation du bonus-malus: 07/2021 - 06/2022

# Sélection du S2 2021:
EMM_ent_S221 <- EMM_ent_21 %>% filter(mois %in% c("07","08","09","10","11","12"))
# Sélection du S1 2022:
EMM_ent_S122 <- EMM_ent_22 %>% filter(mois %in% c("01","02","03","04","05","06"))
# Concaténation verticale des deux tables:
EMM_ent_S221_S122 <- EMM_ent_S221 %>%
  union_all(EMM_ent_S122) %>%
  arrange(siren,mois) %>% compute()

#extrait_12<-EMM_ent_S221_S122 %>% slice_head(n=1000) %>% collect()
EMA_ent_S221_S122<-calcul_EMA(data_EMM_ent=EMM_ent_S221_S122,annee=2122)

# sauvegarde:
write_parquet(EMA_ent_S221_S122,paste0(sauvegarde_EMA,"EMA_ent_S221_S122.parquet"))



# Etape 6: calcul de l'EMA restreint aux intérimaires de chaque entreprise:
# Moyenne arithmétique des EMM "interim" sur les 12 mois de l'année (si les EMM sont bien renseignés sur les 12 mois)
# sinon on ne compte que les mois pour lesquels un EMM est bien calcul? (cas des entreprises avec une activité
# très saisonnière, création ou cessation d'entreprises...)

calcul_EMA_interim<-function(data_EMM_ent,annee){
  
  EMA_interim_ent <- data_EMM_ent %>%
    to_duckdb() %>%
    group_by(siren) %>%
    summarise(EMA=round(mean(EMM,na.rm = T),2)) %>%
    rename(!!paste("EMA_interim",as.character(annee),sep="_"):= EMA) %>% 
    to_arrow() %>% compute()
  
  return(EMA_interim_ent) 
  
}

# connexion aux bases contanant les EMM des entreprises calculés par secteur de leurs établissements:
EMM_ent_sectetab_20 <- open_dataset(paste0(sauvegarde_EMA,"EMM_ent_sectetab_20.parquet"))
EMM_ent_sectetab_21 <- open_dataset(paste0(sauvegarde_EMA,"EMM_ent_sectetab_21.parquet"))
EMM_ent_sectetab_22 <- open_dataset(paste0(sauvegarde_EMA,"EMM_ent_sectetab_22.parquet"))

# Restriction du champ au secteur 7820Z (agences d'emploi temporaire):

EMM_ent_7820Z_20 <- EMM_ent_sectetab_20 %>% filter(secteur=="7820Z")
EMM_ent_7820Z_21 <- EMM_ent_sectetab_21 %>% filter(secteur=="7820Z")
EMM_ent_7820Z_22 <- EMM_ent_sectetab_22 %>% filter(secteur=="7820Z")

EMA_interim_ent_20 <- calcul_EMA_interim(data_EMM_ent=EMM_ent_7820Z_20,annee=2020)
EMA_interim_ent_21 <- calcul_EMA_interim(data_EMM_ent=EMM_ent_7820Z_21,annee=2021)
EMA_interim_ent_22 <- calcul_EMA_interim(data_EMM_ent=EMM_ent_7820Z_22,annee=2022)

# Calcul de l'EMA sur la première période d'observation du bonus-malus: 07/2021 - 06/2022

# Sélection du S2 2021:
EMM_ent_7820Z_S221 <- EMM_ent_7820Z_21 %>% filter(mois %in% c("07","08","09","10","11","12"))
# Sélection du S1 2022:
EMM_ent_7820Z_S122 <- EMM_ent_7820Z_22 %>% filter(mois %in% c("01","02","03","04","05","06"))
# Concaténation verticale des deux tables:
EMM_ent_7820Z_S221_S122 <- EMM_ent_7820Z_S221 %>%
  union_all(EMM_ent_7820Z_S122) %>%
  arrange(siren,mois) %>% compute()

EMA_interim_ent_S221_S122<-calcul_EMA_interim(data_EMM_ent=EMM_ent_7820Z_S221_S122,annee=2122)

# Regroupement des EMA par siren sur les 3 années:

# Connexion à la base contenant la liste des entreprises du champ de l'étude:
liste_siren_etude <- open_dataset(paste0(sauvegarde_EMA,"liste_siren_etude.parquet"))

# AVant le merge, on ne garde que les entreprises qui sont employeuses chaque année (EMA>0 sur chaque période.)

EMA_ent_20_22 <- liste_siren_etude %>%
  inner_join(EMA_ent_20 %>% filter(EMA_2020>0)) %>%
  inner_join(EMA_ent_21 %>% filter(EMA_2021>0)) %>%
  inner_join(EMA_ent_22 %>% filter(EMA_2022>0)) %>%
  inner_join(EMA_ent_S221_S122 %>% filter(EMA_2122>0)) %>%
  left_join(EMA_interim_ent_20) %>%
  left_join(EMA_interim_ent_21) %>%
  left_join(EMA_interim_ent_22) %>%
  left_join(EMA_interim_ent_S221_S122) %>% compute()

# sauvegarde:
write_parquet(EMA_ent_20_22,paste0(sauvegarde_EMA,"EMA_ent_20_22.parquet"))

