
# Ce script calcule les EMM des entreprises ventilés selon le secteur de leurs établisements respectifs.

# Etape 1: connexion aux fichiers Entreprises au format Parquet:
# Source: Base Tous Salariés (BTS) des millésimes 2020, 2021 et 2022.

ent_BTS_20 <- open_dataset("//casd.fr/casdfs/Projets/POLEMOB/Data/DADS_DADS Entreprises_2021/ent_2020-0.parquet")
ent_BTS_21 <- open_dataset("//casd.fr/casdfs/Projets/POLEMOB/Data/DADS_DADS Entreprises_2022/ent_2021-0.parquet")
ent_BTS_22 <- open_dataset("//casd.fr/casdfs/Projets/POLEMOB/Data/DADS_DADS Entreprises_2022/ent_2022-0.parquet")


# Etape 2: Sélection des entreprises (Siren) du champ de l'étude:
# entreprises présentes dans les fichiers Entreprises de BTS pour les années 2020, 2021 et 2022.
# restriction aux champs sectoriels BE, FZ, GI et JU + entreprises employant au moins 1 ETP durant
# chaque année (eff_eqtp>=1).


ent_BTS_20 <- ent_BTS_20 %>% 
  filter(a6 %in% c("BE","FZ","GI","JU") & eff_eqtp>=1) %>%
  select(siren)

ent_BTS_21 <- ent_BTS_21 %>% 
  filter(a6 %in% c("BE","FZ","GI","JU") & eff_eqtp>=1) %>%
  select(siren)

ent_BTS_22 <- ent_BTS_22 %>% 
  filter(a6 %in% c("BE","FZ","GI","JU") & eff_eqtp>=1) %>%
  select(siren)

liste_siren_etude <- ent_BTS_20 %>% 
  left_join(ent_BTS_21) %>%
  left_join(ent_BTS_22) %>% compute()
# 970 064 entreprises (siren) sont retenues pour l'étude.

# sauvegarde:
write_parquet(liste_siren_etude,paste0(sauvegarde_EMA,"liste_siren_etude.parquet"))


# Etape 3: connexion aux différents fichiers Postes (découpage sectoriel au niveau A6) au format Parquet
# Source: Base Tous Salariés (BTS) des millésimes 2020, 2021 et 2022.

postes_BE_BTS_20 <- open_dataset("//casd.fr/casdfs/Projets/POLEMOB/Data/DADS_DADS Postes_2021/Format parquet/post_2020_9_BE_0.parquet")
postes_FZ_BTS_20 <- open_dataset("//casd.fr/casdfs/Projets/POLEMOB/Data/DADS_DADS Postes_2021/Format parquet/post_2020_9_FZ_0.parquet")
postes_GI_BTS_20 <- open_dataset("//casd.fr/casdfs/Projets/POLEMOB/Data/DADS_DADS Postes_2021/Format parquet/post_2020_9_GI_0.parquet")
postes_JU_BTS_20 <- open_dataset("//casd.fr/casdfs/Projets/POLEMOB/Data/DADS_DADS Postes_2021/Format parquet/post_2020_9_JU_0.parquet")

postes_BE_BTS_21 <- open_dataset("//casd.fr/casdfs/Projets/POLEMOB/Data/DADS_DADS Postes_2022/post_2021_9_BE_0.parquet")
postes_FZ_BTS_21 <- open_dataset("//casd.fr/casdfs/Projets/POLEMOB/Data/DADS_DADS Postes_2022/post_2021_9_FZ_0.parquet")
postes_GI_BTS_21 <- open_dataset("//casd.fr/casdfs/Projets/POLEMOB/Data/DADS_DADS Postes_2022/post_2021_9_GI_0.parquet")
postes_JU_BTS_21 <- open_dataset("//casd.fr/casdfs/Projets/POLEMOB/Data/DADS_DADS Postes_2022/post_2021_9_JU_0.parquet")

postes_BE_BTS_22 <- open_dataset("//casd.fr/casdfs/Projets/POLEMOB/Data/DADS_DADS Postes_2022/post_2022_9_BE_0.parquet")
postes_FZ_BTS_22 <- open_dataset("//casd.fr/casdfs/Projets/POLEMOB/Data/DADS_DADS Postes_2022/post_2022_9_FZ_0.parquet")
postes_GI_BTS_22 <- open_dataset("//casd.fr/casdfs/Projets/POLEMOB/Data/DADS_DADS Postes_2022/post_2022_9_GI_0.parquet")
postes_JU_BTS_22 <- open_dataset("//casd.fr/casdfs/Projets/POLEMOB/Data/DADS_DADS Postes_2022/post_2022_9_JU_0.parquet")

# Etape 4:  Calcul de l'EMM par entreprise (selon le secteur de ses établissements, y compris intérimaires employés
# par les agences de travail temporaire (7820Z))


calcul_EMM_entreprise<-function(data_postes,mois){
  
  
  #  Exclusion des postes à ne pas comptabiliser lors du calcul des EMM (loi)
  # - contrat d'apprentissage (modalités 64,65,81 de la variable dispol)
  # - contrat de professionalisation (modalité 61 de la variable dispol)
  # - CUI-CIE (modalités 21 et 41 de la variable dispol)
  # - contrat d'accompagnement (modalité 41 de la variable dispol)
  # - CDD en remplacement d'un salarié absent (?)
  # - Convention de stage (modalité 29 de la variable nat_contrat)
  # - Contrat de volontariat de service civique (modalité 89 de la variable nat_contrat)
  # - Contrat de soutien et d'aide par le travail (modalité 70 de la variable nat_contrat)
  # - Contrat de mission et CDI int?rimaire (modalités 03 et 08 de la variable nat_contrat) WARNING: sur service_public.fr ces contrats ne sont pas exclus!
  # - Ligne de service (modalité 93 de la variable nat_contrat)
  # - Mandat d'élu (modalité 81 de la variable nat_contrat)
  # - Mandat social (modalité 80 de la variable nat_contrat)
  # - Intérimaire en contrat d'apprentissage (?)
  # - Intérimaire en contrat de professionalisation (?)
  
  
  # Exclusion des postes ? ne pas prendre en compte dans le calcul des EMM:
  liste_postes_1<- data_postes %>%
    filter(!(dispol %in% c("21","41","61","64","65","81"))
           & !(nat_contrat %in% c("29","89","70","93","81","80"))) %>%
    select(siren,datdeb,datfin1,datdeb2,datfin,eqtp) 
  
  # Sélection des postes avec des périodes d'emploi couvrant au moins partiellement le mois concerné:
  liste_postes_2 <- liste_postes_1 %>%
    mutate(deb_mois=1+(mois-1)*30,
           fin_mois=deb_mois+29) %>%
    filter((datdeb<deb_mois & datfin1>=deb_mois) | (datdeb>=deb_mois & datdeb<=fin_mois) |
             (datdeb2<deb_mois & datfin>=deb_mois) | (datdeb2>=deb_mois & datdeb2<=fin_mois)) %>% compute()
  
  
  # Calcul des durées (en jours) des postes pour le mois concerné + prorata
  liste_postes_3<-liste_postes_2 %>%
    mutate(cas1=(datfin1<deb_mois & datdeb2<deb_mois & datfin>=deb_mois & datfin<=fin_mois),
           cas2=(datfin1<deb_mois & datdeb2<deb_mois & datfin>=deb_mois & datfin>fin_mois),
           cas3=(datfin1<deb_mois & datdeb2>=deb_mois & datdeb2<=fin_mois & datfin<=fin_mois),
           cas4=(datfin1<deb_mois & datdeb2>=deb_mois & datdeb2<=fin_mois & datfin>fin_mois),
           cas5=(datdeb<deb_mois & datfin1>=deb_mois & datfin1<=fin_mois & is.na(datdeb2)),
           cas6=(datdeb<deb_mois & datfin1>=deb_mois & datfin1<=fin_mois &
                   !is.na(datdeb2) & datdeb2<=fin_mois & datfin<=fin_mois),
           cas7=(datdeb<deb_mois & datfin1>=deb_mois & datfin1<=fin_mois &
                   !is.na(datdeb2) & datdeb2<=fin_mois & datfin>fin_mois),
           cas8=(datdeb<deb_mois & datfin1>=deb_mois & datfin1<=fin_mois &
                   !is.na(datdeb2) & datdeb2>fin_mois),
           cas9=(datdeb<deb_mois & datfin1>fin_mois),
           cas10=(datdeb>=deb_mois & datdeb<=fin_mois  & datfin1>fin_mois),
           cas11=(datdeb>=deb_mois & datdeb<=fin_mois & datfin1<=fin_mois & is.na(datdeb2)),
           cas12=(datdeb>=deb_mois & datdeb<=fin_mois & datfin1<=fin_mois &
                    !is.na(datdeb2) & datdeb2<=fin_mois & datfin<=fin_mois),      
           cas13=(datdeb>=deb_mois & datdeb<=fin_mois & datfin1<=fin_mois &
                    !is.na(datdeb2) & datdeb2<=fin_mois & datfin>fin_mois),
           cas14=(datdeb>=deb_mois & datdeb<=fin_mois & datfin1<=fin_mois &
                    !is.na(datdeb2) & datdeb2>fin_mois)
    ) %>%
    mutate(duree1=datfin-deb_mois+1,
           duree2=datfin-datdeb2+1,
           duree3=fin_mois-datdeb2+1,
           duree4=datfin1-deb_mois+1,
           duree5=datfin1-deb_mois+1+datfin-datdeb2+1,
           duree6=datfin1-deb_mois+1+fin_mois-datdeb2+1,
           duree7=fin_mois-datdeb+1,
           duree8=datfin1-datdeb+1,
           duree9=datfin1-datdeb+1+datfin-datdeb2+1,
           duree10=datfin1-datdeb+1+fin_mois-datdeb2+1
    ) %>% to_duckdb() %>%
    mutate(duree_poste_mois=case_when(
      cas1 ~ duree1,
      cas2 ~ 30,
      cas3 ~ duree2,
      cas4 ~ duree3,
      cas5 ~ duree4,
      cas6 ~ duree5,
      cas7 ~ duree6,
      cas8 ~ duree4,
      cas9 ~ 30,
      cas10 ~ duree7,
      cas11 ~ duree8,
      cas12 ~ duree9,
      cas13 ~ duree10,
      cas14 ~ duree8,
      .default = NA)) %>%
    mutate(poids=duree_poste_mois/30,
           eqtp_poids=poids*eqtp) %>%
    to_arrow() %>%
    compute()
  
  # Calcul de l'EMM de l'entreprise pour le mois donné:
  
  calcul_EMM_ent<-liste_postes_3 %>%
    group_by(siren) %>%
    summarise(EMM=sum(eqtp_poids,na.rm = T)) %>%
    rename(!!paste("EMM",as.character(mois),sep="_"):= EMM) %>% compute()
  
  return(calcul_EMM_ent)
  
}


# Calcul de l'EMM par entreprise (siren):
calcul_EMM_12mois<-function(data_postes,output){
  for (i in 1:12){
    EMM<- calcul_EMM_entreprise(data_postes=data_postes,mois=i)   
    output<-left_join(output,EMM) %>% compute()
  } 
  return(output)
}

# 2020:
# BE
EMM_ent_etabBE_20 <- calcul_EMM_12mois(data_postes=postes_BE_BTS_20,output=liste_siren_etude)
write_parquet(EMM_ent_etabBE_20,paste0(sauvegarde_EMA,"EMM_ent_etabBE_20.parquet"))
# FZ
EMM_ent_etabFZ_20 <- calcul_EMM_12mois(data_postes=postes_FZ_BTS_20,output=liste_siren_etude)
write_parquet(EMM_ent_etabFZ_20,paste0(sauvegarde_EMA,"EMM_ent_etabFZ_20.parquet"))
# GI
EMM_ent_etabGI_20 <- calcul_EMM_12mois(data_postes=postes_GI_BTS_20,output=liste_siren_etude)
write_parquet(EMM_ent_etabGI_20,paste0(sauvegarde_EMA,"EMM_ent_etabGI_20.parquet"))
# JU (hors 7820Z):
EMM_ent_etabJU_hors7820Z_20 <- calcul_EMM_12mois(data_postes=postes_JU_BTS_20 %>% filter(!(apet=="7820Z")),output=liste_siren_etude)
write_parquet(EMM_ent_etabJU_hors7820Z_20,paste0(sauvegarde_EMA,"EMM_ent_etabJU_hors7820Z_20.parquet"))
# 7820Z: on récupère les missions d'intérim
postes_JU_BTS_7820Z_20 <- postes_JU_BTS_20 %>% filter(apet=="7820Z" & !is.na(siret_utilisateur) & contrat_travail=="03") %>%
  mutate(siren=str_sub(siret_utilisateur,1L,9L))

EMM_ent_etabJU_7820Z_20 <- calcul_EMM_12mois(data_postes=postes_JU_BTS_7820Z_20,output=liste_siren_etude)
write_parquet(EMM_ent_etabJU_7820Z_20,paste0(sauvegarde_EMA,"EMM_ent_etabJU_7820Z_20.parquet"))

# 2021:
# BE
EMM_ent_etabBE_21 <- calcul_EMM_12mois(data_postes=postes_BE_BTS_21,output=liste_siren_etude)
write_parquet(EMM_ent_etabBE_21,paste0(sauvegarde_EMA,"EMM_ent_etabBE_21.parquet"))
# FZ
EMM_ent_etabFZ_21 <- calcul_EMM_12mois(data_postes=postes_FZ_BTS_21,output=liste_siren_etude)
write_parquet(EMM_ent_etabFZ_21,paste0(sauvegarde_EMA,"EMM_ent_etabFZ_21.parquet"))
# GI
EMM_ent_etabGI_21 <- calcul_EMM_12mois(data_postes=postes_GI_BTS_21,output=liste_siren_etude)
write_parquet(EMM_ent_etabGI_21,paste0(sauvegarde_EMA,"EMM_ent_etabGI_21.parquet"))
# JU (hors 7820Z):
EMM_ent_etabJU_hors7820Z_21 <- calcul_EMM_12mois(data_postes=postes_JU_BTS_21 %>% filter(!(apet=="7820Z")),output=liste_siren_etude)
write_parquet(EMM_ent_etabJU_hors7820Z_21,paste0(sauvegarde_EMA,"EMM_ent_etabJU_hors7820Z_21.parquet"))
# 7820Z: on r?cup?re les missions d'int?rim
postes_JU_BTS_7820Z_21 <- postes_JU_BTS_21 %>% filter(apet=="7820Z" & !is.na(siret_utilisateur) & contrat_travail=="03") %>%
  mutate(siren=str_sub(siret_utilisateur,1L,9L))

EMM_ent_etabJU_7820Z_21 <- calcul_EMM_12mois(data_postes=postes_JU_BTS_7820Z_21,output=liste_siren_etude)
write_parquet(EMM_ent_etabJU_7820Z_21,paste0(sauvegarde_EMA,"EMM_ent_etabJU_7820Z_21.parquet"))


# 2022:
# BE
EMM_ent_etabBE_22 <- calcul_EMM_12mois(data_postes=postes_BE_BTS_22,output=liste_siren_etude)
write_parquet(EMM_ent_etabBE_22,paste0(sauvegarde_EMA,"EMM_ent_etabBE_22.parquet"))
# FZ
EMM_ent_etabFZ_22 <- calcul_EMM_12mois(data_postes=postes_FZ_BTS_22,output=liste_siren_etude)
write_parquet(EMM_ent_etabFZ_22,paste0(sauvegarde_EMA,"EMM_ent_etabFZ_22.parquet"))
# GI
EMM_ent_etabGI_22 <- calcul_EMM_12mois(data_postes=postes_GI_BTS_22,output=liste_siren_etude)
write_parquet(EMM_ent_etabGI_22,paste0(sauvegarde_EMA,"EMM_ent_etabGI_22.parquet"))
# JU (hors 7820Z):
EMM_ent_etabJU_hors7820Z_22 <- calcul_EMM_12mois(data_postes=postes_JU_BTS_22 %>% filter(!(apet=="7820Z")),output=liste_siren_etude)
write_parquet(EMM_ent_etabJU_hors7820Z_22,paste0(sauvegarde_EMA,"EMM_ent_etabJU_hors7820Z_22.parquet"))
# 7820Z: on r?cup?re les missions d'int?rim
postes_JU_BTS_7820Z_22 <- postes_JU_BTS_22 %>% filter(apet=="7820Z" & !is.na(siret_utilisateur) & contrat_travail=="03") %>%
  mutate(siren=str_sub(siret_utilisateur,1L,9L))

EMM_ent_etabJU_7820Z_22 <- calcul_EMM_12mois(data_postes=postes_JU_BTS_7820Z_22,output=liste_siren_etude)
write_parquet(EMM_ent_etabJU_7820Z_22,paste0(sauvegarde_EMA,"EMM_ent_etabJU_7820Z_22.parquet"))


