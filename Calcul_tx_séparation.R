
# Calcul du taux de séparation par entreprise:


# Etape 0: connexion ? la base des EMA calcul?s pour les entreprises du champ de l'?tude:
EMA_ent_20_22 <- open_dataset(paste0(sauvegarde_EMA,"EMA_ent_20_22.parquet"))
# Transformation en tibble:
EMA_ent_20_22_tb <- EMA_ent_20_22 %>% as_tibble() %>% collect()

EMA_ent_20_22_tb %>% filter(is.na(EMA_2020) & is.na(EMA_2021) & is.na(EMA_2022) & is.na(EMA_2122))
# 140 866 entreprises présentent des EMA non calculés sur toutes les périodes!

summary(EMA_ent_20_22_tb)

# Etape 1: quelques nettoyages sur les EMA: exclusion de certaines entreprises

# filtrage pour ne garder que les entreprises qui pr?sentent un EMA>=1 chaque année:
# et on élimine les cas d'EMA négatifs pour l'intérim 2020...

EMA_ent_20_22_tb2 <- EMA_ent_20_22_tb %>% filter(EMA_2020>=1 & EMA_2021>=1 & EMA_2022>=1 & EMA_2122>=1 &
                                                   (EMA_interim_2020>0 | is.na(EMA_interim_2020)))
# 603 120 entreprises.
summary(EMA_ent_20_22_tb2)

# L'Urssaf arrondi les EMA calculés 2 chiffres après la virgule et on met à zéro les EMA_interim NA:
EMA_ent_20_22_tb2 <- EMA_ent_20_22_tb2 %>% mutate(
  EMA_2020=round(EMA_2020,2),
  EMA_2021=round(EMA_2021,2),
  EMA_2022=round(EMA_2022,2),
  EMA_2122=round(EMA_2122,2)
) %>%
  mutate(EMA_interim_2020=ifelse(is.na(EMA_interim_2020),0,EMA_interim_2020),
         EMA_interim_2021=ifelse(is.na(EMA_interim_2021),0,EMA_interim_2021),
         EMA_interim_2022=ifelse(is.na(EMA_interim_2022),0,EMA_interim_2022),
         EMA_interim_2122=ifelse(is.na(EMA_interim_2122),0,EMA_interim_2122))

summary(EMA_ent_20_22_tb2)

# Calcul du taux de recours ? l'interim: EMA_interim/EMA (en %)
EMA_ent_20_22_tb2 <- EMA_ent_20_22_tb2 %>%
  mutate(tx_recours_interim_20=ifelse(EMA_interim_2020<=EMA_2020,EMA_interim_2020/EMA_2020*100,100),
         tx_recours_interim_21=ifelse(EMA_interim_2021<=EMA_2021,EMA_interim_2021/EMA_2021*100,100),
         tx_recours_interim_22=ifelse(EMA_interim_2022<=EMA_2022,EMA_interim_2022/EMA_2022*100,100),
         tx_recours_interim_2122=ifelse(EMA_interim_2122<=EMA_2122,EMA_interim_2122/EMA_2122*100,100))
summary(EMA_ent_20_22_tb2)

# Etape 2: Enrichissement de la base EMA avec des variables du fichier Entreprises de BTS 2021:
ent_BTS_20 <- open_dataset("//casd.fr/casdfs/Projets/POLEMOB/Data/DADS_DADS Entreprises_2021/ent_2020-0.parquet")
ent_BTS_21 <- open_dataset("//casd.fr/casdfs/Projets/POLEMOB/Data/DADS_DADS Entreprises_2022/ent_2021-0.parquet")
ent_BTS_22 <- open_dataset("//casd.fr/casdfs/Projets/POLEMOB/Data/DADS_DADS Entreprises_2022/ent_2022-0.parquet")

EMA_ent_20_22_arrow <- EMA_ent_20_22_tb2 %>% as_arrow_table()

EMA_20_22_ent_21 <- ent_BTS_21 %>% select(siren,a38,a88,apen,catjur,dat_crea,s_brut,s_net,treffen,tr_ca,eff_eqtp
) %>%
  rename(s_brut_21=s_brut,s_net_21=s_net,treffen_21=treffen,tr_ca_21=tr_ca,eff_eqtp_21=eff_eqtp) %>%
  inner_join(ent_BTS_20 %>% select(siren,s_brut_20=s_brut,s_net_20=s_net,eff_eqtp_20=eff_eqtp)) %>%
  inner_join(ent_BTS_22 %>% select(siren,s_brut_22=s_brut,s_net_22=s_net,eff_eqtp_22=eff_eqtp)) %>%
  inner_join(EMA_ent_20_22_arrow) %>% compute()
# 603 093 entreprises.

#extrait <-EMA_20_22_ent_21 %>% slice_head(n=100) %>% collect()

# Etape 3: Enrichissement de la base EMA avec des variables du fichier FARE 2021 + nombre de séparations
# sur la première période d'observation du bonus_malus -> juillet 2021 - juin 2022 
# (calculs conduits à partir des fichiers MMO et DE du dispositif ForCE, cf. programme SAS: Carac_ul_Fare_2021.sas)
# Champ retenu ici: entreprises (siren) ayant un effectif 2021 (redi_e200) >= 1 ETP.
ent_FARE21_nb_sep_21S2_22S1 <-haven::read_sas(paste0(sauvegarde_table_SAS,"carac_ul_fare_21.sas7bdat"))

ent_FARE21_nb_sep_21S2_22S1 <- ent_FARE21_nb_sep_21S2_22S1 %>% as_arrow_table()

base_etude_ent_BM <- EMA_20_22_ent_21 %>%
  inner_join(ent_FARE21_nb_sep_21S2_22S1,by="siren") %>%
  rename(nb_sep_2122=nb_separations) %>% as_tibble()
# 584 457 entreprises retenues in fine.

summary(base_etude_ent_BM)
#extrait <-base_etude_ent_BM %>% slice_max(nb_sep_2122)
#write_parquet(EMA_20_22_ent_21,"C:/Users/POLEMOB_G_SKLENAR/Documents/M?moire MiE/Data/Calculs_EMA/EMA_ent_20_22.parquet")

# Etape 4: calcul du taux de séparation pour chaque entreprise:

base_etude_ent_BM <- base_etude_ent_BM %>%
  mutate(nb_sep_2122=ifelse(is.na(nb_sep_2122),0,nb_sep_2122)) %>%
  mutate(tx_sep_2122=nb_sep_2122/EMA_2122*100)

summary(base_etude_ent_BM)

# Etape 5: calcul de l'éligibilité à la première modulation du bonus-malus:

base_etude_ent_BM <- base_etude_ent_BM %>%
  mutate(dat_crea_fare=ymd(dat_crea_fare)) %>%
  mutate(date_ref=ymd("2021-07-01")) %>% # convention: on apprécie l'âge de l'entreprise au moment où elle rentre dans le BM (01/07/21)
  mutate(age_0721=year(date_ref)-year(dat_crea_fare)) %>%
  mutate(
    eligibilité_BM_modulation1=case_when(
      age_0721>=5 &
        EMA_2020>=11 & EMA_2021>=11 & EMA_2122>=11 & 
        a38 %in% c("CA","CC","CG","EZ","HZ","IZ","MC") &
        !(apen %in% c("1101Z","1102A","1102B","1103Z","1104Z",
                      "4910Z","4932Z","4939A","4939B","4939C","5010Z",
                      "5030Z","5110Z",
                      "5510Z","5520Z","5530Z","5610A","5610B","5610C","5621Z","5629A",
                      "5629B","5630Z",
                      "7312Z","4720Z","7430Z")) ~ "entreprise éligible",
      .default = "entreprise non éligible"),
    eligibilité_BM_modulation1_2=case_when(
      age_0721>=5 &
        EMA_2020>=11 & EMA_2021>=11 & EMA_2122>=11 & 
        a38 %in% c("CA","CC","CG","EZ","HZ","IZ","MC") ~ "entreprise éligible",
      .default = "entreprise non éligible"))

# TODO: il faudrait aussi rajouter EMA_2019>=11
# TODO: il faudrait raffiner la sélection sectorielle, en tenant compte du code IDCC (convention collective majoritaire)

# Etape 6: Tableau "benchmark" (réplication du Tableau 1 in Dares Analyses n?11 de février 2024)

tableau_benchmark1 <- base_etude_ent_BM %>%
  group_by(a38) %>%
  summarise(nb_ent=n()) %>%
  filter(a38 %in% c("CA","CC","CG","EZ","HZ","IZ","MC"))

tableau_benchmark2 <- base_etude_ent_BM %>% 
  filter(eligibilité_BM_modulation1=="entreprise éligible") %>%
  group_by(a38) %>%
  summarise(nb_ent_eligible=n(),
            EMA_2122=round(sum(EMA_2122,na.rm = T)/1000,0),
            masse_sal_21_1=round(sum(ms_21,na.rm = T)/1000,0),
            masse_sal_21_2=round(sum(s_brut_21,na.rm = T)/1000000,0),
            masse_sal_21_3=round(sum(s_net_21,na.rm = T)/1000000,0),
            nb_sep_2122=round(sum(nb_sep_2122,na.rm = T)/1000,0)
  ) %>%
  filter(a38 %in% c("CA","CC","CG","EZ","HZ","IZ","MC"))

tableau_benchmark3 <- base_etude_ent_BM %>% 
  filter(eligibilité_BM_modulation1=="entreprise éligible") %>%
  group_by(a38) %>%
  summarise(tx_sep_moy_2122=weighted.mean(tx_sep_2122,EMA_2122/sum(EMA_2122),na.rm = T))

tableau_benchmark4 <- base_etude_ent_BM %>% 
  filter(eligibilité_BM_modulation1=="entreprise éligible") %>%
  group_by(a38) %>%
  summarise(tx_sep_med_2122_1=weighted.median(tx_sep_2122,s_net_21/sum(s_net_21),na.rm = T),
            tx_sep_med_2122_2=weighted.median(tx_sep_2122,ms_21/sum(ms_21),na.rm = T),
            tx_sep_med_2122_3=weighted.median(tx_sep_2122,s_brut_21/sum(s_brut_21),na.rm = T))

tableau_benchmark5 <- base_etude_ent_BM %>% 
  filter(eligibilité_BM_modulation1_2=="entreprise éligible") %>%
  group_by(a38) %>%
  summarise(tx_sep_med_2122_1=weighted.median(tx_sep_2122,s_net_21/sum(s_net_21),na.rm = T),
            tx_sep_med_2122_2=weighted.median(tx_sep_2122,ms_21/sum(ms_21),na.rm = T),
            tx_sep_med_2122_3=weighted.median(tx_sep_2122,s_brut_21/sum(s_brut_21),na.rm = T))

# Nombre total d'entreprises concernées au 01/09/22:
tableau_benchmark3 <- tableau_benchmark2 %>% summarise(nb_ent_eligible=sum(nb_ent_eligible))
# 16 986 entreprises
# au lieu des 17 891 entreprises mentionnées par la Dares, on n'est pas si loin!

# Tableau de synthèse:
# A mettre en Annexe du Mémoire.

# TODO!

# Etape 7: Taux médian (pondéré par la masse salariale) notifié le 17/11/22 (en %)

tx_sep_median <- tribble(
  ~a38, ~tx_sep_median_officiel,
  "CA",215.07,
  "CC",126.27,
  "CG",125.28,
  "EZ",70.35,
  "HZ",70.37,
  "IZ",39.87,
  "MC",9.92
)

base_etude_ent_BM <- base_etude_ent_BM %>% 
  left_join(tx_sep_median,by="a38")

# Taux médian (pondéré par la masse salariale) calculé (en %) pour chaque secteur (A38):
tx_sep_median_calculé <- base_etude_ent_BM %>% 
  group_by(a38) %>%
  summarise(tx_sep_median_calculé=weighted.median(tx_sep_2122,s_net_21/sum(s_net_21),na.rm = T)) %>%
  filter(!is.na(a38))

base_etude_ent_BM <- base_etude_ent_BM %>% 
  left_join(tx_sep_median_calculé,by="a38")

base_etude_ent_BM <- base_etude_ent_BM %>%
  mutate(tx_sep_median_calculé_2=ifelse(eligibilité_BM_modulation1=="entreprise éligible",tx_sep_median_officiel,
                                        tx_sep_median_calculé))

# Etape 8: identification des entreprises en bonus/malus et calcul du taux de contribution qui s'applique
# à la période 09/22 - 08/23:

base_etude_ent_BM <- base_etude_ent_BM %>%
  mutate(ratio_tx_sep_2122=tx_sep_2122/tx_sep_median_calculé_2,
         tx_contrib_non_borne=ratio_tx_sep_2122*1.46+2.59,
         tx_contribution=case_when(
           eligibilité_BM_modulation1=="entreprise éligible" &  tx_contrib_non_borne<3.00 ~ 3.00,
           eligibilité_BM_modulation1=="entreprise éligible" &  tx_contrib_non_borne>=3.00 &
             tx_contrib_non_borne<=5.05 ~ tx_contrib_non_borne,
           eligibilité_BM_modulation1=="entreprise éligible" &  tx_contrib_non_borne>5.05 ~ 5.05,
           eligibilité_BM_modulation1=="entreprise non éligible" ~ 4.05
         )
  ) %>%
  mutate(type_modulation=case_when(
    tx_contribution<4.05 ~ "bonus",
    tx_contribution>4.05 ~ "malus",
    tx_contribution==4.05 ~"statu quo"
  )) %>%
  mutate(malus_max=(tx_contribution==5.05))



