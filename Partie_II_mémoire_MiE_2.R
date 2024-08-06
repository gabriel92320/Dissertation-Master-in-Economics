
# Ce script R calcule les différents outcomes et constitue le tibble data_econo (contenant toutes les variables
# pour la partie économétrique)


#3) Construction des variables d'outcome:

# Calcul des variables d'outcome:

# Outcome 1: taux de séparation (calculé au niveau "établissement")

# connexion à la base Etablissements de BTS 2022:
etab_BTS_22<- open_dataset("//casd.fr/casdfs/Projets/POLEMOB/Data/DADS_DADS Etablissements_2022/etab_2022-0.parquet")
etab_BTS_22_arrow <- etab_BTS_22 %>% select(siret,eff_eqtp_et_22=eff_eqtp_et) %>% as_arrow_table()

# chargement de la base SAS nb_sep_ent_2020 (générée par le programme SAS calcul_nb_separations.sas)
nb_sep_etab_2020 <-haven::read_sas(paste0(sauvegarde_table_SAS,"nb_separations_etab_2020.sas7bdat")
)
nb_sep_etab_2020_arrow <- nb_sep_etab_2020 %>% rename(nb_sep_20=nb_separations) %>% as_arrow_table()

# chargement de la base SAS nb_sep_ent_2021 (g?n?r?e par le programme SAS calcul_nb_separations.sas)
nb_sep_etab_2021 <-haven::read_sas(paste0(sauvegarde_table_SAS,"nb_separations_etab_2021.sas7bdat")
)
nb_sep_etab_2021_arrow <- nb_sep_etab_2021 %>% rename(nb_sep_21=nb_separations) %>% as_arrow_table()

# chargement de la base SAS nb_sep_ent_2020 (g?n?r?e par le programme SAS calcul_nb_separations.sas)
nb_sep_etab_2022 <-haven::read_sas(paste0(sauvegarde_table_SAS,"nb_separations_etab_2022.sas7bdat")
)
nb_sep_etab_2022_arrow <- nb_sep_etab_2022 %>% rename(nb_sep_22=nb_separations) %>% as_arrow_table()

# Appariement avec la base d'étude:
base_etude_etab_BM_arrow <- base_etude_etab_BM_tb %>% as_arrow_table()

base_etude_etab_BM_arrow <- base_etude_etab_BM_arrow %>%
  left_join(nb_sep_etab_2020_arrow,by=c("siret"="siret_final")) %>%
  left_join(nb_sep_etab_2021_arrow,by=c("siret"="siret_final")) %>%
  left_join(nb_sep_etab_2022_arrow,by=c("siret"="siret_final")) %>% compute()


base_etude_etab_BM_arrow <- base_etude_etab_BM_arrow %>%
  left_join(etab_BTS_22_arrow,by="siret") %>% compute()

#extrait <- base_etude_etab_BM_arrow %>% slice_head(n=100) %>% collect()
base_etude_etab_BM_tb <- base_etude_etab_BM_arrow %>% as_tibble()


# calcul des taux de s?parations par établissement:
base_etude_etab_BM_tb <- base_etude_etab_BM_tb %>%
  mutate(tx_sep_20=case_when(!is.na(nb_sep_20) & eff_eqtp_et_20>=1 ~ nb_sep_20/eff_eqtp_et_20*100,
                             is.na(nb_sep_20) & eff_eqtp_et_20>=1 ~ 0,
                             .default = NA),
         tx_sep_21=case_when(!is.na(nb_sep_21) & eff_eqtp_et>=1 ~ nb_sep_21/eff_eqtp_et*100,
                             is.na(nb_sep_21) & eff_eqtp_et>=1 ~ 0,
                             .default = NA),
         tx_sep_22=case_when(!is.na(nb_sep_22) & eff_eqtp_et_22>=1 ~ nb_sep_22/eff_eqtp_et_22*100,
                             is.na(nb_sep_22) & eff_eqtp_et_22>=1 ~ 0,
                             .default = NA))

# Champ de calcul: toutes les entreprises
outcome1_all<-base_etude_etab_BM_tb %>%
  group_by(zempt)%>%
  summarise(
    tx_sep_ZE_20=mean(tx_sep_20,na.rm = T),
    tx_sep_ZE_21=mean(tx_sep_21,na.rm = T),
    tx_sep_ZE_22=mean(tx_sep_22,na.rm = T),
    tx_sep_ZE_20_p=weighted.mean(tx_sep_20,w=eff_eqtp_et_20/sum(eff_eqtp_et_20),na.rm = T),
    tx_sep_ZE_21_p=weighted.mean(tx_sep_21,w=eff_eqtp_et/sum(eff_eqtp_et),na.rm = T),
    tx_sep_ZE_22_p=weighted.mean(tx_sep_22,w=eff_eqtp_et_22/eff_eqtp_et_22,na.rm = T)
  ) %>% mutate(tx_sep_ZE_19=NA) 


# passage wide to long:
outcome1_all <- outcome1_all %>%
  select(zempt,tx_sep_ZE_19,tx_sep_ZE_20,
         tx_sep_ZE_21,tx_sep_ZE_22) %>%
  pivot_longer(cols=starts_with("tx_"),
               names_to = "annee",
               values_to = "tx_sep_all"
  ) %>%
  mutate(annee=recode(annee,
                      'tx_sep_ZE_19' = '2019',
                      'tx_sep_ZE_20' = '2020',
                      'tx_sep_ZE_21' = '2021',
                      'tx_sep_ZE_22' = '2022'))


# Champ de calcul: seulement les entreprises traitées
outcome1_ent_t<-base_etude_etab_BM_tb %>%
  filter(eligibilité_BM_modulation1=="entreprise éligible") %>%
  group_by(zempt)%>%
  summarise(
    tx_sep_ZE_20=mean(tx_sep_20,na.rm = T),
    tx_sep_ZE_21=mean(tx_sep_21,na.rm = T),
    tx_sep_ZE_22=mean(tx_sep_22,na.rm = T),
    tx_sep_ZE_20_p=weighted.mean(tx_sep_20,w=eff_eqtp_et_20/sum(eff_eqtp_et_20),na.rm = T),
    tx_sep_ZE_21_p=weighted.mean(tx_sep_21,w=eff_eqtp_et/sum(eff_eqtp_et),na.rm = T),
    tx_sep_ZE_22_p=weighted.mean(tx_sep_22,w=eff_eqtp_et_22/eff_eqtp_et_22,na.rm = T)
  ) %>% mutate(tx_sep_ZE_19=NA) 


# passage wide to long:
outcome1_ent_t <- outcome1_ent_t %>%
  select(zempt,tx_sep_ZE_19,tx_sep_ZE_20,
         tx_sep_ZE_21,tx_sep_ZE_22) %>%
  pivot_longer(cols=starts_with("tx_"),
               names_to = "annee",
               values_to = "tx_sep_et_t"
  ) %>%
  mutate(annee=recode(annee,
                      'tx_sep_ZE_19' = '2019',
                      'tx_sep_ZE_20' = '2020',
                      'tx_sep_ZE_21' = '2021',
                      'tx_sep_ZE_22' = '2022'))

# Champ de calcul: seulement les entreprises non traitées
outcome1_ent_nt<-base_etude_etab_BM_tb %>%
  filter(!eligibilité_BM_modulation1=="entreprise éligible") %>%
  group_by(zempt)%>%
  summarise(
    tx_sep_ZE_20=mean(tx_sep_20,na.rm = T),
    tx_sep_ZE_21=mean(tx_sep_21,na.rm = T),
    tx_sep_ZE_22=mean(tx_sep_22,na.rm = T),
    tx_sep_ZE_20_p=weighted.mean(tx_sep_20,w=eff_eqtp_et_20/sum(eff_eqtp_et_20),na.rm = T),
    tx_sep_ZE_21_p=weighted.mean(tx_sep_21,w=eff_eqtp_et/sum(eff_eqtp_et),na.rm = T),
    tx_sep_ZE_22_p=weighted.mean(tx_sep_22,w=eff_eqtp_et_22/eff_eqtp_et_22,na.rm = T)
  ) %>% mutate(tx_sep_ZE_19=NA) 


# passage wide to long:
outcome1_ent_nt <- outcome1_ent_nt %>%
  select(zempt,tx_sep_ZE_19,tx_sep_ZE_20,
         tx_sep_ZE_21,tx_sep_ZE_22) %>%
  pivot_longer(cols=starts_with("tx_"),
               names_to = "annee",
               values_to = "tx_sep_et_nt"
  ) %>%
  mutate(annee=recode(annee,
                      'tx_sep_ZE_19' = '2019',
                      'tx_sep_ZE_20' = '2020',
                      'tx_sep_ZE_21' = '2021',
                      'tx_sep_ZE_22' = '2022'))



# Outcome 2: taux d'embauche (calculé au niveau "établissement")
                                   
# chargement de la base SAS nb_sep_ent_2020 (générée par le programme SAS calcul_nb_embauches.sas)
nb_emb_etab_2020 <-haven::read_sas(paste0(sauvegarde_table_SAS,"nb_embauches_etab_2020.sas7bdat")
)
nb_emb_etab_2020_arrow <- nb_emb_etab_2020 %>% rename(nb_emb_20=nb_embauches) %>% as_arrow_table()

# chargement de la base SAS nb_sep_ent_2021 (générée par le programme SAS calcul_nb_embauches.sas)
nb_emb_etab_2021 <-haven::read_sas(paste0(sauvegarde_table_SAS,"nb_embauches_etab_2021.sas7bdat")
)
nb_emb_etab_2021_arrow <- nb_emb_etab_2021 %>% rename(nb_emb_21=nb_embauches) %>% as_arrow_table()

# chargement de la base SAS nb_sep_ent_2022 (générée par le programme SAS calcul_nb_embauches.sas)
nb_emb_etab_2022 <-haven::read_sas(paste0(sauvegarde_table_SAS,"nb_embauches_etab_2022.sas7bdat")
)
nb_emb_etab_2022_arrow <- nb_emb_etab_2022 %>% rename(nb_emb_22=nb_embauches) %>% as_arrow_table()

# Appariement avec la base d'étude:
base_etude_etab_BM_arrow <- base_etude_etab_BM_tb %>% as_arrow_table()

base_etude_etab_BM_arrow <- base_etude_etab_BM_arrow %>%
  left_join(nb_emb_etab_2020_arrow,by=c("siret"="siret_final")) %>%
  left_join(nb_emb_etab_2021_arrow,by=c("siret"="siret_final")) %>%
  left_join(nb_emb_etab_2022_arrow,by=c("siret"="siret_final")) %>% compute()


#extrait <- base_etude_etab_BM_arrow %>% slice_head(n=100) %>% collect()
base_etude_etab_BM_tb <- base_etude_etab_BM_arrow %>% as_tibble()


# calcul des taux d'embauche par établissement:
base_etude_etab_BM_tb <- base_etude_etab_BM_tb %>%
  mutate(tx_emb_20=case_when(!is.na(nb_emb_20) & eff_eqtp_et_20>=1 ~ nb_emb_20/eff_eqtp_et_20*100,
                             is.na(nb_emb_20) & eff_eqtp_et_20>=1 ~ 0,
                             .default = NA),
         tx_emb_21=case_when(!is.na(nb_emb_21) & eff_eqtp_et>=1 ~ nb_emb_21/eff_eqtp_et*100,
                             is.na(nb_emb_21) & eff_eqtp_et>=1 ~ 0,
                             .default = NA),
         tx_emb_22=case_when(!is.na(nb_emb_22) & eff_eqtp_et_22>=1 ~ nb_emb_22/eff_eqtp_et_22*100,
                             is.na(nb_sep_22) & eff_eqtp_et_22>=1 ~ 0,
                             .default = NA))

#extrait <- base_etude_etab_BM_tb %>% slice_head(n=100)

# Champ de calcul: toutes les entreprises
outcome2_all<-base_etude_etab_BM_tb %>%
  group_by(zempt)%>%
  summarise(
    tx_emb_ZE_20=mean(tx_emb_20,na.rm = T),
    tx_emb_ZE_21=mean(tx_emb_21,na.rm = T),
    tx_emb_ZE_22=mean(tx_emb_22,na.rm = T),
    tx_emb_ZE_20_p=weighted.mean(tx_emb_20,w=eff_eqtp_et_20/sum(eff_eqtp_et_20),na.rm = T),
    tx_emb_ZE_21_p=weighted.mean(tx_emb_21,w=eff_eqtp_et/sum(eff_eqtp_et),na.rm = T),
    tx_emb_ZE_22_p=weighted.mean(tx_emb_22,w=eff_eqtp_et_22/eff_eqtp_et_22,na.rm = T)
  ) %>% mutate(tx_emb_ZE_19=NA) 


# passage wide to long:
outcome2_all <- outcome2_all %>%
  dplyr:: select(zempt,tx_emb_ZE_19,tx_emb_ZE_20,
                 tx_emb_ZE_21,tx_emb_ZE_22) %>%
  pivot_longer(cols=starts_with("tx_"),
               names_to = "annee",
               values_to = "tx_emb_all"
  ) %>%
  mutate(annee=recode(annee,
                      'tx_emb_ZE_19' = '2019',
                      'tx_emb_ZE_20' = '2020',
                      'tx_emb_ZE_21' = '2021',
                      'tx_emb_ZE_22' = '2022'))


# Champ de calcul: seulement les entreprises traitées
outcome2_ent_t<-base_etude_etab_BM_tb %>%
  filter(eligibilité_BM_modulation1=="entreprise éligible") %>%
  group_by(zempt)%>%
  summarise(
    tx_emb_ZE_20=mean(tx_emb_20,na.rm = T),
    tx_emb_ZE_21=mean(tx_emb_21,na.rm = T),
    tx_emb_ZE_22=mean(tx_emb_22,na.rm = T),
    tx_emb_ZE_20_p=weighted.mean(tx_emb_20,w=eff_eqtp_et_20/sum(eff_eqtp_et_20),na.rm = T),
    tx_emb_ZE_21_p=weighted.mean(tx_emb_21,w=eff_eqtp_et/sum(eff_eqtp_et),na.rm = T),
    tx_emb_ZE_22_p=weighted.mean(tx_emb_22,w=eff_eqtp_et_22/eff_eqtp_et_22,na.rm = T)
  ) %>% mutate(tx_emb_ZE_19=NA) 


# passage wide to long:
outcome2_ent_t <- outcome2_ent_t %>%
  dplyr:: select(zempt,tx_emb_ZE_19,tx_emb_ZE_20,
                 tx_emb_ZE_21,tx_emb_ZE_22) %>%
  pivot_longer(cols=starts_with("tx_"),
               names_to = "annee",
               values_to = "tx_emb_et_t"
  ) %>%
  mutate(annee=recode(annee,
                      'tx_emb_ZE_19' = '2019',
                      'tx_emb_ZE_20' = '2020',
                      'tx_emb_ZE_21' = '2021',
                      'tx_emb_ZE_22' = '2022'))

# Champ de calcul: seulement les entreprises non traitées
outcome2_ent_nt<-base_etude_etab_BM_tb %>%
  filter(!eligibilité_BM_modulation1=="entreprise éligible") %>%
  group_by(zempt)%>%
  summarise(
    tx_emb_ZE_20=mean(tx_emb_20,na.rm = T),
    tx_emb_ZE_21=mean(tx_emb_21,na.rm = T),
    tx_emb_ZE_22=mean(tx_emb_22,na.rm = T),
    tx_emb_ZE_20_p=weighted.mean(tx_emb_20,w=eff_eqtp_et_20/sum(eff_eqtp_et_20),na.rm = T),
    tx_emb_ZE_21_p=weighted.mean(tx_emb_21,w=eff_eqtp_et/sum(eff_eqtp_et),na.rm = T),
    tx_emb_ZE_22_p=weighted.mean(tx_emb_22,w=eff_eqtp_et_22/eff_eqtp_et_22,na.rm = T)
  ) %>% mutate(tx_emb_ZE_19=NA) 


# passage wide to long:
outcome2_ent_nt <- outcome2_ent_nt %>%
  dplyr:: select(zempt,tx_emb_ZE_19,tx_emb_ZE_20,
                 tx_emb_ZE_21,tx_emb_ZE_22) %>%
  pivot_longer(cols=starts_with("tx_"),
               names_to = "annee",
               values_to = "tx_emb_et_nt"
  ) %>%
  mutate(annee=recode(annee,
                      'tx_emb_ZE_19' = '2019',
                      'tx_emb_ZE_20' = '2020',
                      'tx_emb_ZE_21' = '2021',
                      'tx_emb_ZE_22' = '2022'))

# Outcome 3: Short CDD rate -> flux d'embauches en CDD court/flux d'embauches en CDD, en %
# Outcome 4: CDI hiring rate -> flux d'embauches en CDI/flux d'embauches en CDD ou en CDI, en %
                                   
# chargement de la base SAS etab_dpae_2019 (générée par le programme SAS Calcul_nb_CDDcourt_CDI.sas)
etab_dpae_2019 <-haven::read_sas(paste0(sauvegarde_table_SAS,"etab_dpae_2019.sas7bdat")
)
etab_dpae_2019_arrow <- etab_dpae_2019 %>% dplyr::select(siret,tx_emb_CDD_court_19=tx_emb_CDD_court,
                                                         tx_emb_CDI_19=tx_emb_CDI) %>% as_arrow_table()
rm(etab_dpae_2019)

# chargement de la base SAS etab_dpae_2020 (générée par le programme SAS Calcul_nb_CDDcourt_CDI.sas)
etab_dpae_2020 <-haven::read_sas(paste0(sauvegarde_table_SAS,"etab_dpae_2020.sas7bdat")
)
etab_dpae_2020_arrow <- etab_dpae_2020 %>% dplyr::select(siret,tx_emb_CDD_court_20=tx_emb_CDD_court,
                                                         tx_emb_CDI_20=tx_emb_CDI) %>% as_arrow_table()
rm(etab_dpae_2020)

# chargement de la base SAS etab_dpae_2021 (générée par le programme SAS Calcul_nb_CDDcourt_CDI.sas)
etab_dpae_2021 <-haven::read_sas(paste0(sauvegarde_table_SAS,"etab_dpae_2021.sas7bdat")
)
etab_dpae_2021_arrow <- etab_dpae_2021 %>% dplyr::select(siret,tx_emb_CDD_court_21=tx_emb_CDD_court,
                                                         tx_emb_CDI_21=tx_emb_CDI) %>% as_arrow_table()
rm(etab_dpae_2021)

# chargement de la base SAS etab_dpae_2022 (g?n?r?e par le programme SAS Calcul_nb_CDDcourt_CDI.sas)
etab_dpae_2022 <-haven::read_sas(paste0(sauvegarde_table_SAS,"etab_dpae_2022.sas7bdat")
)
etab_dpae_2022_arrow <- etab_dpae_2022 %>% dplyr::select(siret,tx_emb_CDD_court_22=tx_emb_CDD_court,
                                                         tx_emb_CDI_22=tx_emb_CDI) %>% as_arrow_table()
rm(etab_dpae_2022)


# Appariement avec la base d'étude:
base_etude_etab_BM_arrow <- base_etude_etab_BM_tb %>% as_arrow_table()

base_etude_etab_BM_arrow <- base_etude_etab_BM_arrow %>%
  left_join(etab_dpae_2019_arrow,by=c("siret"="siret")) %>%
  left_join(etab_dpae_2020_arrow,by=c("siret"="siret")) %>%
  left_join(etab_dpae_2021_arrow,by=c("siret"="siret")) %>%
  left_join(etab_dpae_2022_arrow,by=c("siret"="siret")) %>% compute()


#extrait <- base_etude_etab_BM_arrow %>% slice_head(n=100) %>% collect()
base_etude_etab_BM_tb <- base_etude_etab_BM_arrow %>% as_tibble()

# outcome 3:

#extrait <- base_etude_etab_BM_tb %>% slice_head(n=100)

# Champ de calcul: toutes les entreprises
outcome3_all<-base_etude_etab_BM_tb %>%
  group_by(zempt)%>%
  summarise(
    tx_emb_CDD_court_19=mean(tx_emb_CDD_court_19,na.rm = T),
    tx_emb_CDD_court_20=mean(tx_emb_CDD_court_20,na.rm = T),
    tx_emb_CDD_court_21=mean(tx_emb_CDD_court_21,na.rm = T),
    tx_emb_CDD_court_22=mean(tx_emb_CDD_court_22,na.rm = T))
# ,
#     tx_emb_CDD_court_19_p=weighted.mean(tx_emb_CDD_court_19,w=eff_eqtp_et_19/sum(eff_eqtp_et_19),na.rm = T),
#     tx_emb_CDD_court_20_p=weighted.mean(tx_emb_CDD_court_20,w=eff_eqtp_et_20/sum(eff_eqtp_et_20),na.rm = T),
#     tx_emb_CDD_court_21_p=weighted.mean(tx_emb_CDD_court_21,w=eff_eqtp_et/sum(eff_eqtp_et),na.rm = T),
#     tx_emb_CDD_court_22_p=weighted.mean(tx_emb_CDD_court_22,w=eff_eqtp_et_22/sum(eff_eqtp_et_22),na.rm = T)
#   ) 


# passage wide to long:
outcome3_all <- outcome3_all %>%
  dplyr:: select(zempt,tx_emb_CDD_court_19,tx_emb_CDD_court_20,
                 tx_emb_CDD_court_21,tx_emb_CDD_court_22) %>%
  pivot_longer(cols=starts_with("tx_"),
               names_to = "annee",
               values_to = "tx_emb_CDD_court_et"
  ) %>%
  mutate(annee=recode(annee,
                      'tx_emb_CDD_court_19' = '2019',
                      'tx_emb_CDD_court_20' = '2020',
                      'tx_emb_CDD_court_21' = '2021',
                      'tx_emb_CDD_court_22' = '2022'))


# Champ de calcul: seulement les entreprises trait?es
outcome3_ent_t<-base_etude_etab_BM_tb %>%
  filter(eligibilité_BM_modulation1=="entreprise éligible") %>%
  group_by(zempt)%>%
  summarise(
    tx_emb_CDD_court_19=mean(tx_emb_CDD_court_19,na.rm = T),
    tx_emb_CDD_court_20=mean(tx_emb_CDD_court_20,na.rm = T),
    tx_emb_CDD_court_21=mean(tx_emb_CDD_court_21,na.rm = T),
    tx_emb_CDD_court_22=mean(tx_emb_CDD_court_22,na.rm = T))


# passage wide to long:
outcome3_ent_t <- outcome3_ent_t %>%
  dplyr:: select(zempt,tx_emb_CDD_court_19,tx_emb_CDD_court_20,
                 tx_emb_CDD_court_21,tx_emb_CDD_court_22) %>%
  pivot_longer(cols=starts_with("tx_"),
               names_to = "annee",
               values_to = "tx_emb_CDD_court_et_t"
  ) %>%
  mutate(annee=recode(annee,
                      'tx_emb_CDD_court_19' = '2019',
                      'tx_emb_CDD_court_20' = '2020',
                      'tx_emb_CDD_court_21' = '2021',
                      'tx_emb_CDD_court_22' = '2022'))

# Champ de calcul: seulement les entreprises non traitées
outcome3_ent_nt<-base_etude_etab_BM_tb %>%
  filter(!eligibilité_BM_modulation1=="entreprise éligible") %>%
  group_by(zempt)%>%
  summarise(
    tx_emb_CDD_court_19=mean(tx_emb_CDD_court_19,na.rm = T),
    tx_emb_CDD_court_20=mean(tx_emb_CDD_court_20,na.rm = T),
    tx_emb_CDD_court_21=mean(tx_emb_CDD_court_21,na.rm = T),
    tx_emb_CDD_court_22=mean(tx_emb_CDD_court_22,na.rm = T))


# passage wide to long:
outcome3_ent_nt <- outcome3_ent_nt %>%
  dplyr:: select(zempt,tx_emb_CDD_court_19,tx_emb_CDD_court_20,
                 tx_emb_CDD_court_21,tx_emb_CDD_court_22) %>%
  pivot_longer(cols=starts_with("tx_"),
               names_to = "annee",
               values_to = "tx_emb_CDD_court_et_nt"
  ) %>%
  mutate(annee=recode(annee,
                      'tx_emb_CDD_court_19' = '2019',
                      'tx_emb_CDD_court_20' = '2020',
                      'tx_emb_CDD_court_21' = '2021',
                      'tx_emb_CDD_court_22' = '2022'))

# outcome 4:

#extrait <- base_etude_etab_BM_tb %>% slice_head(n=100)

# Champ de calcul: toutes les entreprises
outcome4_all<-base_etude_etab_BM_tb %>%
  group_by(zempt)%>%
  summarise(
    tx_emb_CDI_19=mean(tx_emb_CDI_19,na.rm = T),
    tx_emb_CDI_20=mean(tx_emb_CDI_20,na.rm = T),
    tx_emb_CDI_21=mean(tx_emb_CDI_21,na.rm = T),
    tx_emb_CDI_22=mean(tx_emb_CDI_22,na.rm = T))
# ,
#     tx_emb_CDD_court_19_p=weighted.mean(tx_emb_CDD_court_19,w=eff_eqtp_et_19/sum(eff_eqtp_et_19),na.rm = T),
#     tx_emb_CDD_court_20_p=weighted.mean(tx_emb_CDD_court_20,w=eff_eqtp_et_20/sum(eff_eqtp_et_20),na.rm = T),
#     tx_emb_CDD_court_21_p=weighted.mean(tx_emb_CDD_court_21,w=eff_eqtp_et/sum(eff_eqtp_et),na.rm = T),
#     tx_emb_CDD_court_22_p=weighted.mean(tx_emb_CDD_court_22,w=eff_eqtp_et_22/sum(eff_eqtp_et_22),na.rm = T)
#   ) 


# passage wide to long:
outcome4_all <- outcome4_all %>%
  dplyr:: select(zempt,tx_emb_CDI_19,tx_emb_CDI_20,
                 tx_emb_CDI_21,tx_emb_CDI_22) %>%
  pivot_longer(cols=starts_with("tx_"),
               names_to = "annee",
               values_to = "tx_emb_CDI_et"
  ) %>%
  mutate(annee=recode(annee,
                      'tx_emb_CDI_19' = '2019',
                      'tx_emb_CDI_20' = '2020',
                      'tx_emb_CDI_21' = '2021',
                      'tx_emb_CDI_22' = '2022'))


# Champ de calcul: seulement les entreprises traitées
outcome4_ent_t<-base_etude_etab_BM_tb %>%
  filter(eligibilité_BM_modulation1=="entreprise éligible") %>%
  group_by(zempt)%>%
  summarise(
    tx_emb_CDI_19=mean(tx_emb_CDI_19,na.rm = T),
    tx_emb_CDI_20=mean(tx_emb_CDI_20,na.rm = T),
    tx_emb_CDI_21=mean(tx_emb_CDI_21,na.rm = T),
    tx_emb_CDI_22=mean(tx_emb_CDI_22,na.rm = T))


# passage wide to long:
outcome4_ent_t <- outcome4_ent_t %>%
  dplyr:: select(zempt,tx_emb_CDI_19,tx_emb_CDI_20,
                 tx_emb_CDI_21,tx_emb_CDI_22) %>%
  pivot_longer(cols=starts_with("tx_"),
               names_to = "annee",
               values_to = "tx_emb_CDI_et_t"
  ) %>%
  mutate(annee=recode(annee,
                      'tx_emb_CDI_19' = '2019',
                      'tx_emb_CDI_20' = '2020',
                      'tx_emb_CDI_21' = '2021',
                      'tx_emb_CDI_22' = '2022'))

# Champ de calcul: seulement les entreprises non trait?es
outcome4_ent_nt<-base_etude_etab_BM_tb %>%
  filter(!eligibilité_BM_modulation1=="entreprise éligible") %>%
  group_by(zempt)%>%
  summarise(
    tx_emb_CDI_19=mean(tx_emb_CDI_19,na.rm = T),
    tx_emb_CDI_20=mean(tx_emb_CDI_20,na.rm = T),
    tx_emb_CDI_21=mean(tx_emb_CDI_21,na.rm = T),
    tx_emb_CDI_22=mean(tx_emb_CDI_22,na.rm = T))


# passage wide to long:
outcome4_ent_nt <- outcome4_ent_nt %>%
  dplyr:: select(zempt,tx_emb_CDI_19,tx_emb_CDI_20,
                 tx_emb_CDI_21,tx_emb_CDI_22) %>%
  pivot_longer(cols=starts_with("tx_"),
               names_to = "annee",
               values_to = "tx_emb_CDI_et_nt"
  ) %>%
  mutate(annee=recode(annee,
                      'tx_emb_CDI_19' = '2019',
                      'tx_emb_CDI_20' = '2020',
                      'tx_emb_CDI_21' = '2021',
                      'tx_emb_CDI_22' = '2022'))


# Outcomes V et VI: Le niveau annuel de l'emploi (en eqtp) et le niveau moyen du salaire brut par eqtp:

# chargement de la base Etablissements de BTS 2019:
etab_BTS_19 <-haven::read_sas("//casd.fr/casdfs/Projets/POLEMOB/Data/DADS_DADS Etablissements_2020/etab.sas7bdat",
                              col_select = c("SIRET","S_BRUT_1"))
etab_BTS_19_arrow <- etab_BTS_19 %>% rename(siret=SIRET,s_brut_2019=S_BRUT_1) %>% as_arrow_table()

# connexion ? la base Etablissements de BTS 2020:
etab_BTS_20<- open_dataset("//casd.fr/casdfs/Projets/POLEMOB/Data/DADS_DADS Etablissements_2021/etab_2020-0.parquet")
etab_BTS_20_arrow <- etab_BTS_20 %>% select(siret,s_brut_2020=s_brut) %>% as_arrow_table()

# connexion ? la base Etablissements de BTS 2021:
etab_BTS_21<- open_dataset("//casd.fr/casdfs/Projets/POLEMOB/Data/DADS_DADS Etablissements_2022/etab_2021-0.parquet")
etab_BTS_21_arrow <- etab_BTS_21 %>% select(siret,s_brut_2021=s_brut) %>% as_arrow_table()

# connexion ? la base Etablissements de BTS 2022:
etab_BTS_22<- open_dataset("//casd.fr/casdfs/Projets/POLEMOB/Data/DADS_DADS Etablissements_2022/etab_2022-0.parquet")
etab_BTS_22_arrow <- etab_BTS_22 %>% select(siret,s_brut_2022=s_brut) %>% as_arrow_table()

# Appariement avec la base d'étude:
base_etude_etab_BM_arrow <- base_etude_etab_BM_tb %>% as_arrow_table()

base_etude_etab_BM_arrow <- base_etude_etab_BM_arrow %>%
  left_join(etab_BTS_19_arrow,by="siret") %>%
  left_join(etab_BTS_20_arrow,by="siret") %>%
  left_join(etab_BTS_21_arrow,by="siret") %>%
  left_join(etab_BTS_22_arrow,by="siret") %>% compute()

#extrait <- base_etude_etab_BM_arrow %>% slice_head(n=100) %>% collect()
base_etude_etab_BM_tb <- base_etude_etab_BM_arrow %>% as_tibble()

# Champ de calcul: toutes les entreprises
eff_sal_eqtp_et_2019_2022_ZE_all<-base_etude_etab_BM_tb %>%
  group_by(zempt)%>%
  summarise(
    eff_eqtp_et_ZE_19=sum(eff_eqtp_et_19,na.rm = T),
    eff_eqtp_et_ZE_20=sum(eff_eqtp_et_20,na.rm = T),
    eff_eqtp_et_ZE_21=sum(eff_eqtp_et,na.rm = T),
    eff_eqtp_et_ZE_22=sum(eff_eqtp_et_22,na.rm = T),
    s_brut_et_ZE_19=sum(s_brut_2019,na.rm = T),
    s_brut_et_ZE_20=sum(s_brut_2020,na.rm = T),
    s_brut_et_ZE_21=sum(s_brut_2021,na.rm = T),
    s_brut_et_ZE_22=sum(s_brut_2022,na.rm = T)
  ) %>%
  mutate(l_eff_eqtp_et_ZE_19=log(eff_eqtp_et_ZE_19),
         l_eff_eqtp_et_ZE_20=log(eff_eqtp_et_ZE_20),
         l_eff_eqtp_et_ZE_21=log(eff_eqtp_et_ZE_21),
         l_eff_eqtp_et_ZE_22=log(eff_eqtp_et_ZE_22),
         s_brut_eqtp_et_ZE_19=s_brut_et_ZE_19/eff_eqtp_et_ZE_19,
         s_brut_eqtp_et_ZE_20=s_brut_et_ZE_20/eff_eqtp_et_ZE_20,
         s_brut_eqtp_et_ZE_21=s_brut_et_ZE_21/eff_eqtp_et_ZE_21,
         s_brut_eqtp_et_ZE_22=s_brut_et_ZE_22/eff_eqtp_et_ZE_22,
         l_s_brut_eqtp_et_ZE_19=log(s_brut_eqtp_et_ZE_19),
         l_s_brut_eqtp_et_ZE_20=log(s_brut_eqtp_et_ZE_20),
         l_s_brut_eqtp_et_ZE_21=log(s_brut_eqtp_et_ZE_21),
         l_s_brut_eqtp_et_ZE_22=log(s_brut_eqtp_et_ZE_22)
  )
# passage wide to long:
eff_eqtp_et_2019_2022_ZE_all <- eff_sal_eqtp_et_2019_2022_ZE_all %>%
  select(zempt,l_eff_eqtp_et_ZE_19,l_eff_eqtp_et_ZE_20,
         l_eff_eqtp_et_ZE_21,l_eff_eqtp_et_ZE_22) %>%
  pivot_longer(cols=starts_with("l_eff"),
               names_to = "annee",
               values_to = "log_eff_ZE_et_all"
  ) %>%
  mutate(annee=recode(annee,
                      'l_eff_eqtp_et_ZE_19' = '2019',
                      'l_eff_eqtp_et_ZE_20' = '2020',
                      'l_eff_eqtp_et_ZE_21' = '2021',
                      'l_eff_eqtp_et_ZE_22' = '2022'))

sal_eqtp_et_2019_2022_ZE_all <- eff_sal_eqtp_et_2019_2022_ZE_all %>%
  select(zempt,l_s_brut_eqtp_et_ZE_19,l_s_brut_eqtp_et_ZE_20,
         l_s_brut_eqtp_et_ZE_21,l_s_brut_eqtp_et_ZE_22) %>%
  pivot_longer(cols=starts_with("l_s"),
               names_to = "annee",
               values_to = "log_sal_ZE_et_all"
  ) %>%
  mutate(annee=recode(annee,
                      'l_s_brut_eqtp_et_ZE_19' = '2019',
                      'l_s_brut_eqtp_et_ZE_20' = '2020',
                      'l_s_brut_eqtp_et_ZE_21' = '2021',
                      'l_s_brut_eqtp_et_ZE_22' = '2022'))

# Champ de calcul: seulement les entreprises trait?es
eff_sal_eqtp_et_2019_2022_ZE_t<-base_etude_etab_BM_tb %>%
  filter(eligibilité_BM_modulation1=="entreprise éligible") %>%
  group_by(zempt)%>%
  summarise(
    eff_eqtp_et_ZE_19=sum(eff_eqtp_et_19,na.rm = T),
    eff_eqtp_et_ZE_20=sum(eff_eqtp_et_20,na.rm = T),
    eff_eqtp_et_ZE_21=sum(eff_eqtp_et,na.rm = T),
    eff_eqtp_et_ZE_22=sum(eff_eqtp_et_22,na.rm = T),
    s_brut_et_ZE_19=sum(s_brut_2019,na.rm = T),
    s_brut_et_ZE_20=sum(s_brut_2020,na.rm = T),
    s_brut_et_ZE_21=sum(s_brut_2021,na.rm = T),
    s_brut_et_ZE_22=sum(s_brut_2022,na.rm = T)
  ) %>%
  mutate(l_eff_eqtp_et_ZE_19=log(eff_eqtp_et_ZE_19),
         l_eff_eqtp_et_ZE_20=log(eff_eqtp_et_ZE_20),
         l_eff_eqtp_et_ZE_21=log(eff_eqtp_et_ZE_21),
         l_eff_eqtp_et_ZE_22=log(eff_eqtp_et_ZE_22),
         s_brut_eqtp_et_ZE_19=s_brut_et_ZE_19/eff_eqtp_et_ZE_19,
         s_brut_eqtp_et_ZE_20=s_brut_et_ZE_20/eff_eqtp_et_ZE_20,
         s_brut_eqtp_et_ZE_21=s_brut_et_ZE_21/eff_eqtp_et_ZE_21,
         s_brut_eqtp_et_ZE_22=s_brut_et_ZE_22/eff_eqtp_et_ZE_22,
         l_s_brut_eqtp_et_ZE_19=log(s_brut_eqtp_et_ZE_19),
         l_s_brut_eqtp_et_ZE_20=log(s_brut_eqtp_et_ZE_20),
         l_s_brut_eqtp_et_ZE_21=log(s_brut_eqtp_et_ZE_21),
         l_s_brut_eqtp_et_ZE_22=log(s_brut_eqtp_et_ZE_22)
  )
# passage wide to long:
eff_eqtp_et_2019_2022_ZE_t <- eff_sal_eqtp_et_2019_2022_ZE_t %>%
  select(zempt,l_eff_eqtp_et_ZE_19,l_eff_eqtp_et_ZE_20,
         l_eff_eqtp_et_ZE_21,l_eff_eqtp_et_ZE_22) %>%
  pivot_longer(cols=starts_with("l_eff"),
               names_to = "annee",
               values_to = "log_eff_ZE_et_t"
  ) %>%
  mutate(annee=recode(annee,
                      'l_eff_eqtp_et_ZE_19' = '2019',
                      'l_eff_eqtp_et_ZE_20' = '2020',
                      'l_eff_eqtp_et_ZE_21' = '2021',
                      'l_eff_eqtp_et_ZE_22' = '2022'))

sal_eqtp_et_2019_2022_ZE_t <- eff_sal_eqtp_et_2019_2022_ZE_t %>%
  select(zempt,l_s_brut_eqtp_et_ZE_19,l_s_brut_eqtp_et_ZE_20,
         l_s_brut_eqtp_et_ZE_21,l_s_brut_eqtp_et_ZE_22) %>%
  pivot_longer(cols=starts_with("l_s"),
               names_to = "annee",
               values_to = "log_sal_ZE_et_t"
  ) %>%
  mutate(annee=recode(annee,
                      'l_s_brut_eqtp_et_ZE_19' = '2019',
                      'l_s_brut_eqtp_et_ZE_20' = '2020',
                      'l_s_brut_eqtp_et_ZE_21' = '2021',
                      'l_s_brut_eqtp_et_ZE_22' = '2022'))


# Champ de calcul: seulement les entreprises non trait?es
eff_sal_eqtp_et_2019_2022_ZE_nt<-base_etude_etab_BM_tb %>%
  filter(!eligibilité_BM_modulation1=="entreprise éligible") %>%
  group_by(zempt)%>%
  summarise(
    eff_eqtp_et_ZE_19=sum(eff_eqtp_et_19,na.rm = T),
    eff_eqtp_et_ZE_20=sum(eff_eqtp_et_20,na.rm = T),
    eff_eqtp_et_ZE_21=sum(eff_eqtp_et,na.rm = T),
    eff_eqtp_et_ZE_22=sum(eff_eqtp_et_22,na.rm = T),
    s_brut_et_ZE_19=sum(s_brut_2019,na.rm = T),
    s_brut_et_ZE_20=sum(s_brut_2020,na.rm = T),
    s_brut_et_ZE_21=sum(s_brut_2021,na.rm = T),
    s_brut_et_ZE_22=sum(s_brut_2022,na.rm = T)
  ) %>%
  mutate(l_eff_eqtp_et_ZE_19=log(eff_eqtp_et_ZE_19),
         l_eff_eqtp_et_ZE_20=log(eff_eqtp_et_ZE_20),
         l_eff_eqtp_et_ZE_21=log(eff_eqtp_et_ZE_21),
         l_eff_eqtp_et_ZE_22=log(eff_eqtp_et_ZE_22),
         s_brut_eqtp_et_ZE_19=s_brut_et_ZE_19/eff_eqtp_et_ZE_19,
         s_brut_eqtp_et_ZE_20=s_brut_et_ZE_20/eff_eqtp_et_ZE_20,
         s_brut_eqtp_et_ZE_21=s_brut_et_ZE_21/eff_eqtp_et_ZE_21,
         s_brut_eqtp_et_ZE_22=s_brut_et_ZE_22/eff_eqtp_et_ZE_22,
         l_s_brut_eqtp_et_ZE_19=log(s_brut_eqtp_et_ZE_19),
         l_s_brut_eqtp_et_ZE_20=log(s_brut_eqtp_et_ZE_20),
         l_s_brut_eqtp_et_ZE_21=log(s_brut_eqtp_et_ZE_21),
         l_s_brut_eqtp_et_ZE_22=log(s_brut_eqtp_et_ZE_22)
  )
# passage wide to long:
eff_eqtp_et_2019_2022_ZE_nt <- eff_sal_eqtp_et_2019_2022_ZE_nt %>%
  select(zempt,l_eff_eqtp_et_ZE_19,l_eff_eqtp_et_ZE_20,
         l_eff_eqtp_et_ZE_21,l_eff_eqtp_et_ZE_22) %>%
  pivot_longer(cols=starts_with("l_eff"),
               names_to = "annee",
               values_to = "log_eff_ZE_et_nt"
  ) %>%
  mutate(annee=recode(annee,
                      'l_eff_eqtp_et_ZE_19' = '2019',
                      'l_eff_eqtp_et_ZE_20' = '2020',
                      'l_eff_eqtp_et_ZE_21' = '2021',
                      'l_eff_eqtp_et_ZE_22' = '2022'))

sal_eqtp_et_2019_2022_ZE_nt <- eff_sal_eqtp_et_2019_2022_ZE_nt %>%
  select(zempt,l_s_brut_eqtp_et_ZE_19,l_s_brut_eqtp_et_ZE_20,
         l_s_brut_eqtp_et_ZE_21,l_s_brut_eqtp_et_ZE_22) %>%
  pivot_longer(cols=starts_with("l_s"),
               names_to = "annee",
               values_to = "log_sal_ZE_et_nt"
  ) %>%
  mutate(annee=recode(annee,
                      'l_s_brut_eqtp_et_ZE_19' = '2019',
                      'l_s_brut_eqtp_et_ZE_20' = '2020',
                      'l_s_brut_eqtp_et_ZE_21' = '2021',
                      'l_s_brut_eqtp_et_ZE_22' = '2022'))


# 4) Spécification économétrique et inférence causale (effet de la variable de traitement sur l'outcome):

# Création du tibble contenant les data utiles pour les régressions économétriques:

data_econo <-outcome1_all %>%
  inner_join(outcome1_ent_t) %>%
  inner_join(outcome1_ent_nt) %>%
  inner_join(outcome2_all) %>%
  inner_join(outcome2_ent_t) %>%
  inner_join(outcome2_ent_t) %>%
  inner_join(outcome3_all) %>%
  inner_join(outcome3_ent_t) %>%
  inner_join(outcome3_ent_t) %>%
  inner_join(outcome4_all) %>%
  inner_join(outcome4_ent_t) %>%
  inner_join(outcome4_ent_nt) %>%
  inner_join(eff_eqtp_et_2019_2022_ZE_all) %>%
  inner_join(eff_eqtp_et_2019_2022_ZE_t) %>%
  inner_join(eff_eqtp_et_2019_2022_ZE_nt) %>%
  inner_join(sal_eqtp_et_2019_2022_ZE_all) %>%
  inner_join(sal_eqtp_et_2019_2022_ZE_t) %>%
  inner_join(sal_eqtp_et_2019_2022_ZE_nt) %>%
  left_join(variable_traitements) %>%
  mutate(part_ent_concernées_ZE=ifelse(annee %in% c("2021","2022"),part_ent_concernées_ZE,0),
         part_ent_concernées_ZE_p=ifelse(annee %in% c("2021","2022"),part_ent_concernées_ZE_p,0),
         part_ent_malus_ZE=ifelse(annee =="2022",part_ent_malus_ZE,0),
         part_ent_malus_ZE_p=ifelse(annee =="2022",part_ent_malus_ZE_p,0),
         part_ent_tx_max_ZE=ifelse(annee =="2022",part_ent_tx_max_ZE,0),
         part_ent_tx_max_ZE_p=ifelse(annee =="2022",part_ent_tx_max_ZE_p,0),
         tx_moy_modulation_ZE=ifelse(annee =="2022",tx_moy_modulation_ZE,0),
         tx_moy_modulation_ZE_p=ifelse(annee =="2022",tx_moy_modulation_ZE_p,0),
         part_ent_malus_ZE=ifelse(annee =="2022",part_ent_malus_ZE,0)
  ) %>%
  left_join(IV_Bartik_2) %>%
  mutate(Bartik_part_ent_concernées_2=ifelse(annee %in% c("2021","2022"),Bartik_part_ent_concernées_2,0),
         Bartik_part_ent_concernées_p_2=ifelse(annee %in% c("2021","2022"),Bartik_part_ent_concernées_p_2,0),
         Bartik_part_ent_malus_2=ifelse(annee =="2022",Bartik_part_ent_malus_2,0),
         Bartik_part_ent_malus_p_2=ifelse(annee =="2022",Bartik_part_ent_malus_p_2,0),
         Bartik_part_ent_tx_max_2=ifelse(annee =="2022",Bartik_part_ent_tx_max_2,0),
         Bartik_part_ent_tx_max_p_2=ifelse(annee =="2022",Bartik_part_ent_tx_max_p_2,0),
         Bartik_tx_moy_modulation_2=ifelse(annee =="2022",Bartik_tx_moy_modulation_2,0),
         Bartik_tx_moy_modulation_p_2=ifelse(annee =="2022",Bartik_tx_moy_modulation_p_2,0))
