
# Construction de la base base_etude_etab_BM (base d'étude au format "établissement")
# Fait le lien avec les zonages "zones d'emploi" de l'Insee.


# Récupération de la variable ZEMPT (Zone d'emploi d'implantation de l'établissement - lieu de travail)

# connexion à la base Etablissements de BTS 2021:
etab_BTS_21<- open_dataset("//casd.fr/casdfs/Projets/POLEMOB/Data/DADS_DADS Etablissements_2022/etab_2021-0.parquet")

etab_BTS_21_arrow <- etab_BTS_21 %>% as_arrow_table()

base_etude_etab_BM <- etab_BTS_21_arrow %>%
  dplyr::select(siret,siren,zempt,comt,eff_eqtp_et) %>%
  inner_join(base_etude_ent_BM,by="siren") %>% compute()
# 849 409 établissements.

# Chargement du fichier de correspondance entre communes et zones d'emploi (ZE):

# Les communes:
communes_zonages_2021<-readxl::read_excel(path = paste0(sauvegarde_bases_etude,"table-appartenance-geo-communes-21.xlsx"),
                                          sheet = "COM",skip = 5)
# Les arrondissements municipaux (Paris, Lyon, Marseille):
arm_zonages_2021<-readxl::read_excel(path = paste0(sauvegarde_bases_etude,"table-appartenance-geo-communes-21.xlsx"),
                                     sheet = "ARM",skip = 5)
arm_zonages_2021<-arm_zonages_2021[,1:17] #on exclut la variable "COM"

# on concatène les 2 tibbles:
communes_zonages_2021<-rbind(communes_zonages_2021,arm_zonages_2021)

# Construction de la base de données pour l'économétrie

base_etude_etab_BM <- base_etude_etab_BM %>%
  filter(!(comt=="")) %>%
  left_join(communes_zonages_2021,
            by = c("comt"="CODGEO")) %>% compute()

# Nettoyage: on ne garde que les établissements implantés en France métropolitaine et/ou ayant un code ZE renseigné:
base_etude_etab_BM <- base_etude_etab_BM %>%
  filter(!(DEP %in% c("971","972","973","974","975","976")) & !(is.na(ZE2020))) %>% compute()
# 828 743 établissements

base_etude_etab_BM_tb <- base_etude_etab_BM %>% as_tibble()









