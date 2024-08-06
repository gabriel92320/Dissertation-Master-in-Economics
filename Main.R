
# Programme principal qui génère la totalité du code R relatif au stage de fin d'étude - mémoire de recherche MiE
# réalisé au Crest (Ensae) sur le sujet:
# The French scheme of Unemployment Insurance experience rating: an analysis at the level of the French local labor
# markets.

# Auteur: Gabriel Sklénard.

# Chemin pointant vers les scripts R du projet:
scripts_path="C:/Users/POLEMOB_G_SKLENAR/Documents/Mémoire MiE/Codes R/Version finale/Projet R/Mie-Thesis"

# Chemin pointant vers le dossier de sauvegarde des bases intermédiaires au format parquet dans le cadre du
# calcul de l'effectif moyen annuel (EMA) des entreprises:
sauvegarde_EMA="C:/Users/POLEMOB_G_SKLENAR/Documents/Mémoire MiE/Data/Calculs_EMA/"

# Chemin pointant vers le dossier où sont stockés les bases construites pour le mémoire (au format Rdata):
# Ces bases sont ensuite chargées et utilisées dans les scripts relatifs à chaque partie du mémoire.
sauvegarde_bases_etude="C:/Users/POLEMOB_G_SKLENAR/Documents/Mémoire MiE/Data/"

# Chemin pointant vers le dossier contenant les outputs SAS (générés par des programmes SAS):
sauvegarde_table_SAS="C:/Users/POLEMOB_G_SKLENAR/Documents/Mémoire MiE/Bases_etudes/"

# Chemin pointant vers le dossier où sont sauvegardés l'ensemble des figures à insérer dans le Mémoire:
sauvegarde_figures_memoire="C:/Users/POLEMOB_G_SKLENAR/Documents/Mémoire MiE/Outputs/Figures_Mémoire/"



# Etape 0: Chargement des packages utilisés dans ce projet:
source(paste0(scripts_path,"Chargement_packages.R"),encoding = "UTF-8")

# Etant donné la volumétrie des données manipulées, l'utilisation de la RAM est optimisée grâce
# au package Arrow. Le paramétrage ci-dessous a été retenu pour le projet:

# Autoriser arrow à utiliser plusieurs processeurs en même temps
options(arrow.use_threads = T)
# Définir le nombre de processeurs utilisés par arrow
# 10 processeurs sont suffisants dans la plupart des cas
#arrow:::set_cpu_count(10)
arrow:::set_cpu_count(parallel::detectCores() %/% 4)

# Etape 1: construction de la base d'étude "base_etude_ent_BM"
# Entreprises (SIREN) retenues dans l'étude + nombreuses variables caractéristiques +
# variables calculées: effectif moyen mensuel (EMM), effectif moyen annuel (EMA), taux de séparation,
# taux de contribution à l'Assurance-chômage...
# Les bases intermédiaires sont sauvegardés au format "parquet".
# WARNING: étant donné la volumétrie des données, il est conseillé de compiler chaque script ci-dessous
# par morceau en surveillant la consommation de la RAM dans le gestionnaire des tâches!
# En particulier, il est conseillé de fermer régulièrement sa session R, avant chaque gros calcul, en
# commençant par se reconnecter aux fichiers parquet sauvegardés précédemment.

# a. Calcul des EMM des entreprises selon le secteur de leurs établissements respectifs:
source(paste0(scripts_path,"Calcul_EMM_sect.R"),encoding = "UTF-8")

# b. Calcul des EMM (Effectifs moyens mensuels) et de l'EMA (effectif moyen annuel) pour chaque entreprise:
# 2020, 2021, 2022 et la période d'observation du bonus-malus (juillet 2021 à juin 2022).
source(paste0(scripts_path,"Calcul_EMM_EMA.R"),encoding = "UTF-8")
# L'output final est la base des entreprises du champ de l'étude avec un EMA calculé pour les 4 périodes 
# mentionnées ci-dessus. Il est sauvegardé sous: EMA_ent_20_22.parquet

# c. Calcul du nombre de séparations (éligibles au bonus-malus) pour chaque entreprise:
# Compiler le programme SAS: Carac_UL_Fare_2021.sas
# Il génère un output au format table SAS: carac_ul_fare_21.sas7bdat
# Cette table SAS liste les entreprises présentes dans le FARE 2021 + diverses caractértiques comptables
#(chiffre d'affaires, valeur ajoutée, EBE; etc ) et donne le nombre de séparations pour la première période
# d'observation du bonus-malus.
# Ce fichier SAS sera utilisé dans le script R ci-dessous;

# d. Microsimulation du taux de séparation, identification des entreprises éligibles au bonus/malus,
# taux de contribution modulé à l'Assurance chômage + caractéristique...
# Output final, la base d'étude au niveau "entreprise": base_etude_ent_BM
source(paste0(scripts_path,"Calcul_tx_séparations.R"),encoding = "UTF-8")

# Cette base dite d'"étude" (au niveau "entreprise") est sauvegardée au format Rdata
save(base_etude_ent_BM,file=paste0(sauvegarde_bases_etude,"base_etude_ent_BM.Rdata"))


# Etape 2: construction de la base d'étude "base_etude_etab_BM"
# Cette base liste en lignes les établissements des entreprises retenues dans le champ de l'étude
# ie présentes dans la base construite ci-dessus base_etude_ent_BM.Rdata.
# Le gros intérêt de cette base est de se situer à un niveau d'observation permettant de réaliser
# une analyse géographique du marché du travail, et notamment à l'échelle des zones d'emploi (ZE);
# Pour chaque établissement, sont associées certaines infos calculées au niveau "entreprise" (comme
# par ex: le taux de séparation, le taux de contribution, etc...) + d'autres infos concernant l'établissement
# lui-même (secteur, effectif, masse salariale, commune d'implantation...);
# C'est sur cette base qu'est attribuée à chaque établissement une zone d'emploi (ZE) à partir de sa commune
# d'implantation.

# WARNING:
# Enregistrer au préalable le fichier table-appartenance-geo-communes-21.xlsx sous le chemin "sauvegarde_bases_etude"
# (après l'avoir téléchargé à partir du site Web de l'Insee)

source(paste0(scripts_path,"Construction_base_etude_niveau_etab.R"),encoding = "UTF-8")

# Cette base dite d'"étude" (au niveau "établissement") est sauvegardée au format Rdata
save(base_etude_etab_BM_tb,file=paste0(sauvegarde_bases_etude,"base_etude_etab_BM_tb.Rdata"))


######################## DEBUT de la PARTIE 1 du Mémoire ################################################
################# Microsimulation of the French bonus-malus system (first modulation)####################



# Réalise les figures (tableaux, graphiques) de la partie 1 du Mémoire:

# Chargement préalable des bases d'étude (ent/etab):
load(paste0(sauvegarde_bases_etude,"base_etude_ent_BM.Rdata"))
# 584 457 entreprises (siren);
load(paste0(sauvegarde_bases_etude,"base_etude_etab_BM_tb.Rdata"))
# 828 743 ?tablissements (siret);

#Importation du fond de carte (ZE):
# Note: les fonds de carte au format .shp sont à télécharger au préalable sur le site Web de l'Insee.
ze <- st_read(paste0(sauvegarde_bases_etude,"ze2020_2024/ze2020_2024.shp"),quiet = T)

source(paste0(scripts_path,"Partie_I_mémoire_MiE.R"),encoding = "UTF-8")

# Figures sauvegardées pour export CASD (en vue de leur insertion dans le Mémoire):

# Sortie tableau 1:
write.csv2(tableau1,paste0(sauvegarde_figures_memoire,"Tableau1.csv"))

# Sortie tableau 2:
write.csv2(tableau1,paste0(sauvegarde_figures_memoire,"Tableau2.csv"))

# Sortie modèle LOGIT (tableau-graphique sur les odds ratio):
ggsave(paste0(sauvegarde_figures_memoire,"graph2_OR.model_logit1.pdf"),plot=graph2_OR.model_logit1)

# Cartes sur les indicateurs du bonus-malus aggrégés par ZE (sans pondérer par les effectifs des étab):

ggsave(paste0(sauvegarde_figures_memoire,"carte1.pdf"),plot=carte1)
ggsave(paste0(sauvegarde_figures_memoire,"carte2.pdf"),plot=carte2)
ggsave(paste0(sauvegarde_figures_memoire,"carte3.pdf"),plot=carte3)
ggsave(paste0(sauvegarde_figures_memoire,"carte4.pdf"),plot=carte4)
ggsave(paste0(sauvegarde_figures_memoire,"cartes_all.pdf"),plot=cartes_all)

# Cartes sur les indicateurs du bonus-malus aggrégés par ZE:

ggsave(paste0(sauvegarde_figures_memoire,"carte1_annexe.pdf"),plot=carte1_annexe)
ggsave(paste0(sauvegarde_figures_memoire,"carte2_annexe.pdf"),plot=carte2_annexe)
ggsave(paste0(sauvegarde_figures_memoire,"carte3_annexe.pdf"),plot=carte3_annexe)
ggsave(paste0(sauvegarde_figures_memoire,"carte4_annexe.pdf"),plot=carte4_annexe)
ggsave(paste0(sauvegarde_figures_memoire,"cartes_all_annexe.pdf"),plot=cartes_all_annexe)


######################## FIN de la PARTIE 1 du Mémoire ################################################


######################## DEBUT de la PARTIE 2 du Mémoire ################################################
################# Ex-post evaluation of the effects of the French bonus-malus (first ####################
################## modulation) at employement zone level ###############################################

# Réalise les figures (tableaux, graphiques) de la partie 2 du Mémoire:

# Chargement de la base d'étude  au niveau "etab":
load(paste0(sauvegarde_bases_etude,"base_etude_etab_BM_tb.Rdata"))
# 828 743 établissements (siret);

# Etape 1: construction des variables de traitement et des instruments Bartik à l'échelle des ZE
source(paste0(scripts_path,"Partie_II_mémoire_MiE_1.R"),encoding = "UTF-8")

# Etape 2: construction des variables d'outcome et finalisation de la base de données au niveau ZE
# en vue de la partie économétrique:
source(paste0(scripts_path,"Partie_II_mémoire_MiE_2.R"),encoding = "UTF-8")
# WARNING: avant de compiler ce script R, il faut penser à exécuter les codes SAS suivants:
# calcul_nb_separations.sas 
# calcul_nb_separations.sas
# Calcul_nb_CDDcourt_CDI.sas
# Chacun de ces prg SAS créent des tables par année avec le nombre de séparations/embauches/embauches en CDD/
# en CDD courts... par entreprise et par établissement;

# Cette base dite contenant les données utiles pour la partie économétrique (au niveau "ZE") est sauvegardée
# au format Rdata:
save(data_econo,file=paste0(sauvegarde_bases_etude,"data_econo.Rdata"))

# Etape 3: estimation économétrique de l'effet causal du bonus-malus sur les différentes outcomes
load(paste0(sauvegarde_bases_etude,"data_econo.Rdata"))

source(paste0(scripts_path,"Partie_II_mémoire_MiE_3.R"),encoding = "UTF-8")

# Sauvegarde des cartes générées:

# Niveaux 2022:
ggsave(paste0(sauvegarde_figures_memoire,"carte_outcome1_22.pdf"),plot=carte_outcome1_22)
ggsave(paste0(sauvegarde_figures_memoire,"carte_outcome2_22.pdf"),plot=carte_outcome2_22)
ggsave(paste0(sauvegarde_figures_memoire,"carte_outcome3_22.pdf"),plot=carte_outcome3_22)
ggsave(paste0(sauvegarde_figures_memoire,"carte_outcome4_22.pdf"),plot=carte_outcome4_22)
ggsave(paste0(sauvegarde_figures_memoire,"carte_outcome5_22.pdf"),plot=carte_outcome5_22)
ggsave(paste0(sauvegarde_figures_memoire,"carte_outcome6_22.pdf"),plot=carte_outcome6_22)
ggsave(paste0(sauvegarde_figures_memoire,"cartes_all_outcomes_1_4_22.pdf"),plot=cartes_all_outcomes_1_4_22)
ggsave(paste0(sauvegarde_figures_memoire,"cartes_all_outcomes_5_6_22.pdf"),plot=cartes_all_outcomes_5_6_22)

# Différences premières 2021-2022:

# Sauvegarde:
ggsave(paste0(sauvegarde_figures_memoire,"carte_outcome1_21_22.pdf"),plot=carte_outcome1_21_22)
ggsave(paste0(sauvegarde_figures_memoire,"carte_outcome2_21_22.pdf"),plot=carte_outcome2_21_22)
ggsave(paste0(sauvegarde_figures_memoire,"carte_outcome3_21_22.pdf"),plot=carte_outcome3_21_22)
ggsave(paste0(sauvegarde_figures_memoire,"carte_outcome4_21_22.pdf"),plot=carte_outcome4_21_22)
ggsave(paste0(sauvegarde_figures_memoire,"carte_outcome5_21_22.pdf"),plot=carte_outcome5_21_22_2)
ggsave(paste0(sauvegarde_figures_memoire,"carte_outcome6_21_22.pdf"),plot=carte_outcome6_21_22)
ggsave(paste0(sauvegarde_figures_memoire,"cartes_all_outcomes_1_4_21_22.pdf"),plot=cartes_all_outcomes_1_4_21_22)
ggsave(paste0(sauvegarde_figures_memoire,"cartes_all_outcomes_5_6_21_22.pdf"),plot=cartes_all_outcomes_5_6_21_22)


# Etape 4: estimation économétrique de l'effet causal du bonus-malus sur les différentes outcomes
source(paste0(scripts_path,"Partie_II_mémoire_MiE_4.R"),encoding = "UTF-8")

######################## FIN de la PARTIE 2 du Mémoire ################################################


######################## DEBUT de la PARTIE 3 du Mémoire ################################################
################# Extension-variants of the bonus-malus: quantification by microsimulation ##############
#########################################################################################################

# Variant 1: Extend the French bonus-malus scheme to all employers

# Variant 2: modification of the French schedule for calculating UI tax rates

# Variant 3: microsimulation of the American experience-rating "reserve ratio" on French data
# TODO! (malheureusement, pas assez de temps pour traiter cette partie!)

# Chargement préalable des bases d'étude (ent/etab):
load(paste0(sauvegarde_bases_etude,"base_etude_ent_BM.Rdata"))
# 584 457 entreprises (siren);

source(paste0(scripts_path,"Partie_II_mémoire_MiE_4.R"),encoding = "UTF-8")

# Exports CSV des tableaux de résultat:
write.csv2(tableau_simulation1,paste0(sauvegarde_figures_memoire,"tableau_simulation1.csv"))
write.csv2(tableau2_simul2,paste0(sauvegarde_figures_memoire,"tableau2_simul2.csv"))

# Export des graphiques:
ggsave(paste0(sauvegarde_figures_memoire,"Graph_simulation1.pdf"),plot=Graph_simulation1)
ggsave(paste0(sauvegarde_figures_memoire,"Graph_simulation2.pdf"),plot=Graph_simulation2)




