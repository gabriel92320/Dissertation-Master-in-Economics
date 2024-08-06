
/* Calcul du nombre de débuts de contrats par entreprise sur les années civiles 2019, 2020, 2021 et 2022)*/

/* Librairie perso pour stocker les bases d'études à analyser en R: */
libname Mybases "C:\Users\POLEMOB_G_SKLENAR\Documents\Mémoire MiE\Bases_etudes";

/* Fichiers MMO (fichiers des Mouvements de Main-d'Oeuvre) issus du dispositif Midas: */
*libname ForCEMMO "\\casd.fr\casdfs\Projets\POLEMOB\Data\FORCE_FORCE_2023T2\MMO";
libname MidasMMO "\\casd.fr\casdfs\Projets\POLEMOB\Data\MIDAS_MIDAS_2024T1\MMO";
/* Couvre la période 2017T1 à 2023T4*/

/* Débuts de contrat en 2020*/

/* Fins de contrat enregistrées entre janvier 2020 et décembre 2020*/
proc sql;
create table debuts_contrat_2020 as
select id_midas,L_contrat_sqn,siret_af,siret_ut,DebutCTT,finctt,Nature,DispPolitiquePublique,MotifRupture
from MidasMMO.Mmo_2_2020_m4
where Secteur_PUBLIC = 0 /* contrats dans des établissements du secteur privé */
and substr(DebutCTT,1,7) in ("2020-01","2020-02","2020-03","2020-04","2020-05","2020-06","2020-07","2020-08","2020-09","2020-10","2020-11","2020-12") /* mois d'enregistrement des fins de contrat */
and DispPolitiquePublique not in ("21","41","42","61","64","65","70","71"/*,"92"*/) /* Natures de contrat de travail explicitement exclues du bonus/malus:
21: CUI - Contrat Initiative Emploi
41: CUI - Contrat d'Accompagnement dans l'Emploi
42: CUI - Contrat d'accès à l'emploi - DOM
61: Contrat de professionnalisation
64: Contrat d'apprentissage entreprises artisanales ou de moins de 11 salariés (loi du 3 janvier 1979) 
65: Contrat d’apprentissage entreprises non inscrites au répertoire des métiers d’au moins 11 salariés (loi de 1987)
70: Contrat à durée déterminée pour les séniors
71: Contrat à durée déterminée d’insertion
92: Stage de la formation professionnelle (pas notifié dans la doc sur le Bonus-malus!)
*/
/* and Nature not in () Natures de contrat de travail explicitement exclues du bonus/malus:*/
order by id_midas,L_contrat_sqn
;
quit;

/* On crée une variable Siret_final qui correspond au numéro Siret de l'établissement d'affectation (Siret_AF) dans le cas où il
ne s'agit pas d'un contrat d'interim, et au numéro Siret de l'établissement utilisateur dans le cas d'un contrat d'interim. */

data debuts_contrat_2020;
set debuts_contrat_2020;

if nature = "03" then do;
siret_final = siret_ut;
end;
else do;
siret_final = siret_af;
end;

run;

/* Calcul du nombre d'embauches par établissement (SIRET): */

proc sort data=debuts_contrat_2020;
by siret_final;
run;

data debuts_contrat_2020;
set debuts_contrat_2020;
compteur=1;
run;

proc means data=debuts_contrat_2020 noprint missing;
class siret_final;
var compteur;
output out=nb_embauches_etab_2020 sum(compteur)=nb_embauches;
run;

data Mybases.nb_embauches_etab_2020 (keep=siret_final nb_embauches);
set nb_embauches_etab_2020;
if siret_final="" then delete;
run;


/* Débuts de contrat en 2021*/

/* Fins de contrat enregistrées entre janvier 2021 et décembre 2021*/
proc sql;
create table debuts_contrat_2021 as
select id_midas,L_contrat_sqn,siret_af,siret_ut,DebutCTT,finctt,Nature,DispPolitiquePublique,MotifRupture
from MidasMMO.Mmo_2_2021_m4
where Secteur_PUBLIC = 0 /* contrats dans des établissements du secteur privé */
and substr(DebutCTT,1,7) in ("2021-01","2021-02","2021-03","2021-04","2021-05","2021-06","2021-07","2021-08","2021-09","2021-10","2021-11","2021-12") /* mois d'enregistrement des fins de contrat */
and DispPolitiquePublique not in ("21","41","42","61","64","65","70","71"/*,"92"*/) /* Natures de contrat de travail explicitement exclues du bonus/malus:
21: CUI - Contrat Initiative Emploi
41: CUI - Contrat d'Accompagnement dans l'Emploi
42: CUI - Contrat d'accès à l'emploi - DOM
61: Contrat de professionnalisation
64: Contrat d'apprentissage entreprises artisanales ou de moins de 11 salariés (loi du 3 janvier 1979) 
65: Contrat d’apprentissage entreprises non inscrites au répertoire des métiers d’au moins 11 salariés (loi de 1987)
70: Contrat à durée déterminée pour les séniors
71: Contrat à durée déterminée d’insertion
92: Stage de la formation professionnelle (pas notifié dans la doc sur le Bonus-malus!)
*/
/* and Nature not in () Natures de contrat de travail explicitement exclues du bonus/malus:*/
order by id_midas,L_contrat_sqn
;
quit;

/* On crée une variable Siret_final qui correspond au numéro Siret de l'établissement d'affectation (Siret_AF) dans le cas où il
ne s'agit pas d'un contrat d'interim, et au numéro Siret de l'établissement utilisateur dans le cas d'un contrat d'interim. */

data debuts_contrat_2021;
set debuts_contrat_2021;

if nature = "03" then do;
siret_final = siret_ut;
end;
else do;
siret_final = siret_af;
end;

run;

/* Calcul du nombre d'embauches par établissement (SIRET): */

proc sort data=debuts_contrat_2021;
by siret_final;
run;

data debuts_contrat_2021;
set debuts_contrat_2021;
compteur=1;
run;

proc means data=debuts_contrat_2021 noprint missing;
class siret_final;
var compteur;
output out=nb_embauches_etab_2021 sum(compteur)=nb_embauches;
run;

data Mybases.nb_embauches_etab_2021 (keep=siret_final nb_embauches);
set nb_embauches_etab_2021;
if siret_final="" then delete;
run;


/* Débuts de contrat en 2022*/

/* Fins de contrat enregistrées entre janvier 2022 et décembre 2022*/
proc sql;
create table debuts_contrat_2022 as
select id_midas,L_contrat_sqn,siret_af,siret_ut,DebutCTT,finctt,Nature,DispPolitiquePublique,MotifRupture
from MidasMMO.Mmo_2_2022_m4
where Secteur_PUBLIC = 0 /* contrats dans des établissements du secteur privé */
and substr(DebutCTT,1,7) in ("2022-01","2022-02","2022-03","2022-04","2022-05","2022-06","2022-07","2022-08","2022-09","2022-10","2022-11","2022-12") /* mois d'enregistrement des fins de contrat */
and DispPolitiquePublique not in ("21","41","42","61","64","65","70","71"/*,"92"*/) /* Natures de contrat de travail explicitement exclues du bonus/malus:
21: CUI - Contrat Initiative Emploi
41: CUI - Contrat d'Accompagnement dans l'Emploi
42: CUI - Contrat d'accès à l'emploi - DOM
61: Contrat de professionnalisation
64: Contrat d'apprentissage entreprises artisanales ou de moins de 11 salariés (loi du 3 janvier 1979) 
65: Contrat d’apprentissage entreprises non inscrites au répertoire des métiers d’au moins 11 salariés (loi de 1987)
70: Contrat à durée déterminée pour les séniors
71: Contrat à durée déterminée d’insertion
92: Stage de la formation professionnelle (pas notifié dans la doc sur le Bonus-malus!)
*/
/* and Nature not in () Natures de contrat de travail explicitement exclues du bonus/malus:*/
order by id_midas,L_contrat_sqn
;
quit;

/* On crée une variable Siret_final qui correspond au numéro Siret de l'établissement d'affectation (Siret_AF) dans le cas où il
ne s'agit pas d'un contrat d'interim, et au numéro Siret de l'établissement utilisateur dans le cas d'un contrat d'interim. */

data debuts_contrat_2022;
set debuts_contrat_2022;

if nature = "03" then do;
siret_final = siret_ut;
end;
else do;
siret_final = siret_af;
end;

run;

/* Calcul du nombre d'embauches par établissement (SIRET): */

proc sort data=debuts_contrat_2022;
by siret_final;
run;

data debuts_contrat_2022;
set debuts_contrat_2022;
compteur=1;
run;

proc means data=debuts_contrat_2022 noprint missing;
class siret_final;
var compteur;
output out=nb_embauches_etab_2022 sum(compteur)=nb_embauches;
run;

data Mybases.nb_embauches_etab_2022 (keep=siret_final nb_embauches);
set nb_embauches_etab_2022;
if siret_final="" then delete;
run;



