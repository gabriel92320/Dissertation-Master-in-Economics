
/* Calcul du nombre de séparations (telles que définies dans le cadre de la mesure de bonus-malus) par entreprise
sur les années civiles 2019, 2020, 2021 et 2022)*/

/* Librairie perso pour stocker les bases d'études à analyser en R: */
libname Mybases "C:\Users\POLEMOB_G_SKLENAR\Documents\Mémoire MiE\Bases_etudes";

/* Fichiers MMO (fichiers des Mouvements de Main-d'Oeuvre) issus du dispositif Midas: */
*libname ForCEMMO "\\casd.fr\casdfs\Projets\POLEMOB\Data\FORCE_FORCE_2023T2\MMO";
libname MidasMMO "\\casd.fr\casdfs\Projets\POLEMOB\Data\MIDAS_MIDAS_2024T1\MMO";
/* Couvre la période 2017T1 à 2023T4*/

/* Fichiers FH (Fichiers Historiques des demandeurs d'emploi) issus du dispositif Midas: */
*libname ForCEFH "\\casd.fr\casdfs\Projets\POLEMOB\Data\FORCE_FORCE_2023T2\FH";
libname MidasFH "\\casd.fr\casdfs\Projets\POLEMOB\Data\MIDAS_MIDAS_2024T1\FHS";
/* Couvre la période 2014T1 à 2023T4*/

/* Fichier "entreprise" de la base Tous salariés 2021: */

*libname BTSent21"\\casd.fr\casdfs\Projets\POLEMOB\Data\DADS_DADS Entreprises_2021";

/* Fichier "établissement" de la base Tous salariés 2021: */
*libname BTSeta20"\\casd.fr\casdfs\Projets\POLEMOB\Data\DADS_DADS Etablissements_2020";
*libname BTSeta21"\\casd.fr\casdfs\Projets\POLEMOB\Data\DADS_DADS Etablissements_2021";

/* A. Détermination du nombre de séparations éligibles à la mesure du bonus/malus par entreprise (SIREN) */

/* 1. Sélection des fins de contrats éligibles au bonus-malus durant les années civiles 2019, 2020, 2021 et 2022. */


/* 2019 */

/* Fins de contrat enregistrées entre janvier 2019 et décembre 2019*/
proc sql;
create table fins_contrat_2019 as
select id_midas,L_contrat_sqn,siret_af,siret_ut,DebutCTT,finctt,Nature,DispPolitiquePublique,MotifRupture
from MidasMMO.Mmo_2_2019_m4
where Secteur_PUBLIC = 0 /* contrats dans des établissements du secteur privé */
and substr(finctt,1,7) in ("2019-01","2019-02","2019-03","2019-04","2019-05","2019-06","2019-07","2019-08","2019-09","2019-10","2019-11","2019-12") /* mois d'enregistrement des fins de contrat */
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
and MotifRupture not in ("066","059","081") /* Motifs de rupture de contrat explicitement exclus du bonus/malus:
059: démission (TODO: il faudrait ici n'exclure que les démissions de salarié en CDI)
066: décès du salarié
081: fin de contrat d'apprentissage
*/
order by id_midas,L_contrat_sqn
;
quit;

/* On crée une variable Siret_final qui correspond au numéro Siret de l'établissement d'affectation (Siret_AF) dans le cas où il
ne s'agit pas d'un contrat d'interim, et au numéro Siret de l'établissement utilisateur dans le cas d'un contrat d'interim. */

data fins_contrat_2019;
set fins_contrat_2019;

if nature = "03" then do;
siret_final = siret_ut;
end;
else do;
siret_final = siret_af;
end;

run;

/* 2. Cas 1: Sélection parmi les fins de contrats éligibles, celles qui ont donné lieu à une inscription à Pôle Emploi dans les 3 mois suivant la fin de contrat:*/

/* 2019*/

proc sql;
create table inscrip_PE_2019 as
select id_midas,ndem,datins,motins,motins_a
from Midasfh.De_tempo
where datins >="01JAN2019"d and datins <="30MAR2020"d
order by id_midas asc,ndem desc;
quit;

proc sql;
create table fins_contrat_2019_2 as
select *
from fins_contrat_2019 
where id_midas  in (select id_midas from inscrip_PE_2019)
;
quit;

proc sort data=inscrip_PE_2019;
by id_midas;
run;

data inscrip_PE_2019_2 (keep=id_midas datins);
set inscrip_PE_2019;
run;

proc transpose data=inscrip_PE_2019_2 out=datins;
by id_midas;
var datins;
run;
data datins (keep=id_midas col1-col10);
set datins;
run;

proc sql;
create table fins_contrat_2019_2 as
select *
from fins_contrat_2019_2 as a
left join datins as b
on a.id_midas=b.id_midas;
quit;

data fins_contrat_2019_2;
set fins_contrat_2019_2;
finCTT2=input(finCTT,yymmdd10.);
format finCTT2 yymmdd10.;
diff_datins1_finCTT=col1-finCTT2;
diff_datins2_finCTT=col2-finCTT2;
diff_datins3_finCTT=col3-finCTT2;
diff_datins4_finCTT=col4-finCTT2;
diff_datins5_finCTT=col5-finCTT2;
diff_datins6_finCTT=col6-finCTT2;
diff_datins7_finCTT=col7-finCTT2;
diff_datins8_finCTT=col8-finCTT2;
diff_datins9_finCTT=col9-finCTT2;
diff_datins10_finCTT=col10-finCTT2;

run;

data Fins_contrat_2019_cas_1 (keep=id_midas L_contrat_sqn siret_final DebutCTT finctt Nature DispPolitiquePublique MotifRupture);
set fins_contrat_2019_2;
where (diff_datins1_finCTT>=0 & diff_datins1_finCTT<=90) ! (diff_datins2_finCTT>=0 & diff_datins2_finCTT<=90) ! (diff_datins3_finCTT>=0 & diff_datins3_finCTT<=90)
! (diff_datins4_finCTT>=0 & diff_datins4_finCTT<=90) ! (diff_datins5_finCTT>=0 & diff_datins5_finCTT<=90) ! (diff_datins6_finCTT>=0 & diff_datins6_finCTT<=90)
! (diff_datins7_finCTT>=0 & diff_datins7_finCTT<=90) ! (diff_datins8_finCTT>=0 & diff_datins8_finCTT<=90) ! (diff_datins9_finCTT>=0 & diff_datins9_finCTT<=90)
! (diff_datins10_finCTT>=0 & diff_datins10_finCTT<=90);
run;


/* 3. Cas 2: Sélection parmi les fins de contrats éligibles et n'ayant pas donné lieu à 1 inscription dans les 3 mois (cas 1), celles qui sont 
intervenues alors que l'ancien salarié/intérimaire était déjà inscrit à France Travail */

/* sélection des fins de contrat éligibles au BM mais n'ayant pas donné lieu à une inscription à France Travail dans les 3 mois (comme dans le cas 1 ci_dessus)*/
proc sql;
create table Fins_contrat_2019_non_cas_1 as
select *
from fins_contrat_2019 
where L_contrat_sqn  not in (select L_contrat_sqn from Fins_contrat_2019_cas_1)
;
quit;

/* sélection des inscriptions à France Travail intervenues avant le 12/12/2019(fin de l'année civile 2019)
(et n'ayant pas été clôturées avant le 01/01/22) */
proc sql;
create table inscrip_PE_cas_2 as
select id_midas,ndem,datins,motins,motins_a,datann
from Midasfh.De_tempo
where datins <="31DEC2019"d and ((datann=.) ! (datann>"31DEC2019"d))
order by id_midas asc,ndem desc;
quit;

/* sélection des fins de contrat pour lesquelles l'ancien salarié/intérimaire est inscrit dans cette liste inscrip_PE_cas_2*/
proc sql;
create table Fins_contrat_2019_non_cas_1_2 as
select *
from Fins_contrat_2019_non_cas_1 
where id_midas  in (select id_midas from inscrip_PE_cas_2)
;
quit;

proc sort data=inscrip_PE_cas_2;
by id_midas;
run;

data inscrip_PE_cas_2_2 (keep=id_midas datins);
set inscrip_PE_cas_2;
run;

proc transpose data=inscrip_PE_cas_2_2 out=datins_cas2;
by id_midas;
var datins;
run;

data datins_cas2 (keep=id_midas col1-col2);
set datins_cas2;
run;

proc sql;
create table Fins_contrat_2019_non_cas_1_2 as
select *
from Fins_contrat_2019_non_cas_1_2 as a
left join datins_cas2 as b
on a.id_midas=b.id_midas;
quit;

data Fins_contrat_2019_non_cas_1_2;
set Fins_contrat_2019_non_cas_1_2;
finCTT2=input(finCTT,yymmdd10.);
format finCTT2 yymmdd10.;
diff_datins1_finCTT=col1-finCTT2;
diff_datins2_finCTT=col2-finCTT2;
run;

/* sélection des fins de contrat intervenues alors que l'ancien salarié/intérimaire était déjà inscrit à France Travail*/
data Fins_contrat_2019_cas_2 (keep=id_midas L_contrat_sqn siret_final DebutCTT finctt Nature DispPolitiquePublique MotifRupture);
set Fins_contrat_2019_non_cas_1_2;
where (diff_datins1_finCTT<0) ! (diff_datins2_finCTT<0);
run;

/* 4. concaténation des 2 listes des fins de contrats éligibles au BM retenues dans le calcul du taux de séparation (cas 1 et 2) */

data Fins_contrat_2019_cas_1et2;
set Fins_contrat_2019_cas_1 Fins_contrat_2019_cas_2;
run;

/* 5. Calcul du nombre de séparations éligibles au bonus/malus par établissement (SIRET): */

proc sort data=Fins_contrat_2019_cas_1et2;
by siret_final;
run;

data Fins_contrat_2019_cas_1et2;
set Fins_contrat_2019_cas_1et2;
compteur=1;
run;

proc means data=Fins_contrat_2019_cas_1et2 noprint missing;
class siret_final;
var compteur;
output out=nb_separations_etab_2019 sum(compteur)=nb_separations;
run;

data Mybases.nb_separations_etab_2019 (keep=siret_final nb_separations);
set nb_separations_etab_2019;
if siret_final="" then delete;
run;

/* 6. Calcul du nombre de séparations éligibles au bonus/malus par unité légale (SIREN): */

data nb_separations_etab_2019 (rename=(siret_final=siret));
set Mybases.nb_separations_etab_2019;
siren=substr(siret_final,1,9);
run;

data nb_separations_etab_2019;
retain siren siret nb_separations;
set nb_separations_etab_2019;
run;

proc sort data=nb_separations_etab_2019;
by siren siret;
run;

data nb_separations_etab_2019;
set nb_separations_etab_2019;
if (siren=siret) then delete;
run;

proc means data=nb_separations_etab_2019 noprint missing;
class siren;
var nb_separations;
output out=nb_separations_ul_2019 sum(nb_separations)=nb_separations;
run;

data nb_separations_ul_2019;
set nb_separations_ul_2019;
if siren='' then delete;
drop _type_;
rename _freq_=nb_etab;
run;

data Mybases.nb_sep_ent_2019;
set nb_separations_ul_2019;
run;



/* 2020 */

/* Fins de contrat enregistrées entre janvier 2020 et décembre 2020*/
proc sql;
create table fins_contrat_2020 as
select id_midas,L_contrat_sqn,siret_af,siret_ut,DebutCTT,finctt,Nature,DispPolitiquePublique,MotifRupture
from MidasMMO.Mmo_2_2020_m4
where Secteur_PUBLIC = 0 /* contrats dans des établissements du secteur privé */
and substr(finctt,1,7) in ("2020-01","2020-02","2020-03","2020-04","2020-05","2020-06","2020-07","2020-08","2020-09","2020-10","2020-11","2020-12") /* mois d'enregistrement des fins de contrat */
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
and MotifRupture not in ("066","059","081") /* Motifs de rupture de contrat explicitement exclus du bonus/malus:
059: démission (TODO: il faudrait ici n'exclure que les démissions de salarié en CDI)
066: décès du salarié
081: fin de contrat d'apprentissage
*/
order by id_midas,L_contrat_sqn
;
quit;

/* On crée une variable Siret_final qui correspond au numéro Siret de l'établissement d'affectation (Siret_AF) dans le cas où il
ne s'agit pas d'un contrat d'interim, et au numéro Siret de l'établissement utilisateur dans le cas d'un contrat d'interim. */

data fins_contrat_2020;
set fins_contrat_2020;

if nature = "03" then do;
siret_final = siret_ut;
end;
else do;
siret_final = siret_af;
end;

run;

/* 2. Cas 1: Sélection parmi les fins de contrats éligibles, celles qui ont donné lieu à une inscription à Pôle Emploi dans les 3 mois suivant la fin de contrat:*/

/* 2020*/

proc sql;
create table inscrip_PE_2020 as
select id_midas,ndem,datins,motins,motins_a
from Midasfh.De_tempo
where datins >="01JAN2020"d and datins <="30MAR2021"d
order by id_midas asc,ndem desc;
quit;

proc sql;
create table fins_contrat_2020_2 as
select *
from fins_contrat_2020 
where id_midas  in (select id_midas from inscrip_PE_2020)
;
quit;

proc sort data=inscrip_PE_2020;
by id_midas;
run;

data inscrip_PE_2020_2 (keep=id_midas datins);
set inscrip_PE_2020;
run;

proc transpose data=inscrip_PE_2020_2 out=datins;
by id_midas;
var datins;
run;
data datins (keep=id_midas col1-col10);
set datins;
run;

proc sql;
create table fins_contrat_2020_2 as
select *
from fins_contrat_2020_2 as a
left join datins as b
on a.id_midas=b.id_midas;
quit;

data fins_contrat_2020_2;
set fins_contrat_2020_2;
finCTT2=input(finCTT,yymmdd10.);
format finCTT2 yymmdd10.;
diff_datins1_finCTT=col1-finCTT2;
diff_datins2_finCTT=col2-finCTT2;
diff_datins3_finCTT=col3-finCTT2;
diff_datins4_finCTT=col4-finCTT2;
diff_datins5_finCTT=col5-finCTT2;
diff_datins6_finCTT=col6-finCTT2;
diff_datins7_finCTT=col7-finCTT2;
diff_datins8_finCTT=col8-finCTT2;
diff_datins9_finCTT=col9-finCTT2;
diff_datins10_finCTT=col10-finCTT2;

run;

data Fins_contrat_2020_cas_1 (keep=id_midas L_contrat_sqn siret_final DebutCTT finctt Nature DispPolitiquePublique MotifRupture);
set fins_contrat_2020_2;
where (diff_datins1_finCTT>=0 & diff_datins1_finCTT<=90) ! (diff_datins2_finCTT>=0 & diff_datins2_finCTT<=90) ! (diff_datins3_finCTT>=0 & diff_datins3_finCTT<=90)
! (diff_datins4_finCTT>=0 & diff_datins4_finCTT<=90) ! (diff_datins5_finCTT>=0 & diff_datins5_finCTT<=90) ! (diff_datins6_finCTT>=0 & diff_datins6_finCTT<=90)
! (diff_datins7_finCTT>=0 & diff_datins7_finCTT<=90) ! (diff_datins8_finCTT>=0 & diff_datins8_finCTT<=90) ! (diff_datins9_finCTT>=0 & diff_datins9_finCTT<=90)
! (diff_datins10_finCTT>=0 & diff_datins10_finCTT<=90);
run;


/* 3. Cas 2: Sélection parmi les fins de contrats éligibles et n'ayant pas donné lieu à 1 inscription dans les 3 mois (cas 1), celles qui sont 
intervenues alors que l'ancien salarié/intérimaire était déjà inscrit à France Travail */

/* sélection des fins de contrat éligibles au BM mais n'ayant pas donné lieu à une inscription à France Travail dans les 3 mois (comme dans le cas 1 ci_dessus)*/
proc sql;
create table Fins_contrat_2020_non_cas_1 as
select *
from fins_contrat_2020 
where L_contrat_sqn  not in (select L_contrat_sqn from Fins_contrat_2020_cas_1)
;
quit;

/* sélection des inscriptions à France Travail intervenues avant le 12/12/2020(fin de l'année civile 2020)
(et n'ayant pas été clôturées avant le 01/01/22) */
proc sql;
create table inscrip_PE_cas_2 as
select id_midas,ndem,datins,motins,motins_a,datann
from Midasfh.De_tempo
where datins <="31DEC2020"d and ((datann=.) ! (datann>"31DEC2020"d))
order by id_midas asc,ndem desc;
quit;

/* sélection des fins de contrat pour lesquelles l'ancien salarié/intérimaire est inscrit dans cette liste inscrip_PE_cas_2*/
proc sql;
create table Fins_contrat_2020_non_cas_1_2 as
select *
from Fins_contrat_2020_non_cas_1 
where id_midas  in (select id_midas from inscrip_PE_cas_2)
;
quit;

proc sort data=inscrip_PE_cas_2;
by id_midas;
run;

data inscrip_PE_cas_2_2 (keep=id_midas datins);
set inscrip_PE_cas_2;
run;

proc transpose data=inscrip_PE_cas_2_2 out=datins_cas2;
by id_midas;
var datins;
run;

data datins_cas2 (keep=id_midas col1-col2);
set datins_cas2;
run;

proc sql;
create table Fins_contrat_2020_non_cas_1_2 as
select *
from Fins_contrat_2020_non_cas_1_2 as a
left join datins_cas2 as b
on a.id_midas=b.id_midas;
quit;

data Fins_contrat_2020_non_cas_1_2;
set Fins_contrat_2020_non_cas_1_2;
finCTT2=input(finCTT,yymmdd10.);
format finCTT2 yymmdd10.;
diff_datins1_finCTT=col1-finCTT2;
diff_datins2_finCTT=col2-finCTT2;
run;

/* sélection des fins de contrat intervenues alors que l'ancien salarié/intérimaire était déjà inscrit à France Travail*/
data Fins_contrat_2020_cas_2 (keep=id_midas L_contrat_sqn siret_final DebutCTT finctt Nature DispPolitiquePublique MotifRupture);
set Fins_contrat_2020_non_cas_1_2;
where (diff_datins1_finCTT<0) ! (diff_datins2_finCTT<0);
run;

/* 4. concaténation des 2 listes des fins de contrats éligibles au BM retenues dans le calcul du taux de séparation (cas 1 et 2) */

data Fins_contrat_2020_cas_1et2;
set Fins_contrat_2020_cas_1 Fins_contrat_2020_cas_2;
run;

/* 5. Calcul du nombre de séparations éligibles au bonus/malus par établissement (SIRET): */

proc sort data=Fins_contrat_2020_cas_1et2;
by siret_final;
run;

data Fins_contrat_2020_cas_1et2;
set Fins_contrat_2020_cas_1et2;
compteur=1;
run;

proc means data=Fins_contrat_2020_cas_1et2 noprint missing;
class siret_final;
var compteur;
output out=nb_separations_etab_2020 sum(compteur)=nb_separations;
run;

data Mybases.nb_separations_etab_2020 (keep=siret_final nb_separations);
set nb_separations_etab_2020;
if siret_final="" then delete;
run;

/* 6. Calcul du nombre de séparations éligibles au bonus/malus par unité légale (SIREN): */

data nb_separations_etab_2020 (rename=(siret_final=siret));
set Mybases.nb_separations_etab_2020;
siren=substr(siret_final,1,9);
run;

data nb_separations_etab_2020;
retain siren siret nb_separations;
set nb_separations_etab_2020;
run;

proc sort data=nb_separations_etab_2020;
by siren siret;
run;

data nb_separations_etab_2020;
set nb_separations_etab_2020;
if (siren=siret) then delete;
run;

proc means data=nb_separations_etab_2020 noprint missing;
class siren;
var nb_separations;
output out=nb_separations_ul_2020 sum(nb_separations)=nb_separations;
run;

data nb_separations_ul_2020;
set nb_separations_ul_2020;
if siren='' then delete;
drop _type_;
rename _freq_=nb_etab;
run;

data Mybases.nb_sep_ent_2020;
set nb_separations_ul_2020;
run;






/* 2021 */

/* Fins de contrat enregistrées entre janvier 2021 et décembre 2021*/
proc sql;
create table fins_contrat_2021 as
select id_midas,L_contrat_sqn,siret_af,siret_ut,DebutCTT,finctt,Nature,DispPolitiquePublique,MotifRupture
from MidasMMO.Mmo_2_2021_m4
where Secteur_PUBLIC = 0 /* contrats dans des établissements du secteur privé */
and substr(finctt,1,7) in ("2021-01","2021-02","2021-03","2021-04","2021-05","2021-06","2021-07","2021-08","2021-09","2021-10","2021-11","2021-12") /* mois d'enregistrement des fins de contrat */
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
and MotifRupture not in ("066","059","081") /* Motifs de rupture de contrat explicitement exclus du bonus/malus:
059: démission (TODO: il faudrait ici n'exclure que les démissions de salarié en CDI)
066: décès du salarié
081: fin de contrat d'apprentissage
*/
order by id_midas,L_contrat_sqn
;
quit;

/* On crée une variable Siret_final qui correspond au numéro Siret de l'établissement d'affectation (Siret_AF) dans le cas où il
ne s'agit pas d'un contrat d'interim, et au numéro Siret de l'établissement utilisateur dans le cas d'un contrat d'interim. */

data fins_contrat_2021;
set fins_contrat_2021;

if nature = "03" then do;
siret_final = siret_ut;
end;
else do;
siret_final = siret_af;
end;

run;

/* 2. Cas 1: Sélection parmi les fins de contrats éligibles, celles qui ont donné lieu à une inscription à Pôle Emploi dans les 3 mois suivant la fin de contrat:*/

/* 2021*/

proc sql;
create table inscrip_PE_2021 as
select id_midas,ndem,datins,motins,motins_a
from Midasfh.De_tempo
where datins >="01JAN2021"d and datins <="30MAR2022"d
order by id_midas asc,ndem desc;
quit;

proc sql;
create table fins_contrat_2021_2 as
select *
from fins_contrat_2021 
where id_midas  in (select id_midas from inscrip_PE_2021)
;
quit;

proc sort data=inscrip_PE_2021;
by id_midas;
run;

data inscrip_PE_2021_2 (keep=id_midas datins);
set inscrip_PE_2021;
run;

proc transpose data=inscrip_PE_2021_2 out=datins;
by id_midas;
var datins;
run;
data datins (keep=id_midas col1-col10);
set datins;
run;

proc sql;
create table fins_contrat_2021_2 as
select *
from fins_contrat_2021_2 as a
left join datins as b
on a.id_midas=b.id_midas;
quit;

data fins_contrat_2021_2;
set fins_contrat_2021_2;
finCTT2=input(finCTT,yymmdd10.);
format finCTT2 yymmdd10.;
diff_datins1_finCTT=col1-finCTT2;
diff_datins2_finCTT=col2-finCTT2;
diff_datins3_finCTT=col3-finCTT2;
diff_datins4_finCTT=col4-finCTT2;
diff_datins5_finCTT=col5-finCTT2;
diff_datins6_finCTT=col6-finCTT2;
diff_datins7_finCTT=col7-finCTT2;
diff_datins8_finCTT=col8-finCTT2;
diff_datins9_finCTT=col9-finCTT2;
diff_datins10_finCTT=col10-finCTT2;

run;

data Fins_contrat_2021_cas_1 (keep=id_midas L_contrat_sqn siret_final DebutCTT finctt Nature DispPolitiquePublique MotifRupture);
set fins_contrat_2021_2;
where (diff_datins1_finCTT>=0 & diff_datins1_finCTT<=90) ! (diff_datins2_finCTT>=0 & diff_datins2_finCTT<=90) ! (diff_datins3_finCTT>=0 & diff_datins3_finCTT<=90)
! (diff_datins4_finCTT>=0 & diff_datins4_finCTT<=90) ! (diff_datins5_finCTT>=0 & diff_datins5_finCTT<=90) ! (diff_datins6_finCTT>=0 & diff_datins6_finCTT<=90)
! (diff_datins7_finCTT>=0 & diff_datins7_finCTT<=90) ! (diff_datins8_finCTT>=0 & diff_datins8_finCTT<=90) ! (diff_datins9_finCTT>=0 & diff_datins9_finCTT<=90)
! (diff_datins10_finCTT>=0 & diff_datins10_finCTT<=90);
run;


/* 3. Cas 2: Sélection parmi les fins de contrats éligibles et n'ayant pas donné lieu à 1 inscription dans les 3 mois (cas 1), celles qui sont 
intervenues alors que l'ancien salarié/intérimaire était déjà inscrit à France Travail */

/* sélection des fins de contrat éligibles au BM mais n'ayant pas donné lieu à une inscription à France Travail dans les 3 mois (comme dans le cas 1 ci_dessus)*/
proc sql;
create table Fins_contrat_2021_non_cas_1 as
select *
from fins_contrat_2021 
where L_contrat_sqn  not in (select L_contrat_sqn from Fins_contrat_2021_cas_1)
;
quit;

/* sélection des inscriptions à France Travail intervenues avant le 12/12/2021(fin de l'année civile 2021)
(et n'ayant pas été clôturées avant le 01/01/22) */
proc sql;
create table inscrip_PE_cas_2 as
select id_midas,ndem,datins,motins,motins_a,datann
from Midasfh.De_tempo
where datins <="31DEC2021"d and ((datann=.) ! (datann>"31DEC2021"d))
order by id_midas asc,ndem desc;
quit;

/* sélection des fins de contrat pour lesquelles l'ancien salarié/intérimaire est inscrit dans cette liste inscrip_PE_cas_2*/
proc sql;
create table Fins_contrat_2021_non_cas_1_2 as
select *
from Fins_contrat_2021_non_cas_1 
where id_midas  in (select id_midas from inscrip_PE_cas_2)
;
quit;

proc sort data=inscrip_PE_cas_2;
by id_midas;
run;

data inscrip_PE_cas_2_2 (keep=id_midas datins);
set inscrip_PE_cas_2;
run;

proc transpose data=inscrip_PE_cas_2_2 out=datins_cas2;
by id_midas;
var datins;
run;

data datins_cas2 (keep=id_midas col1-col2);
set datins_cas2;
run;

proc sql;
create table Fins_contrat_2021_non_cas_1_2 as
select *
from Fins_contrat_2021_non_cas_1_2 as a
left join datins_cas2 as b
on a.id_midas=b.id_midas;
quit;

data Fins_contrat_2021_non_cas_1_2;
set Fins_contrat_2021_non_cas_1_2;
finCTT2=input(finCTT,yymmdd10.);
format finCTT2 yymmdd10.;
diff_datins1_finCTT=col1-finCTT2;
diff_datins2_finCTT=col2-finCTT2;
run;

/* sélection des fins de contrat intervenues alors que l'ancien salarié/intérimaire était déjà inscrit à France Travail*/
data Fins_contrat_2021_cas_2 (keep=id_midas L_contrat_sqn siret_final DebutCTT finctt Nature DispPolitiquePublique MotifRupture);
set Fins_contrat_2021_non_cas_1_2;
where (diff_datins1_finCTT<0) ! (diff_datins2_finCTT<0);
run;

/* 4. concaténation des 2 listes des fins de contrats éligibles au BM retenues dans le calcul du taux de séparation (cas 1 et 2) */

data Fins_contrat_2021_cas_1et2;
set Fins_contrat_2021_cas_1 Fins_contrat_2021_cas_2;
run;

/* 5. Calcul du nombre de séparations éligibles au bonus/malus par établissement (SIRET): */

proc sort data=Fins_contrat_2021_cas_1et2;
by siret_final;
run;

data Fins_contrat_2021_cas_1et2;
set Fins_contrat_2021_cas_1et2;
compteur=1;
run;

proc means data=Fins_contrat_2021_cas_1et2 noprint missing;
class siret_final;
var compteur;
output out=nb_separations_etab_2021 sum(compteur)=nb_separations;
run;

data Mybases.nb_separations_etab_2021 (keep=siret_final nb_separations);
set nb_separations_etab_2021;
if siret_final="" then delete;
run;

/* 6. Calcul du nombre de séparations éligibles au bonus/malus par unité légale (SIREN): */

data nb_separations_etab_2021 (rename=(siret_final=siret));
set Mybases.nb_separations_etab_2021;
siren=substr(siret_final,1,9);
run;

data nb_separations_etab_2021;
retain siren siret nb_separations;
set nb_separations_etab_2021;
run;

proc sort data=nb_separations_etab_2021;
by siren siret;
run;

data nb_separations_etab_2021;
set nb_separations_etab_2021;
if (siren=siret) then delete;
run;

proc means data=nb_separations_etab_2021 noprint missing;
class siren;
var nb_separations;
output out=nb_separations_ul_2021 sum(nb_separations)=nb_separations;
run;

data nb_separations_ul_2021;
set nb_separations_ul_2021;
if siren='' then delete;
drop _type_;
rename _freq_=nb_etab;
run;

data Mybases.nb_sep_ent_2021;
set nb_separations_ul_2021;
run;



/* 2022 */

/* Fins de contrat enregistrées entre janvier 2022 et décembre 2022*/
proc sql;
create table fins_contrat_2022 as
select id_midas,L_contrat_sqn,siret_af,siret_ut,DebutCTT,finctt,Nature,DispPolitiquePublique,MotifRupture
from MidasMMO.Mmo_2_2022_m4
where Secteur_PUBLIC = 0 /* contrats dans des établissements du secteur privé */
and substr(finctt,1,7) in ("2022-01","2022-02","2022-03","2022-04","2022-05","2022-06","2022-07","2022-08","2022-09","2022-10","2022-11","2022-12") /* mois d'enregistrement des fins de contrat */
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
and MotifRupture not in ("066","059","081") /* Motifs de rupture de contrat explicitement exclus du bonus/malus:
059: démission (TODO: il faudrait ici n'exclure que les démissions de salarié en CDI)
066: décès du salarié
081: fin de contrat d'apprentissage
*/
order by id_midas,L_contrat_sqn
;
quit;

/* On crée une variable Siret_final qui correspond au numéro Siret de l'établissement d'affectation (Siret_AF) dans le cas où il
ne s'agit pas d'un contrat d'interim, et au numéro Siret de l'établissement utilisateur dans le cas d'un contrat d'interim. */

data fins_contrat_2022;
set fins_contrat_2022;

if nature = "03" then do;
siret_final = siret_ut;
end;
else do;
siret_final = siret_af;
end;

run;

/* 2. Cas 1: Sélection parmi les fins de contrats éligibles, celles qui ont donné lieu à une inscription à Pôle Emploi dans les 3 mois suivant la fin de contrat:*/

/* 2022*/

proc sql;
create table inscrip_PE_2022 as
select id_midas,ndem,datins,motins,motins_a
from Midasfh.De_tempo
where datins >="01JAN2022"d and datins <="30MAR2023"d
order by id_midas asc,ndem desc;
quit;

proc sql;
create table fins_contrat_2022_2 as
select *
from fins_contrat_2022 
where id_midas  in (select id_midas from inscrip_PE_2022)
;
quit;

proc sort data=inscrip_PE_2022;
by id_midas;
run;

data inscrip_PE_2022_2 (keep=id_midas datins);
set inscrip_PE_2022;
run;

proc transpose data=inscrip_PE_2022_2 out=datins;
by id_midas;
var datins;
run;
data datins (keep=id_midas col1-col10);
set datins;
run;

proc sql;
create table fins_contrat_2022_2 as
select *
from fins_contrat_2022_2 as a
left join datins as b
on a.id_midas=b.id_midas;
quit;

data fins_contrat_2022_2;
set fins_contrat_2022_2;
finCTT2=input(finCTT,yymmdd10.);
format finCTT2 yymmdd10.;
diff_datins1_finCTT=col1-finCTT2;
diff_datins2_finCTT=col2-finCTT2;
diff_datins3_finCTT=col3-finCTT2;
diff_datins4_finCTT=col4-finCTT2;
diff_datins5_finCTT=col5-finCTT2;
diff_datins6_finCTT=col6-finCTT2;
diff_datins7_finCTT=col7-finCTT2;
diff_datins8_finCTT=col8-finCTT2;
diff_datins9_finCTT=col9-finCTT2;
diff_datins10_finCTT=col10-finCTT2;

run;

data Fins_contrat_2022_cas_1 (keep=id_midas L_contrat_sqn siret_final DebutCTT finctt Nature DispPolitiquePublique MotifRupture);
set fins_contrat_2022_2;
where (diff_datins1_finCTT>=0 & diff_datins1_finCTT<=90) ! (diff_datins2_finCTT>=0 & diff_datins2_finCTT<=90) ! (diff_datins3_finCTT>=0 & diff_datins3_finCTT<=90)
! (diff_datins4_finCTT>=0 & diff_datins4_finCTT<=90) ! (diff_datins5_finCTT>=0 & diff_datins5_finCTT<=90) ! (diff_datins6_finCTT>=0 & diff_datins6_finCTT<=90)
! (diff_datins7_finCTT>=0 & diff_datins7_finCTT<=90) ! (diff_datins8_finCTT>=0 & diff_datins8_finCTT<=90) ! (diff_datins9_finCTT>=0 & diff_datins9_finCTT<=90)
! (diff_datins10_finCTT>=0 & diff_datins10_finCTT<=90);
run;


/* 3. Cas 2: Sélection parmi les fins de contrats éligibles et n'ayant pas donné lieu à 1 inscription dans les 3 mois (cas 1), celles qui sont 
intervenues alors que l'ancien salarié/intérimaire était déjà inscrit à France Travail */

/* sélection des fins de contrat éligibles au BM mais n'ayant pas donné lieu à une inscription à France Travail dans les 3 mois (comme dans le cas 1 ci_dessus)*/
proc sql;
create table Fins_contrat_2022_non_cas_1 as
select *
from fins_contrat_2022 
where L_contrat_sqn  not in (select L_contrat_sqn from Fins_contrat_2022_cas_1)
;
quit;

/* sélection des inscriptions à France Travail intervenues avant le 12/12/2022(fin de l'année civile 2022)
(et n'ayant pas été clôturées avant le 01/01/22) */
proc sql;
create table inscrip_PE_cas_2 as
select id_midas,ndem,datins,motins,motins_a,datann
from Midasfh.De_tempo
where datins <="31DEC2022"d and ((datann=.) ! (datann>"31DEC2022"d))
order by id_midas asc,ndem desc;
quit;

/* sélection des fins de contrat pour lesquelles l'ancien salarié/intérimaire est inscrit dans cette liste inscrip_PE_cas_2*/
proc sql;
create table Fins_contrat_2022_non_cas_1_2 as
select *
from Fins_contrat_2022_non_cas_1 
where id_midas  in (select id_midas from inscrip_PE_cas_2)
;
quit;

proc sort data=inscrip_PE_cas_2;
by id_midas;
run;

data inscrip_PE_cas_2_2 (keep=id_midas datins);
set inscrip_PE_cas_2;
run;

proc transpose data=inscrip_PE_cas_2_2 out=datins_cas2;
by id_midas;
var datins;
run;

data datins_cas2 (keep=id_midas col1-col2);
set datins_cas2;
run;

proc sql;
create table Fins_contrat_2022_non_cas_1_2 as
select *
from Fins_contrat_2022_non_cas_1_2 as a
left join datins_cas2 as b
on a.id_midas=b.id_midas;
quit;

data Fins_contrat_2022_non_cas_1_2;
set Fins_contrat_2022_non_cas_1_2;
finCTT2=input(finCTT,yymmdd10.);
format finCTT2 yymmdd10.;
diff_datins1_finCTT=col1-finCTT2;
diff_datins2_finCTT=col2-finCTT2;
run;

/* sélection des fins de contrat intervenues alors que l'ancien salarié/intérimaire était déjà inscrit à France Travail*/
data Fins_contrat_2022_cas_2 (keep=id_midas L_contrat_sqn siret_final DebutCTT finctt Nature DispPolitiquePublique MotifRupture);
set Fins_contrat_2022_non_cas_1_2;
where (diff_datins1_finCTT<0) ! (diff_datins2_finCTT<0);
run;

/* 4. concaténation des 2 listes des fins de contrats éligibles au BM retenues dans le calcul du taux de séparation (cas 1 et 2) */

data Fins_contrat_2022_cas_1et2;
set Fins_contrat_2022_cas_1 Fins_contrat_2022_cas_2;
run;

/* 5. Calcul du nombre de séparations éligibles au bonus/malus par établissement (SIRET): */

proc sort data=Fins_contrat_2022_cas_1et2;
by siret_final;
run;

data Fins_contrat_2022_cas_1et2;
set Fins_contrat_2022_cas_1et2;
compteur=1;
run;

proc means data=Fins_contrat_2022_cas_1et2 noprint missing;
class siret_final;
var compteur;
output out=nb_separations_etab_2022 sum(compteur)=nb_separations;
run;

data Mybases.nb_separations_etab_2022 (keep=siret_final nb_separations);
set nb_separations_etab_2021;
if siret_final="" then delete;
run;

/* 6. Calcul du nombre de séparations éligibles au bonus/malus par unité légale (SIREN): */

data nb_separations_etab_2021 (rename=(siret_final=siret));
set Mybases.nb_separations_etab_2021;
siren=substr(siret_final,1,9);
run;

data nb_separations_etab_2021;
retain siren siret nb_separations;
set nb_separations_etab_2021;
run;

proc sort data=nb_separations_etab_2021;
by siren siret;
run;

data nb_separations_etab_2021;
set nb_separations_etab_2021;
if (siren=siret) then delete;
run;

proc means data=nb_separations_etab_2021 noprint missing;
class siren;
var nb_separations;
output out=nb_separations_ul_2021 sum(nb_separations)=nb_separations;
run;

data nb_separations_ul_2021;
set nb_separations_ul_2021;
if siren='' then delete;
drop _type_;
rename _freq_=nb_etab;
run;

data Mybases.nb_sep_ent_2021;
set nb_separations_ul_2021;
run;




