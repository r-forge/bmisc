#--------------------------------------------------------------------------------#
#                            Importation des données                             #
#--------------------------------------------------------------------------------#
Data <- file.path("I:/2011/DArchambault/donnees/CSV/")  # Écrire l'arborescence du répertoire
Fct <- file.path("I:/2011/DArchambault/fonctions R/")
reps=c('COM','PSM','NGSL','SGSL','GADUS','HAMMOND')
Fig <- rep(file.path("I:/2011/DArchambault/figures/PDF/"),length(reps))  # Écrire l'arborescence du répertoire
Fig=paste(Fig,reps,'/',sep='')


psm=read.csv(paste(Data,'Gulf halibut-releve PSM-Nord du Golfe-cbio-Bruno-16 juin.csv', sep=''))
sgsl=read.csv(paste(Data,'Gulf halibut-releve MPO SGSL-sud Golfe-cbio-Bruneau-16 juin.csv', sep=''))
ngsl=read.csv(paste(Data,'Gulf halibut-releve NGSL-Nord du Golfe-cbio-Bruno-16 juin.csv', sep=''))
com=read.csv(paste(Data,'Donnees age-commercial 1990_2005-juin 2011 bruneau-DA 16 juin.csv', sep=''))
#com=read.csv(paste(Data,'Donnees age-commercial 1990_2005-juin 2011 bruneau.csv', sep=''))
ham=read.csv(paste(Data,'Donnees age-recherche Hammond-1987_1990-juin 2011-bruneau.csv', sep=''))
gad=read.csv(paste(Data,'Donnees age-recherche Gadus-1983_1994-juin 2011-bruneau.csv', sep=''))

n.psm=names(psm)
n.sgsl=names(sgsl)
n.ngsl=names(ngsl)
n.com=names(com)
n.ham=names(ham)
n.gad=names(gad)

psm$no_rel[is.na(psm$no_rel)]=99
sgsl$no_rel[is.na(sgsl$no_rel)]=99
ngsl$no_rel[is.na(ngsl$no_rel)]=99
ham$no_rel[is.na(ham$no_rel)]=99
gad$no_rel[is.na(gad$no_rel)]=99
psm$no_rel [psm$no_rel ==99]=0
sgsl$no_rel[sgsl$no_rel==99]=0
ngsl$no_rel[ngsl$no_rel==99]=0
ham$no_rel [ham$no_rel ==99]=0
gad$no_rel [gad$no_rel ==99]=0

