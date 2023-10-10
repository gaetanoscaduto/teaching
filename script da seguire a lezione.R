#VOI DOVETE PRIMA RUNNARE "install.packages("nome della libreria")
#prima di ognuna di queste la prima volta che usate queste liberie!

#SOLO LA PRIMA VOLTA

install.packages("rio")
install.packages("ggplot2")
install.packages("texreg")


library(rio)
library(ggplot2)
library(texreg)

options(scipen = 999) #no notazione scientifica

#Carichiamo il dataset
########## 
setwd("your/path/here") #qui dovete copia

data = import("ris2022_lezione.RDS")

#Siccome non voglio dati incompleti, seleziono solo
#le osservazioni complete
data = data[complete.cases(data), ]


head(data) #Sbirciamo nel nostro dataset

View(data) #diamo un'occhiata meglio

#Facciamo un po' di statistiche descrittive! 
#################

summary(data) #L'equivalente di browse in stata

#Vediamo un po' di statistiche univariate

mean(data$num_residenti)

median(data$num_residenti)

max(data$M5S)

min(data$superficie)
#La media dei residenti in Lombardia e' diversa da quella in Molise?

mean(data[data$regione == "Lombardia", ]$num_residenti)
mean(data[data$regione == "Molise", ]$num_residenti)

#Nota: volete vedere quanto han preso i partiti nel vostro comune? 
#Seguite la prossima riga

data[data$comune == "MILANO", ]

#Beh, sembra tutto a posto! 

#Vediamo una tabella univariata

table(data$areeint)

#Vediamo un esempio di statistica bivariata

table(data$macroarea, data$areeint)

#Beh, un po' difficile da interpretare cosi'

prop.table(table(data$macroarea, data$areeint))

#un po' bruttina da leggere...

round(prop.table(table(data$macroarea, data$areeint)), digits = 2)


#Ma io voglio una tavola di contingenza per colonna, così non si capisce niente!

round(prop.table(table(data$macroarea, data$areeint), 1), digits = 2)

#Anzi le voglio per riga!

round(prop.table(table(data$macroarea, data$areeint), 2), digits = 2)

############# GRAFICI

#Qui usiamo la library ggplot2

#Proviamo a fare un semplice boxplot della percentuale di voti presa da PD

ggplot(data, aes(x=PD))+
  geom_density()


#broviamo a fare un barplot sul partito piÃ¹ votato su tutti i comuni!

ggplot(data, aes(x=most_voted))+
  geom_bar()

#Così triste! Facciamolo colorato!


ggplot(data, aes(x=most_voted))+
  geom_bar(aes(fill=most_voted))


#Vediamo se c'è una correlazione fra il voto al pd ed il numero di residenti 
#(partito della ztl)


ggplot(data, aes(x=PD, y=num_residenti))+
  geom_point()

#azz... non si vede molto...


ggplot(data, aes(x=PD, y=log(num_residenti)))+
  geom_point()

#trucchetto per vedere meglio...


ggplot(data, aes(x=PD, y=log(num_residenti)))+
  geom_point(alpha = 1/10)

#Proviamo a metterci una linea vediamo che succede
ggplot(data, aes(x=PD, y=log(num_residenti)))+
  geom_point()+
  geom_smooth()

#Sembrerebbe proprio di sì! Se voglio la retta di regressione...

ggplot(data, aes(x=PD, y=log(num_residenti)))+
  geom_point(alpha=1/10)+
  geom_smooth(method="lm")




#Vogliamo fare qualche altro grafico? Sono aperto a richeste! Improvvisiamo!

########### Infine, vediamo un po' di regressioni! 

regr1 = lm(data=data, M5S ~ macroarea)
screenreg(regr1)

regr2 = lm(data=data, M5S ~ macroarea + areeint)
screenreg(regr2)

#Notate che se avessimo fatto prima aree interne...

regr1bis = lm(data=data, M5S ~ areeint)
screenreg(regr1bis)

#Che secondo me ci dice qualcosa sul confounding (anche se la causalità 
#non è banale qui)...


regr3 = lm(data=data, M5S ~ macroarea + areeint + num_residenti + superficie)
screenreg(regr3)


