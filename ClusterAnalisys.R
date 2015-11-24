#' # Regional Clustering
#' ## Obiettivi della ricerca
#' 
#' L'obiettivo del documento e' trovare un insieme di regioni europee che si ritengono essere strutturalmente simili alla Provincia Autonoma di Trento su cui poi potersi poi confrontare su  indicatori obiettivo.
#' La metodologia proposta prevede la costruzione di cluster basati su indicatori "strutturali" ritenuti importanti e caratterizzanti della realta' provinciale.  
#' Gli indicatori che si e' pensato rappresentativi della situazione demografica e economica regionale sono:
#' * **Demografia**:
#'   - Popolazione media
#'   - Indice di vecchiaia
#'   - Eta' media
#'   - Superficie
#' * **Economia**:
#'   - Addetti nell'industria
#'   - Addetti nei servizi
#'   - Addetti totali
#'   - Valore aggiunto totale
#'   - Percentuale di valore aggiunto dall'agricoltura
#'   - Percentuale di valore aggiunto dall'industria
#'   - Percentuale di valore aggiunto dai servizi
#'   - Occuapti totali
#'   - Percentuale di occupati nell'agricoltura
#'   - Percentuale di occupati nell'industria
#'   - Percentuale di occupati nei servizi
#' * **Istruzione**:
#'   - NEET
#'   - Percentuale di persone 25-64 con almeno diploma superiore
#' * **Ricerca e innovazione**:
#'   - Percentuale di occupati in settori ad alto contenuto tecnologico o ad alta intensita' di conoscenza  
#'   
#' Poiche' la procedura di clustering non puo' lavorare con i valori NULL sono costretto a eliminare tutte le righe in cui compare un NA. 
#' In partioclare gli indicatori riguardanti gli addetti sono mancanti su molte regioni e interi stati, particolarmente REGNO UNITO, GERMANIA e BELGIO. Delle 272 regioni europee ne rimarebbero utilizzabili solo 136.
#' Considerata l'importanza di queste regioni ho ritenuto opportuno eliminare gli indicatori piuttosto che le righe.
#' 
#' ## Risultati
#' 
#' L'analisi ha portato a determinare un cluster di regioni europee caratterizzate da una dimensione medio-piccola, con popolazione più bassa della media. I territori selezionati si contraddistinguono inoltre per una predominanza del settore terziario a scapito del settore industriale e agricolo. Molto piu' basso della media europea e' il numero di NEET

#' ********
#' # Metodologia
#' ## Preparazione dataset
#'
#+ echo=FALSE, reults='hide', message=FALSE
library(dplyr)
library(cluster)
library(fpc)
library(rgl)
library(knitr)
library(xtable)
library(ggplot2)
source('RadialPlot.R')

knit_hooks$set(
    rgl = function(before, options, envir) {
        if (!before) {
            ## after a chunk has been evaluated
            if (rgl.cur() == 0)
                return()  # no active device
            name = paste(options$fig.path, options$label, sep = '')
            rgl.snapshot(paste(name, '.png', sep = ''), fmt = 'png')
            return(paste('<img src=', name, '.png />', sep = ''))
        }
    }
)

#' Leggo i dati preparati da Paolo nel file 'LASTVALUE.csv'
indOrig <-
    read.csv2(
        'LASTVALUE.csv', sep = ',', skip = 21, header = T, stringsAsFactors = F
    )

#' Il Dataset e' composto da 272 osservazioni su 18 variabili
#+ echo=F, results='hide'
indOrig <- select(indOrig, -contains('ADDIMP'), -contains('AGRI'))
summary(indOrig)
indOrig <- na.omit(indOrig)

#' Poiche' i valori fanno riferimento a fenomeni diversi tra loro standardizzo gli indicatori.
ind <- scale(indOrig[5:17])
row.names(ind) <- indOrig$GEO

#' ***
#' ## K-Medie

#' Il metodo di clustering delle K-Medie e' il piu' utilizzato. Richiede che l'analista specifici il numero di cluster da estrarre. Un grafico della somma dei quadrati all'interno dei gruppi per il numero di cluster estratti puo' aiutare a scegliere il numero piu' appropriato di cluster. L'analista decide la numerosita' di cluster individuando il punto in cui l'aggiunta di un cluster (asse orizzontale) non comporta un significativo aumento dell'informazione (asse verticale).

#' Tuttavia dopo approfondita analisi il metodo delle K-Medie non risulta adeguato nel caso specifico, in quanto i punti sono troppo ravvicinati tra loro, e non si conformano a dei cluster chiaramente separati, e quindi la procedura delle K-Medie non riesce a determinare i cluster in modo deterministico.
#' Il grafico di seguito mostra la rappresentazione dei punti su uno spazio tridemnsionale utlizzando le prime tre componenti principali.

#+ results='hide', echo=F
pcdf <- princomp(ind, cor = T, score = T) #Analisi componenti principali
summary(pcdf)
open3d(windowRect = c(50, 10, 1000, 1000))
plot3d(
    pcdf$scores, size = 1, type = 's', main = 'Grafico componenti principali'
)
text3d(
    pcdf$scores, texts = indOrig$GEO, cex = 0.8, adj = c(0.5, 2)
)


#+ fancy-rgl, rgl=T, echo=F
rgl.viewpoint(
    theta = 70, phi = 20, fov = 30, zoom = 0.8
)

#' ***
#' ## Metodo gerarchico

#' Si decide percio' di utilizzare un metodo di clustering gerarchico, che accoppia via via i punti piu' vicini nello spazio. Si rende pero' necessario decidere il tipo di distanza da utilizzare, il metodo di clusterizzazione e infine il numero di cluster.
#' Si sceglie di utilizzare la distanza massima per minimizzare le differenze; si sceglie cioe' come distanza tra due punti il massimo tra le distanze di ogni dimensione
#+ fig.width=15, fig.heigth=5
# Matrice delle distanze
d <- dist(ind, method = "maximum")
# Crea l'albero
fit <- hclust(d, method="ward.D2") 
# Disegna il grafico
plot(fit)
# Divide l'albero all'altezza di 20 cluster
group <- cutree(fit, k=20)
# Disegna i rettangoli rossi attorno ai 20 cluster
rect.hclust(fit, k=20, border="red")

#' ***
#' ## Analisi dei cluster
#' Trento (ITH2) risulta essere all'interno del cluster numero 1
#+ echo=F
indOrig <- data.frame(indOrig, group)
ind <- data.frame(ind, group)
indOrig$group[indOrig$GEO=='ITH2']
cluster <- filter(indOrig, group==indOrig$group[indOrig$GEO=='ITH2'])


#' Il cluster 2 risulta essere cosi' composto:
#+ echo=F, results='asis'
print(xtable(select(cluster, NUTS2=GEO, Denominazione=denoGEO, Stato=stato)), type='html', include.rownames = F)

avgs <- ind %>%
    group_by(group) %>%
    summarise_each(funs(mean))

avgs <- as.data.frame(avgs)
colnames(avgs) <- c('group', 'Popolazione media','Indice di vecchiaia', 'Eta\' media', 'Superficie', 'Occupati totali', 'Valore aggiunto totale', '% Occupati industria', '% Occupati Servizi', '% Valore aggiunto industria', '% Valore aggiunto servizi', 'NEET', ' % Titolo superiore', '% Occupati alta tecnologia')


#+ echo=F, results='asis'
CreateRadialPlot(filter(avgs, group == 1), grid.min = -2, centre.y = -3, grid.max = 2, background.circle.colour = 'white', group.line.width = 1, group.point.size = 3, grid.line.width = 0.2, gridline.mid.colour = 'grey')

CreateRadialPlot(filter(avgs, group %in% 2:6), grid.min = -2, centre.y = -3, grid.max = 2.5, background.circle.colour = 'white', group.line.width = 1, group.point.size = 3, grid.line.width = 0.2, gridline.mid.colour = 'grey')

CreateRadialPlot(filter(avgs, group %in% 7:11), grid.min = -2, centre.y = -3, grid.max = 4, background.circle.colour = 'white', group.line.width = 1, group.point.size = 3, grid.line.width = 0.2, gridline.mid.colour = 'grey')

CreateRadialPlot(filter(avgs, group %in% 12:15), grid.min = -3, centre.y = -5, grid.max = 10, background.circle.colour = 'white', group.line.width = 1, group.point.size = 3, grid.line.width = 0.2, gridline.mid.colour = 'grey')


# create a new empty object called 'temp' in which to store a zip file
# containing boundary data
temp <- tempfile(fileext = ".zip")
# now download the zip file from its location on the Eurostat website and
# put it into the temp object
download.file("http://ec.europa.eu/eurostat/cache/GISCO/geodatafiles/NUTS_2010_60M_SH.zip", temp)
# now unzip the boundary data
unzip(temp)

library(rgdal)
EU_NUTS <- readOGR(dsn = "./NUTS_2010_60M_SH/data", layer = "NUTS_RG_60M_2010")
EU_NUTS <- spTransform(EU_NUTS, CRS("+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +no_defs"))



ToRemove <- EU_NUTS@data$STAT_LEVL!=2 | grepl('FR9',EU_NUTS@data$NUTS_ID)
EU_NUTS <- EU_NUTS[!ToRemove,]
plot(EU_NUTS)
EU_NUTS@data = data.frame(EU_NUTS@data, indOrig[match(EU_NUTS@data$NUTS_ID, indOrig$GEO), ])

EU_NUTS <- EU_NUTS[!is.na(EU_NUTS@data$group),]

plot(EU_NUTS, col = rainbow(20)[EU_NUTS@data$group], add =T)
#plot(EU_NUTS2, add = T)

legend(x = 3750000, y = 11800000, legend=unique(EU_NUTS@data$group), fill=rainbow(20)[unique(EU_NUTS@data$group)])

