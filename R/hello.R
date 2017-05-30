# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Cmd + Shift + B'
#   Check Package:             'Cmd + Shift + E'
#   Test Package:              'Cmd + Shift + T'

#' Titol
#'
#' Encontrar Lista de Versiones encontradas de URLs de paises para sacar vulns de cada version
#'
#' @param ListaVersiones
#'
#' @return
#' @export
#'
#' @examples
cargaVulnerabilidades <- function(ListaVersiones) {
  AllVulns <- list()
  for(version in ListaVersiones){
    res <- getURL(url = paste("https://wpvulndb.com/api/v2/wordpresses/", version, sep=""))
    AllVulns <- tryCatch({
      append(AllVulns, fromJSON(res))
    }, error = function(err) {
      print(paste("ERROR in version", i,":",err))
    })
  }
  return(AllVulns)
}

#' Title
#'
#' @param TotalVersionesURLPaises
#'
#' @return
#' @export
#'
#' @examples
listarVersiones <- function(TotalVersionesURLPaises){
  ListaVersiones <- TotalVersionesURLPaises$NumVersion
  ListaVersiones <- as.character(ListaVersiones[!is.na(ListaVersiones)])
  ListaVersiones = sub("([[:punct:]])","",ListaVersiones)
  ListaVersiones = sub("([[:punct:]])","",ListaVersiones)
  ListaVersiones <- unique(ListaVersiones)
  return(ListaVersiones)
}

#' Title
#'
#' @param AllVulns
#' @param ListaVersiones
#'
#' @return
#' @export
#'
#' @examples
totalVulnerabilidades <- function(AllVulns, ListaVersiones){
  df_resum <- data.frame()
  df_vulns1version<-as.data.frame(AllVulns[[1]])
  df_total <- cbind(ReleaseVersion=as.character.Date(df_vulns1version$release_date[1]),VulnsVersion=length(df_vulns1version$release_date))
  df_total <- cbind(df_total,NumVersion=names(AllVulns[1]))
  for(i in 2:length(ListaVersiones)) {
    df_vulns1version<-as.data.frame(AllVulns[[i]])
    df_resum <- cbind(ReleaseVersion=as.character.Date(df_vulns1version$release_date[1]),VulnsVersion=length(df_vulns1version$release_date),NumVersion=names(AllVulns[i]))
    df_total<-rbind(df_total,df_resum)
  }

  TotalVulnsVersion<-as.data.frame(df_total)
  TotalVulnsVersion <- unique(TotalVulnsVersion)
  # Valores que son Factors los cambiamos para poder tratar y ordenar
  TotalVulnsVersion$VulnsVersion <- as.numeric(as.character(TotalVulnsVersion$VulnsVersion))
  TotalVulnsVersion$NumVersion <- as.character(TotalVulnsVersion$NumVersion)
  return(TotalVulnsVersion)
}

#' Title
#'
#' @param path
#'
#' @return
#' @export
#'
#' @examples
cargaUrls <- function(path){
  op <- options(gvis.plot.tag='chart')

  # Creando objeto json
  json_file <- fromJSON(path, flatten = TRUE )


  # Creando un Dataframe que contiene todas las URLs de todos los paises
  # con campo pais
  df_json <-data.frame(do.call("cbind",json_file$urls[[1]]))
  df_json_paises <-cbind(df_json,pais=names(json_file$urls[1]))

  for(i in 2:length(json_file$urls)) {

    df_json <-data.frame(do.call("cbind",json_file$urls[[i]]))
    df_json_pais <-cbind(df_json,pais=names(json_file$urls[i]))
    df_json_paises <- rbind(df_json_paises,df_json_pais)
  }



  # Eliminamos las duplicadas que llamaremos Deduplicado
  df_json_dedup <- unique(df_json_paises)


  # Cambiamos las versiones -1 por NA
  df_json_dedup$version[which(df_json_dedup$version==-1)]<-NA


  # Sumamos Numero de URLs de cada version
  TotalVersionesURLPaises <- count(df_json_dedup, pais, NumVersion = version)
  #is.data.frame(TotalVersionesURLPaises)
  TotalVersionesURLPaises$pais <- TotalVersionesURLPaises$pais %>% toupper

  ###
  # TotalVersionesURLPaises - Contiene la tabla de (pais, NumVersion, n)
  ###

  return(TotalVersionesURLPaises)
}

#' Title
#'
#' @param TotalVersionesURLPaises
#'
#' @return
#' @export
#'
#' @examples
GeneraTotalDF <- function(TotalVersionesURLPaises){
  #Tabla con todos los Datos acumulados

  TotalVersionesURLPaises_sinna <- TotalVersionesURLPaises[complete.cases(TotalVersionesURLPaises),]
  TotalVersionesURLVulnsPaises<-merge(TotalVersionesURLPaises_sinna, TotalVulnsVersion, by = "NumVersion")
  TotalVersionesURLVulnsPaises <- unique(TotalVersionesURLVulnsPaises)
  df_temporal <- tidyr::separate(TotalVersionesURLVulnsPaises, ReleaseVersion, c("YearReleaseVersion","MonthReleaseVersion","DayReleaseVersion"), sep = "-")

  TotalVersionesURLVulnsPaises <- cbind(TotalVersionesURLVulnsPaises,AñoPubVersion=as.numeric(df_temporal$YearReleaseVersion))
  return(TotalVersionesURLVulnsPaises)
}

#' Title
#'
#' @param TotalVersionesURLVulnsPaises
#'
#' @return
#' @export
#'
#' @examples
tarta <- function(TotalVersionesURLVulnsPaises){
  ResulVersionesMundo <- summarise(group_by(TotalVersionesURLVulnsPaises,NumVersion), TotalVerMundo = sum(VulnsVersion))
  ResulVersionesMundo <- ResulVersionesMundo[order(-ResulVersionesMundo$TotalVerMundo),]

  Pie <- gvisPieChart(ResulVersionesMundo,options=list(width="1000px", height="800px"))
  return(Pie)
}

#' Title
#'
#' @param TotalVersionesURLVulnsPaises
#'
#' @return
#' @export
#'
#' @examples
barras <- function(TotalVersionesURLVulnsPaises){
  ResulPaises <- summarise(group_by(TotalVersionesURLVulnsPaises,pais), TotalVulnsPais = sum(VulnsVersion))
  ResulPaises <- ResulPaises[order(-ResulPaises$TotalVulnsPais),]

  Column <- gvisColumnChart(ResulPaises)
  return(Column)
}

#' Title
#'
#' @param TotalVersionesURLVulnsPaises
#'
#' @return
#' @export
#'
#' @examples
relojes <- function(TotalVersionesURLVulnsPaises){
  ResulPaisesAño <- summarise(group_by(TotalVersionesURLVulnsPaises,pais,AñoPubVersion), TotalVulnsPais = sum(VulnsVersion))
  ResulPaisesAño <- ResulPaisesAño[order(-ResulPaisesAño$TotalVulnsPais),]
  ResulPaisesRiesgoAño <- mutate(ResulPaisesAño,Riesgo=(2017-AñoPubVersion)*TotalVulnsPais)
  ResulPaisesRiesgoTotal <- summarise(group_by(ResulPaisesRiesgoAño,pais), TotalRiesgo=sum(Riesgo))
  ResulPaisesRiesgoTotal <- ResulPaisesRiesgoTotal[order(-ResulPaisesRiesgoTotal$TotalRiesgo),]

  Media_Riesgo <- as.integer(mean(ResulPaisesRiesgoTotal$TotalRiesgo))
  Max_Riesgo <- max(ResulPaisesRiesgoTotal$TotalRiesgo)
  Naranja <- ((Max_Riesgo - Media_Riesgo)/2)+ Media_Riesgo
  Top <- 8
  Gauge <-  gvisGauge(head(ResulPaisesRiesgoTotal,n=Top),
                      options=list(min=0, max=Max_Riesgo, greenFrom=0,
                                   greenTo=Media_Riesgo, yellowFrom=Media_Riesgo, yellowTo=Naranja,
                                   redFrom=Naranja, redTo=Max_Riesgo, width=800, height=600))
  return(Gauge)
}

#' Title
#'
#' @param TotalVersionesURLVulnsPaises
#'
#' @return
#' @export
#'
#' @examples
mapa <- function(TotalVersionesURLVulnsPaises){
  ResulPaisesAño <- summarise(group_by(TotalVersionesURLVulnsPaises,pais,AñoPubVersion), TotalVulnsPais = sum(VulnsVersion))
  ResulPaisesAño <- ResulPaisesAño[order(-ResulPaisesAño$TotalVulnsPais),]
  ResulPaisesRiesgoAño <- mutate(ResulPaisesAño,Riesgo=(2017-AñoPubVersion)*TotalVulnsPais)
  ResulPaisesRiesgoTotal <- summarise(group_by(ResulPaisesRiesgoAño,pais), TotalRiesgo=sum(Riesgo))
  ResulPaisesRiesgoTotal <- ResulPaisesRiesgoTotal[order(-ResulPaisesRiesgoTotal$TotalRiesgo),]

  MapaRiesgo <- gvisGeoChart(ResulPaisesRiesgoTotal, locationvar = "pais", colorvar = "TotalRiesgo", options=list(width="900px", height="600px", region='150', colorAxis="{colors:['yellow', 'red']}",backgroundColor="lightblue"))
  return(MapaRiesgo)
}
