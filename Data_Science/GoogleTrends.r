#   __      _____             _              __        _       
# <(o )___ |  _  |___ ___ ___| |_ ___ ___   |  |   ___| |_ ___ 
# ( ._> /  |   __|  _| -_|_ -|  _| -_|_ -|  |  |__| .'| . |_ -|
#  `---'   |__|  |_| |___|___|_| |___|___|  |_____|__,|___|___|
#==============================================================================
# Google Trends and R
#==============================================================================
# Title          : GoogleTrendsTS.r
# Description    : Analyzing Google Trends Data in R.
# Author         : Isaias V. Prestes <isaias.prestes@gmail.com>
# Date           : 20180412
# Version        : 0.0.1
# Usage          : Run in R 3.4
# Notes          : based on 
#                  Jake Hoare (DisplayR)
# https://datascienceplus.com/analyzing-google-trends-data-in-r/
#                  gtrendsR documentation
# https://github.com/PMassicotte/gtrendsR
#                  Peer Christensen https://rpubs.com/PChristensen/307008
#
# R version      : 3.4
#==============================================================================

#==============================================================================
# PACKAGE INSTALATION
#==============================================================================
if(!require("devtools")) install.packages("devtools")
if(!require("gtrendsR")) install.packages("gtrendsR") # google data
# devtools::install_github('PMassicotte/gtrendsR', ref = 'new-api')
if(!require("ggplot2")) install.packages("ggplot2")
if(!require("forecast")) install.packages("forecast") # time series
if(!require("maps")) install.packages("maps") # map plots
if(!require("reshape2")) install.packages("reshape2")
if(!require("xxxxx")) install.packages("xxxxx")
if(!require("xxxxx")) install.packages("xxxxx")

#==============================================================================
# LIBRARY DEPENDENCE
#==============================================================================
library(devtools)
library(gtrendsR)
library(reshape2)
library(ggplot2)
library(forecast)
library(xxxxx)
library(xxxxx)
library(xxxxx)
library(xxxxx)

# Things that I like...
Sys.setenv(TZ = "UTC")
google.trends = gtrends(c("Fortran","Sudoku"), gprop = "web", time = "all")
plot(google.trends)
google.trends = gtrends(c("Fortran","Sudoku"), gprop = "web", time =  "today+5-y")
plot(google.trends)
google.trends = gtrends(c("Fortran"), gprop = "web", time =  "today+5-y")
windows() ; plot(google.trends)
google.trends = gtrends(c("Sudoku"), gprop = "web", time =  "today+5-y")
windows() ; plot(google.trends)

#==============================================================================
# Fire in the hole!
#==============================================================================

# Search Parameters -----------------------------------------------------------
search_terms <- c("Machine Learning", "Big Data", "R programming language", "Statitics")
# Countries codes https://en.wikipedia.org/wiki/ISO_3166-2
# detaild version http://www.unece.org/cefact/codesfortrade/codes_index.html
search_country <- c("BR") # default is worldwide
# Time interval - Time span between two dates (ex.: "2010-01-01 2010-04-03")
search_time <- "2004-01-01 2018-01-04" 
# Options available are "news","images", "froogle", "youtube"
search_type <- c("web") 
# A character denoting the category, defaults to “0”.
search_category <- 0
# ISO language code
search_language <- "pt-BR"

data_scientist <- 
gtrends(search_terms, geo = search_country, time = search_time,
    gprop = search_type, category = search_category, hl = search_language)

## Playing with time format
search_terms <- c("Python", "Machine Learning")
gtrends(search_terms, time = "now 1-H") # Last hour
gtrends(search_terms, time = "now 4-H") # Last four hours
gtrends(search_terms, time = "now 1-d") # Last day
gtrends(search_terms, time = "now 7-d") # Last seven days
gtrends(search_terms, time = "today 1-m") # last 30 days
gtrends(search_terms, time = "today 3-m") # last 90 days
gtrends(search_terms, time = "today 12-m") # last 12 months
gtrends(search_terms, time = "today+5-y") # last 5 years (default)
gtrends(search_terms, time = "all") # Since the beginning of Google Trends (2004)

data_scientist$interest_over_time%>%
  as_tibble()%>%
  mutate(hits=as.numeric(hits))%>%
ggplot(aes(x=date,y=hits,colour=keyword))+
  geom_line()+theme_bw()+
  labs(y="Popularidade",x="Ano",colour="Termo de pesquisa:", title="Série temporal da popularidade dos termos de pesquisas")

# Gtrends function actually returns a list of data frames.
str(hurricanes)  
  
  
  
  
  
  
#==============================================================================
# Time series analysis
#==============================================================================

#Decompondo a série para o termo Estatística
ts_Estatistica=data_scientist$interest_over_time%>%
  as_tibble()%>%
  mutate(hits=as.numeric(hits))%>%
  filter(keyword=="Estatística")%>%
  select(hits)%>%
  na.omit()%>%
  ts(freq=12)

ts_dec_Estatistica=decompose(ts_Estatistica)

#Decompondo a série para o termo Bigdata
ts_BigData=data_scientist$interest_over_time%>%
  as_tibble()%>%
  mutate(hits=as.numeric(hits))%>%
  filter(keyword=="Big Data")%>%
  select(hits)%>%
  na.omit()%>%
  ts(freq=12)
ts_dec_BigData=decompose(ts_BigData)

g1=ts_dec_Estatistica %>% autoplot+ theme_bw()
g2=ts_dec_BigData %>% autoplot+theme_bw()
gridExtra::grid.arrange(g1,g2,ncol=2)

# Trend test (Wald-Wolfowitz)
randtests::runs.test(ts_Estatistica)
randtests::runs.test(ts_BigData)

# SAZONALIDADE

data=data.frame(cbind(serie=as.numeric(ts_Estatistica),mes_ano=rep(seq(1,12),11)))
kruskal.test(data=data,serie~mes_ano) 
data=data.frame(cbind(serie=as.numeric(ts_BigData),mes_ano=rep(seq(1,12),11)))
kruskal.test(data=data,serie~mes_ano) 

# MÉTODO DE AMORTECIMENTO EXPONENCIAL DE HOLT-WINTERS (DADOS COM SAZONALIDADE)

ajuste_com_sazonalidade_Estatistica<-HoltWinters(ts_Estatistica)
plot(ajuste_com_sazonalidade_Estatistica)

ajuste_com_sazonalidade_BigData<-HoltWinters(ts_BigData)
plot(ajuste_com_sazonalidade_BigData)

suppressMessages(library(forecast))

#Para estatistica
previsao_com_sazonalidade_Estatistica<-forecast(ajuste_com_sazonalidade_Estatistica,h = 12)
plot(previsao_com_sazonalidade_Estatistica)

#Para BigData
previsao_com_sazonalidade_BigData<-forecast(ajuste_com_sazonalidade_BigData,h = 12)
plot(previsao_com_sazonalidade_BigData)




#==============================================================================
# Map plot
#==============================================================================
Fortran = gtrends(c("Fortran"), gprop = "web",time="2017-08-18 2017-08-25", geo = c("US"))
Fortran = Fortran$interest_by_region
Fortran$region = sapply(Fortran$location,tolower)
statesMap = map_data("state")
FortranMerged = merge(statesMap,Fortran,by="region")
FortranPlot=ggplot() +
  geom_polygon(data=FortranMerged,aes(x=long,y=lat,group=group,fill=hits),colour="white") +
  scale_fill_continuous(low="thistle2",high="darkred",guide="colorbar",trans="log10") +
  theme_bw() +
  labs(title="Google search interest for Fortran in each state")
FortranPlot


Fortran = gtrends(c("Fortran"), gprop = "web", time="2018-03-03 2018-04-03",geo = c("US"))
Fortran = Fortran$interest_by_region
statesMap = map_data("state")
Fortran$region = sapply(Fortran$location,tolower)
FortranMerged = merge(statesMap ,Fortran,by="region")

regionLabels <- aggregate(cbind(long, lat) ~ region, data=FortranMerged, 
                          FUN=function(x) mean(range(x)))

FortranPlot=ggplot() +
  geom_polygon(data=FortranMerged,aes(x=long,y=lat,group=group,fill=hits),colour="white") +
  scale_fill_continuous(low="thistle1",high="darkblue",guide="colorbar",trans="log10") +
  geom_text(data=regionLabels, aes(long, lat, label = region), size=2) +
  theme_bw() +
  coord_fixed(1.3) +
  labs(title="Google search interest for Fortran language programming in each state\nfrom the week prior to landfall in the US") 
FortranPlot

  