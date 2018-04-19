#   __      _____             _              __        _       
# <(o )___ |  _  |___ ___ ___| |_ ___ ___   |  |   ___| |_ ___ 
# ( ._> /  |   __|  _| -_|_ -|  _| -_|_ -|  |  |__| .'| . |_ -|
#  `---'   |__|  |_| |___|___|_| |___|___|  |_____|__,|___|___|
#==============================================================================
# Tables on reports
#==============================================================================
# Title          : GoogleTrendsTS.r
# Description    : Better tables for reports - data presentation
# Author         : Isaias V. Prestes <isaias.prestes@gmail.com>
# Date           : 20180414
# Version        : 0.0.1
# Usage          : Run in R 3.4
# Notes          : based on Felipe Gomes
# https://gomesfellipe.github.io/post/2018-01-12-tabelas-incriveis-com-r/tabelas-incriveis-com-r/
#
# R version      : 3.4
#==============================================================================

#==============================================================================
# PACKAGE INSTALATION
#==============================================================================
if(!require("devtools")) install.packages("devtools")
if(!require("DT")) install.packages("DT")
if(!require("formattable")) install.packages("formattable")
# devtools::install_github("renkun-ken/formattable")
if(!require("knitr")) install.packages("knitr")
if(!require("kableExtra")) install.packages("kableExtra")
if(!require("dplyr")) install.packages("dplyr")
if(!require("sparkline")) install.packages("sparkline")
# install_github('htmlwidgets/sparkline')
if(!require("htmlwidgets")) install.packages("htmlwidgets")

#==============================================================================
# LIBRARY DEPENDENCE
#==============================================================================
library(devtools)
library(DT)
library(formattable)
library(knitr)
library(kableExtra)
library(dplyr)
library(sparkline)
library(htmlwidgets)

#==============================================================================
# DT package
#==============================================================================
DT::datatable(iris[1:20, c(5, 1:4)], rownames = FALSE)

#==============================================================================
# formattable package
#==============================================================================

# Exemplo de formatação para resultados de porcentagem:
percent(c(0.1, 0.02, 0.03, 0.12))

# Exemplo de formatação para resultados de na casa do milhar:
accounting(c(1000, 500, 200, -150, 0, 1200))

# criando um data.frame
df <- data.frame(
  id = 1:10, 
  Nomes = c("Sofia", "Kiara", "Dunki", "Edgar", "Aline","Gertrudes", "Genovena", "Champanhe", "Pérola", "Penelope"),
  Kilos = accounting(c(20000, 30000, 50000, 70000, 47000,80000,45000,35000,20000,25000), format = "d"),
  Crescimento = percent(c(0.1, 0.2, 0.5, 0.95, 0.97,0.45,0.62,0.57,0.37, 0.3), format = "d"),
  Suficiente = formattable(c(T, F, T, F, T,F,F,T,T,F), "Sim", "Não"))
  
formattable(df, list(
  id = color_tile("white", "orange"),
  Suficiente = formatter("span", style = x ~ ifelse(x == T, 
                                               style(color = "green", font.weight = "bold"), NA)),
  area(col = c(Kilos)) ~ normalize_bar("lightgrey", 0.2),
  Crescimento = formatter("span",
                          style = x ~ style(color = ifelse(rank(-x) <= 3, "green", "gray")),
                          x ~ sprintf("%.2f (rank: %02g)", x, rank(-x)))
))

#==============================================================================
# knitr and kableExtra packages
#==============================================================================

mtcars[1:10, 1:2] %>%
  mutate(
    car = row.names(.),
    # Você não precisa de formato = "html" se você já definiu opções (knitr.table.format)
    mpg = cell_spec(mpg, "html", color = ifelse(mpg > 20, "red", "blue")),
    cyl = cell_spec(cyl, "html", color = "white", align = "c", angle = 45, 
                    background = factor(cyl, c(4, 6, 8), 
                                        c("#666666", "#999999", "#BBBBBB")))) %>%
  select(car, mpg, cyl) %>%
  kable("html", escape = F) %>%
  kable_styling("striped", full_width = F)
  
#Outro exemplo colorido legal:
iris[1:10, ] %>%
  mutate_if(is.numeric, function(x) {
    cell_spec(x, "html", bold = T, color = spec_color(x, end = 0.9),
              font_size = spec_font_size(x))
  }) %>%
  mutate(Species = cell_spec(
    Species, "html", color = "white", bold = T,
    background = spec_color(1:10, end = 0.9, option = "A", direction = -1)
  )) %>%
  kable("html", escape = F, align = "c") %>%
  kable_styling("striped", full_width = F)
  
#Integrando com formattable
suppressMessages(library(formattable))
mtcars[1:5, 1:4] %>%
  mutate(
    car = row.names(.),
    mpg = color_tile("white", "orange")(mpg),
    cyl = cell_spec(cyl, "html", angle = (1:5)*60, 
                    background = "red", color = "white", align = "center"),
    disp = ifelse(disp > 200,
                  cell_spec(disp, "html", color = "red", bold = T),
                  cell_spec(disp, "html", color = "green", italic = T)),
    hp = color_bar("lightgreen")(hp)
  ) %>%
  select(car, everything()) %>%
  kable("html", escape = F) %>%
  kable_styling("hover", full_width = F) %>%
  column_spec(5, width = "3cm") %>%
  add_header_above(c(" ", "Hello" = 2, "World" = 2))
  
#==============================================================================
# sparkline package
#==============================================================================
x = rnorm(20)
sparkline(x)
sparkline(x, type = 'bar')
sparkline(x, type = 'box')
