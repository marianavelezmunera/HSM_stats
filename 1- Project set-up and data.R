# Project set-up

library(readxl)
library(tidyverse)
library(showtext)
library(cowplot)
library(ggpubr)
library(patchwork)
library(gt)

# Mariana's data
HSM_ML <- read_excel("HSM_ML.xlsx", col_types = c("text", "text", "text", "numeric","numeric", "numeric", "text", "skip", "skip", "skip", "skip", "skip", "skip"))

# My data
HSM_MV <- read_excel("HSM_MV.xlsx", col_types = c("text", "text", "text", "numeric","numeric", "numeric", "text", "skip", "skip", "skip", "skip", "skip", "skip"))

# Merge
data<-rbind(HSM_ML,HSM_MV)

str(data)
unique(data$Skip)
       
#Font
font_add_google("Lato","Lato") #Font
font.families()
showtext_auto()
