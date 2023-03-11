# Load required R PACKAGES =====================================================
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(RColorBrewer)
library(colourpicker)
library(ggplot2)
library(ISOweek)
library(DT)
library(knitr)
library(scales)
library(ggplot2)
library(janitor)
library(readxl)
# ==============================================================================

# Load Modules =================================================================
# General ----------------------------------------------------------------------
source("./Modules/JaarOverzicht/percentages.R")
source("./Modules/JaarOverzicht/Plots/simple_barplot.R")
source("./Modules/JaarOverzicht/Plots/simple_piechart.R")
# Data prep --------------------------------------------------------------------
source("./Functions/DataPrep/ophalen_basisdata.R")
source("./Modules/JaarOverzicht/data_preparation.R")
source("./Modules/JaarOverzicht/data_visualisation.R")
# Persberichten algemeen -------------------------------------------------------
source("./Modules/JaarOverzicht/DataVisual/Persberichten/Algemeen/Persberichten/DataVisual_PbBerichtAlgKwartaal.R")
source("./Modules/JaarOverzicht/DataVisual/Persberichten/Algemeen/Persberichten/DataVisual_PbBerichtAlgMaand.R")
source("./Modules/JaarOverzicht/DataVisual/Persberichten/Algemeen/Persberichten/DataVisual_PbBerichtAlgDag.R")
source("./Modules/JaarOverzicht/DataVisual/Persberichten/Algemeen/Persberichten/DataVisual_PbBerichtAlgWeek.R")
source("./Modules/JaarOverzicht/DataVisual/Persberichten/Algemeen/Persberichten/DataVisual_PbBerichtAlgBeleid.R")
# Persconferenties algemeen ----------------------------------------------------
source("./Modules/JaarOverzicht/DataVisual/Persberichten/Algemeen/Persconferenties/DataVisual_PbConferentieAlgKwartaal.R")
source("./Modules/JaarOverzicht/DataVisual/Persberichten/Algemeen/Persconferenties/DataVisual_PbConferentieAlgMaand.R")
source("./Modules/JaarOverzicht/DataVisual/Persberichten/Algemeen/Persconferenties/DataVisual_PbConferentieAlgDag.R")
source("./Modules/JaarOverzicht/DataVisual/Persberichten/Algemeen/Persconferenties/DataVisual_PbConferentieAlgWeek.R")
source("./Modules/JaarOverzicht/DataVisual/Persberichten/Algemeen/Persconferenties/DataVisual_PbConferentieAlgBeleid.R")
# PErsberichten per beleid------------------------------------------------------
source("./Modules/JaarOverzicht/DataVisual/Persberichten/PerBeleid/DataVisual_PbBeleidMaand.R")
source("./Modules/JaarOverzicht/DataVisual/Persberichten/PerBeleid/DataVisual_PbBeleidDeelbeleid.R")
# Persberichten per verzender --------------------------------------------------
source("./Modules/JaarOverzicht/DataVisual/Persberichten/PerVerzender/DataVisual_PbVerzenderVerzender.R")
source("./Modules/JaarOverzicht/DataVisual/Persberichten/PerVerzender/DataVisual_PbVerzenderBeleid.R")
source("./Modules/JaarOverzicht/DataVisual/Persberichten/PerVerzender/DataVisual_PbVerzenderMaand.R")
source("./Modules/JaarOverzicht/DataVisual/Persberichten/PerVerzender/DataVisual_PbVerzenderDeelbeleid.R")
# Persberichten per type -------------------------------------------------------
source("./Modules/JaarOverzicht/DataVisual/Persberichten/PerType/DataVisual_PbType.R")
# Persreturn per beleid --------------------------------------------------------
source("./Modules/JaarOverzicht/DataVisual/Persreturn/Beleid/DataVisual_PrBeleidAlg.R")
source("./Modules/JaarOverzicht/DataVisual/Persreturn/Beleid/DataVisual_PrBeleidDeelbeleid.R")
# Persreturn per medium --------------------------------------------------------
source("./Modules/JaarOverzicht/DataVisual/Persreturn/Medium/DataVisual_PrMedium.R")
# ==============================================================================

# Create Global Variables ======================================================
# Data collectors for report generation ----------------------------------------
Persberichten.alg <- list()
Persconferenties.alg <- list()
Persberichten.beleid.maand <- list()
Persberichten.beleid.beleid <- list()
Persberichten.verzender.alg <- list()
Persberichten.verzender.maand <- list()
Persberichten.verzender.beleid <- list()
Persberichten.type <- list()
Persreturn.beleid <- list()
Persreturn.medium <- list()
# ==============================================================================