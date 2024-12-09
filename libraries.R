
.packages = c("tidyverse","devtools","readxl",
              "sp","sf","rgdal",
              "rnaturalearth","RCurl","XML",
              "here", "raster", "exactextractr",
              "Redmonder","gridExtra","corrplot","mgcViz","ggspatial",
              "ggrepel","patchwork","png","grid","janitor","purrr")

# Install CRAN packages (if not already installed)
.inst <- .packages %in% installed.packages()

if(length(.packages[!.inst]) > 0) install.packages(.packages[!.inst], )

# Load packages into session 
lapply(.packages, require, character.only=TRUE)

remotes::install_github("iobis/robis")

# Load packages into session 
lapply(.packages, require, character.only=TRUE)

