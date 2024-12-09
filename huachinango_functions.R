#' huachinango_functions
#' @author Hem Nalini Morzaria-Luna
#' @description 

clean_bienpesca <- function(this.data, inegi.cost.comm, locs.pesca){
  
  print(this.data)
  clean.data <- readxl::read_xlsx(this.data) 
  
  print(names(clean.data))
  if("entidad" %in% names(clean.data)){
    
    bienpesca.data <- clean.data %>% 
      dplyr::mutate(NOM_ENT = stringi::stri_trans_general(str = entidad, id = "Latin-ASCII")) %>%
      dplyr::mutate(NOM_MUN = stringi::stri_trans_general(str = municipio, id = "Latin-ASCII")) %>%
      dplyr::mutate(NOM_LOC = stringi::stri_trans_general(str = localidad, id = "Latin-ASCII")) %>%
      dplyr::mutate(mont_tot=monto_conapesca + monto_gob_edo) %>%
      dplyr::mutate(year = as.character(año)) 
  }
  
  if("ENTIDAD" %in% names(clean.data)){
    
    bienpesca.data <- clean.data %>% 
      dplyr::mutate(NOM_ENT = stringi::stri_trans_general(str = ENTIDAD, id = "Latin-ASCII")) %>%
      dplyr::mutate(NOM_MUN = stringi::stri_trans_general(str = MUNICIPIO, id = "Latin-ASCII")) %>%
      dplyr::mutate(NOM_LOC = stringi::stri_trans_general(str = LOCALIDAD, id = "Latin-ASCII")) %>%
      dplyr::rename(mont_tot=`MONTO FEDERAL`) %>%
      dplyr::mutate(year = as.character(lubridate::year(FECHA))) 
    
   
  }
  
  if("NOMBRE ESTADO" %in% names(clean.data)){
    
    bienpesca.data <- clean.data %>% 
      dplyr::mutate(NOM_ENT = stringi::stri_trans_general(str = `NOMBRE ESTADO`, id = "Latin-ASCII")) %>%
      dplyr::mutate(NOM_MUN = stringi::stri_trans_general(str = `NOMBRE MUNICIPIO`, id = "Latin-ASCII")) %>%
      dplyr::mutate(NOM_LOC = stringi::stri_trans_general(str = `NOMBRE LOCALIDAD`, id = "Latin-ASCII")) %>%
      dplyr::mutate(mont_tot=`RECURSO FEDERAL APORTADO` + `RECURSO ESTATAL APORTADO`) %>%
      dplyr::mutate(year = as.character(EJERCICIO)) 
  }
    
  locs.pesca <- googlesheets4::read_sheet("1eJXjwlHqEzSAZaTmE_XyN93fu6EI0I8igRH4AdvXOl8") %>% 
    distinct(NOM_LOC, NOM_ENT, NOM_LOC_REV, NOM_ENT_REV) %>% 
    mutate(NOM_LOC= stringi::stri_trans_general(str = NOM_LOC, id = "Latin-ASCII"), NOM_LOC = toupper(NOM_LOC)) %>% 
    mutate(NOM_ENT= stringi::stri_trans_general(str = NOM_ENT, id = "Latin-ASCII"), NOM_ENT = toupper(NOM_ENT)) %>% 
    mutate(NOM_LOC_REV= stringi::stri_trans_general(str = NOM_LOC_REV, id = "Latin-ASCII"), NOM_LOC_REV = toupper(NOM_LOC_REV)) %>% 
    mutate(NOM_ENT_REV= stringi::stri_trans_general(str = NOM_ENT_REV, id = "Latin-ASCII"), NOM_ENT_REV = toupper(NOM_ENT_REV))
  
  corr.bienpesca.data <- bienpesca.data %>% 
    dplyr::left_join(locs.pesca, by = c("NOM_LOC","NOM_ENT")) %>% 
    dplyr::filter(!is.na(NOM_LOC_REV)) %>%  
    dplyr::mutate(NOM_LOC = NOM_LOC_REV) %>%  
    dplyr::mutate(NOM_ENT = NOM_ENT_REV)  

  bienpesca.data.names <- bienpesca.data %>% 
    dplyr::left_join(locs.pesca, by = c("NOM_LOC","NOM_ENT")) %>% 
    dplyr::filter(is.na(NOM_LOC_REV)) %>%  
    dplyr::bind_rows(corr.bienpesca.data) 
  
  
    if(("rnpa" %in% names(bienpesca.data.names))==TRUE){
      
      bienpesca.data.rnp <- bienpesca.data.names %>%
        dplyr::mutate(rnpa = as.character(rnpa)) %>% 
        dplyr::select(year, mont_tot, NOM_ENT, NOM_MUN, NOM_LOC, rnpa)
      
    }
    
    if(("RNPA" %in% names(bienpesca.data.names))==TRUE){
      
      bienpesca.data.rnp <- bienpesca.data.names %>%
        dplyr::mutate(rnpa = as.character(RNPA)) %>% 
        dplyr::select(year, mont_tot, NOM_ENT, NOM_MUN, NOM_LOC, rnpa)
      
    }
    
    if(!"RNPA" %in% names(bienpesca.data.names) & !"rnpa" %in% names(bienpesca.data.names)){
      
      bienpesca.data.rnp <- bienpesca.data.names %>%
        dplyr::mutate(rnpa="BIENPESCA") %>% 
        dplyr::select(year, mont_tot, NOM_ENT, NOM_MUN, NOM_LOC, rnpa)
      
    }
    
    bienpesca.data.sel <- bienpesca.data.rnp %>%
      dplyr::mutate(NOM_LOC_NEW= dplyr::if_else(NOM_LOC=="SAN PEDRO MIXTEPEC -DTO. 22 --OAXACA","SAN PEDRO MIXTEPEC",
                                                dplyr::if_else(NOM_LOC=="SAN PEDRO MIXTEPEC -DTO. 26 --OAXACA", "SAN PEDRO MIXTEPEC",
                                                               dplyr::if_else(NOM_LOC=="AH-KIM-PECH", "CAMPECHE", NOM_LOC)))) %>%
      tidyr::separate(NOM_LOC_NEW, c("nom1","nom2"), "-", remove = FALSE) %>%
      dplyr::mutate(NOM_LOC_NEW= dplyr::if_else(!is.na(nom2), nom1, NOM_LOC_NEW)) %>%
      dplyr::select(-NOM_LOC, -nom1, -nom2) %>%
      dplyr::rename(NOM_LOC = NOM_LOC_NEW) %>%
      dplyr::mutate(NOM_LOC = dplyr::if_else(NOM_LOC=="CAMPECHE","SAN FRANCISCO DE CAMPECHE",
                                             dplyr::if_else(NOM_LOC=="CARMEN","CIUDAD DEL CARMEN",NOM_LOC))) 
  
  return(bienpesca.data.sel)
}

get_fishery_office_catch <- function(huachinango.catch){
  
  googlesheets4::gs4_deauth()
  
  #Use CVE_LOC to disambiguate duplicate state/locations
  fish.offices.conapesca <- googlesheets4::read_sheet("1FS2Rs9rL5OQH4n4PM5HmYJqQ0RGtWgWBoC1njpoHXRY") %>% 
    keep_when(CVE_LOC!="NULL") %>% 
    mutate(CVE_LOC = as.character(CVE_LOC)) %>% 
    dplyr::pull(CVE_LOC)

  #make list of unique fishery offices  
  fish.offices.catch <- huachinango.catch %>%
    dplyr::distinct(fishery_office, NOM_ENT, rnp_code) %>%
    dplyr::rename(NOM_LOC = fishery_office) %>% 
    mutate(NOM_LOC= stringi::stri_trans_general(str = NOM_LOC, id = "Latin-ASCII"), NOM_LOC = toupper(NOM_LOC)) %>% 
    mutate(NOM_ENT= stringi::stri_trans_general(str = NOM_ENT, id = "Latin-ASCII"), NOM_ENT = toupper(NOM_ENT))
    
  locs.pesca <- googlesheets4::read_sheet("1eJXjwlHqEzSAZaTmE_XyN93fu6EI0I8igRH4AdvXOl8") %>% 
    distinct(NOM_LOC, NOM_ENT, NOM_LOC_REV, NOM_ENT_REV) %>% 
    mutate(NOM_LOC= stringi::stri_trans_general(str = NOM_LOC, id = "Latin-ASCII"), NOM_LOC = toupper(NOM_LOC)) %>% 
    mutate(NOM_ENT= stringi::stri_trans_general(str = NOM_ENT, id = "Latin-ASCII"), NOM_ENT = toupper(NOM_ENT)) %>% 
    mutate(NOM_LOC_REV= stringi::stri_trans_general(str = NOM_LOC_REV, id = "Latin-ASCII"), NOM_LOC_REV = toupper(NOM_LOC_REV)) %>% 
    mutate(NOM_ENT_REV= stringi::stri_trans_general(str = NOM_ENT_REV, id = "Latin-ASCII"), NOM_ENT_REV = toupper(NOM_ENT_REV))

   corr.fish.offices <- fish.offices.catch %>% 
    dplyr::left_join(locs.pesca, by = c("NOM_LOC","NOM_ENT")) %>% 
    dplyr::filter(!is.na(NOM_LOC_REV)) %>%  
    dplyr::mutate(NOM_LOC = NOM_LOC_REV) %>%  
    dplyr::mutate(NOM_ENT = NOM_ENT_REV)  %>% 
    dplyr::distinct(NOM_LOC, NOM_ENT, rnp_code)
   
   fish.offices <- fish.offices.catch %>% 
     dplyr::left_join(locs.pesca, by = c("NOM_LOC","NOM_ENT")) %>% 
     dplyr::filter(is.na(NOM_LOC_REV)) %>%  
     dplyr::distinct(NOM_LOC, NOM_ENT, rnp_code) %>% 
     dplyr::bind_rows(corr.fish.offices) %>% 
     dplyr::distinct(NOM_LOC, NOM_ENT, rnp_code)
   
   fish.offices.inegi <- fish.offices %>% 
    dplyr::left_join(inegigc, by=c("NOM_ENT","NOM_LOC")) %>% 
    dplyr::mutate(CVE_LOC = as.character(CVE_LOC)) %>% 
    dplyr::filter(POBTOT>20)
  
   #check for locations not found
   
   # fish.offices.inegi %>% dplyr::arrange(NOM_ENT, NOM_LOC) %>% View()
   
   # inegigc %>% dplyr::filter(NOM_ENT=="MICHOACAN DE OCAMPO") %>% dplyr::filter(stringr::str_detect(NOM_LOC, "GUACAMAYA")) %>% dplyr::arrange(NOM_LOC)
   
   # inegigc %>% dplyr::filter(NOM_ENT=="SONORA") %>% dplyr::distinct(NOM_LOC) %>% dplyr::filter(stringr::str_detect(NOM_LOC, "PUERTO"))
   
   #test for duplicate locations, different Municipio
   
   fish.offices.inegi %>% janitor::get_dupes(NOM_ENT,NOM_LOC) %>% distinct(NOM_ENT, NOM_LOC)
   
   #use fishery offices to correct duplicates, these have been checked by address
   
   fixed.dupes.codes <- fish.offices.inegi %>% 
     dplyr::distinct(NOM_ENT, NOM_LOC, CVE_LOC) %>% 
     janitor::get_dupes(NOM_ENT,NOM_LOC) %>% 
     dplyr::filter(CVE_LOC %in% fish.offices.conapesca) %>% 
     dplyr::pull(CVE_LOC)
   
   dupe.codes <- fish.offices.inegi %>% 
     dplyr::distinct(NOM_ENT, NOM_LOC, CVE_LOC) %>% 
     janitor::get_dupes(NOM_ENT,NOM_LOC) %>% 
     dplyr::filter(!CVE_LOC %in% fixed.dupes.codes) %>% 
     dplyr::pull(CVE_LOC)
     
   
   fish.offices.georef <- fish.offices.inegi %>% 
     dplyr::filter(!CVE_LOC %in% dupe.codes) %>% 
     dplyr::arrange(NOM_ENT, NOM_LOC) 
  
  return(fish.offices.georef)
}

extract_comms <- function(this.data, inegi.cost.comm){
  
  
  mun.ent.codes <- readr::read_csv(paste0(input.dir,"/processed_data/","mun_ent_codes.csv"))
  
  ent.cost <- mun.ent.codes %>%
    dplyr::mutate(entidad=toupper(ent_nam)) %>%
    dplyr::mutate(entidad = stringi::stri_trans_general(str = entidad, id = "Latin-ASCII")) %>%
    dplyr::distinct(entidad) %>%
    dplyr::pull(entidad)
  
  mun.cost <- mun.ent.codes %>%
    dplyr::mutate(municipio =toupper(mun_nam)) %>%
    dplyr::mutate(municipio = stringi::stri_trans_general(str = municipio, id = "Latin-ASCII")) %>%
    dplyr::distinct(municipio) %>%
    dplyr::pull(municipio)
  
  mun.codes <- mun.ent.codes %>%
    dplyr::distinct(mun_cve) %>%
    dplyr::pull(mun_cve)
  
  if("entidad" %in% names(this.data)){
    
    this.data.selec <- this.data %>%
      dplyr::mutate(NOM_ENT = toupper(entidad)) %>%
      dplyr::mutate(NOM_ENT = stringi::stri_trans_general(str = NOM_ENT, id = "Latin-ASCII")) %>%
      dplyr::mutate(NOM_MUN = toupper(municipio)) %>%
      dplyr::mutate(NOM_MUN = stringi::stri_trans_general(str = NOM_MUN, id = "Latin-ASCII")) %>%
      dplyr::mutate(NOM_LOC = toupper(localidad)) %>%
      dplyr::mutate(NOM_LOC = stringi::stri_trans_general(str = NOM_LOC, id = "Latin-ASCII")) %>%
      dplyr::filter(NOM_ENT %in% ent.cost) %>%
      dplyr::filter(NOM_MUN %in% mun.cost)
  }
  
  if("ENTIDAD" %in% names(this.data) & !"CVE_LOC" %in% names(this.data) & !"NOM_ENT" %in% names(this.data)){
    
    this.data.selec <- this.data %>%
      dplyr::mutate(NOM_ENT = toupper(ENTIDAD)) %>%
      dplyr::mutate(NOM_ENT = stringi::stri_trans_general(str = NOM_ENT, id = "Latin-ASCII")) %>%
      dplyr::mutate(NOM_MUN = toupper(MUNICIPIO)) %>%
      dplyr::mutate(NOM_MUN = stringi::stri_trans_general(str = NOM_MUN, id = "Latin-ASCII")) %>%
      dplyr::mutate(NOM_LOC = stringi::stri_trans_general(str = LOCALIDAD, id = "Latin-ASCII")) %>%
      dplyr::filter(NOM_ENT %in% ent.cost) %>%
      dplyr::filter(NOM_MUN %in% mun.cost) %>%
      dplyr::left_join(inegi.cost.comm, by = c("NOM_MUN","NOM_ENT","NOM_LOC"))
    
    
  }
  
  if("ENTIDAD" %in% names(this.data) & "CVE_LOC" %in% names(this.data)){
    
    this.data.selec <- this.data %>%
      dplyr::mutate(ENTIDAD = toupper(ENTIDAD)) %>%
      dplyr::mutate(ENTIDAD = stringi::stri_trans_general(str = ENTIDAD, id = "Latin-ASCII")) %>%
      dplyr::mutate(MUNICIPIO = toupper(MUNICIPIO)) %>%
      dplyr::mutate(MUNICIPIO = stringi::stri_trans_general(str = MUNICIPIO, id = "Latin-ASCII")) %>%
      dplyr::filter(ENTIDAD %in% ent.cost) %>%
      dplyr::filter(MUNICIPIO %in% mun.cost) %>%
      dplyr::mutate(no_loc = nchar(CVE_LOC), CVE_LOC = as.character(CVE_LOC)) %>%
      dplyr::mutate(CVE_LOC = dplyr::if_else(no_loc==8,paste0("0",CVE_LOC),CVE_LOC))
    
    
  }
  
  if("NOM_ENT" %in% names(this.data) & !"CVE_LOC" %in% names(this.data)){
    
    this.data.selec <- this.data %>%
      dplyr::mutate(NOM_ENT = toupper(NOM_ENT)) %>%
      dplyr::mutate(NOM_ENT = stringi::stri_trans_general(str = NOM_ENT, id = "Latin-ASCII")) %>%
      dplyr::mutate(NOM_MUN = toupper(NOM_MUN)) %>%
      dplyr::mutate(NOM_MUN = stringi::stri_trans_general(str = NOM_MUN, id = "Latin-ASCII")) %>%
      dplyr::filter(NOM_ENT %in% ent.cost) %>%
      dplyr::filter(NOM_MUN %in% mun.cost) %>%
      dplyr::mutate(NOM_LOC = toupper(NOM_LOC)) %>%
      dplyr::mutate(NOM_LOC = stringi::stri_trans_general(str = NOM_LOC, id = "Latin-ASCII"))
    
    
    
  }
  if("NOMBRE ESTADO" %in% names(this.data)){
    
    this.data.selec <- this.data %>%
      dplyr::mutate(`NOMBRE ESTADO` = toupper(`NOMBRE ESTADO`)) %>%
      dplyr::mutate(`NOMBRE ESTADO` = stringi::stri_trans_general(str = `NOMBRE ESTADO`, id = "Latin-ASCII")) %>%
      dplyr::mutate(`NOMBRE MUNICIPIO` = toupper(`NOMBRE MUNICIPIO`)) %>%
      dplyr::mutate(`NOMBRE MUNICIPIO` = stringi::stri_trans_general(str = `NOMBRE MUNICIPIO`, id = "Latin-ASCII")) %>%
      dplyr::filter(`NOMBRE ESTADO` %in% ent.cost) %>%
      dplyr::filter(`NOMBRE MUNICIPIO` %in% mun.cost) %>%
      dplyr::mutate(CVE_LOC = as.character(`CLAVE LOCALIDAD`), no_loc = nchar(CVE_LOC)) %>%
      dplyr::mutate(CVE_LOC = dplyr::if_else(no_loc==8,paste0("0",CVE_LOC),CVE_LOC))
    
    
  }
  
  if("CVE_MUN" %in% names(this.data) & !"nom_mun" %in% names(this.data)){
    
    this.data.selec <- this.data %>%
      dplyr::filter(`CVE_MUN` %in% mun.codes)
    
  }
  if("nom_ent" %in% names(this.data)){
    
    this.data.selec <- this.data %>%
      dplyr::mutate(NOM_ENT = toupper(nom_ent)) %>%
      dplyr::mutate(NOM_ENT = stringi::stri_trans_general(str = NOM_ENT, id = "Latin-ASCII")) %>%
      dplyr::mutate(NOM_MUN = toupper(nom_mun)) %>%
      dplyr::mutate(NOM_MUN = stringi::stri_trans_general(str = NOM_MUN, id = "Latin-ASCII")) %>%
      dplyr::mutate(NOM_LOC = toupper(nom_loc)) %>%
      dplyr::mutate(NOM_LOC = stringi::stri_trans_general(str = NOM_LOC, id = "Latin-ASCII")) %>%
      dplyr::filter(NOM_ENT %in% ent.cost) %>%
      dplyr::filter(NOM_MUN %in% mun.cost) %>%
      mutate(across(where(~any(stringr::str_detect(., "^\\d+$"))), as.numeric))
  }
  
  return(this.data.selec)
}

make_code <- function(this.tibble){
  
      code.tibble <- this.tibble %>%
      dplyr::mutate(cve_loc = as.character(CVE_LOC), cve_mun = as.character(CVE_MUN), cve_ent = as.character(CVE_ENT)) %>%
      dplyr::mutate(CVE_ENT = as.character(CVE_ENT), CVE_MUN = as.character(CVE_MUN), CVE_LOC = as.character(CVE_LOC)) %>%
      dplyr::mutate(no_loc = nchar(CVE_LOC), no_mun = nchar(CVE_MUN), no_ent = nchar(CVE_ENT)) %>%
      dplyr::mutate(CVE_ENT = dplyr::if_else(no_ent==1,paste0("0",cve_ent), cve_ent)) %>%
      dplyr::mutate(CVE_MUN = dplyr::if_else(no_mun==1, paste0(CVE_ENT,"00", cve_mun),
                                             dplyr::if_else(no_mun==2, paste0(CVE_ENT,"0", cve_mun),
                                                            dplyr::if_else(no_mun==3, paste0(CVE_ENT, cve_mun),
                                                                           dplyr::if_else(no_mun==4, paste0("0", cve_mun), cve_mun))))) %>%
      dplyr::mutate(CVE_LOC=dplyr::if_else(no_loc==1, paste0(CVE_MUN,"000",cve_loc),
                                           dplyr::if_else(no_loc==2, paste0(CVE_MUN, "00",cve_loc),
                                                          dplyr::if_else(no_loc==3, paste0(CVE_MUN, "0", cve_loc),
                                                                         dplyr::if_else(no_loc==4, paste0(CVE_MUN, cve_loc),
                                                                                        dplyr::if_else(no_loc==8, paste0("0",cve_loc), cve_loc)))))) %>%
      dplyr::select(CVE_ENT, CVE_LOC, CVE_MUN, everything())
    
  return(code.tibble)
}


get_inegi <- function(){
  
  comunidadesgc <- data.table::fread(paste0("~/coastalvulnerability-inputs/datos_INEGI_2020/conjunto_de_datos/","conjunto_de_datos_iter_00_cpv2020.csv"))
  
  comunidadesgc.data <- comunidadesgc %>%
    dplyr::rename(CVE_ENT=ENTIDAD, CVE_MUN=MUN, CVE_LOC=LOC) %>% 
    dplyr::select(NOM_ENT, NOM_MUN, NOM_LOC, CVE_ENT, CVE_MUN, CVE_LOC, POBTOT, POBFEM, POBMAS, LONGITUD, LATITUD) %>% 
    mutate(NOM_MUN = toupper(stringi::stri_trans_general(str = NOM_MUN, id = "Latin-ASCII"))) %>% 
    mutate(NOM_ENT = toupper(stringi::stri_trans_general(str = NOM_ENT, id = "Latin-ASCII"))) %>% 
    mutate(NOM_LOC = toupper(stringi::stri_trans_general(str = NOM_LOC, id = "Latin-ASCII"))) %>% 
    dplyr::mutate(across(POBTOT:POBMAS, as.numeric)) %>%
    make_code() 
  
  #convert from lat long to decimal degrees
  inegigc <- comunidadesgc.data %>%
    tidyr::separate(LONGITUD,c("lon_deg","lon"),"°") %>%
    tidyr::separate(lon,c("lon_min","lon_sec"),"'") %>%
    dplyr::mutate(lon_sec = gsub("\".*\ W","",lon_sec)) %>%
    tidyr::separate(LATITUD,c("lat_deg","lat"),"°") %>%
    tidyr::separate(lat,c("lat_min","lat_sec"),"'") %>%
    dplyr::mutate(lat_sec = gsub("\".*\ N","",lat_sec)) %>%
    dplyr::mutate(across(lon_deg:lat_sec,as.numeric)) %>%
    dplyr::mutate(dec_lon= (lon_deg + (lon_min / 60) + (lon_sec / 3600))*-1,
                  dec_lat=(lat_deg + (lat_min / 60) + (lat_sec / 3600))) %>%
    dplyr::mutate(deci_lon = dec_lon, deci_lat = dec_lat) %>% 
    dplyr::filter(!is.na(deci_lat)) %>% 
    dplyr::select(NOM_ENT, NOM_MUN, NOM_LOC, CVE_ENT, CVE_MUN, CVE_LOC, POBTOT, POBFEM, POBMAS, deci_lon, deci_lat)
  
  
 return(inegigc)
  
}


#Checked above this line
#####



get_catch <- function(catch.file, locs.pesca){
  
  correct.ent <- locs.pesca %>%
    select(-NOM_LOC_REV)
  
  correct.loc <- locs.pesca %>%
    select(-NOM_ENT_REV) 
  
  rnp.uncoded <- catch.file %>%
    mutate(NOM_LOC= gsub("Ã‘", "N", NOM_LOC),
           NOM_LOC= gsub("A'", "N", NOM_LOC),
           NOM_LOC= gsub("\\?", "N", NOM_LOC),
           fishery_office = gsub("Ã‘", "N", fishery_office), 
           sp_name = gsub("Ã‘", "N", sp_name)) %>% 
    mutate(fishery_office= stringi::stri_trans_general(str = fishery_office, id = "Latin-ASCII"), fishery_office = toupper(fishery_office)) %>% 
    mutate(NOM_LOC= stringi::stri_trans_general(str = NOM_LOC, id = "Latin-ASCII"), NOM_LOC = toupper(NOM_LOC)) %>% 
    mutate(NOM_ENT= stringi::stri_trans_general(str = NOM_ENT, id = "Latin-ASCII"), NOM_ENT = toupper(NOM_ENT)) %>% 
    dplyr::select(rnp_code, NOM_ENT, NOM_LOC, fishery_office, sp_name, landed_w_kg, year)
  
  this.permit.corr <- rnp.uncoded %>%
    dplyr::left_join(correct.ent, by =c("NOM_LOC","NOM_ENT")) %>%
    mutate(NOM_ENT = dplyr::if_else(!is.na(NOM_ENT_REV), NOM_ENT_REV, NOM_ENT)) %>%
    dplyr::left_join(correct.loc, by =c("NOM_LOC","NOM_ENT")) %>%
    mutate(NOM_LOC = dplyr::if_else(!is.na(NOM_LOC_REV), NOM_LOC_REV, NOM_LOC)) %>% 
    select(-NOM_LOC_REV, -NOM_ENT_REV) 
  
  correct.loc.fishery <- locs.pesca %>%
    select(NOM_LOC_REV, NOM_LOC, NOM_ENT) %>% 
    rename(fishery_office = NOM_LOC)
  
  #eliminate records from freshwater fisheries    
  
  
  fishery.catch.locs <- this.permit.corr %>% 
    dplyr::left_join(correct.loc.fishery, by =c("fishery_office","NOM_ENT")) %>%
    mutate(fishery_office = dplyr::if_else(!is.na(NOM_LOC_REV), NOM_LOC_REV, fishery_office)) %>% 
    select(-NOM_LOC_REV) %>% 
    keep_when(NOM_ENT %in% coast.selecc) %>% 
    keep_when(NOM_LOC!="NO CONSIDERADO") %>%
    keep_when(!grepl("(CULT)",NOM_LOC)) %>%
    keep_when(!grepl("(CULT\\.)",NOM_LOC)) %>% 
    keep_when(!grepl("PRESA ",NOM_LOC)) %>%
    keep_when(!grepl("LAGO ",NOM_LOC)) %>% 
    keep_when(!grepl("RIO ",NOM_LOC)) %>% 
    keep_when(!grepl("ESTANQUE ",NOM_LOC)) %>% 
    keep_when(!grepl("ARROYO ",NOM_LOC)) 
  
  
  data.table::fwrite(fishery.catch.locs, here::here("data-raw","fisheries_data",paste0("catch_file_",this.catchfileno,".csv")))
  
  
}
clean_locs_pesca <- function(){
  
  googlesheets4::gs4_deauth()
  
  #CONTINUE HERE, ADD COL NAMES
  locs.corr.pesca <- googlesheets4::read_sheet("1Nww_0aSf2yQv9YqK8LqmJmIM2wwpHZCSpG1NcGfIfHQ", sheet = "localidades_captura_inegi") %>% 
    keep_when(!is.na(NOM_LOC_REV)) %>% 
    dplyr::rename(NOM_ENT_REV=ENTIDAD) %>% 
    dplyr::select(NOM_LOC, NOM_ENT, NOM_LOC_REV, NOM_ENT_REV)
  
  locs.pesca <- googlesheets4::read_sheet("1eJXjwlHqEzSAZaTmE_XyN93fu6EI0I8igRH4AdvXOl8") %>% 
    mutate(NOM_LOC= stringi::stri_trans_general(str = NOM_LOC, id = "Latin-ASCII"), NOM_LOC = toupper(NOM_LOC)) %>% 
    mutate(NOM_ENT= stringi::stri_trans_general(str = NOM_ENT, id = "Latin-ASCII"), NOM_ENT = toupper(NOM_ENT)) %>% 
    mutate(NOM_LOC_REV= stringi::stri_trans_general(str = NOM_LOC_REV, id = "Latin-ASCII"), NOM_LOC_REV = toupper(NOM_LOC_REV)) %>% 
    mutate(NOM_ENT_REV= stringi::stri_trans_general(str = NOM_ENT_REV, id = "Latin-ASCII"), NOM_ENT_REV = toupper(NOM_ENT_REV)) %>%
    dplyr::filter(!is.na(NOM_LOC_REV)) %>% 
    dplyr::bind_rows(locs.corr.pesca) %>%
    distinct(NOM_LOC, NOM_ENT, NOM_LOC_REV, NOM_ENT_REV)
  
  
  return(locs.pesca)
}


get_rnpa_catch <- function(this.catchfileno, catch.files, coast.selecc){
  
  this.catchfile <- catch.files[this.catchfileno]
  print(this.catchfile)
  
  catch.file <- data.table::fread(this.catchfile, skip=2)
  
  catch.file.rnp <- catch.file %>%
    dplyr::filter(`NOMBRE ESTADO` %in% coast.selecc) %>%
    dplyr::group_by(`RNPA UNIDAD ECONOMICA`,`NOMBRE PRINCIPAL`,`NOMBRE ESPECIE`,`AÑO CORTE`) %>%
    dplyr::summarise(tot_weight_kg=sum(`PESO DESEMBARCADO_KILOGRAMOS`), tot_value_pesos=sum(VALOR_PESOS)) %>%
    dplyr::ungroup() %>%
    dplyr::rename(rnp_code=`RNPA UNIDAD ECONOMICA`, catch_name = `NOMBRE PRINCIPAL`, sp_name = `NOMBRE ESPECIE`, year = `AÑO CORTE`)
  
  
  return(catch.file.rnp)
}

extract_permits <- function(thispermitfile.no, permit.files, inegi.cost.comm, locs.pesca){
  
  this.permitfile <- permit.files[thispermitfile.no]
  
  print(paste("file", this.permitfile))
  
  this.permit.data <- readxl::read_xlsx(this.permitfile)
  
  names.data <- colnames(this.permit.data)
  print(names.data)
  
  correct.ent <- locs.pesca %>%
    dplyr::select(-NOM_LOC_REV)
  
  correct.loc <- locs.pesca %>%
    dplyr::select(-NOM_ENT_REV) %>%
    keep_when(!is.na(NOM_LOC_REV))
  
  if("CLAVE DE RNP" %in% names.data) {
    
    this.permit.data <- this.permit.data %>%
      dplyr::rename(rnp_code =`CLAVE DE RNP`)
  }
  
  
  if("RNPA" %in% names.data){
    this.permit.data <- this.permit.data %>%
      dplyr::rename(rnp_code =`RNPA`)
  }
  
  if("RNP_TITULAR" %in% names.data) {
    
    this.permit.data <- this.permit.data %>%
      dplyr::rename(rnp_code = RNP_TITULAR)
  }
  
  if("RNPA ACTIVO" %in% names.data) {
    
    this.permit.data <- this.permit.data %>%
      dplyr::rename(rnp_code =`RNPA ACTIVO`)
    
  }
  
  if("ENTIDAD" %in% names.data){
    
    this.permit.data <- this.permit.data %>%
      dplyr::rename(NOM_ENT=ENTIDAD)
  }
  
  if("ESTADO" %in% names.data){
    
    this.permit.data <- this.permit.data %>%
      dplyr::rename(NOM_ENT=ESTADO)
  }
  
  if("NOMBRE ESTADO" %in% names.data){
    
    this.permit.data <- this.permit.data %>%
      dplyr::rename(NOM_ENT=`NOMBRE ESTADO`)
  }
  
  if("LOCALIDAD" %in% names.data){
    
    this.permit.data <- this.permit.data %>%
      dplyr::rename(NOM_LOC=LOCALIDAD)
  }
  
  
  if("NOMBRE OFICINA" %in% names.data){
    
    this.permit.data <- this.permit.data %>%
      dplyr::rename(NOM_LOC=`NOMBRE OFICINA`)
  }
  
  
  this.permit.loc <- this.permit.data %>%
    dplyr::mutate(NOM_ENT= stringi::stri_trans_general(str = NOM_ENT, id = "Latin-ASCII"), NOM_ENT = toupper(NOM_ENT), NOM_ENT = stringr::str_squish(NOM_ENT)) %>%
    dplyr::mutate(NOM_LOC = stringi::stri_trans_general(str = NOM_LOC, id = "Latin-ASCII"), NOM_LOC = toupper(NOM_LOC), NOM_LOC = stringr::str_squish(NOM_LOC)) %>%
    dplyr::mutate(NOM_LOC=gsub("A'", "N", NOM_LOC))
  
  this.permit.inegi <- this.permit.loc %>%
    dplyr::left_join(correct.ent, by =c("NOM_LOC","NOM_ENT")) %>%
    mutate(NOM_ENT = dplyr::if_else(!is.na(NOM_ENT_REV), NOM_ENT_REV, NOM_ENT)) %>%
    dplyr::left_join(correct.loc, by =c("NOM_LOC","NOM_ENT")) %>%
    mutate(NOM_LOC = dplyr::if_else(!is.na(NOM_LOC_REV), NOM_LOC_REV, NOM_LOC)) %>%
    dplyr::left_join(inegi.cost.comm, by =c("NOM_ENT","NOM_LOC")) %>%
    dplyr::distinct(NOM_ENT, rnp_code, NOM_MUN, NOM_LOC, CVE_MUN, CVE_LOC, deci_lat, deci_lon, POBTOT)
  
  
  return(this.permit.inegi)
}


clean_files <- function(thisnofile, beneficios.csv, tipo.beneficio){
  
  print(thisnofile)
  thisfile <- beneficios.csv[thisnofile]
  print(thisfile)
  
  thisbeneficio <-  tipo.beneficio[thisnofile]
  print(thisbeneficio)
  
  this.data <- read_csv(here("inputs",thisfile), local = locale(encoding = "latin1")) %>% 
    janitor::clean_names() 
  
  thesenames <- names(this.data)
  
  if(any(thesenames=="pesqueria")){
    
   rev.data <- this.data %>% 
    dplyr::select(ejercicio, cve_inegi, rnpa, monto_federal_aportado, pesqueria) %>% 
    dplyr::mutate(beneficio = thisbeneficio, 
                  pesqueria = tolower(pesqueria),
                  cve_inegi = as.character(cve_inegi))
          
  }
  
  if(!any(thesenames=="pesqueria")){
    
    rev.data <- this.data %>% 
      dplyr::select(ejercicio, cve_inegi, rnpa, monto_federal_aportado) %>% 
      dplyr::mutate(beneficio = thisbeneficio,
                    pesqueria = NA_character_,
                    cve_inegi = as.character(cve_inegi))
      
    
  }
 
   print(rev.data)
  return(rev.data) 
}