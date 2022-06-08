#' huachinango_functions
#' @author Hem Nalini Morzaria-Luna
#' @description 

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