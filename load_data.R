load2020 <- T

############ Load PPI 2020 ################
# Update necessary files from excel file: bfs_ppi_2020.xlsx
if (load2020) {
  
  #download.file("https://dam-api.bfs.admin.ch/hub/api/dam/assets/22864328/master", destfile="bfs_ppi_2020.xlsx")
  ppis.names.2020 <- read_excel("bfs_ppi_2020.xlsx", sheet = "INDEX_m", range = paste0("D8:D260"))
  ppis.names.2020[is.na(ppis.names.2020)] <- "PPI"
  colnames(ppis.names.2020) <- "Produktcode"

  ppis_weights.all <- read_excel("weightsAll.xlsx")
  
  ppis.2010 <- read_excel("bfs_ppi_2020.xlsx", sheet = "INDEX_m", range = paste0("DC8:FK260")) %>%
    mutate_all(as.numeric) %>% 
    as.matrix() %>% 
    t() %>%
    as_tibble()
  
  colnames(ppis.2010) <- ppis.names.2020$Produktcode
  
  ppis.2010 <- as_tibble(lapply(ppis.2010, function(x) {if (!is.na(x[1])) {x/x[1]*100} else {x} }))
  
  PPI.2010 <- ppis.2010 %>%
    dplyr::select(PPI)
  
  ppis.2010 <- ppis.2010 %>%
    dplyr::select(-PPI)
  
  ppis.2015 <- read_excel("bfs_ppi_2020.xlsx", sheet = "INDEX_m", range = paste0("FK8:HS260")) %>%
    mutate_all(as.numeric) %>% 
    #slice(-1) %>%
    as.matrix() %>% 
    #na.omit() %>%
    t() %>%
    as_tibble()
  
  colnames(ppis.2015) <- ppis.names.2020$Produktcode
  
  ppis.2015 <- as_tibble(lapply(ppis.2015, function(x) {if (!is.na(x[1])) {x/x[1]*100} else {x} }))
  
  PPI.2015 <- ppis.2015 %>%
    dplyr::select(PPI)
  
  ppis.2015 <- ppis.2015 %>%
    dplyr::select(-PPI)
  
  
  ppis.2020 <- read_excel("bfs_ppi_2020.xlsx", sheet = "INDEX_m", skip=7) %>%
    dplyr::select(-c(1:15)) %>%
    slice(-c(253:277)) %>%
    mutate_all(as.numeric) %>% 
    #slice(-1) %>%
    as.matrix() %>% 
    #na.omit() %>%
    t() %>%
    as_tibble() %>%
    slice(-c(1:211))
  
  colnames(ppis.2020) <- ppis.names.2020$Produktcode
  
  PPI.2020 <- ppis.2020 %>%
    dplyr::select(PPI)
  
  ppis.2020 <- ppis.2020 %>%
    dplyr::select(-PPI)
  
 
  ppis_dez.all <- read_excel("bfs_ppi_2020.xlsx", sheet = "INDEX_m", range = paste0("P8:IG260")) %>%
    mutate_all(as.numeric) %>% 
    #slice(-1) %>%
    as.matrix() %>% 
    #na.omit() %>%
    t() %>%
    as_tibble() %>% 
    slice(c(212, 152, 92))
  
  colnames(ppis_dez.all) <- ppis.names.2020$Produktcode
  
  
  
  ppis_level_names.2020 <- read_excel("bfs_ppi_2020.xlsx", sheet = "INDEX_m", range = paste0("F9:K260")) %>% 
    t() %>%
    as_tibble()
  
  colnames(ppis_level_names.2020) <- ppis.names.2020$Produktcode[2:nrow(ppis.names.2020)]
  
  
  ppis_level.2020 <- as_tibble(ifelse(is.na(ppis_level_names.2020), 0, 1))
  
  selectedSeries <- full_join(ppis.2020, ppis.2015) %>%
    full_join(ppis.2010) %>%
    dplyr::select_if(~ !any(is.na(.))) %>%
    colnames()
  
  ppis.2010 <- ppis.2010 %>% dplyr::select(all_of(selectedSeries)) %>%
    dplyr::select_if(~ !any(is.na(.))) 
  ppis.2015 <- ppis.2015 %>% dplyr::select(all_of(selectedSeries)) %>%
    dplyr::select_if(~ !any(is.na(.))) 
  ppis.2020 <- ppis.2020 %>% dplyr::select(all_of(selectedSeries)) %>%
    dplyr::select_if(~ !any(is.na(.))) 
  
  ppis_dez.all <- ppis_dez.all%>% dplyr::select(all_of(selectedSeries)) %>%
    dplyr::select_if(~ !any(is.na(.))) 
  
  ppis_weights.all <- ppis_weights.all %>% dplyr::select(all_of(selectedSeries))
  ppis_level.2020 <- ppis_level.2020 %>% dplyr::select(all_of(selectedSeries))
  ppis_level_names.2020l <- ppis_level_names.2020 %>% dplyr::select(all_of(selectedSeries))
  
  times <- ts(PPI.2020, start=c(2020,12), freq=12)
  actual_y <- end(times)[1]
  actual_m <- end(times)[2]
  
  save(PPI.2010, PPI.2015, PPI.2020, 
       ppis.2010, ppis.2015, ppis.2020, 
       ppis_dez.all, actual_y, actual_m,
       ppis_level.2020, ppis_level_names.2020, 
       ppis_weights.all, file = "PPI_2020.RData")
  
}
