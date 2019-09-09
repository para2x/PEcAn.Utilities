

dblist<-list(user='bety',password='bety',host='128.197.168.114' ,dbname='bety',driver='PostgreSQL')
con <- try(PEcAn.DB::db.open(dblist), silent = TRUE)

dplyr::tbl(con, 'cultivars_pfts')%>%
  as_tibble() %>% nrow()


dplyr::tbl(con, 'pfts_species')%>%
  as_tibble() %>% nrow()


dplyr::tbl(con, 'cultivars')%>%
  as_tibble() %>% 
  distinct(specie_id) %>%
  nrow()

dplyr::tbl(con, 'sites_cultivars')%>%
  as_tibble() %>% 
  nrow()


priors <- dplyr::tbl(con, 'pfts_priors') %>%
  filter(pft_id=="1000000095")%>%
  full_join(
    dplyr::tbl(con, 'priors'),
    by=('prior_id'='id')
  ) %>%
  filter(!is.na(pft_id))%>%
  collect()


#%>%
  # dplyr::filter(id == as.numeric(settings$model$id)) %>%
  # dplyr::inner_join(dplyr::tbl(con, "modeltypes_formats"), by = c('modeltype_id')) %>%
  # dplyr::collect() %>%
  # dplyr::filter(required == TRUE) %>%
  # dplyr::pull(tag)

tbl(con, "history")

cmd <-  paste0("SELECT workflows.id, workflows.folder, workflows.start_date, workflows.end_date, workflows.started_at, workflows.finished_at, attributes.value," ,
"CONCAT(coalesce(sites.sitename, ''), ', ', coalesce(sites.city, ''), ', ', coalesce(sites.state, ''), ', ', coalesce(sites.country, '')) AS sitename, " ,
"CONCAT(coalesce(models.model_name, ''), ' ', coalesce(models.revision, '')) AS modelname, modeltypes.name " ,
"FROM workflows " ,
"LEFT OUTER JOIN sites on workflows.site_id=sites.id " ,
"LEFT OUTER JOIN models on workflows.model_id=models.id " ,
"LEFT OUTER JOIN modeltypes on models.modeltype_id=modeltypes.id " ,
"LEFT OUTER JOIN attributes ON workflows.id=attributes.container_id AND attributes.container_type='workflows' ")

history <- PEcAn.DB::db.query(cmd, con)

tbl(con, "machines")

path.copy <-dplyr::tbl(con, "dbfiles") %>%
  dplyr::filter(machine_id == !!PEcAn.DB::get.id("machines", 'hostname', PEcAn.remote::fqdn(), con)) %>%
  dplyr::select(file_path) %>%
  dplyr::collect()%>%
  dplyr::sample_n(1000) %>%
  dplyr::filter(grepl("pecan.data", .)) %>%
  dplyr::sample_n(1)%>%
  dplyr::pull(file_path)

if (length(path.copy)>0){
  
}else{
  
}


PEcAn.DB::db.query("geometry::STGeomFromText('POLYGON ((0 0, 150 0, 150 150, 0 150, 0 0))', 0)", con)
