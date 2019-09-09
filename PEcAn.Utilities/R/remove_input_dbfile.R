library(tidyverse)

settings <- PEcAn.settings::read.settings(file.choose())
con <- PEcAn.DB::db.open(settings$database$bety)

remove_input_dbfile <- function(input_id, con) {

  # deleting the childern --------------------
  my.childs <- PEcAn.DB::db.query(paste0("select * from inputs WHERE parent_id=", input_id), con)
  
  my.childs$id %>% 
    purrr::map(function(child.id){

      db.file.record <-
        PEcAn.DB::db.query(paste0("select * from dbfiles WHERE container_id=", child.id), con)
      
      db.file.record$id %>% 
        purrr::map(~PEcAn.DB::db.query(paste0("DELETE FROM dbfiles WHERE id=",.x), con))
      
      out <- PEcAn.DB::db.query(paste0("DELETE FROM inputs WHERE id=", child.id), con)
      
      PEcAn.logger::logger.info(paste0("Records were successfully deleted for input id = ", input_id))
    })
  
  
  #----------- Now itself
  db.file.record <- NULL
  
  record.input <-
    PEcAn.DB::db.query(paste0("select * from inputs WHERE id=", input_id), con)
  
  
  if (nrow(record.input) > 0) {
    db.file.record <-
      PEcAn.DB::db.query(paste0("select * from dbfiles WHERE container_id=", input_id), con)

  } else{
    PEcAn.logger::logger.severe(paste0("No record was found for input id = ", input_id))
  }
  
  if (!is.null(db.file.record) & nrow(db.file.record)>0) {
    db.file.record$id %>% 
      purrr::map(~PEcAn.DB::db.query(paste0("DELETE FROM dbfiles WHERE id=",.x), con))
    
    PEcAn.logger::logger.info(paste0("Records were successfully deleted for input id = ", input_id))
  }
  out <- PEcAn.DB::db.query(paste0("DELETE FROM inputs WHERE id=", input_id), con)
  
  out
}

inps <- tbl(con,'inputs') %>%
  collect()%>%
  filter(grepl("ERA5",name)) %>%
  arrange(updated_at)

print(nrow(inps))

inps$id[-1]%>%
  rev() %>%
  purrr::map(~remove_input_dbfile(.x, con))



list.dirs("/fs/data1/pecan.data/dbfiles/ERA5_CF_site_0-758")[-1] %>%
  unlink(recursive = TRUE)
