#little script for inserting the posterior files (that you get from someone else) on your machines to the DB

# physical space where files are, e.g.:
# marginal distributions
post_distns_path <- "/fs/data2/output/PEcAn_1000010381/pft/temperate.deciduous.ALL/post.distns.pda.temperate.deciduous.ALL_1000022945.Rdata"
# PDA posterior, ensemble samples will be generated from this, not the marginal distributions 
tmcmc_distns_path <- "/fs/data2/output/PEcAn_1000010381/pft/temperate.deciduous.ALL/trait.mcmc.pda.temperate.deciduous.ALL_1000022945.Rdata"

# just any settings file that will help you connect DB and has the pftname, model name etc
settings <- read.settings("pecan.CONFIGS.xml")

## Open database connection
con <- try(PEcAn.DB::db.open(settings$database$bety), silent = TRUE)
on.exit(PEcAn.DB::db.close(con))

# i : which pft you want, here temp.dec was my 2nd pft
i <- 2
pft.id <- PEcAn.DB::db.query(paste0("SELECT pfts.id FROM pfts, modeltypes WHERE pfts.name='",
                                    settings$pfts[[i]]$name, 
                                    "' and pfts.modeltype_id=modeltypes.id and modeltypes.name='", 
                                    settings$model$type, "'"), 
                             con)[["id"]]


# this will create the record, we'll add files in the following lines
posteriorid <-  PEcAn.DB::db.query(paste0("INSERT INTO posteriors (pft_id) VALUES (",
                                          pft.id, ") RETURNING id"), con)


PEcAn.logger::logger.info(paste0("--- Posteriorid for ", settings$pfts[[i]]$name, " is ", posteriorid, " ---"))
settings$pfts[[i]]$posteriorid <- posteriorid

# insert files
PEcAn.DB::dbfile.insert(dirname(post_distns_path), basename(post_distns_path), "Posterior", posteriorid, con)
PEcAn.DB::dbfile.insert(dirname(tmcmc_distns_path), basename(tmcmc_distns_path), "Posterior", posteriorid, con)