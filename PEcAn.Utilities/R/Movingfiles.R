
multi.settings <- PEcAn.settings::read.settings(file.choose())
multi.settings <-as.MultiSettings(multi.settings)

PEcAn.remote::open_tunnel(multi.settings[[1]]$host$name, 
                          user = multi.settings[[1]]$host$user,
                          tunnel_dir = dirname(multi.settings[[1]]$host$tunnel),
                          tunnel_script =file.path("/fs/data3/hamzed/pecan/web/sshtunnel.sh"))

getwd()
filen <- 'out_Grass.Rdata'
remote.copy.to(multi.settings$host,
               filen,
               paste0('/projectnb/dietzelab/hamzed/HPDA/PostAnalaysis/out_Grass.Rdata'),
               delete=TRUE)
