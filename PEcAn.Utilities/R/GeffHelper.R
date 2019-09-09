library(tidyverse)
(1:21) %>%
  map(
    ~
      PEcAn.SIPNET::met2model.SIPNET (
        in.path = paste0(
          "/fs/data3/kzarada/pecan.data/dbfiles/NOAA_GEFS_CF_gapfill_site_0-676/NOAA_GEFS.",
          .x
        ),
        in.prefix = paste0(
          "NOAA_GEFS.Willow Creek (US-WCr).",
          .x,
          ".2018-11-01T00:00.2018-11-17T00:00"
        ),
        outfolder = "/fs/data3/kzarada/pecan.data/dbfiles/NOAA_GEFS_SIPNET_site_0-676",
        start_date = "2018-11-11",
        end_date = "2018-11-19",
        overwrite = T,
        year.fragment = T
      )
  )
