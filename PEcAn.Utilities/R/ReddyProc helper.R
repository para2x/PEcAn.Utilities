library(REddyProc)
library(dplyr)

#+++ Load data with 1 header and 1 unit row from (tab-delimited) text file
fileName <- getExamplePath('Example_DETha98.txt', isTryDownload = TRUE)
EddyData.F <- if (length(fileName)) fLoadTXTIntoDataframe(fileName) else
  # or use example dataset in RData format provided with REddyProc
  Example_DETha98
#+++ Add time stamp in POSIX time format
EddyDataWithPosix.F <- fConvertTimeToPosix(EddyData.F, 'YDH',Year.s = 'Year'
                                           ,Day.s = 'DoY',Hour.s = 'Hour')
#+++ Initalize R5 reference class sEddyProc for post-processing of eddy data
#+++ with the variables needed for post-processing later
EddyProc.C <- sEddyProc$new('DE-Tha', EddyDataWithPosix.F, 
                            c('NEE','Rg','Tair','VPD', 'Ustar'))


EddyProc.C$sPlotFingerprintY('NEE', Year = 1998)



uStarTh <- EddyProc.C$sEstUstarThresholdDistribution(nSample = 100L, probs = c(0.05, 0.5, 0.95)) 
#filter(uStarTh, aggregationMode == "year")
select(uStarTh, -seasonYear)

uStarThAnnual <- usGetAnnualSeasonUStarMap(uStarTh)[-2]
uStarSuffixes <- colnames(uStarThAnnual)[-1]
print(uStarThAnnual)




EddyProc.C$sMDSGapFillAfterUStarDistr('NEE',
                                      uStarTh= uStarThAnnual,
                                      uStarSuffixes = uStarSuffixes,
                                      FillAll = TRUE
)


grep("NEE_.*_f$",names(EddyProc.C$sExportResults()), value = TRUE)
grep("NEE_.*_fsd$",names(EddyProc.C$sExportResults()), value = TRUE)

#+ fig.width=5, fig.height=5
EddyProc.C$sPlotFingerprintY('NEE_U05_f', Year = 1998)
EddyProc.C$sPlotFingerprintY('NEE_U50_f', Year = 1998)
EddyProc.C$sPlotFingerprintY('NEE_U95_f', Year = 1998)

FilledEddyData.F <- EddyProc.C$sExportResults()
CombinedData.F <- cbind(EddyData.F, FilledEddyData.F)

library(highcharter)
CombinedData.F %>%
  mutate(Date=as.Date(DoY, origin = paste0(Year,"-01-01"))) %>%
  dplyr::select(NEE_U05_f,NEE_U50_f,NEE_U95_f,Date)%>%
  timetk::tk_xts()->readytoplot


highchart(type = "stock") %>% 
  hc_add_theme(hc_theme_538())%>% 
  hc_title(text = "NEE FLUX uncertainty") %>% 
  hc_add_series(readytoplot[,1], name="NEE_U05_f") %>% 
  hc_add_series(readytoplot[,2], name="NEE_U50_f") %>%
  hc_add_series(readytoplot[,3], name="NEE_U95_f")
