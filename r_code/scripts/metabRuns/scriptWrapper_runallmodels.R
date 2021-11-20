#scriptWrapper_runallmodels

#Wrapper script to facilitate running each of the individual lake
#scripts in one big batch

setwd('~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/r_code/scripts')
source('script/metabRuns/Jordan.R')
rm(list=ls())
#Looks good. 

setwd('~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/r_code/scripts')
source('script/metabRuns/Almberga.R')
rm(list=ls())
#All good. 

setwd('~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/r_code/scripts')
source('script/metabRuns/Sugg.R')
rm(list=ls())
#Check if there are enough complete PAR observations to run this model. So far getting nothing.

setwd('~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/r_code/scripts')
source('script/metabRuns/Barco.R')
rm(list=ls())
# Tweaked params to get about a month of output; see notes. 

setwd('~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/r_code/scripts')
source('script/metabRuns/Castle.R')
rm(list=ls())
# Looks good.

setwd('~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/r_code/scripts')
source('script/metabRuns/LittleRock.R')
rm(list=ls())
# Tweaked params to get about a month of output; see notes. 

setwd('~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/r_code/scripts')
source('script/metabRuns/PrairiePothole.R')
rm(list=ls())
# Model fits look good but number of days are pretty low. So similar story as LittleRock and Barco.

setwd('~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/r_code/scripts')
source('script/metabRuns/PrairieLake.R')
rm(list=ls())
# Model fits look good but number of days are pretty low. So similar story as LittleRock and Barco.


setwd('~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/r_code/scripts')
source('script/metabRuns/Toolik.R')
rm(list=ls())
# Model runs, but no results. Not enough data? Hard to diagnose. 


setwd('~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/r_code/scripts')
source('script/metabRuns/TheLoch.R')
rm(list=ls())
# Looks good, just need to do a sensitivity analysis to see how much of a difference modifying zMix makes
# Notes, this script runs all years separately (2016-2019) because the timeSteps vary btwn years

setwd('~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/r_code/scripts')
source('script/metabRuns/SkyPond.R')
rm(list=ls())
# Looks good, just need to do a sensitivity analysis to see how much of a difference modifying zMix makes
# Notes, this script runs all years separately (2016-2019) because the timeSteps vary btwn years


setwd('~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/r_code/scripts')
source('script/metabRuns/Oneida.R')
rm(list=ls())
#Looks good. Need to possibly re-run when I figure out the exact wind height measurement. 


setwd('~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/r_code/scripts')
source('script/metabRuns/Taupo.R')
rm(list=ls())
#Looks good!

setwd('~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/r_code/scripts')
source('script/metabRuns/Gollinsee')
rm(list=ls())
#Looks good!

setwd('~/Google Drive/Research (common)/Research/Data/R/GLEON-Kelly-test/r_code/scripts')
source('script/metabRuns/Schulzensee')
rm(list=ls())
#Looks good!
