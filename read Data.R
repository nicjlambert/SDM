
# read in reference CSV for estimated resident population and population projections
pop_prj <- read.csv('~/SDM/Data/POP_PROJ_REGION_STE.csv',
                   header=T,
                   sep=",",
                   quote='"',
                   strip.white=T,
                   stringsAsFactors=F,
                   fill=T)

erp <- read.csv('~/SDM/Data/ABS_ERP_COMP_STE.csv',
                    header=T,
                    sep=",",
                    quote='"',
                    strip.white=T,
                    stringsAsFactors=F,
                    fill=T)
