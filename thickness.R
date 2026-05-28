library(parallel)
library(rgl)
library(Rvcg)
library(Morpho)
library(readxl)
library(dplyr)

## set working directory!
#setwd("C:/Users/Quadrula/Univ. of Oklahoma Dropbox/Alex Franzen/Fusconaia_project_data/Alex_scanning/shell_thickness")
setwd("~/Univ. of Oklahoma Dropbox/Alex Franzen/Fusconaia_project_data/Alex_scanning/shell_thickness") # on mac

#set path to meshes
path_mesh <- '../20250127_3d_scans_thickness/'
#set path where you want to store data
path_sz_thk<- 'size_thick/'

#Read in WKST with column 'meshid' ie names of scanned specimens
WKST <- read_xlsx('wkst.xlsx', sheet = "Sheet1")

# measure size and thickness
lapply(1:nrow(WKST),function(ii){
  # ii=1
  cat(ii, '\n')
  tempmeshid <- WKST[[ii,'meshid']]

  tryCatch({

  if(!file.exists(paste0(path_sz_thk,tempmeshid,'.data'))) {
    mesh_ext <- vcgImport(paste0(path_mesh,tempmeshid,'_LV_ext.stl'))
    mesh_int <- vcgImport(paste0(path_mesh,tempmeshid,'_LV_int.stl'))
    ext_pts <- vcgSample(mesh_ext,SampleNum=10000,type="pd",
      threads=1,strict=T)
    cSize <- cSize(ext_pts)
    ext <- vcgSample(mesh_ext,SampleNum=10000,type="pd",threads=1)
    int <- vcgSample(mesh_int,SampleNum=10000,type="pd",threads=1)
    thickness <- vcgKDtree(int,ext,1)$distance
    thick_mm <- median(thickness)
    out <- data.frame(tempmeshid,cSize,thick_mm)
    save(out,file=stringr::str_c(path_sz_thk,tempmeshid,'.data'))
  }
  },error=function(e){
    save(out,file=stringr::str_c(path_sz_thk,tempmeshid,'_FAILED.data'))
  })
})

# read in shell size and thickness
CSTK <- lapply(1:nrow(WKST),function(ii){
  # ii <- 1
  cat(ii, '\n')
  tempmeshid <- WKST[[ii,'meshid']]
  load(stringr::str_c(path_sz_thk,tempmeshid,'.data'))
  out %>%
     rename(meshid=tempmeshid)
}) %>% bind_rows()

write.csv(CSTK, file = 'measurements.csv')
