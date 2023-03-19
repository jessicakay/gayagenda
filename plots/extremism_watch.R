# 

exfresh<-function(arg="ex"){
  if(arg=="ex"){
    googledrive::drive_find(pattern = "rhetoric_tracker",verbose = TRUE ) ->> ext_sheet 
    exdat <<- NULL
    for(i in 1:dim(ext_sheet)[1]){
      if(i==1){googlesheets4::read_sheet(ext_sheet$id[i]) ->> exdat
      }else{rbind(dat, googlesheets4::read_sheet(ext_sheet$id[i])) -> exdat }
       exdat -> exds ; exdat ->> exds ; assign("exds",exds,envir = .GlobalEnv)}
    exds %>% group_by(EntryTitle) %>% as_tibble(as.data.frame()) ->> exds
  assign("exds",exds,envir = .GlobalEnv)
  }
}
exfresh()
