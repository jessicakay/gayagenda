# 

exfresh<-function(arg="ex"){
  if(arg=="ex"){
    googledrive::drive_find(pattern = "rhetorictracker",verbose = TRUE ) ->> ext_sheet 
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

exds %>% mutate(theday=str_extract(EntryPublished,pattern = "[a-zA-Z]+\\s[0-9]+\\,\\s20[0-9]+")) %>%
  mutate(the_day=as.Date(mdy(theday))) ->> exds
substring(str_extract(exds$EntryURL, pattern="https:\\/\\/?[a-z]+.[a-zA-Z0-9]+?.?[a-z]+/"), first=9) -> exds$pullURL

table(exds$pullURL) %>% View()
