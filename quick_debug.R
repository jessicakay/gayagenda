# quick debugber
# jessicakay

googledrive::drive_find(pattern = "trans news tracker",verbose = TRUE ) -> data_sheets 

headers <- c("EntryPublished",
             "EntryTitle",
             "EntryURL",
             "EntryContent",
             "FeedTitle",
             "FeedURL",
             "keyword",
             "region")

fix_list<-NULL

for(i in 1:dim(data_sheets)[1]){
  googlesheets4::read_sheet(data_sheets$id[i])->this_sheet
  if(!setequal(names(this_sheet),headers)){
    fix_list<-c(fix_list,i)
  }
}
