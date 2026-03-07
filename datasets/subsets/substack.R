colnames(sub2)[2]<-"Substack_CDN"
sub2 %>% arrange(desc(as.integer(subscribers))) %>% View()

sub2 %>% mutate(CDN_detected=
                  case_when(
                    Substack_CDN==FALSE ~ "metadata",
                    Substack_CDN==TRUE ~ "headers",
                    Substack_CDN=="error" & str_detect(profile_data, "subscriber") ~ "metadata" 
                    ))%>%
        mutate(verified=
                 case_when(
                  Substack_CDN==TRUE ~ "verified",
                  !is.integer(subscribers) ==TRUE ~ "verified",
                 )
               ) -> sub2

sub2$Substack_CDN[(sub2$Substack_CDN==FALSE & sub2$profile_data=="error")]<-"not detected"

