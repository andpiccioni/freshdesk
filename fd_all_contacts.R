#' This function extracts all contacts from freshdesk and populates a .csv file
#' with the following columns:
#' $ Id   
#' $ Name 
#' $ Email:

fd_all_contacts <- function(){
  
  require(httr, quietly = TRUE)
  require(dplyr, quietly = TRUE)
  apikey <- "z6L822MqF9kZ2K3D4J8c"
  ini_curl <- "https://alk.freshdesk.com/api/v2/contacts?&page="
  page_num <- 1
  full_curl <- paste0(ini_curl, page_num)
  req <- GET(full_curl, authenticate(apikey, "X", type = "basic"))
  
  ctc_list <- content(req)
  
  ctc_num <- length(ctc_list)
  while(ctc_num==30){
    page_num <- page_num + 1
    full_curl <- paste0(ini_curl, page_num)
    req <- GET(full_curl, authenticate(apikey, "X", type = "basic"))
    rep_date <- content(req)
    ctc_list <- append(ctc_list, rep_date)
    ctc_num <- length(rep_date)
  }
  
  contacts_list <- data.frame(matrix(nrow=length(ctc_list), ncol=3))
  colnames(contacts_list) <- c("Id", "Name","Email")
  
  for(i in seq_along(ctc_list)){
    contacts_list[i,1] <-ctc_list[[i]]$id
    if(!is.null(ctc_list[[i]]$name)){
      contacts_list[i,2] <- ctc_list[[i]]$name
    }
    if(!is.null(ctc_list[[i]]$email)){
      contacts_list[i,3] <- ctc_list[[i]]$email
    }
  }
  
  # new_ctc <- new_ctc[!is.na(new_ctc$Email), ]
  # contacts <- rbind(contacts, new_ctc)
  
  write.csv2(contacts_list, file = 'contacts_list.csv', sep = ',', col.names = TRUE)
  # return(contacts)
  
}
