#' This function takes the day as argument and the contacts_fd.csv file 
#' as data input, to create a list of contacts including email and name
#' which will be then updated by the function itself.
#' The function uses FD api to extract contacts and outputs a new 
#' "contacts_fd.csv" file.
 
FD_contacts <- function(day){
  require(httr, quietly = TRUE)
  require(dplyr, quietly = TRUE)
  wd <- getwd() 
  file_name <- paste0(wd, "/", "contacts_fd.csv")
  contacts <- read.csv(file_name, stringsAsFactors = FALSE, sep=";") %>% select("Full.name", "Email")## loading full contact list
  apikey <- "z6L822MqF9kZ2K3D4J8c"
  ini_curl <- "https://alk.freshdesk.com/api/v2/search/contacts?query=\"created_at:%27"
  fin_curl <- "%27\"&page="
  page_num <- 1
  full_curl <- paste0(ini_curl, day, fin_curl, page_num)
  req <- GET(full_curl, authenticate(apikey, "X", type = "basic"))
  
  rev_date <- content(req)
  ctc_list <- rev_date[[1]]
  
  rev_num <- length(rev_date[[1]])
  while(rev_num==30){
    page_num <- page_num + 1
    full_curl <- paste0(ini_curl, day, fin_curl, page_num)
    req <- GET(full_curl, authenticate(apikey, "X", type = "basic"))
    rev_date <- content(req)
    ctc_list <- append(ctc_list, rev_date[[1]])
    rev_num <- length(rev_date[[1]])
  }
  
  new_ctc <- data.frame(matrix(nrow=length(ctc_list), ncol=2))
  colnames(new_ctc) <- c("Full.name", "Email")
  for(i in seq_along(ctc_list)){
    if(length(ctc_list[[i]]$email)>0){
      new_ctc[i,1] <-ctc_list[[i]]$name
      new_ctc[i,2] <- ctc_list[[i]]$email    
    }
  }
  
  new_ctc <- new_ctc[!is.na(new_ctc$Email), ]
  contacts <- rbind(contacts, new_ctc)
  
  write.csv2(contacts, file = file_name)
  return(contacts)
}

