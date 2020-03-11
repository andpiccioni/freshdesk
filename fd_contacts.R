
#' This function takes the day as argument and the contact_list.csv file
#' as data input, to create a list of contacts including id, email and name
#' which will be then updated by the function itself.
#' The function uses FD api to extract contacts.

fd_new_contacts <- function(day){
  require(httr, quietly = TRUE)
  require(dplyr, quietly = TRUE)
  wd <- getwd()
  file_name <- paste0(wd, "/", "contacts_list.csv")
  contacts <- read.csv(file_name, stringsAsFactors = FALSE, sep=";")%>% select("Id", "Name", "Email") %>% distinct(.keep_all = T) ## loading full contact list
  apikey <- Sys.getenv("fd_api")
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

  new_ctc <- data.frame(matrix(nrow=length(ctc_list), ncol=3))
  colnames(new_ctc) <- c("Id", "Name","Email")
  for(i in seq_along(ctc_list)){
    new_ctc[i,1] <-ctc_list[[i]]$id
    if(!is.null(ctc_list[[i]]$name)){
      new_ctc[i,2] <- ctc_list[[i]]$name
    }
    if(!is.null(ctc_list[[i]]$email)){
      new_ctc[i,3] <- ctc_list[[i]]$email
    }
  }

  new_ctc <- new_ctc[!is.na(new_ctc$Email), ]
  contacts <- rbind(contacts, new_ctc)

  write.csv2(contacts, file = file_name)
  return(contacts)
}


