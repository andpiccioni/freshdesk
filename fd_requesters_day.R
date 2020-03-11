#' This function takes the day as argument and creates a list with two columns:
#'  - the ticket number
#'  - CoPilot id
#'  Its output includes
#'  - the list of all requests created since the day
#'  - a csv file of the requesters without duplicates
#'  - number of requests

requesters_day <- function(day) {
  
  ## Libraries
  require(httr, quietly = TRUE)
  require(stringr, quietly = TRUE)
  require(magrittr, quietly = TRUE)
  require(openxlsx, quietly = TRUE)
  require(dplyr, quietly = TRUE)
  
  ## Constants
  apikey <- Sys.getenv("fd_api")
  ini_curl <- "https://alk.freshdesk.com/api/v2/search/tickets?query=\"tag:null%20AND%20group_id:19000105695%20AND%20created_at:>%27"
  fin_curl <- "%27\"&page="
  get_url <- "https://alk.freshdesk.com/api/v2/contacts/"
  
  ## variabili
  page_num <- 1
  full_curl <- paste0(ini_curl, day, fin_curl, page_num)
  req <- GET(full_curl, authenticate(apikey, "X", type = "basic"))
  
  ## storing the content of the call
  rev_date <- content(req)
  tckt_list <- rev_date[[1]]
  
  rev_num <- length(rev_date[[1]])
  while(rev_num==30){
    page_num <- page_num + 1
    full_curl <- paste0(ini_curl, day, fin_curl, page_num)
    req <- GET(full_curl, authenticate(apikey, "X", type = "basic"))
    rev_date <- content(req)
    tckt_list <- append(tckt_list, rev_date[[1]])
    rev_num <- length(rev_date[[1]])
  }
  
  requesters <- data.frame(matrix(ncol=3,nrow=0))
  colnames(requesters) <- c("ticket", "email", "curl")
  
  for(i in 1:length(tckt_list)) {
    if(is.null(tckt_list[[i]]$custom_fields$copilot_id) || tckt_list[[i]]$custom_fields$copilot_id=="NA") {
      contact_id <- tckt_list[[i]]$requester_id
      get_contact <- paste0(get_url, contact_id, '\"')
      req_contact <-
        GET(get_contact, authenticate(apikey, "X", type = "basic"))
      contact_details <- content(req_contact)
      requesters[i, 2] <- contact_details$email
    }else{
      requesters[i, 2] <- tckt_list[[i]]$custom_fields$copilot_id
    }
    requesters[i, 1] <- tckt_list[i][[1]]$id
  }
  

  
  requesters_uni <- requesters %>%
    distinct(email, .keep_all = TRUE) %>%
    filter(!grepl("@alk.com", email)) %>%
    filter(!grepl("@trimble.com", email))
  
  write.csv2(requesters_uni, file = "requests_uni_day.csv", row.names = FALSE)
  write.csv2(requesters, file = "requesters.csv", row.names = FALSE)
  
  total_req <- as.numeric(nrow(requesters_uni))
  return(total_req)

}
