#' This function uses FD api to extract all requests from a certain date (argument) and returns the following details per request:
#' "Ticket",
#' "Agent",
#' "Requester",
#' "Email",
#' "Platform",
#' "OS Version",
#' "Devicename",
#' "1st Problem Type",
#' "1st Issue Type",
#' "2nd Problem Type",
#' "2nd Issue Type",
#' "Tags",
#' "Language",
#' "Created", 
#' "Updated",
#' "Description"
#
#' Main features of the df are:
#' - AppStore tickets are included only
#' - 1 record for each problem type populated
#' - no duplicates where tickets have been merged
#' - NA removed where possible
#' - date formatted as dd-mm-yy HH:MM
#'
#'

fd_df <- function(day){
  
  ## The packages to load:
  
  require(httr)
  require(stringr)
  require(magrittr)
  require(openxlsx)
  require(dplyr)
  require(tidyr)
  require(readr)
  require(reshape2)
  require(rlist)
  require(ggplot2)
  require(lubridate)
  
  ## Set the different arguments or parameters:
  
  FD_api <- "z6L822MqF9kZ2K3D4J8c"
  apikey <- FD_api # constant
  group_id <- 19000105695 # constant
  created_at <- day # user input
  
  ## loading the contacts list
  contacts <- read.csv('C:/Users/apiccioni/Documents/AIFD/contacts_list.csv', sep=";", stringsAsFactors = FALSE)
  
  ## List all the tickets by date of creation (outputs a list)
  
  ini_url <- "https://alk.freshdesk.com/api/v2/tickets?include=description&per_page=100"
  page_url <- "&page="
  page_num <- 1
  full_url <- paste0(ini_url, page_url, page_num)
  
  requests <- GET(full_url, authenticate(apikey, "X", type = "basic"))
  
  ## storing the list including all requests
  req_all_tkt <- content(requests)
  req_list <- req_all_tkt[[1]]
  
  num_req <- length(req_all_tkt)
  while (num_req == 100) {
    page_num <- page_num + 1
    full_url <- paste0(ini_url, page_url, page_num)
    
    requests <- GET(full_url, authenticate(apikey, "X", type = "basic"))
    req_content <- content(requests)
    req_all_tkt <- append(req_all_tkt, req_content)
    num_req <- length(req_content)
  }
  

  appstore <- req_all_tkt %>%
    list.filter(group_id == 19000105695) %>%
    list.exclude("merged" %in% tags) %>%
    list.filter(created_at>=day)
  
  ## create dataframe including requesters id and emails
  
  
  ## create an empty dataframe to store all requests
  all_df <- data.frame(matrix(ncol = 16, nrow = 0))
  colnames(all_df) <- c(
    "Ticket",
    "Agent",
    "Requester",
    "Email",
    "Platform",
    "OS Version",
    "Devicename",
    "1st Problem Type",
    "1st Issue Type",
    "2nd Problem Type",
    "2nd Issue Type",
    "Tags",
    "Language",
    "Created", 
    "Updated", 
    "Description")
  
  
  # populate the dataframe
  for (i in 1:length(appstore)) {
    all_df[i, 'Ticket'] <- appstore[[i]]$id
    if(!is.null(appstore[[i]]$custom_fields$main_problem_type)){all_df[i, '1st Problem Type'] <- appstore[[i]]$custom_fields$main_problem_type}
    if(!is.null(appstore[[i]]$responder_id)) {all_df[i, 'Agent'] <- appstore[[i]]$responder_id}
    all_df[i, 'Requester'] <- appstore[[i]]$requester_id
    user_email <- grep(appstore[[i]]$requester_id, contacts$Id)
    all_df[i, 'Email'] <- ifelse(sum(grepl(appstore[[i]]$requester_id, contacts$Id))>0, contacts$Email[(grep(appstore[[i]]$requester_id, contacts$Id))], '') 
    
    if(!is.null(appstore[[i]]$responder_id)) {all_df[i, 'Agent'] <- appstore[[i]]$responder_id}
    
    if(!is.null(appstore[[i]]$custom_fields$device_platform)){all_df[i, 'Platform'] <- appstore[[i]]$custom_fields$device_platform}
    if(!is.null(appstore[[i]]$custom_fields$mobile_device)) {all_df[i, 'OS Version'] <- appstore[[i]]$custom_fields$mobile_device}
    
    if(!is.null(appstore[[i]]$custom_fields$device_make_and_model)){all_df[i, 'Devicename'] <- appstore[[i]]$custom_fields$device_make_and_model}
    if(!is.null(appstore[[i]]$custom_fields$specific_issue)){all_df[i, '1st Issue Type'] <- appstore[[i]]$custom_fields$specific_issue}
    if(!is.null(appstore[[i]]$custom_fields$cf_2nd_problem_type)){all_df[i, '2nd Problem Type'] <- appstore[[i]]$custom_fields$cf_2nd_problem_type}
    if(!is.null(appstore[[i]]$custom_fields$cf_2nd_specific_issue)){all_df[i, '2nd Issue Type'] <- appstore[[i]]$custom_fields$cf_2nd_specific_issue}
    
     
    if(!is.null(appstore[[i]]$tags)){
      tags_ls <- appstore[i][[1]]$tags
      all_df[i, 'Tags'] <- paste(tags_ls, collapse = ", ")
      }
    if(!is.null(appstore[[i]]$custom_fields$language)){all_df[i, 'Language'] <- appstore[[i]]$custom_fields$language}
    if(!is.null(appstore[[i]]$created_at)){all_df[i, 'Created'] <- appstore[[i]]$created_at}
    if(!is.null(appstore[[i]]$updated_at)){all_df[i, 'Updated'] <- appstore[[i]]$updated_at}
    all_df[i, 'Description'] <- appstore[[i]]$description_text
  }
  

  all_df$Created <- as.Date(all_df$Created)
  all_df$Updated <- as.Date(all_df$Updated)
  
  return(all_df)
}


