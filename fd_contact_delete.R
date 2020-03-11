#' Functions to soft delete and hard delete contacts from FD
#'
#'
sdelete_contact <- function(contact_id){
  require(httr)
  apikey <- Sys.getenv("fd_api")
  delete <- 'https://alk.freshdesk.com/api/v2/contacts/'
  url_query <- paste0(delete, contact_id)
  DELETE(url_query, authenticate(apikey, "X", type="basic"), verbose())
}

hdelete_contact <- function(contact_id){
  require(httr)
  apikey <- Sys.getenv("fd_api")
  delete <- 'https://alk.freshdesk.com/api/v2/contacts/'
  url_query <- paste0(delete, contact_id, '/hard_delete')
  DELETE(url_query, authenticate(apikey, "X", type="basic"), verbose())
}


