#' Create a dataframe from a table on a HTML page
#'
#' @param url url of a web page
#'
#' @return dataframe
#' @export
#' @importFrom rvest html_nodes
#' @importFrom rvest html_table
#'
#' @examples
#' url <- "https://www.castorus.com/s/Paris,51185,2-1----10000"
#' get_table(url)
get_table <- function(url) {
  table <- url %>%
    read_html() %>%
    html_nodes("table") %>%
    html_table(header = NA)
  table <- as.data.frame(table)
  return(table)
}

#' Create a dataframe with all the urls with the description page of the ads
#'
#' @param url url of a web page
#'
#' @return dataframe
#' @export
#' @importFrom rvest html_nodes
#' @importFrom rvest html_attr
#'
#' @examples
#' url <- "https://www.castorus.com/s/Paris,51185,2-1----10000"
#' get_link(url)
get_link <- function(url){
  link <- url %>%
    read_html() %>%
    html_nodes("table") %>%
    html_nodes("a") %>%
    html_attr("href")
  link <- paste0("https://www.castorus.com", link)
  link <- as.data.frame(link)
  return(link)
}

#' Get the description of an apartment
#'
#' @param url url of a web page
#'
#' @return dataframe
#' @export
#' @importFrom rvest html_nodes
#' @importFrom rvest html_text
#'
#' @examples
#' url <- "https://www.castorus.com/paris-5eme-ardsmnt,d63055365"
#' get_description(url)
get_description <- function(url) {
  description <- tryCatch(
    url %>%
      read_html() %>%
      html_nodes(".contenu_bw") %>%
      html_text %>%
      unlist(),
    error = function(e){NA})
  description <- as.data.frame(description[8])
  return(description)
}

#' Create a dataframe with all the ads on Castorus website and the urls of their descriptions
#'
#' @param list_url list of urls
#'
#' @return dataframe
#' @export
#' @import dplyr
#' @importFrom purrr map
#' @importFrom data.table rbindlist
#' @importFrom xml2 read_html
#'
#' @examples
#' url1 <- "https://www.castorus.com/s/Paris,51185,2-1----10000"
#' url2 <- "https://www.castorus.com/s/Paris,51185,2-1---10001-20000"
#' list_url <- list(url1, url2)
#' get_dataframe(list_url)
get_dataframe <- function(list_url){
  castorus_table <- map(list_url, get_table)
  castorus_table <- rbindlist(castorus_table)
  url_description <- map(list_url, get_link)
  url_description_df <- rbindlist(url_description)
  colnames(url_description_df) <- "links"
  castorus_table <- cbind(castorus_table, url_description_df)
  castorus_table$links <- as.character(castorus_table$links)
  castorus_description <- map(castorus_table$links, get_description)
  castorus_description <- rbindlist(castorus_description)
  castorus_description <- as.data.frame(castorus_description) %>%
    rename(Description = 'description[8]')
  castorus_data <- cbind(castorus_table, castorus_description)
  return(castorus_data)
}

#' Remove accents from a text
#'
#' @param text character
#'
#' @return character
#' @export
#'
#' @examples
#' unaccent("hétérogéneité")
unaccent <- function(text) {
  text <- gsub("['`^~\"]", " ", text)
  text <- iconv(text, to="ASCII//TRANSLIT//IGNORE")
  text <- gsub("['`^~\"]", "", text)
  return(text)
}

#' Clean the dataframe: keep only apartments, remove euro signs, extract the street..
#'
#' @param castorus_data dataframe
#'
#' @return dataframe
#' @export
#' @import dplyr
#' @importFrom tidyr separate
#' @importFrom stringr str_detect
#'
#' @examples
#' url1 <- "https://www.castorus.com/s/Paris,51185,2-1----10000"
#' url2 <- "https://www.castorus.com/s/Paris,51185,2-1---10001-20000"
#' list_url <- list(url1, url2)
#' castorus_data <- get_dataframe(list_url)
#' clean_dataframe(castorus_data)
clean_dataframe <- function(castorus_data){
  castorus_data <- castorus_data %>%
    separate(ville,into = c("ville","rue"),sep = "[(:)]",remove = TRUE)
  castorus_data$prix <- as.character(castorus_data$prix)
  castorus_data$prix <- gsub("[^[:alnum:][:blank:]?&/\\-]", "", castorus_data$prix)
  castorus_data$prix <- gsub("[[:space:]]", "", castorus_data$prix)
  castorus_data$prix <- gsub("^[0]+", "", castorus_data$prix)
  castorus_data$prix <- as.numeric(castorus_data$prix)
  castorus_data <- castorus_data %>%
    mutate(Type = case_when((piec. != 1 & prix >= 50000) | (piec. == 1) ~ 1,
                            TRUE~ 0))
  castorus_data <- castorus_data %>%
    filter(Type == 1)
  castorus_data$ville <- tolower(castorus_data$ville)
  castorus_data <- castorus_data %>%
    dplyr::filter(grepl("paris", ville))
  castorus_data <- castorus_data %>%
    separate(ville,into = c("ville","arrondissement"),sep = "(?<=[a-zA-Z])\\s*(?=[0-9])",remove = TRUE)
  castorus_data$arrondissement <- trimws(castorus_data$arrondissement)
  castorus_data$Description <- tolower(castorus_data$Description)
  castorus_data$Description <- unaccent(castorus_data$Description)
  castorus_data$Description <- gsub("a\\(c\\)","e",castorus_data$Description)
  castorus_data$Description <- gsub("[^[:alnum:][:blank:]?&/\\-]", "", castorus_data$Description)
  castorus_data$Description <- gsub(".*:","",castorus_data$Description)
  castorus_data$arrondissement <- substr(castorus_data$arrondissement, 1, 2)
  castorus_data$arrondissement <- sub("e","", castorus_data$arrondissement)
  castorus_data$arrondissement <- as.integer(castorus_data$arrondissement)
return(castorus_data)
}



