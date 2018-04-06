library(dplyr)
library(jsonlite)
library(purrr)
library(tidyr)
library(ggplot2)
library(stringr)

loadNamespace("httr")

apiUrl <- "https://api.projectoxford.ai/emotion/v1.0/recognizeInVideo?outputStyle=perFrame"

key <- 'XXXXXXXXXXXXXXXXXXXXXXXXX'
  
urlVideo <- 'https://drive.google.com/uc?export=download&id=0B0ABksVe1m1QVVZMcXR6UUUyd28'

mybody <- list(url = urlVideo)

faceEMO <- httr::POST(
  url = apiUrl,
  httr::content_type('application/json'),
  httr::add_headers(.headers = c('Ocp-Apim-Subscription-Key' = key)),
  body = mybody,
  encode = 'json'
)

operationLocation <- httr::headers(faceEMO)[["operation-location"]]

while(TRUE){
  ret <- httr::GET(operationLocation,
                   httr::add_headers(.headers = c('Ocp-Apim-Subscription-Key' = key)))
  
  con <- httr::content(ret)
  
  if(is.null(con$status)){
    warning("Connection Error, retry after 1 minute")
    Sys.sleep(60)
  } else if (con$status == "Running" | con$status == "Uploading"){
    cat(paste0("status ", con$status, "\n"))
    cat(paste0("progress ", con$progress, "\n"))
    Sys.sleep(60)
  } else {
    cat(paste0("status ", con$status, "\n"))
    break()
  }
}

data <- (con$processingResult %>% jsonlite::fromJSON())$fragments



data$events <- purrr::map(data$events, function(events){
  events %>% purrr::map(function(event){
    jsonlite::flatten(event)
  }) %>% bind_rows()
})

test <- unnest(data, events)

trump <- test %>%
  gather(key, value, c(scores.anger, scores.contempt, scores.disgust, scores.fear, scores.happiness, scores.neutral, scores.sadness, scores.surprise)) %>%
  #filter(!key == "scores.neutral") %>%
  #filter(id == 0) %>%
  mutate(key = str_replace(key, "scores.", "")) %>%
  mutate(candidate = "Trump")

trump$sec <- trump$start/30

ggplot(trump, aes(sec, value, group = key, col = key)) +
  #geom_line(color = fac) +  would display all the non-smoothed lines
  geom_smooth(method = "loess", n = 100000, se = F,  span = 0.1) +
  facet_wrap(~ candidate, ncol = 1) + theme_minimal()
