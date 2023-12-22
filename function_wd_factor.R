
if(!require(rvest)){ install.packages("rvest") ; library(rvest) }
if(!require(tidyverse)){ install.packages("tidyverse") ; library(tidyverse) }

wd_factor <- function(dane, wd) {

  # pobieramy zakresy danych z rvest
    
  url <-
    'http://snowfence.umn.edu/Components/winddirectionanddegreeswithouttable3.htm'
  
  page <- read_html(url)
  
  # modyfikujemy tabele
  
  directions_raw <-
    page %>% html_node('td table') %>% html_table(header = TRUE)
  
  directions <- directions_raw %>%
    set_names( ~ tolower(sub(' Direction', '', .x))) %>%
    slice(-1) %>%
    separate(degree,
             c('degree_min', 'degree_max'),
             sep = '\\s+-\\s+',
             convert = TRUE)
  
  rm(url, page, directions_raw)
  
  dane <- dane |> 
    mutate(wd = if_else(wd == 0, 360, wd))
  
  dane <- dane %>%
    mutate(
      wd_cardinal = cut(
        {{wd}}, 
        breaks = c(0, directions$degree_max, 360),
        labels = c(directions$cardinal, 'N')
    ))
  
  return(dane)
}

# źródło kodu: 
# https://community.rstudio.com/t/convert-wind-direction-degrees-into-factors-in-a-data-frame/14636/2

## TEST ----------------

## Usuń komentarze dla koduc - zaznacz i ctrl+shift+c


## przykłdowe dane

# wind_dir <- data_frame(
#   date = structure(c(13514, 13515, 13516, 13517, 13518, 13519), class = "Date"),
#   SN = c(84084, 84084, 84084, 84084, 84084, 84084),
#   ws = c(21.3, 19.9, 19.3, 21.7, 14.4, 13.9),
#   stopnie = c(34.8, 37.8, 38.2, 15.1, 359, 355)
# )

##  przykład

# wind_dir |>
#   wd_factor(wd = stopnie)
