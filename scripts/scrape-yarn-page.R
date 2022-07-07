# load packages ----------------------------------------------------------------

library(tidyverse)
library(rvest)
library(robotstxt)
library(magick)

# can we scrape?


paths_allowed("https://www.jamiesonsofshetland.co.uk/jamiesons-of-shetland-spindrift-1-c.asp")

# set urls ----------------------------------------------------------------------

urls <- c("https://www.jamiesonsofshetland.co.uk/jamiesons-of-shetland-spindrift-1-c.asp",
          "https://www.jamiesonsofshetland.co.uk/jamiesons-of-shetland-spindrift-70190nproducts1curpage-2-1-c.asp",
          "https://www.jamiesonsofshetland.co.uk/jamiesons-of-shetland-spindrift-70190nproducts1curpage-3-1-c.asp")

# Creating our function  --------------------------------------------------------------


scrape_yarn_page <- function(url){
  

# read first page --------------------------------------------------------------
  
page <- read_html(url)
  
# scrape yarn name ----------------------------------------------------------------

yarn_name <- page %>%
  html_elements(".product__item__title") %>%
 # html_element("h3 a") %>%
  html_text() %>%
  str_squish()

# scrape image path -----------------------------------------------------------------

image <- page %>%
  html_elements("img") %>%
  html_attr('src') %>%
  str_subset(pattern = ".jpg")

image_html <- paste0("<img src=\"", image, "\"", " alt=\"", yarn_name, "\" style=\"width:150px;height:100px;\">")

# scrape link to yarn info page -----------------------------------------------------------------

yarn_link <- page %>% 
  html_elements('a') %>%
  html_attr('href') %>%
  str_subset(pattern = "spindrift") %>%
  str_subset(pattern = "-p.asp") %>%
  unique()

link <- str_c("https://www.jamiesonsofshetland.co.uk/", yarn_link, sep = "")

# create table -----------------------------------------------------------------

yarn_table <- data.frame("Name" = yarn_name, 
                         "Yarn" = image_html, 
                         "Link" = link, 
                         "Image_URL" = image)  

yarn_table <- yarn_table %>%
  mutate("Name" = paste0("<a href=\"", Link, "\">", Name, "</a>"))

yarn_table

}

# map over all urls and output a data frame ------------------------------------

yarn_table <- map_dfr(urls, scrape_yarn_page)

# creating the colour palette

get_top_color_blend <- function(image_path) {
  image_i <- image_read(image_path)
  img <- image_convolve(image_i, 'Gaussian:0x5', scaling = '60,20%')
  top_hex <- image_data(img) %>%
    apply(2:3, paste, collapse = "") %>% 
    as.vector %>% table() %>%  as.data.frame() %>% 
    setNames(c("hex", "freq")) %>%
    mutate(hex = paste("#", hex, sep="")) %>%
    slice_max(order_by = freq, n = 1) %>%
    pull(hex)
  
  return(top_hex) # Keep researching to eplace with average
}

get_top_color <- function(image_path) {
  img <- image_read(image_path)
  top_hex <- image_data(img) %>%
    apply(2:3, paste, collapse = "") %>% 
    as.vector %>% table() %>%  as.data.frame() %>% 
    setNames(c("hex", "freq")) %>%
    mutate(hex = paste("#", hex, sep="")) %>%
    slice_max(order_by = freq, n = 3) %>%
    pull(hex)
  
  return(top_hex[3]) #
}

get_top3_color <- function(image_path) {
  img <- image_read(image_path)
  top_hex <- image_data(img) %>%
    apply(2:3, paste, collapse = "") %>% 
    as.vector %>% table() %>%  as.data.frame() %>% 
    setNames(c("hex", "freq")) %>%
    mutate(hex = paste("#", hex, sep="")) %>%
    slice_max(order_by = freq, n = 3) %>%
    pull(hex)
  
  return(top_hex[2]) #
}

# Function to count the colours (adapted from Jeroen Ooms and Matt Dray: https://www.rostrum.blog/2018/11/25/art-of-the-possible/)


for(i in 1:nrow(yarn_table)){
  yarn_table$`Hex Code (B)`[i] <- get_top_color_blend(yarn_table$Image_URL[i])
  yarn_table$`Hex Code (M)`[i] <- get_top_color(yarn_table$Image_URL[i])
  yarn_table$`Hex Code (3)`[i] <- get_top3_color(yarn_table$Image_URL[i])
  
}



# write out data frame ---------------------------------------------------------

write_csv(yarn_table, file = "template/yarn_table.csv")
