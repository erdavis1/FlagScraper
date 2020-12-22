library(rvest)
library(dplyr)

options(stringsAsFactors = FALSE)

#url to scrape links from
url <- "https://www.crwflags.com/fotw/flags/jp-city.html"

#get all links to municpalities
links <- read_html(url)  %>% html_nodes("a")  %>% html_attr('href')
links <- links[grep("jp-[0-9]+.*.html$", links)]

#go to each municipality and get url of each image on the page
images <- data.frame(link = character())
for (i in 1:length(links)) {
  img <- read_html(paste0("https://www.crwflags.com/fotw/flags/", links[i])) %>% html_nodes("img") %>% html_attr('src')
  images <- bind_rows(images, data.frame(link = img))
}

images <- unique(images)
images <- images[grepl("../images/j/", images$link), ]
images <- as.data.frame(images)
images$images <- substr(images$images, 3, nchar(images$images))


#now actually download the images 
for (i in 1:nrow(images)) {
  url <- paste0('https://www.crwflags.com/fotw', images$images[i])
  file <- substr(images$images[i], 11, nchar(images$images[i]))
  download.file(url,  file, mode = "wb")
}
