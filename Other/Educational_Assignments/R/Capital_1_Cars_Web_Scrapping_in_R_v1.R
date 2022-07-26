#if(!require(devtools)) install.packages("devtools")
#if(!require(tidyverse)) install.packages("tidyverse")
#library(devtools)

## Installation of RSelenium
#devtools::install_github("johndharrison/binman")
#devtools::install_github("johndharrison/wdman")
#devtools::install_github("ropensci/RSelenium")


#Load Libraries
library(RSelenium)
##checkForServer() # search for and download Selenium Server java binary.  Only need to run once.
##startServer() # run Selenium Server binary
## Above method not works anymore in R, needs Docker Selenium Images
library(tidyverse)
library(rvest)
library(dplyr)
## Installation of Docker Engine preferably some other machine/node
## https://docs.docker.com/engine/install/ubuntu/
## https://www.digitalocean.com/community/tutorials/how-to-install-and-use-docker-on-ubuntu-22-04
## Steps involved in Scrapping Data
## 1. Goto Home Page
## 2. Click on Car Type
## 3. Click on Car for next page
## 4. Scroll Down Click on Features and Fetch details from this PAge
## go Back click on other car

## let's start
## this will check whether scrapping allowed for website or NOT
robotstxt::paths_allowed("<URL of Capital1 Cars>")

#Function to scroll to desired element -- Not used
scrollTO <- function(remDr,webElem){
  remDr$executeScript("arguments[0].scrollIntoView(true):", args = list(webElem))
  webElem$highlightElement()
}

# This Function fetches "Year,Model,Car Name,Car City, Car State" from Car Grid
collect_car_make_model <- function(pg,car_details_list){
  pg <- remDr$getPageSource() %>% .[[1]] %>% read_html()
  nytnyt()
  ## For Year and Make
  year_make_model <- strsplit( pg %>% html_nodes(".vehicle-info__year-make-model") %>% html_text(), " ")
  ## Extracting City and State Code
  car_city_state <- tail(strsplit(pg %>% html_nodes(".vehicle-info__geo.ng-star-inserted") %>% html_text(), " ")[[1]], -2)
  if (length(car_city_state) >= 3)  {
    car_city = sub(",","",paste(car_city_state[1],car_city_state[2], collapse = " "))
    car_state = car_city_state[3]
  } else {
    car_city = sub(",","",car_city_state[1])
    car_state = car_city_state[2]
  }
  ## For Car Model Name, Remove "For Sale" using head() and combine list of words using paste()
  car_model_name <- head(strsplit( pg %>% html_nodes(".vehicle-info__trim") %>% html_text(), " ")[[1]],-2)
  car_name <- paste(car_model_name, collapse = " ")
  nytnyt(periods = c(6, 6.5))
  car_details_list[["Year"]] <<- year_make_model[[1]][1]
  car_details_list[["Model"]] <<- year_make_model[[1]][2]
  car_details_list[["Name"]] <<- car_name
  car_details_list[["City"]] <<- car_city
  car_details_list[["State"]] <<- car_state
}

# This Function fetches other required details specific to the car such as mileage, engine type,Color, etc.
collect_car_details <- function(pg,car_details_list){
  pg <- remDr$getPageSource() %>% .[[1]] %>% read_html()
  nytnyt()
  car_details <- pg %>% html_nodes(".detail-tile.grv-col--sm-2.grv-col--md-4.grv-col--lg-3") %>% html_text2()
  nytnyt(periods = c(4, 5.0))
  car_addl_details <- pg %>% html_nodes(".detail-tile.wide-text.grv-col--sm-2.grv-col--md-4.grv-col--lg-3") %>% html_text2()
  nytnyt(periods = c(4, 5.0))
  ## Loop over items and store in list
  for (i in 1:(length(car_details)-1)) {
    tmp_variable = strsplit(as.character(car_details[[i]]), split = "\n")
    ## Replace SPACE with _ in DataFrame Columns
    x <- gsub("[[:space:]]", "_", tmp_variable[[1]][1])
    car_details_list[[x]] <<- tmp_variable[[1]][2]
  }
  
  ## Loop over items and store in list just for to handle Stock Number and VIN 
  for (i in 1:2) {
    tmp_variable = strsplit(as.character(car_addl_details[[i]]), split = "\n")
    x <- gsub("[[:space:]]", "_", tmp_variable[[1]][1])
    car_details_list[[x]] <<- tmp_variable[[1]][2]
  }
  
}

# Function for Random Sleep to be diligent scrapping
nytnyt <- function(periods = c(3.5, 4.0)) {
  tiktik <- runif(1, periods[1], periods[2])
  cat(paste0(Sys.time()), "- Sleeping for ", round(tiktik,2), "seconds\n")
  Sys.sleep(tiktik)
}



# Variables Used
df <- data.frame(matrix(ncol = 17, nrow = 0))
x <- c("Year","Model","Name","City","State","Condition","Mileage","MPG","Body_Style","Exterior_Color","Interior_Color",
       "Transmission","Drive_Train","Fuel_Type","Engine","Stock_Number","VIN")
colnames(df) <- x
car_details_list <- list()
no_of_pages = 10

# sample csv name
csv_fname = "capital1-cars-web-scapping-using-R.csv"


## Main ##
## Initialize Driver, make sure that docker is up and running, accepting requests on port 445
remDr <- remoteDriver(remoteServerAddr = "localhost", port=4445L, browserName = "chrome")
remDr$open()
## This Code is specific to Capital1 Cars, since various CSS Element tags are Hard-Coded
remDr$navigate("<URL of Capital1 Cars>")


#find the Car Type Element
webElem <- remDr$findElement("css", '.shop-button.grv-text--tiny.grv-text--small-semibold.ng-star-inserted')

webElem$highlightElement()
webElem$clickElement()
## Add Timer Here
nytnyt(periods = c(4, 5))
## scroll to the middle of page
bodyStyleElem <- remDr$findElement("css", '.ng-star-inserted')
nytnyt(periods = c(4, 5))
bodyStyleElem$sendKeysToElement(list(key = "page_down"))


## Tag for Car Body Style Filter
bodyStyleElem <- remDr$findElement("id", value = "grv-expansion-panel-header-10")
bodyStyleElem$clickElement()
## Add Timer Here
nytnyt()
## Tag for SUV CAr Type, Please change below <for> tag for other Car Category indexes ranges from 56 to 64
carTypeCheck <- remDr$findElement(using = "xpath", value = '//*[(@for = "grv-checkbox-60-input")]')
carTypeCheck$highlightElement()
carTypeCheck$clickElement()
## Add Timer Here
nytnyt(periods = c(5, 5.5))
bodyStyleElem$sendKeysToElement(list(key = "page_up"))
nytnyt(periods = c(2, 3.0))


# For loop PAge Turns
for (grid_page_value in 1:no_of_pages) {
  carGrid <- remDr$findElements(using = "xpath", value = '//*[(@class = "search-page-grid-card grv-color--charcoal-70")]')

  # This For loop fetches card details from Grid
  for (i in 1:(length(carGrid))) {

    nytnyt(periods = c(6, 6.5))
    carGrid <- remDr$findElements(using = "xpath", value = '//*[(@class = "search-page-grid-card grv-color--charcoal-70")]')
    nytnyt(periods = c(4, 4.5))
    carGrid[[i]]$clickElement()
    ### Add Timer here
    nytnyt(periods = c(2, 3.0))
    ## Car Details Page
    pg <- remDr$getPageSource() %>% .[[1]] %>% read_html()
    #cat(paste0(pg))
    nytnyt(periods = c(6, 6.5))

    ## Collect Car Model, Year, Name
    collect_car_make_model(pg,car_details_list)
    
    
    bodyStyleElem <- remDr$findElement("css", '.vdp-footer-active')
    nytnyt(periods = c(1, 2))
    bodyStyleElem$sendKeysToElement(list(key = "page_down"))
    bodyStyleElem$sendKeysToElement(list(key = "page_down"))
    nytnyt(periods = c(2, 3))
    carDetailsPage <- remDr$findElement("css", value = ".grv-row.grv-row--fill.car-details")
    nytnyt(periods = c(1, 2))
    carDetailsPage$highlightElement()
    ## Collect More Detailed info about Car
    collect_car_details(pg,car_details_list)
    
    df <- rbind(df, car_details_list)
    # Write End Result to CSV at last, this is bad  idea to write results at last.
    write.csv(df, csv_fname, row.names = FALSE)
    
    # Write Page wise results to CSV file write.table can appends to existing file, 
    # write.csv overwrites file
    #write.table(df, file=csv_fname)
    
    ## Go back to previous Search Results
    nytnyt(periods = c(1, 2))
    bodyStyleElem$sendKeysToElement(list(key = "page_up"))
    bodyStyleElem$sendKeysToElement(list(key = "page_up"))
    remDr$goBack()
    nytnyt(periods = c(3, 3.5))
    
 }
  carGrid <- remDr$findElements(using = "xpath", value = '//*[(@class = "search-page-grid-card grv-color--charcoal-70")]')
  bodyStyleElem$sendKeysToElement(list(key = "page_down"))
  bodyStyleElem$sendKeysToElement(list(key = "down_arrow"))
  bodyStyleElem$sendKeysToElement(list(key = "down_arrow"))
  nytnyt(periods = c(2, 3.0))
  ## Page Numbers
  # Tag - .page-number.grv-text--normal-semibold.ng-star-inserted
  ## Handling upward Page movements 
  #href1 = '/cars/find/New+York-NY/?radius=50&page='
  href1 = '/cars/find/New+York-NY/new-used/all-years/all-makes/all-models/all-trims/SUV?radius=50&page='
  nytnyt()
  href2 = grid_page_value + 1
  #href3 = "&sort=priceLowest&bodyStyle=SUV"
  href3 = '&sort=priceLowest'
  href_url = paste(href1,href2,href3,sep = "")
  # Function to Override + to achieve usages of variables in findElement
  '+' <- function(e1, e2) {
    if (is.character(e1) | is.character(e2)) {
      paste0(e1, e2)
    } else {
      base::`+`(e1, e2)
    }
  }
  
  ## Move to next Page
  getPageNumber <- remDr$findElement(using = "xpath", value = '//*[(@class = "page-number grv-text--normal-semibold ng-star-inserted")]')
  pageNumbers <- remDr$findElement(using = "xpath", "//*[(@aria-label = 'Page " + href2 + "')]")
  pageNumbers$highlightElement()
  pageNumbers$clickElement()
  nytnyt()
  bodyStyleElem$sendKeysToElement(list(key = "page_up"))
  bodyStyleElem$sendKeysToElement(list(key = "page_up"))
  nytnyt(periods = c(2, 3.0))
  

}


remDr$close()

