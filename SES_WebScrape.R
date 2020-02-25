# Created by Sam Parm on YEAR-MT-DY
# Validated by NAME on YEAR-MT-DY
# Purpose: Web scrape employee salaries data  for agency of interet in State of Indiana and output as CSV file to working directory
# Production Schedule (if program is to be run on a regular basis):
# Limitations and Warnings: see data source info link
# Data Source Info: 
  #Indiana state gov posts all gov employee salaries online for transperancy and public use 
  #More info here: https://www.in.gov/itp/1130.htm
  #Source url for the salary search tool: https://www.in.gov/apps/gov/salaries/

# Program Derived From "other_program_name.R" (if applicable)
# Program Flow Description (high level review of the steps of the program) ; ----
#  1) Load libraries and define key parameters
#  2) Develop functions to web scrape data
#  3) Scrape data from agency of interest
#  4) Export data
#  X) Clean up. 

# Leave mark at beginning of program log see when the run started (if needed)
#print("###############################################")
#paste("Run started at", Sys.time())
#print("###############################################")

# 1) Load libraries and define key constants, formats, etc. ; ----
#### PROJECT CODE GOES HERE ####
library(rvest)
library(tidyverse)
library(rio)
library(lubridate)

# 2) Develop functions to pull data from web tool  ####

source_url = "https://www.in.gov/apps/gov/salaries/"

get_firstStep <- function(agency) {
  agency <- agency %>% str_replace_all(" ", "+") %>% str_replace_all("&", "%26") %>% 
    str_replace_all('"',"%27")
  example_url <- "https://www.in.gov/apps/gov/salaries/?searchPerformed=true&firstName=&lastName=&agency=HEALTH&offset=0&max=25"
  new_url <- example_url %>% str_replace("HEALTH", agency)
  return(new_url)
}

get_lastStep <- function(url) {
  webpage <- read_html(url)
  tbls_ls2 <- webpage %>%
    html_nodes("a.step") %>% html_attr("href")
  tbls_ls2<-data.frame(tbls_ls2) 
  last_url <- tbls_ls2 %>% tail(.,1) %>% .[[1]] %>% 
    as.character() %>% str_c("https://www.in.gov",.)
  return(last_url)
}

get_URLlist <- function(last_URL) {
  last_step <- last_URL %>% str_extract("offset=.*&") %>% str_extract("\\d+") %>% as.numeric()
  base_url <- last_URL %>% str_extract(".*(&offset=)")
  health_step <-seq(0,last_step, 25)
  list_url<-paste(base_url,health_step,"&max=25",sep="")
  return(list_url)
}

my_webPull <- function(url) {
  webpage <- read_html(url)
  tbls_ls2 <- webpage %>%
    html_nodes("table") %>%
    .[2] %>%
    html_table(fill = TRUE)
  tbls_ls2<-data.frame(tbls_ls2) 
  return(tbls_ls2)
}

my_dataPull <- function(list_url){
  SalariesData<-lapply(list_url, my_webPull)
  SalariesData2<-data.frame(SalariesData[1]) # get first data pull 
  for (i in 2:n_distinct(list_url)){
    #print(i)
    SalariesData2<-rbind(SalariesData2,data.frame(SalariesData[i])) # get rest data pulls for agency
  }
  #SalariesData2 %>% mutate(pull_date=today())
  SalariesData2 <- unique(SalariesData2) 
  return(SalariesData2)
}

getData <- function(agency_name) {
  ISDH_URLlist<- get_firstStep(agency_name) %>% 
    get_lastStep() %>% 
    get_URLlist()
  health_data <- my_dataPull(ISDH_URLlist)
  return(health_data)
}
# 3) Scrape data for agency of interest using functions created ####

#specify agency of interest here 
#make sure it matches the tool's name for the agency (visit link above to check naming)

#health_data <- getData("HEALTH")
mph_data <- getData("MANAGEMENT PERF HUB")
#fssa_admin <- getData("FAMILY & SOCIAL SVCS ADMIN")
# workerscomp_data <- getData("WORKER'S COMP BRD")


# 4) Export data just scraped ####

build_exportURL <- function(dataset){
  export_url <- paste(getwd(),"/SalariesData_",
                      dataset[1,3],"_",
                      today(),".csv",sep="")
  return(export_url)
}

# export_url <- build_exportURL(health_data)
# export(health_data,export_url)

export_url <- build_exportURL(mph_data)
export(mph_data,export_url)

# export_url <- build_exportURL(fssa_admin)
# export(fssa_admin,export_url)

# export_url <- build_exportURL(workerscomp_data)
# export(workerscomp_data,export_url)

#data is export to an excel file which is now in your working directory see 'export_url' for path

# X) Clean up. ----
# Delete all objects that were created to clean up environment
# Uncomment when ready to run
#rm(list=ls())

# Leave mark at end of program to see when the run ended (if needed)
#print("###############################################")
#paste("Run ended at", Sys.time())
#print("###############################################")