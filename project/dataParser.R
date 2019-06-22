library('rvest')
library('stringr')

urls = c('http://www.edukayurtlari.com/webmining/DIL.html?fbclid=IwAR3H4yFrr6-9oa4gCTBmlsVL0UAr4tnaxCTXv3S_W3WmimqeZlKyuNSPQm0',
         'http://www.edukayurtlari.com/webmining/SOZ.html?fbclid=IwAR3H4yFrr6-9oa4gCTBmlsVL0UAr4tnaxCTXv3S_W3WmimqeZlKyuNSPQm0',
         'http://www.edukayurtlari.com/webmining/SAY.html?fbclid=IwAR3H4yFrr6-9oa4gCTBmlsVL0UAr4tnaxCTXv3S_W3WmimqeZlKyuNSPQm0',
         'http://www.edukayurtlari.com/webmining/EA.html?fbclid=IwAR3H4yFrr6-9oa4gCTBmlsVL0UAr4tnaxCTXv3S_W3WmimqeZlKyuNSPQm0') 

mainFunc <- function(urls){
  Departments <- data.frame()
  for(i in urls){
    webpage <- read_html(i)
    name_data <- getColons('.dt-left strong',webpage)
    dept_data <- getColons('.dt-left+ .vcenter strong', webpage)
    city_data  <- getColons('td:nth-child(5)', webpage) 
    type_data  <- getColons('td:nth-child(6)', webpage)
    cost_data  <- getColons('td:nth-child(7)', webpage)
    period_data  <- getColons('td:nth-child(8)', webpage)
    quota_data  <- getColons('td:nth-child(9) font:nth-child(2)', webpage)
    rank_data  <- getColons('td:nth-child(12) font:nth-child(2)', webpage)
    minPoint_data  <- getColons('.sorting_1 font:nth-child(2)', webpage)
    lang_data  <- getColons('a~ font', webpage)
    point_type  <- getColons('.panel-title strong', webpage)
    
    rank_data<-gsub("---",NA, rank_data)
    rank_data<-gsub("Dolmadı",NA, rank_data)
    
    minPoint_data<-gsub("---",NA, minPoint_data)
    minPoint_data<-gsub("Dolmadı", NA, minPoint_data)
    
    lang_data <- gsub("\\(", "", lang_data)
    lang_data <- gsub("\\)", " ", lang_data)
    point_type <- strsplit(point_type," ")[[1]][1]
    
    single_df<-data.frame(Name = name_data,
                          Department = dept_data,
                          City = city_data,
                          Point_Type = point_type,
                          Type = type_data,
                          Cost = cost_data,
                          Period = period_data,
                          Quota = quota_data,
                          Rank = rank_data,
                          'Base_Score' = minPoint_data,
                          Summary = lang_data)
    Departments <- rbind(Departments, single_df)
    Sys.sleep(2)
  }
  Departments <- changeColTypes(Departments)
  return(Departments)
}

changeColTypes <- function(df){
  df$Name = as.character(df$Name)
  df$Department = as.character(df$Department)
  df$Rank = gsub("\\.", "", df$Rank)
  df$Base_Score = gsub(",", ".", df$Base_Score)
  df$Rank = as.numeric(as.character(df$Rank))
  df$Base_Score = as.double(as.character(df$Base_Score))
  df$Quota = as.character(df$Quota)
  df$Summary = as.character(df$Summary)
  return(df)
}

getColons <- function(name, webpage){
  data_html <- html_nodes(webpage,name)
  
  data <- html_text(data_html)
  return (data)
}

trim <- function(d){
  d$City = gsub(" ","", d$City)
  d$City = as.factor(d$City)
  return(d)
}

Departments <- mainFunc(urls)
backup <- Departments
Departments <- backup
setwd("~/Desktop/project")
locations <- read.csv("~/webApp/data/output.csv",header=FALSE)

Departments$Name <- as.factor(str_trim(Departments$Name))
Departments$Department <- as.factor(Departments$Department)

for(i in c(1:NROW(locations))){
  Departments[Departments[,"Name"] == locations[i,1],"Latitude"] <- locations[i,2]
  Departments[Departments[,"Name"] == locations[i,1],"Longitude"] <- locations[i,3]
}

Departments = trim(Departments)
levels(Departments$City)

write.csv(Departments, file = "webApp/departments.csv",row.names=TRUE)

