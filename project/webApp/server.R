library("tidyverse")

server <- function(input, output, session) {
  filteredData <- reactive({allDepartments %>% filter(
    Department %in% input$dept,
    City %in% input$city,
    Base_Score >= input$baseScore[1],
    Base_Score <= input$baseScore[2])
  })
  
  output$map <- renderLeaflet({
    leaflet(allDepartments) %>% addTiles() %>% fitBounds(~min(allDepartments$Longitude), ~min(allDepartments$Latitude), ~max(allDepartments$Longitude), ~max(allDepartments$Latitude))
  })
  flag <- 0
  
  observe({
    leafIcons <- ~icons(
      iconUrl = ifelse(Type == "Devlet",
                       ifelse(Period == "Gündüz", "./sun.png", "moon.png"),
                       "./money.png"
      ),
      iconWidth = 24, iconHeight = 24,
      iconAnchorX = 12, iconAnchorY = 23
    )
    leafletProxy("map", data = filteredData()) %>%
      clearMarkers() %>%
      addControl(html = html_legend, position = "bottomright")%>%               #Legend
      fitBounds(~min(Longitude), ~min(Latitude), ~max(Longitude), ~max(Latitude)) %>%
      addMarkers(lng=~jitter(Longitude,factor = 0.01,amount = 0), icon = leafIcons, lat=~jitter(Latitude, factor = 0.01, amount = 0), 
                 popup = ~paste("<strong>",Name,"</strong><br/>",
                                "<strong>",Department,"</strong><br/>",
                                "<strong>Açıklama : </Strong>",Summary,"<br/>",
                                "<strong>Kontenjan : </Strong>",Quota,"<br/>",
                                "<strong>Taban Puanı : </Strong>",Base_Score,"<br/>",
                                "<strong>Sıralama : </Strong>",Rank,"<br/>"
                                ))
  })
  
  output$tableView <- DT::renderDataTable({
    df <- allDepartments %>%
      filter(
        Department %in% input$dept2,
        City %in% input$city2,
        Type %in% input$type2,
        Period %in% input$period2
      ) %>% arrange(Rank) %>% select(1,2,3,5,6,7,8,9,10,11)
  })
  
  #Legend icons  
  html_legend <- "<img src= 'data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAABoAAAAaCAYAAACpSkzOAAAAAXNSR0IArs4c6QAAAARnQU1BAACxjwv8YQUAAAAJcEhZcwAADsMAAA7DAcdvqGQAAAKYSURBVEhLtZZJyI1RGMeveZYMKxQpY4ZIhrChpCiWxAobC5svYoMMidgQFiRZSJQVsRCKSEm2pKRkCMks8+933vd897jd937vK/71657nOec94/Occ2sV1R+ewOFg/SMthElZsV0T4RdcD1ZdnWA5DA9WBfWDn/AKBunIVTTQWtB/MlgVdQH8+GCwMjUbyO18AfqX6qiqsfAVvsEYHciV3oOtwcq0GxzkSrBKqEf+m2ov/IA5wcrkYN2zYtBx+AKTg1VXN+idFetaBHZ4EcbrSDQYZsIZeAfOXu7DDrB+IETFwHgKr6EvtGsKvAE7cKv2g+oC+yB27mSewfvE9xLmghoFtyDWXYau8Iec2SFwoO+5HQf5BJtzX9R0OA/WfwS3bmNuPwZX5eoK5cHPyjHE7WQ2NJMdHQE7vwt9YAn0gtI6C3awKVjF8tAfgG0X6CjSUJiWYFSpt+AWDglWa7mtDmSUqgkQ+zP3OkPYGhtF3AIjxfJzKCMT1fZG5qq8nLIFaqfhTsI2ME9s4KrKyIO3vTnl/ejNEfu7CS231DzxY6OrIx0F27YFq6J2gh8bwq3CdDR8BtNipI4yGgB7wIgbBvGy9P0xuhrlIA/BNgdgKpyDZdBUzng1mOV+ZII60Dz4kPvs0OgyTzyTY+AdZ90N6Anrc1uuQuN7VlsJaQOvpCjLRmKsT3G7XImDKMN4DcQJezeG0I4aAT5ajUt2zzeAGW/kmCen4AR48NabK+sg7dAj8IyN4A7ldt4GZ7ZYR64ZkD7Z8b7zlf0rxcTzgozviuem71KwMsVkNXB8bSvJCPPfjh2s0JGr6D+Dr6v+XcGqIF/bR+BDmOZQ0UD6PfjtwaqoZkk6DhzoWrD+o4ws/5jMD1Zp1Wq/AVjAs0S4zG9VAAAAAElFTkSuQmCC'> Örgün Öğretim<br/>
  <img src='data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAABgAAAAYCAYAAADgdz34AAABmUlEQVRIS7XVv0vVYRTH8ZdSNNgQGIU61BC1hP+CSXOSQ3Mk1FhDQwWBDkFKJTkIIZKD/4FgkEM4NjpIW0MQREE/SMikQjnwBA9fnnu/1y/3Ptvlfp/P+5zPOc85fXp8+nqsrxPAMD5hv0kw7QAjmMZv3G4iHndKgH6MYQEXcRVr3QRcwgtcwF+cxrduAcKWVxhNgl9xsql4yaJlTGXWReSD3QKcxyaGKoID+NUUkhf5JuZxvCJ2GW+6AXiKOzhSEQvbAt7o5Bms4HqhdT/iCraaEHJAtGZEGu8gP9GqAb+LncNCcsAM7uNYQSS66TEWsXsYSA6YxEucaCHwOWUyhx+dQnJA9PtbnGtzeQ/v8AjriN9tT3UW3Uo2VDupKhKT9QM28D5lFAGeTTZ//3+hCgj/o6DXCu1aF2zYdg9L+YelaXoGzzCBo3Wq6f8Qf44n1Vffah/E0HuIGy26KueGHQ+wWhopdRttPKUd0zXsi4zC/z/4ideYTfUoJlsHiEvx8KJ4sR9O4V9aodv4UrdKOwF0WIbyZz0HHAC5oEQZ7ouExwAAAABJRU5ErkJggg=='> İkinci Öğretim(Diğerleri)<br/>
  <img src='data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAABgAAAAYCAYAAADgdz34AAAAAXNSR0IArs4c6QAAAARnQU1BAACxjwv8YQUAAAAJcEhZcwAADsMAAA7DAcdvqGQAAAFLSURBVEhL5dS/K4VhGMbxEwaDkTKLlMEif4I/QDYDBpOVTAaJbAbZTOyUTGaTSZFfxaCMSoki+fH95px6e577HMexiKs+9fZ03ff7nvecTum3pAWLuCnz2rMf5wTvX7DTcOYRLS2y03D6EC0tsvNP04kdRK+lyI7db6Uf/hzTZXc4Ts5k15m60otbpEu0j9HkrMIZZ2umDaeIFmygA62YLp+lnHVH1SwgGrxCMwYxDLONqOuOMO14RDS0B7MMn7ILS4i67nBXlklEA7qG/z3dOCyf1eKuLLuIyhXr8P16ozm8IerJXVnOEJWL/CRDMFuIOnJXlntEZa1gABc4gJlF1JW7slT7grUJM4Gpz8vSKqKuHpDlHFFZLxhDE8wInhF15a4sa4jKRT71THIWsZelB0+IBioucZScpdzhrjC+41dEg/Vwdhx/JqXSBwFbASiCz1jEAAAAAElFTkSuQmCC'>\tÜcretli(Özel)"
}