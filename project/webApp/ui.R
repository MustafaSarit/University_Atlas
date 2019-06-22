ui <- navbarPage("Tercih Rehberi", id="nav",
  tabPanel("Harita",
    div(class="outer",
      tags$head(
      includeCSS("./styles.css")
      ),
      leafletOutput("map", width = "100%", height = "100%"),
      absolutePanel(top = 50, left = 50,draggable = TRUE,
        pickerInput("dept",strong("Bölüm"),choices = levels(allDepartments$Department),options = list(`actions-box` = TRUE),multiple = TRUE, selected = "Bilgisayar Mühendisliği"),
        pickerInput("city",strong("Şehir"),choices = levels(allDepartments$City),selected = levels(allDepartments$City),options = list(`actions-box` = TRUE),multiple = TRUE),
        sliderInput("baseScore", strong("Taban Puanı"),min = min(allDepartments$Base_Score,na.rm = TRUE), max =max(allDepartments$Base_Score,na.rm = TRUE),value = c(min(allDepartments$Base_Score,na.rm = TRUE),max(allDepartments$Base_Score,na.rm = TRUE)))
      )
    )
  ),tabPanel("Tablo",
             tabPanel("Data explorer",
                      fluidRow(
                        column(3,
                               pickerInput("dept2",strong("Bölüm"),choices = levels(allDepartments$Department), selected = "Bilgisayar Mühendisliği",options = list(`actions-box` = TRUE),multiple = TRUE)
                               ),
                        column(3,
                               pickerInput("city2",strong("Şehir"),choices = levels(allDepartments$City),selected = levels(allDepartments$City),options = list(`actions-box` = TRUE),multiple = TRUE)
                        )
                      ),fluidRow(
                        column(3,
                               pickerInput("period2",strong("Öğrenim Türü"),choices = levels(allDepartments$Period),selected = levels(allDepartments$Period),options = list(`actions-box` = TRUE),multiple = TRUE)
                        ),
                        column(3,
                               pickerInput("type2",strong("Üniversite Tipi"),choices = levels(allDepartments$Type),selected = levels(allDepartments$Type),options = list(`actions-box` = TRUE),multiple = TRUE)
                        )
                      ),
                      hr(),
                      DT::dataTableOutput("tableView")
             )
  ))