#### Operational Risk Workflow Tool
#### v1, created by G.Lovric




library(shinydashboard)
library(shiny)
library(shinyjs)
library(shinyBS)
library(DT)
library(rhandsontable)
library(shinycssloaders)
library(RJDBC)
library(RSQLite)
library(plyr)
library(dplyr)
library(magrittr)
library(mailR)
library(knitr)
library(DBI)
library(wordcloud)
library(gridExtra)
library(grid)
library(png)
library(openxlsx)
library(scales)


list1<-c(
  'Legal entity x',
  'Legal entity y',
  '...'
  
)

list_business<-c(
  'Select...',
  'System Downtime',
  'Data Quality and Data Integrity',
  'Privacy Breach',
  'Failure on Regulatory Reporting',
  'Customer Access to Systems',
  'Fraud',
  'Physical Damage to IT Infrastructure',
  'Systems End of Life/ End of Support'
)

list_financial<-c(
  'Select...',
  'Data Entry Errors',
  'Accounting Errors',
  'Loss of Client Assets',
  'Failure in Safeguarding Process',
  'General Failure in Predefined Process',
  'Policy Breach'
)

list_legal<-c(
  'Select...',
  'External Fraud',
  'Outsourcing Risk/ Failure',
  'Cybersecurity',
  'Natural Catastophe',
  'Vandalism'
)


list_IT<-c(
  'Select...',
  'Vandalism',
  'Workplace Safety',
  'Internal Fraud',
  'Misconduct',
  'Employee Health'
)






ui <- dashboardPage(skin = "purple",
                    
                    
                    dashboardHeader(title = "Operational Risk Tool",tags$li(a(img(src = 'rshiny.png',
                                                                                  title = "Company Home", height = "40px"),
                                                                              style = "padding-top:10px; padding-bottom:10px;"),
                                                                            class = "dropdown")
                                    ),
                    dashboardSidebar(
                      uiOutput("userpanel"),
                      sidebarMenu(id = "tabs",
                                  menuItem("Dashboard", tabName = "intro",icon = icon("play")),
                                  menuItem("Report an Incident", tabName = "Report", icon = icon("list-alt")), 
                             
                               
                                  uiOutput("usermanual"),
                                  uiOutput("opriskpolicy")
                      )
                    ),
                    dashboardBody(
                      
                      tags$head(tags$script(src = "enter_button.js"), tags$style(HTML('.content-wrapper, .right-side {
                                background-color: #15ffe7;
                              
            }'
                                                                                      
                                                                                      ))),
                      tabItems(
                        tabItem(tabName = "intro",
                                fluidPage(
                                  fluidRow( 
                                    valueBoxOutput("warning3"), 
                                    valueBoxOutput("MC"), 
                                    valueBoxOutput("out") 
                                  ),
                                  fluidRow( 
                                    valueBoxOutput("warning2"), 
                                    valueBoxOutput("MC2"), 
                                    valueBoxOutput("out2") 
                                  ),
                                  fluidRow( 
                                    column(width = 4, plotOutput("plot11")),
                                    column(width = 4, plotOutput("plot1")), 
                                    column(width= 4, plotOutput("plot111"))),
                  
                                  
                                  fluidRow(
                                    h2("Risk Items - Overview")),
                                  fluidRow(
                                    DT::dataTableOutput("cases"),
                                    downloadButton("download_cases")
                                  )
                                )
                        ),
                        # Second tab content
                        tabItem(tabName = "Report",
                               
                        
                                
                                fluidRow(
                                  column(width = 12,offset=0.5,style='margin-bottom:30px;border:1px solid;',
                                         p(strong("1. Description of the Operational Risk Incident")),
                                         fluidRow(column (width = 4, selectInput("riskcategory", "Incident Category*", c("Select...","Systems","Processes","People","External"))),
                                                  column(width = 4, 
                                                         conditionalPanel(
                                                    condition = "input.riskcategory == 'Systems'",
                                                      selectInput("sel1a", 
                                                                  label = "Incident Subcategory*", 
                                                                  choices = list_business)
                                                    ),
                                                    conditionalPanel(
                                                      condition = "input.riskcategory == 'Processes'",
                                                      selectInput("sel1b", 
                                                                  label = "Incident Subcategory*", 
                                                                  choices = list_financial)
                                                    ),
                                                    conditionalPanel(
                                                      condition = "input.riskcategory == 'People'",
                                                      selectInput("sel1c", 
                                                                  label = "Incident Subcategory*", 
                                                                  choices = list_IT)
                                                    ),
                                                    conditionalPanel(
                                                      condition = "input.riskcategory == 'External'",
                                                      selectInput("sel1d", 
                                                                  label = "Incident Subcategory*", 
                                                                  choices = list_legal)
                                                    )
                                                    )
                                                 
                                         ),
                                         fluidRow(column(width=4, textAreaInput(inputId = "riskdesc", label = "Short description of incident*",value = "", height = 300, width=300)),
                                                  column(width=4, textAreaInput(inputId = "riskdesc1", label = "Long description of incident*",value = "", height = 300, width=1060)),
                                                  column(width = 4,"")
                                         ),
                                         fluidRow(column(width = 4, selectInput("riskregion", "Region*", c("Select...","APAC","EMEA","Americas", "Global"))),
                                                  column(width = 4,selectInput("legalentity","Legal Entity*",c("Select...",list1))),
                                                  column(width = 2,dateInput("date", "Date*",value = Sys.Date())),
                                                  column(width = 2,"")
                                                 
                                         )
                                  ),
                                  column(width = 4,"")), 
                                
                                fluidRow(
                                  column(width = 12,offset=0.5,style='margin-bottom:30px;border:1px solid;',
                                         p(strong("2. Risk Assessment and Mitigation")),
                                         fluidRow(column (width = 4, wellPanel(id="border_Impact1",tags$div(title="Please click on the link for additional information", selectInput("Impact1", "Impact*", c("Select...","Low","Minor","Moderate","High", "Significant")),tags$a(href ="javascript:window.open('test.png', 'testme', 'width=900,height=600');","Additional information")))),
                                                  column(width = 4, wellPanel(id="border_likelihood1",tags$div(title="Please click on the link for additional information", selectInput("likelihood1", "Likelihood*", c("Select...","Low","Minor","Moderate","High", "Significant")),tags$a(href ="javascript:window.open('test2.png', 'testme', 'width=900,height=600');","Additional information")))),
                                                  column(width = 4, wellPanel(id="border_riskscore1",tags$div(title="Please click on the link for additional information", selectInput("riskscore1","Risk Score*",c("Select...","Low","Minor","Moderate","High", "Significant")),tags$a(href ="javascript:window.open('test4score.png', 'testme', 'width=900,height=600');","Additional information"))))),
                                         
                                         
                                         
                                         fluidRow(column(width=4, textAreaInput(inputId = "mitigation", label = "Mitigating Action taken*",value = "", height = 150, width=860)),
                                                  column(width = 4,""),
                                                  column(width = 4,selectInput("health","Health Status of Mitigation",c("Select...","Poor","Fair","Satisfactory", "Strong")))
                                         ),
                                         fluidRow(column(width=4, ""),
                                                  column(width = 4,""),
                                                  column(width=4, "")
                                                  
                                         )
                                  ),
                                  column(width = 4,"")),
                                
                                fluidRow(
                                  column(width = 12,offset=0.5,style='margin-bottom:30px;border:1px solid;',
                                         p(strong("3. Impact Analysis")),
                                         fluidRow(column (width = 4, numericInput("ImpactUSD", "Financial Impact in USD", "")),
                                                  column(width = 4, selectInput("frequency", "Impact Type", c("Select...","Loss","Surplus","Risk"))),
                                                  column(width = 4,selectInput("Affected_Systems", "Affected Systems", c("Select...","No System Affected","System A","System B","other systems (specify in description)")))),
                                         fluidRow(column(width=4, wellPanel(id="border_Impact2",tags$div(title="Please click on the link for additional information", selectInput("Business_Critical", "Business Critical issue*", c("Select...","Yes","No")),tags$a(href ="javascript:window.open('businesscritical.png', 'testme', 'type=fullWindow, fullscreen, scrollbars=yes');","Additional information")))),
                                                  column(width = 4, wellPanel(id="border_likelihood2",tags$div(title="Please click on the link for additional information", selectInput("Data_Privacy", "Data Privacy issue*", c("Select...","Yes","No")),tags$a(href ="javascript:window.open('dataprivacy.png', 'testme', 'width=900,height=600');","Additional information")))),
                                                  column(width = 4,wellPanel(id="border_speed",tags$div(title="Please click on the link for additional information", selectInput("Cybersecurity","Cybersecurity issue*",c("Select...","Yes","No")),tags$a(href ="javascript:window.open('cybersecurity.png', 'testme', 'width=900,height=600');","Additional information"))))
                                         ),
                                         fluidRow(column(width=4, selectInput("Impacted_Department", "Impacted Department", c("Select...","Accounting and Finance","Compliance and Risk","Business/ GTM","Operations","Legal", "Internal Audit", "IT","Treasury/ Liquidity Management")))
                                                  
                                                
                                  )),
                                  column(width = 4,"")),
                                
                                
                                actionButton("save", "Save input"),
                                p(strong("* denotes mandatory fields")),
                                uiOutput ("confirm")
                                
                                  )
                        
                        
                                ) 
                                                     )
                      )

server <- function(input, output, session) {
  
  
 
  
  dlOut2<-reactive({
   if (req(input$ok) || req (input$okay)) {
      if (input$ok >= 1 || input$okay >= 1) {
        if (input$tabs == "intro") {
          con <- dbConnect(SQLite(), dbname = "data/test.db")
          Users <- dbGetQuery(con, "SELECT * FROM tab")
          Region_selected <- Users$Region[which(Users$UserId == input$UserId)]
          if(Region_selected=="Global"){
            Users_region<-Users %>%
              select (UserId)
          }
          else{
            Users_region <- Users %>% 
              dplyr::filter (Region == Region_selected) %>% 
              select (UserId)
          }
          value1 <- as.numeric(nrow(dbGetQuery(con,  paste0("SELECT * FROM Cases WHERE UserId IN ('", paste0(as.character(unlist(Users_region)), collapse = "','") ,"')"))))
     
        }
      }
    }  
  })
  
  dlOut3<-reactive({

    if (req(input$ok) || req (input$okay)) {
      if (input$ok >= 1 || input$okay >= 1) {
        if (input$tabs == "intro") {
          con <- dbConnect(SQLite(), dbname = "data/test.db")
          Users <- dbGetQuery(con, "SELECT * FROM tab")
          Region_selected <- Users$Region[which(Users$UserId == input$UserId)]
          if(Region_selected=="Global"){
            Users_region<-Users %>%
              select (UserId)
          }
          else{
            Users_region <- Users %>% 
              dplyr::filter (Region == Region_selected) %>% 
              select (UserId)
          }
          value1 <- as.numeric(nrow(dbGetQuery(con,  paste0("SELECT * FROM Cases WHERE Risk_Score ='High' AND UserId IN ('", paste0(as.character(unlist(Users_region)), collapse = "','") ,"')"))))
         
        }
      }
    }  
    
  })
  
  dlOut4<-reactive({
       if (req(input$ok) || req (input$okay)) {
      if (input$ok >= 1 || input$okay >= 1) {
        if (input$tabs == "intro") {
          con <- dbConnect(SQLite(), dbname = "data/test.db")
          Users <- dbGetQuery(con, "SELECT * FROM tab")
          Region_selected <- Users$Region[which(Users$UserId == input$UserId)]
          if(Region_selected=="Global"){
            Users_region<-Users %>%
              select (UserId)
          }
          else{
            Users_region <- Users %>% 
              dplyr::filter (Region == Region_selected) %>% 
              select (UserId)
          }
          value1 <- tryCatch(sum(dbGetQuery(con,  paste0("SELECT ImpactUSD FROM Cases WHERE ImpactUSD>0 AND UserId IN ('", paste0(as.character(unlist(Users_region)), collapse = "','") ,"')")), na.rm = TRUE),error=function(e) e=0)
        
        }
      }
    } 
    
  })
  
  dlOut5<-reactive({
       if (req(input$ok) || req (input$okay)) {
      if (input$ok >= 1 || input$okay >= 1) {
        if (input$tabs == "intro") {
          con <- dbConnect(SQLite(), dbname = "data/test.db")
          Users <- dbGetQuery(con, "SELECT * FROM tab")
          Region_selected <- Users$Region[which(Users$UserId == input$UserId)]
          if(Region_selected=="Global"){
            Users_region<-Users %>%
              select (UserId)
          }
          else{
            Users_region <- Users %>% 
              dplyr::filter (Region == Region_selected) %>% 
              select (UserId)
          }
          value1 <- nrow(dbGetQuery(con,  paste0("SELECT Business_Critical FROM Cases WHERE Business_Critical='Yes' AND UserId IN ('", paste0(as.character(unlist(Users_region)), collapse = "','") ,"')")))
        
        }
      }
    } 
    
  })
  
  dlOut6<-reactive({
     
    if (req(input$ok) || req (input$okay)) {
      if (input$ok >= 1 || input$okay >= 1) {
        if (input$tabs == "intro") {
          con <- dbConnect(SQLite(), dbname = "data/test.db")
          Users <- dbGetQuery(con, "SELECT * FROM tab")
          Region_selected <- Users$Region[which(Users$UserId == input$UserId)]
          if(Region_selected=="Global"){
            Users_region<-Users %>%
              select (UserId)
          }
          else{
            Users_region <- Users %>% 
              dplyr::filter (Region == Region_selected) %>% 
              select (UserId)
          }
          test1 <- as.data.frame(dbGetQuery(con,  paste0("SELECT Legal_Entity, COUNT(*) FROM Cases WHERE UserId IN ('", paste0(as.character(unlist(Users_region)), collapse = "','") ,"') GROUP BY Legal_Entity")))
          test1<-test1[test1$Legal_Entity!="Select...",]
          test1<-test1[test1$Legal_Entity!="",]
          value1<-test1[order(test1$`COUNT(*)`,decreasing=TRUE)[1],1]
   
        }
      }
    } 
    
  })
  
  dlOut7<-reactive({
    
    if (req(input$ok) || req (input$okay)) {
      if (input$ok >= 1 || input$okay >= 1) {
        if (input$tabs == "intro") {
          con <- dbConnect(SQLite(), dbname = "data/test.db")
          Users <- dbGetQuery(con, "SELECT * FROM tab")
          Region_selected <- Users$Region[which(Users$UserId == input$UserId)]
          if(Region_selected=="Global"){
            Users_region<-Users %>%
              select (UserId)
          }
          else{
            Users_region <- Users %>% 
              dplyr::filter (Region == Region_selected) %>% 
              select (UserId)
          }
          test1 <- as.data.frame(dbGetQuery(con,  paste0("SELECT Risk_Category, COUNT(*) FROM Cases WHERE UserId IN ('", paste0(as.character(unlist(Users_region)), collapse = "','") ,"') GROUP BY Risk_Category")))
          test1<-test1[test1$Risk_Category!="Select...",]
          value1<-test1[order(test1$`COUNT(*)`,decreasing=TRUE)[1],1]
          
        }
      }
    } 
    
  })
  
  
  rv <- reactiveValues(new_CaseID = NULL, cases = NULL, confident=NULL)
  
  source("login.R", local = TRUE)
  
  output$confirm <- renderUI ({
    if (input$save >= 1) {
      tags$label(paste( "Case is stored under the following Case Id:",rv$new_CaseID))
    }
  })
  
  output$confirm2 <- renderUI ({
    if (input$update_case >= 1) {
      tags$label(paste( "Case updated!"))
    }
  })

  observeEvent(input$save,{
    
    
    withProgress(message = 'Submitting data',
                 detail = 'This may take a while...', value = 0, {
                   for (i in 1:15) {
                     incProgress(1/15)
                     Sys.sleep(0.25)
                   }
                 })
    
    
    con <- dbConnect(SQLite(), dbname = "data/test.db")
    CaseIDs <- dbGetQuery(con, "SELECT CaseID FROM Cases")
    dbDisconnect(con)
    new_CaseID <- paste0("RC",max(as.numeric(substr(unlist(CaseIDs),3,6)))+1)
    rv$new_CaseID <- new_CaseID
    sel11<-c(input$sel1a,input$sel1b,input$sel1c,input$sel1d,input$sel1e)
    sel11<-sel11[sel11!="Select..."]
    
    
      rv$confident<-c("<insert email address of person(s) who should receive privileged and confidential information>")
    
    
    
    new_inputs <- data.frame(
                   CaseID = new_CaseID, 
                   Risk_Category = input$riskcategory,
                   Incident_Type = sel11,
                   Short_Description = input$riskdesc, 
                   Long_Description = input$riskdesc1, 
                   Region = input$riskregion,
                   Legal_Entity = input$legalentity,
                   Date = input$date,
                   Impact = input$Impact1,
                   Likelihood = input$likelihood1,
                   Risk_Score = input$riskscore1,
                   Mitigation = input$mitigation,
                   Health_Mitigation = input$health,
                   ImpactUSD = as.numeric(input$ImpactUSD),
                   Impact_Type = input$frequency,
                   Affected_Systems = input$Affected_Systems,
                   Business_Critical = input$Business_Critical,
                   Data_Privacy = input$Data_Privacy,
                   Cybersecurity = input$Cybersecurity,
                   Impacted_Department = input$Impacted_Department,
                   UserId = input$UserId,
                   Status = "Open"
                   )
    con <- dbConnect(SQLite(), dbname = "data/test.db")
    dbWriteTable(con, "Cases",new_inputs, append = TRUE)
    dbDisconnect(con)
    
    
    # !! resets the values
     updateSelectInput(session,"riskcategory", choices=c("Select...","Systems","Processes","People","External"))
     updateTextAreaInput(session,inputId = "riskdesc",value="") 
     updateTextAreaInput(session,inputId = "riskdesc1",value="")
   
     updateSelectInput(session,"riskregion", choices=c("Select...","APAC","EMEA","Americas", "Global"))
     updateSelectInput(session,"legalentity",choices=c("Select...",list1))
     updateDateInput(session,"date", value = Sys.Date())
     updateSelectInput(session,"Impact1", choices=c("Select...","Low","Minor","Moderate","High", "Significant"))
     updateSelectInput(session,"likelihood1", choices=c("Select...","Low","Minor","Moderate","High", "Significant"))
     updateSelectInput(session,"riskscore1",choices=c("Select...","Low","Minor","Moderate","High", "Significant"))
     updateTextAreaInput(session, inputId = "mitigation", value="")
     updateSelectInput(session,"health",choices=c("Select...","Poor","Adequate", "Excellent"))
     updateNumericInput(session, "ImpactUSD", value= "")
     updateSelectInput(session,"frequency", choices=c("Loss","Surplus","Risk")) 
     updateSelectInput(session,"Affected_Systems", choices=c("Select...","Yes","No"))
     
     updateSelectInput(session,"Business_Critical", "Business Critical", c("Select...","Yes","No"))
     updateSelectInput(session,"Data_Privacy", choices=c("Select...","Yes","No"))
     updateSelectInput(session,"Cybersecurity",choices=c("Select...","Yes","No"))
     
     
     updateSelectInput(session,"Impacted_Department", choices= c("Select...","Accounting and Finance","Compliance and Risk","Business/ GTM","Operations","Legal", "Internal Audit", "IT","Treasury/ Liquidity Management"))
   
    
  
    subject_code <- paste0("New case filed - ",new_CaseID)
    
      
  
     con <- dbConnect(SQLite(), dbname = "data/test.db")
    Users <- dbGetQuery(con, "SELECT * FROM tab")
    User_assigned <-Users$Name[which(Users$UserId == new_inputs$UserId)]
    new_inputs2<-new_inputs
    new_inputs2$UserId<-User_assigned
    
    new_data<-t(new_inputs2)

    colnames(new_data)<-new_data[1,]
    
  
       library(openxlsx)
        CasesFinal1<-loadWorkbook("data/RiskItem.xlsx")
       CasesFinal<-read.xlsx(CasesFinal1,sheet="RiskItem")
       writeData(CasesFinal1,sheet="RiskItem",new_data, startCol = 2)
       saveWorkbook(CasesFinal1,"data/RiskItem.xlsx", overwrite = TRUE)
    
    
  
  })
  

 output$cases <- DT::renderDataTable({
   if (req(input$ok) || req (input$okay)) {
   if (input$ok >= 1 || input$okay >= 1) {
   if (input$tabs == "intro") {
   con <- dbConnect(SQLite(), dbname = "data/test.db")
   Users <- dbGetQuery(con, "SELECT * FROM tab")
   Region_selected <- Users$Region[which(Users$UserId == input$UserId)]
   if(Region_selected=="Global"){
     Users_region<-Users %>%
       select (UserId)
   }
   else{
   Users_region <- Users %>% 
     dplyr::filter (Region == Region_selected) %>% 
     select (UserId)
   }
   cases <- dbGetQuery(con,  paste0("SELECT CaseID, Short_Description, Risk_Category,  Date, Risk_Score, Legal_Entity, Business_Critical, Impacted_Department FROM Cases WHERE UserId IN ('", paste0(as.character(unlist(Users_region)), collapse = "','") ,"')"))
   cases$Date <- format(as.Date(cases$Date, origin="1970-01-01"), "%Y/%m/%d")
   cases_full <- dbGetQuery(con,  paste0("SELECT * FROM Cases WHERE UserId IN ('", paste0(as.character(unlist(Users_region)), collapse = "','") ,"')"))
   dbDisconnect(con)
   rv$cases <- cases_full
   datatable(cases,options=list(ordering = TRUE,order=c(3, 'desc'),searchCols = list(NULL, NULL, NULL, NULL,NULL, NULL, NULL, NULL), paging = FALSE, searchHighlight = TRUE), rownames = FALSE, selection = "single", filter='top') %>% formatStyle(
     'Business_Critical',
     
     backgroundColor = styleEqual("Yes",'lightpink'))
  
   }
   }
   }
 })
 
 
 output$cases45 <- DT::renderDataTable({
   if (req(input$ok) || req (input$okay)) {
     if (input$ok >= 1 || input$okay >= 1) {
       if (input$tabs == "kris") {
         con <- dbConnect(SQLite(), dbname = "data/test.db")
         Users <- dbGetQuery(con, "SELECT * FROM tab")
         Region_selected <- Users$Region[which(Users$UserId == input$UserId)]
         if(Region_selected=="Global"){
           Users_region<-Users %>%
             select (UserId)
         }
         else{
           Users_region <- Users %>% 
             dplyr::filter (Region == Region_selected) %>% 
             select (UserId)
         }
         
         cases <- dbGetQuery(con,  paste0("SELECT CaseID, Short_Description, Risk_Category,  Date, Risk_Score, Legal_Entity, Risk_Owner FROM Cases WHERE UserId IN ('", paste0(as.character(unlist(Users_region)), collapse = "','") ,"')"))
            cases$Date <- format(as.Date(cases$Date, origin="1970-01-01"), "%Y/%m/%d")
         cases_full <- dbGetQuery(con,  paste0("SELECT * FROM Cases WHERE UserId IN ('", paste0(as.character(unlist(Users_region)), collapse = "','") ,"')"))
         dbDisconnect(con)
         rv$cases <- cases_full
         datatable(cases,options=list(ordering = TRUE,order=c(3, 'desc'),searchCols = list(NULL, list(search = "GPFI"), NULL, NULL,NULL, list(search = "Open"), NULL), paging = FALSE, searchHighlight = TRUE), rownames = FALSE, selection = "single", filter='top')
        
       }
     }
   }
 })
 

 
 
 output$cases2 <- DT::renderDataTable({
   if (req(input$ok) || req (input$okay)) {
     if (input$ok >= 1 || input$okay >= 1) {
       if (input$tabs == "assigned") {
         con <- dbConnect(SQLite(), dbname = "data/test.db")
         Users <- dbGetQuery(con, "SELECT * FROM tab")
         Region_selected <- Users$Region[which(Users$UserId == input$UserId)]
         User_assigned <-Users$Name[which(Users$UserId == input$UserId)]
         if(Region_selected=="Global"){
           Users_region<-Users %>%
             select (UserId)
         }
         else{
           Users_region <- Users %>% 
             dplyr::filter (Region == Region_selected) %>% 
             select (UserId)
         }
         cases <- dbGetQuery(con,  paste0("SELECT CaseID, Short_Description, Risk_Category,  Date, Risk_Score, Legal_Entity, Business_Critical, Impacted_Department FROM Cases WHERE UserId IN ('", paste0(as.character(unlist(Users_region)), collapse = "','") ,"')"))
         cases$Date <- format(as.Date(cases$Date, origin="1970-01-01"), "%Y/%m/%d")
         cases_full <- dbGetQuery(con,  paste0("SELECT * FROM Cases WHERE UserId IN ('", paste0(as.character(unlist(Users_region)), collapse = "','") ,"')"))
         dbDisconnect(con)
         rv$cases <- cases_full
         datatable(cases,options=list(ordering = TRUE,order=c(3, 'desc'),searchCols = list(NULL, NULL, NULL, NULL,NULL, NULL, NULL, NULL), paging = FALSE, searchHighlight = TRUE), rownames = FALSE, selection = "single", filter='top') %>% formatStyle(
           'Business_Critical',
           #order=c(3, 'desc')
           backgroundColor = styleEqual("Yes",'lightpink'))
         #dom='t'#
       }
     }
   }
 })
 
 
 
 output$download_cases <- downloadHandler( 
   filename = paste0("Cases_",format(Sys.time(), "%Y%m%d_%H%M%S"),".csv"), 
   content = function(file) { 
     write.csv(rv$cases, file) 
   }, 
   contentType = "text/csv" 
 ) 


 Modal_cases_details <- function() {

   
  
   con <- dbConnect(SQLite(), dbname = "data/test.db")
  
   User_right <- dbGetQuery(con, paste0("SELECT User_Right FROM tab WHERE UserId='", input$UserId,"'"))
 
   
   
   Users <- dbGetQuery(con, "SELECT * FROM tab")
   Region_selected <- Users$Region[which(Users$UserId == input$UserId)]
   if(Region_selected=="Global"){
     Users_region<-Users %>%
       select (UserId)
   }
   else{
     Users_region <- Users %>% 
       dplyr::filter (Region == Region_selected) %>% 
       select (UserId)
   }
   

   cases <- dbGetQuery(con,  paste0("SELECT * FROM Cases WHERE UserId IN ('", paste0(as.character(unlist(Users_region)), collapse = "','") ,"')"))
   
   
   
   cases<- cases %>% 
   
   dplyr::filter(rownames(cases)==input$cases_rows_selected)

   
   modalDialog(
     fluidPage(tabsetPanel(
       tabPanel("Cases details",
                DT::dataTableOutput("cases_details")),
       if(User_right=="W"){
       tabPanel("Edit case",
                selectInput("riskcategory_1", "Incident Category", c("Select...","Systems","Processes","People","External"),selected =cases$Risk_Category),
                selectInput("sel1_1",label = "Incident Subcategory", choices = c(list_business,list_financial,list_IT,list_legal),selected=cases$Incident_Type),
                
                textAreaInput(inputId = "riskdesc_1", label = "Short description of incident",value = cases$Short_Description, height = 300, width=300),
                textAreaInput(inputId = "riskdesc1_1", label = "Long description of incident",value = cases$Long_Description, height = 300, width=860),
                selectInput("riskregion_1", "Region", c("Select...","APAC","EMEA","Americas", "Global"),selected=cases$Region),
                selectInput("legalentity_1","Legal Entity",c("Select...",list1),selected=cases$Legal_Entity),
               
                selectInput("Impact1_1", "Impact", c("Select...","Low","Minor","Moderate","High", "Significant"),selected=cases$Impact),
                selectInput("likelihood1_1", "Likelihood/Severity", c("Select...","Low","Minor","Moderate","High", "Significant"),selected=cases$Likelihood),
                selectInput("riskscore1_1","Risk Score",c("Select...","Low","Minor","Moderate","High", "Significant"), selected=cases$Risk_Score),
                textAreaInput(inputId = "mitigation_1", label = "Mitigating Action taken",value = cases$Mitigation, height = 150, width=860),
                selectInput("health_1","Health Status of Mitigation",c("Select...","Poor","Fair","Satisfactory", "Strong"),selected=cases$Health_Mitigation),
                wellPanel(id="border_Impact1",style = "background: #F8785D",numericInput("ImpactUSD_1", "Financial Impact in USD", value=cases$ImpactUSD)),
                
                selectInput("frequency_1", "Impact Type", c("Select...","Loss","Surplus","Risk"),selected=cases$Impact_Type),
                selectInput("Affected_Systems_1", "Affected Systems", c("Select...","System A","System B","...","other systems (specify in description)"),selected=cases$Affected_Systems),
                selectInput("Business_Critical_1", "Business Critical issue", c("Select...","Yes","No"),selected=cases$Business_Critical),
                selectInput("Data_Privacy_1", "Data Privacy issue", c("Select...","Yes","No"),selected=cases$Data_Privacy),
                selectInput("Cybersecurity_1","Cybersecurity issue",c("Select...","Yes","No"),selected=cases$Cybersecurity),
               
                selectInput("Impacted_Department_1","Impacted Department",c("Select...","Accounting and Finance","Compliance and Risk","Business/ GTM","Operations","Legal", "Internal Audit", "IT","Treasury/ Liquidity Management"), selected =cases$Impacted_Department),
                selectInput("status","Status",choices =c("Open", "Closed"), selected = cases$Status),
                
                wellPanel(id="border_Impact1",tags$div(title="Please click on the link for additional information", actionButton("update_case", "Update case"),tags$b(" If you are closing the case, make sure the final Loss/ Surplus number is entered into the red fields"))),
                uiOutput ("confirm2")
                )}
       else {
         tabPanel("Edit case",
                  h4("User has read-only access and cannot edit cases."),
                  h4("Please use 'Comments' tab to provide updates.")
                  )
       }
       ,
       tabPanel("Comments",
                DT::dataTableOutput("comments"),
                textAreaInput("new_comment","Enter new comment", width= 400, height=200),
                actionButton("submit_comment", "Submit new comment")
       ), 
       if(User_right=="W"){
       tabPanel("Committee reviews",
                DT::dataTableOutput("committees"),
                fluidRow(
                column(width=4,selectInput("select_committee","Escalation body",choices = c("Select...","Risk Commitee", "Risk Taskforce","Local Risk Committee", "Steering Committee","Incident Management Committee"))),
                column(width=4,dateInput("date_committee","Date",value=Sys.Date(),max = Sys.Date()))),
                textAreaInput("new_comment_committee","Minutes/ Comments", width= 550, height=100),
                actionButton("submit_committee", "Submit update")
       )}
       else{
         tabPanel("Committee reviews",
                  h4("User has read-only access and cannot update this tab."),
                  h4("Please use 'Comments' tab to provide updates.")
         )
       }
       , tabPanel("Loss/ Recovery Tracking",
                DT::dataTableOutput("comments5"),
                textAreaInput("new_comment5","Enter Updated Loss/ Recovery Data", width= 400, height=200),
                actionButton("submit_comment5", "Submit")
       )
       
     )
     
     
     ), 
     footer = fluidRow( 
     
       actionButton ("dismiss", "Dismiss")),
     size = "l",
     easyClose = FALSE
   )
 }
 
 Modal_cases_details2 <- function() {

   con <- dbConnect(SQLite(), dbname = "data/test.db")
 
   User_right <- dbGetQuery(con, paste0("SELECT User_Right FROM tab WHERE UserId='", input$UserId,"'"))
   
   
   
   Users <- dbGetQuery(con, "SELECT * FROM tab")
   Region_selected <- Users$Region[which(Users$UserId == input$UserId)]
   if(Region_selected=="Global"){
     Users_region<-Users %>%
       select (UserId)
   }
   else{
     Users_region <- Users %>% 
       dplyr::filter (Region == Region_selected) %>% 
       select (UserId)
   }
   

   cases <- dbGetQuery(con,  paste0("SELECT * FROM Cases WHERE UserId IN ('", paste0(as.character(unlist(Users_region)), collapse = "','") ,"')"))
   
   
   
   cases<- cases %>% 
  
   dplyr::filter(rownames(cases)==input$cases2_rows_selected)
   
   modalDialog(
     fluidPage(tabsetPanel(
       tabPanel("Cases details",
                DT::dataTableOutput("cases_details2")),
       if(User_right=="W"){
         tabPanel("Edit case",
                  selectInput("riskcategory_1", "Incident Category", c("Select...","Systems","Processes","People","External"),selected =cases$Risk_Category),
                  selectInput("sel1_1",label = "Incident Subcategory", choices = c(list_business,list_financial,list_IT,list_legal),selected=cases$Incident_Type),
                 
                  textAreaInput(inputId = "riskdesc_1", label = "Short description of incident",value = cases$Short_Description, height = 300, width=300),
                  textAreaInput(inputId = "riskdesc1_1", label = "Long description of incident",value = cases$Long_Description, height = 300, width=860),
                  selectInput("riskregion_1", "Region", c("Select...","APAC","EMEA","Americas", "Global"),selected=cases$Region),
                  selectInput("legalentity_1","Legal Entity",c("Select...",list1),selected=cases$Legal_Entity),
                 
                  selectInput("Impact1_1", "Impact", c("Select...","Low","Minor","Moderate","High", "Significant"),selected=cases$Impact),
                  selectInput("likelihood1_1", "Likelihood/Severity", c("Select...","Low","Minor","Moderate","High", "Significant"),selected=cases$Likelihood),
                  selectInput("riskscore1_1","Risk Score",c("Select...","Low","Minor","Moderate","High", "Significant"), selected=cases$Risk_Score),
                  textAreaInput(inputId = "mitigation_1", label = "Mitigating Action taken",value = cases$Mitigation, height = 150, width=860),
                  selectInput("health_1","Health Status of Mitigation",c("Select...","Poor","Fair","Satisfactory", "Strong"),selected=cases$Health_Mitigation),
                  numericInput("ImpactUSD_1", "Financial Impact in USD", value=cases$ImpactUSD),
                  
                  selectInput("frequency_1", "Impact Type", c("Select...","Loss","Surplus","Risk"),selected=cases$Impact_Type),
                  selectInput("Affected_Systems_1", "Affected Systems", c("Select...","System A","System B","...","other systems (specify in description)"),selected=cases$Affected_Systems),
                  selectInput("Business_Critical_1", "Business Critical issue", c("Select...","Yes","No"),selected=cases$Business_Critical),
                  selectInput("Data_Privacy_1", "Data Privacy issue", c("Select...","Yes","No"),selected=cases$Data_Privacy),
                  selectInput("Cybersecurity_1","Cybersecurity issue",c("Select...","Yes","No"),selected=cases$Cybersecurity),
                  
                  selectInput("Impacted_Department_1","Impacted Department",c("Select...","Accounting and Finance","Compliance and Risk","Business/ GTM","Operations","Legal", "Internal Audit", "IT","Treasury/ Liquidity Management"), selected =cases$Impacted_Department),
                  selectInput("status","Status",choices =c("Open", "Closed"), selected = cases$Status),
                  actionButton("update_case", "Update case"),
                  uiOutput ("confirm2")
         )}
       else {
         tabPanel("Edit case",
                  h4("User has read-only access and cannot edit cases."),
                  h4("Please use 'Comments' tab to provide updates.")
         )
       }
       ,
       tabPanel("Comments",
                DT::dataTableOutput("comments2"),
                textAreaInput("new_comment","Enter new comment", width= 400, height=200),
                actionButton("submit_comment2", "Submit new comment")
       ), 
       if(User_right=="W"){
         tabPanel("Committee reviews",
                  DT::dataTableOutput("committees2"),
                  fluidRow(
                    column(width=4,selectInput("select_committee","Escalation body",choices = c("Select...","Risk Committee", "Risk Taskforce","Local Risk Committee", "Steering Committee","Incident Management Committee"))),
                    column(width=4,dateInput("date_committee","Date",value=Sys.Date(),max = Sys.Date()))),
                  textAreaInput("new_comment_committee","Minutes/ Comments", width= 550, height=100),
                  actionButton("submit_committee2", "Submit update")
         )}
       else{
         tabPanel("Committee reviews",
                  h4("User has read-only access and cannot update this tab."),
                  h4("Please use 'Comments' tab to provide updates.")
         )
       }
     )), 
     footer = fluidRow( 
     
       actionButton ("dismiss", "Dismiss")),
     size = "l",
     easyClose = FALSE
   )
 }
 
 selection1<-reactive({
   if (length(input$cases_rows_selected)>=1) 
     vector() else input$cases_rows_selected})

 observeEvent(input$dismiss,{

   
   removeModal()
     output$cases <- DT::renderDataTable({
      if (req(input$ok) || req (input$okay)) {
       if (input$ok >= 1 || input$okay >= 1) {
      if (input$tabs == "intro") {
            con <- dbConnect(SQLite(), dbname = "data/test.db")
            Users <- dbGetQuery(con, "SELECT * FROM tab")
            Region_selected <- Users$Region[which(Users$UserId == input$UserId)]
            if(Region_selected=="Global"){
              Users_region<-Users %>%
                select (UserId)
            }
            else{
              Users_region <- Users %>% 
                dplyr::filter (Region == Region_selected) %>% 
                select (UserId)
            }
           
        
            cases <- dbGetQuery(con,  paste0("SELECT CaseID, Short_Description, Risk_Category,  Date, Risk_Score, Legal_Entity, Business_Critical, Impacted_Department FROM Cases WHERE UserId IN ('", paste0(as.character(unlist(Users_region)), collapse = "','") ,"')"))
            cases$Date <- format(as.Date(cases$Date, origin="1970-01-01"), "%Y/%m/%d")
            cases_full <- dbGetQuery(con,  paste0("SELECT * FROM Cases WHERE UserId IN ('", paste0(as.character(unlist(Users_region)), collapse = "','") ,"')"))
            dbDisconnect(con)
            rv$cases <- cases_full
            datatable(cases,options=list(ordering = TRUE,order=c(3, 'desc'),searchCols = list(NULL, NULL, NULL, NULL,NULL, NULL ,NULL, NULL), paging = FALSE, searchHighlight = TRUE), rownames = FALSE, selection = "single", filter='top') %>% formatStyle(
              'Business_Critical',
              
              backgroundColor = styleEqual("Yes",'lightpink'))
            
          
            
          }
        }
      }
   })
   
 })
  
 observeEvent(input$cases_rows_selected, {
   showModal(Modal_cases_details())
   })  
 
 
 observeEvent(input$cases2_rows_selected, {
   showModal(Modal_cases_details2())
 }) 


 
output$cases_details <- DT::renderDataTable({
  con <- dbConnect(SQLite(), dbname = "data/test.db")
  Users <- dbGetQuery(con, "SELECT * FROM tab")
  Region_selected <- Users$Region[which(Users$UserId == input$UserId)]
  if(Region_selected=="Global"){
    Users_region<-Users %>%
      select (UserId)
  }
  else{
    Users_region <- Users %>% 
      dplyr::filter (Region == Region_selected) %>% 
      select (UserId)
  }

  cases <- dbGetQuery(con,  paste0("SELECT * FROM Cases WHERE UserId IN ('", paste0(as.character(unlist(Users_region)), collapse = "','") ,"')"))
  cases$Date <- format(as.Date(cases$Date, origin="1970-01-01"), "%Y/%m/%d")
  
   for (i in (1:nrow(cases))){
     Users2<-dbGetQuery(con, paste0("SELECT * FROM tab where UserId='",cases$UserId[i],"'"))
     cases$UserId[i]<-Users2$Name 
   }
   
   mytestnow<- cases$CaseID[which(rownames(cases)==input$cases_rows_selected)]
   case_details <- t(cases[which(cases$CaseID == mytestnow[length(mytestnow)]), 2:ncol(cases)])
  
 
   colname <- t(cases[which(cases$CaseID == mytestnow[length(mytestnow)]), 1])
  
  dbDisconnect(con)

 datatable(case_details,options=list(dom='t', ordering = FALSE, paging = FALSE), colnames = colname, selection = "none")

  
 }) 


output$cases_details2 <- DT::renderDataTable({
  con <- dbConnect(SQLite(), dbname = "data/test.db")
  Users <- dbGetQuery(con, "SELECT * FROM tab")
  Region_selected <- Users$Region[which(Users$UserId == input$UserId)]
  if(Region_selected=="Global"){
    Users_region<-Users %>%
      select (UserId)
  }
  else{
    Users_region <- Users %>% 
      dplyr::filter (Region == Region_selected) %>% 
      select (UserId)
  }

  cases <- dbGetQuery(con,  paste0("SELECT * FROM Cases WHERE UserId IN ('", paste0(as.character(unlist(Users_region)), collapse = "','") ,"')"))
  cases$Date <- format(as.Date(cases$Date, origin="1970-01-01"), "%Y/%m/%d")
  cases$GRC_reviewdate<-format(as.Date(as.numeric(cases$GRC_reviewdate), origin="1970-01-01"), "%Y/%m/%d")
  for (i in (1:nrow(cases))){
    Users2<-dbGetQuery(con, paste0("SELECT * FROM tab where UserId='",cases$UserId[i],"'"))
    cases$UserId[i]<-Users2$Name 
  }
  mytestnow2<- cases$CaseID[which(rownames(cases)==input$cases2_rows_selected)]
  case_details <- t(cases[which(cases$CaseID == mytestnow2[length(mytestnow2)]), 2:ncol(cases)])
  
  colname <- t(cases[which(cases$CaseID == mytestnow2[length(mytestnow2)]), 1])
 
  dbDisconnect(con)

  datatable(case_details,options=list(dom='t', ordering = FALSE, paging = FALSE), colnames = colname, selection = "none")

  
}) 



output$comments <- DT::renderDataTable({
  input$submit_comment
  con <- dbConnect(SQLite(), dbname = "data/test.db")
  Users <- dbGetQuery(con, "SELECT * FROM tab")
  Region_selected <- Users$Region[which(Users$UserId == input$UserId)]
  if(Region_selected=="Global"){
    Users_region<-Users %>%
      select (UserId)
  }
  else{
    Users_region <- Users %>% 
      dplyr::filter (Region == Region_selected) %>% 
      select (UserId)
  }

  cases <- dbGetQuery(con,  paste0("SELECT CaseID FROM Cases WHERE UserId IN ('", paste0(as.character(unlist(Users_region)), collapse = "','") ,"')"))
 
  comments <- dbGetQuery(con, 'SELECT ID, comment, UserId_Comment,  strftime("%m-%d-%Y %H:%M:%S", Timestamp, "unixepoch") as Timestamp FROM Comments')
  mytestnow<- cases$CaseID[which(rownames(cases)==input$cases_rows_selected)]
  
    comments <- comments %>% 
      dplyr::filter(ID == cases [which(cases$CaseID == mytestnow[length(mytestnow)]),])
    if(nrow(comments)!=0){
    for (i in (1:nrow(comments))){
    Users2<-dbGetQuery(con, paste0("SELECT * FROM tab where UserId='",comments$UserId_Comment[i],"'"))
    comments$UserId_Comment[i]<-Users2$Name 
    }}
    
  dbDisconnect(con)
  datatable(comments,options=list(dom='t', ordering = FALSE, paging = FALSE), selection = "none")
  
})


output$comments2 <- DT::renderDataTable({
  input$submit_comment2
  con <- dbConnect(SQLite(), dbname = "data/test.db")
  Users <- dbGetQuery(con, "SELECT * FROM tab")
  Region_selected <- Users$Region[which(Users$UserId == input$UserId)]
  if(Region_selected=="Global"){
    Users_region<-Users %>%
      select (UserId)
  }
  else{
    Users_region <- Users %>% 
      filter (Region == Region_selected) %>% 
      select (UserId)
  }
 
  cases <- dbGetQuery(con,  paste0("SELECT CaseID FROM Cases WHERE UserId IN ('", paste0(as.character(unlist(Users_region)), collapse = "','") ,"')"))
  
  comments <- dbGetQuery(con, 'SELECT ID, comment, UserId_Comment,  strftime("%m-%d-%Y %H:%M:%S", Timestamp, "unixepoch") as Timestamp FROM Comments')
  mytestnow2<- cases$CaseID[which(rownames(cases)==input$cases2_rows_selected)]
  comments <- comments %>% 
    dplyr::filter(ID == cases [which(cases$CaseID == mytestnow2[length(mytestnow2)]),])
  if(nrow(comments)!=0){
    for (i in (1:nrow(comments))){
      Users2<-dbGetQuery(con, paste0("SELECT * FROM tab where UserId='",comments$UserId_Comment[i],"'"))
      comments$UserId_Comment[i]<-Users2$Name 
    }}
  
  dbDisconnect(con)
  datatable(comments,options=list(dom='t', ordering = FALSE, paging = FALSE), selection = "none")
  
})


output$comments5 <- DT::renderDataTable({
  input$submit_comment5
  con <- dbConnect(SQLite(), dbname = "data/test.db")
  Users <- dbGetQuery(con, "SELECT * FROM tab")
  Region_selected <- Users$Region[which(Users$UserId == input$UserId)]
  if(Region_selected=="Global"){
    Users_region<-Users %>%
      select (UserId)
  }
  else{
    Users_region <- Users %>% 
      dplyr::filter (Region == Region_selected) %>% 
      select (UserId)
  }

  cases <- dbGetQuery(con,  paste0("SELECT CaseID FROM Cases WHERE UserId IN ('", paste0(as.character(unlist(Users_region)), collapse = "','") ,"')"))
  
  comments <- dbGetQuery(con, 'SELECT ID, comment AS Recovery, UserId_Comment AS User,  strftime("%m-%d-%Y %H:%M:%S", Timestamp, "unixepoch") as Timestamp FROM Recovery')
  mytestnow<- cases$CaseID[which(rownames(cases)==input$cases_rows_selected)]
  
  comments <- comments %>% 
    dplyr::filter(ID == cases [which(cases$CaseID == mytestnow[length(mytestnow)]),])
  if(nrow(comments)!=0){
    for (i in (1:nrow(comments))){
      Users2<-dbGetQuery(con, paste0("SELECT * FROM tab where UserId='",comments$User[i],"'"))
      comments$User[i]<-Users2$Name 
    }}
  
  dbDisconnect(con)
  datatable(comments,options=list(dom='t', ordering = FALSE, paging = FALSE), selection = "none")
  
})




output$committees <- DT::renderDataTable({
  input$submit_committee
  con <- dbConnect(SQLite(), dbname = "data/test.db")
  Users <- dbGetQuery(con, "SELECT * FROM tab")
  Region_selected <- Users$Region[which(Users$UserId == input$UserId)]
  if(Region_selected=="Global"){
    Users_region<-Users %>%
      select (UserId)
  }
  else{
    Users_region <- Users %>% 
      filter (Region == Region_selected) %>% 
      select (UserId)
  }
  

  cases <- dbGetQuery(con,  paste0("SELECT CaseID FROM Cases WHERE UserId IN ('", paste0(as.character(unlist(Users_region)), collapse = "','") ,"')"))
  committees <- dbGetQuery(con, 'SELECT ID, Committee, UserId_Committee, Date, Comment FROM Committees')
  committees$Date <- format(as.Date(committees$Date, origin="1970-01-01"), "%Y/%m/%d")
  mytestnow<- cases$CaseID[which(rownames(cases)==input$cases_rows_selected)]
  
  committees <- committees %>% 
    dplyr::filter(ID == cases [which(cases$CaseID == mytestnow[length(mytestnow)]),])
  if(nrow(committees)!=0){
    for (i in (1:nrow(committees))){
      Users2<-dbGetQuery(con, paste0("SELECT * FROM tab where UserId='",committees$UserId_Committee[i],"'"))
      committees$UserId_Committee[i]<-Users2$Name 
    }}
  
  
  dbDisconnect(con)
  datatable(committees,options=list(dom='t', ordering = FALSE, paging = FALSE), selection = "none")
  
})

output$committees2 <- DT::renderDataTable({
  input$submit_committee2
  con <- dbConnect(SQLite(), dbname = "data/test.db")
  Users <- dbGetQuery(con, "SELECT * FROM tab")
  Region_selected <- Users$Region[which(Users$UserId == input$UserId)]
  if(Region_selected=="Global"){
    Users_region<-Users %>%
      select (UserId)
  }
  else{
    Users_region <- Users %>% 
      filter (Region == Region_selected) %>% 
      select (UserId)
  }
  

  cases <- dbGetQuery(con,  paste0("SELECT CaseID FROM Cases WHERE UserId IN ('", paste0(as.character(unlist(Users_region)), collapse = "','") ,"')"))
  committees <- dbGetQuery(con, 'SELECT ID, Committee, UserId_Committee, Date, Comment FROM Committees')
  committees$Date <- format(as.Date(committees$Date, origin="1970-01-01"), "%Y/%m/%d")
  mytestnow2<- cases$CaseID[which(rownames(cases)==input$cases2_rows_selected)]
  committees <- committees %>% 
    dplyr::filter(ID == cases [which(cases$CaseID == mytestnow2[length(mytestnow2)]),])
  if(nrow(committees)!=0){
    for (i in (1:nrow(committees))){
      Users2<-dbGetQuery(con, paste0("SELECT * FROM tab where UserId='",committees$UserId_Committee[i],"'"))
      committees$UserId_Committee[i]<-Users2$Name 
    }}
  
  
  dbDisconnect(con)
  datatable(committees,options=list(dom='t', ordering = FALSE, paging = FALSE), selection = "none")
  
})

output$warning3 <- renderValueBox({
  valueBox( 
    value1 <-  tags$p(dlOut2(), style = "font-size: 66%;"),
    subtitle = "Number of Cases in the Database", 
    icon = icon("file"),
    color = "aqua"
  )
})

output$MC <- renderValueBox({
  valueBox( 
    value1 <-  tags$p(dlOut3(), style = "font-size: 66%;"),
    subtitle = "Number of High Risk Cases in the Database", 
    icon = icon("fire"),
    color = "red"
  )
})

output$out <- renderValueBox({
  valueBox( 
    value1 <-  tags$p(format(dlOut4(),big.mark = ",",scientific=FALSE), style = "font-size: 66%;"),
    subtitle = "Total Financial Impact", 
    icon = icon("money"),
    color = "yellow"
  )
})

output$out2 <- renderValueBox({
  valueBox( 
    value1 <-  tags$p(dlOut5(), style = "font-size: 66%;"),
    subtitle = "Business Critical Cases", 
    icon = icon("table"),
    color = "light-blue"
  )
})

output$MC2 <- renderValueBox({
  valueBox( 
    value1 <-  tags$p(dlOut6(), style = "font-size: 66%;"),
    subtitle = "Top reported entity", 
    icon = icon("check"),
    color = "lime"
  )
})

output$warning2 <- renderValueBox({
  valueBox( 
    value1 <-  tags$p(dlOut7(), style = "font-size: 66%;"),
    subtitle = "Top reported risk type", 
    icon = icon("check"),
    color = "lime"
  )
})

output$plot1 <- renderPlot({

  
  if (req(input$ok) || req (input$okay)) {
    if (input$ok >= 1 || input$okay >= 1) {
      if (input$tabs == "intro") {
        con <- dbConnect(SQLite(), dbname = "data/test.db")
        Users <- dbGetQuery(con, "SELECT * FROM tab")
        Region_selected <- Users$Region[which(Users$UserId == input$UserId)]
        if(Region_selected=="Global"){
          Users_region<-Users %>%
            select (UserId)
        }
        else{
          Users_region <- Users %>% 
            dplyr::filter (Region == Region_selected) %>% 
            select (UserId)
        }
        value1 <- dbGetQuery(con,  paste0("SELECT Long_Description FROM Cases WHERE UserId IN ('", paste0(as.character(unlist(Users_region)), collapse = "','") ,"')"))
        value1<-unlist(value1)
        par(bg = "#15ffe7")
        wordcloud(value1,min.freq=10,max.words=100,random.color = TRUE,colors=brewer.pal(8, "Dark2"))
      
      }
    }
  }

})



output$plot111 <- renderPlot({
library(ggplot2)
#mitigation vs score



if (req(input$ok) || req (input$okay)) {
  if (input$ok >= 1 || input$okay >= 1) {
    if (input$tabs == "intro") {
      con <- dbConnect(SQLite(), dbname = "data/test.db")
      Users <- dbGetQuery(con, "SELECT * FROM tab")
      Region_selected <- Users$Region[which(Users$UserId == input$UserId)]
      if(Region_selected=="Global"){
        Users_region<-Users %>%
          select (UserId)
      }
      else{
        Users_region <- Users %>% 
          dplyr::filter (Region == Region_selected) %>% 
          select (UserId)
      }
      value1 <- dbGetQuery(con,  paste0("SELECT * FROM Cases WHERE UserId IN ('", paste0(as.character(unlist(Users_region)), collapse = "','") ,"')"))
      
      value1$Risk_Score<- trimws(value1$Risk_Score)
      value1<-value1[value1$Risk_Score!="",]
      
      value1<-value1[value1$Health_Mitigation!="",]
      value1<-value1[value1$Health_Mitigation!="Select...",]
      
      new1<-dplyr::count(value1, Risk_Score, Health_Mitigation)
      par(bg = "#15ffe7")
      ggplot(new1) + geom_col(aes(x = Risk_Score, y = n, fill = Health_Mitigation), position = "fill") + 
        scale_x_discrete(limits=c("Low", "Minor","Moderate","High","Significant")) + ylab("Percentage") + xlab("Risk Score") + labs(fill = "Health Status of Mitigation")+theme(plot.background = element_rect(fill = "#15ffe7"))
      
    }
  }
}

})




output$plot11 <- renderPlot({

  if (req(input$ok) || req (input$okay)) {
    if (input$ok >= 1 || input$okay >= 1) {
      if (input$tabs == "intro") {
        con <- dbConnect(SQLite(), dbname = "data/test.db")
        Users <- dbGetQuery(con, "SELECT * FROM tab")
        Region_selected <- Users$Region[which(Users$UserId == input$UserId)]
        if(Region_selected=="Global"){
          Users_region<-Users %>%
            select (UserId)
        }
        else{
          Users_region <- Users %>% 
            dplyr::filter (Region == Region_selected) %>% 
            select (UserId)
        }
        value1 <- dbGetQuery(con,  paste0("SELECT * FROM Cases WHERE UserId IN ('", paste0(as.character(unlist(Users_region)), collapse = "','") ,"')"))
    
        value2 <- dbGetQuery(con, "SELECT * FROM tab")
        value1$legalentity<-0
        for (i in (1:length(value1$Legal_Entity))){
          value1$legalentity[i]<-ifelse(length(value2$Entity[which(value2$UserId==value1$UserId[i])])==0,0,value2$Entity[which(value2$UserId==value1$UserId[i])])
        }

        par(bg = "#15ffe7")
        pie(table(value1$legalentity[which(value1$legalentity!=0)]), main = "Breakdown of Reporting Entities")
      }
    }
  }
  
 
})


output$userpanel <- renderUI({
  
  if (!is.null(input$UserId)) {
    con <- dbConnect(SQLite(), dbname = "data/test.db")
    User1 <- tryCatch(dbGetQuery(con, paste0("SELECT Name FROM tab where UserId='",input$UserId,"'")),error = function(e) e="") 
    sidebarUserPanel(
      span("Logged in as ", User1),
      subtitle = a(icon("sign-out"), "Logout", href="<insert your application link here>"))
  }
})

output$usermanual <- renderUI({

  if (!is.null(input$UserId)) {
    
    sidebarUserPanel("",
    
      
      subtitle = a(icon("book"), "Training Deck", href="<link to your pdf document>.pdf",target="_blank")
      )
  }
})


output$opriskpolicy <- renderUI({

  if (!is.null(input$UserId)) {
    
    sidebarUserPanel("",
                    
                     
                     subtitle = a(icon("book"), "OpRisk Policy", href="<link to your policy>.pdf",target="_blank")
    )
  }
})


observeEvent(input$update_case, {
  con <- dbConnect(SQLite(), dbname = "data/test.db")
  Users <- dbGetQuery(con, "SELECT * FROM tab")
  Region_selected <- Users$Region[which(Users$UserId == input$UserId)]
  if(Region_selected=="Global"){
    Users_region<-Users %>%
      select (UserId)
  }
  else{
    Users_region <- Users %>% 
      dplyr::filter (Region == Region_selected) %>% 
      select (UserId)
  }
  
 
  cases <- dbGetQuery(con,  paste0("SELECT CaseID FROM Cases WHERE UserId IN ('", paste0(as.character(unlist(Users_region)), collapse = "','") ,"')"))
  mytestnow<- cases$CaseID[which(rownames(cases)==input$cases_rows_selected)]
  dbExecute(con, paste0("UPDATE cases SET Risk_Category ='", input$riskcategory_1,
                        "' ,Incident_Type ='", input$sel1_1,                 
                        "' ,Short_Description ='", gsub("[\r\n\'\"?/]"," ",input$riskdesc_1), 
                        "' ,Long_Description ='", gsub("[\r\n\'\"?/]"," ",input$riskdesc1_1), 
                        "' ,Region ='", input$riskregion_1,
                        "' ,Legal_Entity ='", input$legalentity_1,
                        "' ,Impact ='", input$Impact1_1,
                        "' ,Likelihood ='", input$likelihood1_1,
                        "' ,Risk_Score ='", input$riskscore1_1,
                        "' ,Mitigation ='", gsub("[\r\n\'\"?/]"," ",input$mitigation_1),
                        "' ,Health_Mitigation ='", input$health_1,
                        "' ,ImpactUSD ='", as.numeric(input$ImpactUSD_1),
                        "' ,Impact_Type='", input$frequency_1,
                        "' ,Affected_Systems='", input$Affected_Systems_1,
                        "' ,Business_Critical ='", input$Business_Critical_1,
                        "' ,Data_Privacy ='", input$Data_Privacy_1,
                        "' ,Cybersecurity ='", input$Cybersecurity_1,
                        "' ,Impacted_Department ='", input$mpacted_Department_1,
                        "' ,UserId ='", input$UserId, 
                        "' ,Status ='", input$status, 
                        "' WHERE CaseID= '", mytestnow,"'"))
  dbDisconnect(con)
  
})

observeEvent(input$submit_comment,{
  
  con <- dbConnect(SQLite(), dbname = "data/test.db")
  Users <- dbGetQuery(con, "SELECT * FROM tab")
  Region_selected <- Users$Region[which(Users$UserId == input$UserId)]
  if(Region_selected=="Global"){
    Users_region<-Users %>%
      select (UserId)
  }
  else{
    Users_region <- Users %>% 
      dplyr::filter (Region == Region_selected) %>% 
      select (UserId)
  }
  

  cases <- dbGetQuery(con,  paste0("SELECT CaseID FROM Cases WHERE UserId IN ('", paste0(as.character(unlist(Users_region)), collapse = "','") ,"')"))
  mytestnow<- cases$CaseID[which(rownames(cases)==input$cases_rows_selected)]
   ID <- cases [which(cases$CaseID == mytestnow),]
  new_comment <- data.frame(ID = ID, comment = input$new_comment, UserId_Comment = input$UserId, Timestamp = Sys.time())
  dbWriteTable(con, "Comments",new_comment, append = TRUE)
  dbDisconnect(con)
  updateTextAreaInput(session,"new_comment",value = "")
})


observeEvent(input$submit_comment5,{
  
  con <- dbConnect(SQLite(), dbname = "data/test.db")
  Users <- dbGetQuery(con, "SELECT * FROM tab")
  Region_selected <- Users$Region[which(Users$UserId == input$UserId)]
  if(Region_selected=="Global"){
    Users_region<-Users %>%
      select (UserId)
  }
  else{
    Users_region <- Users %>% 
      dplyr::filter (Region == Region_selected) %>% 
      select (UserId)
  }
  

  cases <- dbGetQuery(con,  paste0("SELECT CaseID FROM Cases WHERE UserId IN ('", paste0(as.character(unlist(Users_region)), collapse = "','") ,"')"))
  mytestnow<- cases$CaseID[which(rownames(cases)==input$cases_rows_selected)]
  ID <- cases [which(cases$CaseID == mytestnow),]
  new_comment <- data.frame(ID = ID, comment = input$new_comment5, UserId_Comment = input$UserId, Timestamp = Sys.time())
  dbWriteTable(con, "Recovery",new_comment, append = TRUE)
  dbDisconnect(con)
  updateTextAreaInput(session,"new_comment5",value = "")
})


observeEvent(input$submit_comment2,{
  
  con <- dbConnect(SQLite(), dbname = "data/test.db")
  Users <- dbGetQuery(con, "SELECT * FROM tab")
  Region_selected <- Users$Region[which(Users$UserId == input$UserId)]
  if(Region_selected=="Global"){
    Users_region<-Users %>%
      select (UserId)
  }
  else{
    Users_region <- Users %>% 
      dplyr::filter (Region == Region_selected) %>% 
      select (UserId)
  }

  cases <- dbGetQuery(con,  paste0("SELECT CaseID FROM Cases WHERE UserId IN ('", paste0(as.character(unlist(Users_region)), collapse = "','") ,"')"))
  mytestnow2<- cases$CaseID[which(rownames(cases)==input$cases2_rows_selected)]
  ID <- cases [which(cases$CaseID == mytestnow2),]
  new_comment <- data.frame(ID = ID, comment = input$new_comment, UserId_Comment = input$UserId, Timestamp = Sys.time())
  dbWriteTable(con, "Comments",new_comment, append = TRUE)
  dbDisconnect(con)
  updateTextAreaInput(session,"new_comment",value = "")
})



observeEvent(input$submit_committee,{
  
  con <- dbConnect(SQLite(), dbname = "data/test.db")
  Users <- dbGetQuery(con, "SELECT * FROM tab")
  Region_selected <- Users$Region[which(Users$UserId == input$UserId)]
  if(Region_selected=="Global"){
    Users_region<-Users %>%
      select (UserId)
  }
  else{
    Users_region <- Users %>% 
      dplyr::filter (Region == Region_selected) %>% 
      select (UserId)
  }

  cases <- dbGetQuery(con,  paste0("SELECT CaseID FROM Cases WHERE UserId IN ('", paste0(as.character(unlist(Users_region)), collapse = "','") ,"')"))
  mytestnow<- cases$CaseID[which(rownames(cases)==input$cases_rows_selected)]
  ID <- cases [which(cases$CaseID == mytestnow),]
    new_committee <- data.frame(ID = ID, Committee = input$select_committee,  Date = input$date_committee, Comment = input$new_comment_committee, UserId_Committee = input$UserId)
  dbWriteTable(con, "Committees",new_committee, append = TRUE)
  dbDisconnect(con)
  updateSelectInput(session,"select_committee",choices = c("Select...","GRC", "GRC Taskforce","Local Risk Committee", "Steering Committee","Incident Management Committee"))
  updateDateInput(session,"date_committee",value=Sys.Date(),max = Sys.Date())
  updateTextAreaInput(session,"new_comment_committee",value = "")
})


observeEvent(input$submit_committee2,{
  
  con <- dbConnect(SQLite(), dbname = "data/test.db")
  Users <- dbGetQuery(con, "SELECT * FROM tab")
  Region_selected <- Users$Region[which(Users$UserId == input$UserId)]
  if(Region_selected=="Global"){
    Users_region<-Users %>%
      select (UserId)
  }
  else{
    Users_region <- Users %>% 
      dplyr::filter (Region == Region_selected) %>% 
      select (UserId)
  }

  cases <- dbGetQuery(con,  paste0("SELECT CaseID FROM Cases WHERE UserId IN ('", paste0(as.character(unlist(Users_region)), collapse = "','") ,"')"))
  mytestnow2<- cases$CaseID[which(rownames(cases)==input$cases2_rows_selected)]
  ID <- cases [which(cases$CaseID == mytestnow2),]
  new_committee <- data.frame(ID = ID, Committee = input$select_committee,  Date = input$date_committee, Comment = input$new_comment_committee, UserId_Committee = input$UserId)
  dbWriteTable(con, "Committees",new_committee, append = TRUE)
  dbDisconnect(con)
  updateSelectInput(session,"select_committee",choices = c("Select...","Risk Committee", "Risk Taskforce","Local Risk Committee", "Steering Committee","Incident Management Committee"))
  updateDateInput(session,"date_committee",value=Sys.Date(),max = Sys.Date())
  updateTextAreaInput(session,"new_comment_committee",value = "")
})



email <- function(subject_code, test) {
  
  
  
  send.mail(from = "OpRiskDB@<your provider here>.com",
          
               to = rv$confident,
     
                   subject= subject_code,
         #      body = test,
        body= c("Please find enclosed the latest risk item submitted to the Operational Risk Database. For further information, please contact ....") ,
                   smtp = list(host.name = "xx.xx.xx.xxx", port = 25),
                   html = TRUE,
                   inline = TRUE,
                   authenticate = FALSE,
        attach.files =c("<your folder>/data/RiskItem.xlsx"),
                   send = TRUE)
}


  
}

shinyApp(ui, server)