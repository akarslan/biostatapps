options(scipen = 999)
library(shiny)
source("init.R",local = T,encoding = "UTF-8")

ui=tagList(tags$style(HTML("body{font-family:Bahnschrift;font-style: normal;}")),
  navbarPage(header=tags$head(tags$style(type="text/css", "body {padding-top: 70px;}",".tab-content { overflow: visible; }"),
                              tags$style(HTML(".navbar-default .navbar-brand {color: #f39c12; font-size: 20px;font-family: Bahnschrift;font-weight: bold;}
                                              .navbar .navbar-nav {font-size: 17px; font-family: Bahnschrift;}"
                                              ))),
             title=tags$div(style="display: inline-block;vertical-align:top;padding-top: 5px;","Associative Classification Software"),
             windowTitle = "Associative Classification Software",
             inverse = FALSE,position = "fixed-top",
             theme = shinythemes::shinytheme("flatly"),id = "maintab",
             tabPanel(div(img(src="homepage.png",height = 30, width = 30),"Home"),value = "introtab",column(width = 12,
                                                        column(width = 12,br(),strong(textOutput("gen_info_title"))),
                                                        column(width = 12,textOutput("gen_info"),br()),
                                                        column(width = 12,strong(textOutput("about_soft_title"))),
                                                        column(width = 12,textOutput("about_soft_1")),
                                                        column(width = 12,textOutput("about_soft_2"),br()),
                                                        column(width = 12,strong(textOutput("used_libraries_title"))),
                                                        column(width = 11,DT::dataTableOutput("used_libraries"),br()),
                                                        column(width = 12,strong(textOutput("cita_title"))),
                                                        column(width = 12,textOutput("cita_text"),br())
                                                        ),
                      tags$head(tags$style("#gen_info_title{color: black; font-size: 22px; font-family: Bahnschrift; text-align: justify;}")),
                      tags$head(tags$style("#gen_info{color: black; font-size: 18px; font-family: Bahnschrift; text-align: justify;}")),
                      tags$head(tags$style("#about_soft_title{color: black; font-size: 22px; font-family: Bahnschrift; text-align: justify;}")),
                      tags$head(tags$style("#about_soft_1{color: black; font-size: 18px; font-family: Bahnschrift; text-align: justify;}")),
                      tags$head(tags$style("#about_soft_2{color: black; font-size: 18px; font-family: Bahnschrift; text-align: justify;}")),
                      tags$head(tags$style("#used_libraries_title{color: black; font-size: 22px; font-family: Bahnschrift; text-align: justify;}")),
                      tags$head(tags$style("#cita_title{color: black; font-size: 22px; font-family: Bahnschrift; text-align: justify;}")),
                      tags$head(tags$style("#cita_text{color: black; font-size: 18px; font-family: Bahnschrift; text-align: justify;}")),
                      br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),
                      column(width = 12,align="center",shinyWidgets::actionBttn(inputId = "intro_right",label = "Start", style = "jelly",color = "danger",icon=icon("play")))
                      ),
             tabPanel(div(img(src="data.png",height = 30, width = 30),"Data"),value = "datatab",
                      fluidRow(br(),
                        column(2, shinyLP::panel_div("success", div(img(src="upload.png",height = 28, width = 28),style="font-family: Bahnschrift;",strong("File upload")),content = 
                                                       tagList(column(width = 12,br(),br(),br(),br(),br(),br(),br(),br(),br(),
                                                                      fluidRow(column(width=12,strong("Note:"),"The data files you upload to this tool must have", 
                                                                                      tags$code('.xls/xlsx'),",", tags$code('.arff'),",", tags$code('.sav'),"and",tags$code('.csv/.txt'), 
                                                                                      "extensions.",align="justify"),
                                                                               br(),column(width=12,tags$style(".shiny-file-input-progress {display: none}"),fileInput("upload_data",label = NULL,buttonLabel = "Choose",placeholder = NULL))
                                                                               ))
                                                               ,br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br()
                                                       )
                        )),
                        column(6, shinyLP::panel_div(class_type = "info", div(img(src="datatable.png",height = 28, width = 28),style="font-family: Bahnschrift;",strong("Data view")),
                                                     content = tagList(column(width = 12,DT::dataTableOutput("data_view"))
                                                     ,br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br()
                                                     )
                        )),
                        column(4,
                               shinyLP::panel_div(class_type = "warning", div(img(src="type.png",height = 28, width = 28),style="font-family: Bahnschrift;",strong("Variable types")),
                                                          content = tagList(column(width = 12,DT::dataTableOutput("vartypetable"),
                                                                                   tags$script(HTML(
                                                                                     "Shiny.addCustomMessageHandler('unbindDT', function(id) {
                                                                                                      var $table = $('#'+id).find('table');
                                                                                                      if($table.length > 0){Shiny.unbindAll($table.DataTable().table().node());}
                                                                                                      })"))
                                                                                   )
                                                          ,br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br()
                                                          )
                        )),
                        column(width = 12,align="center",
                               shinyWidgets::actionBttn(inputId = "data_left",label = "Previous", style = "jelly",color = "danger",icon=icon("hand-point-left")),
                               shinyalert::useShinyalert(),
                               shinyWidgets::actionBttn(inputId = "data_right",label = "Next", style = "jelly",color = "danger",icon=icon("hand-point-right"))
                               )
                        )
                      ),
             tabPanel(div(img(src="analysis.png",height = 30, width = 30),"Analysis"),value = "analysistab",br(),
                      sidebarLayout(
                        sidebarPanel(tags$style(".well {background-color:#E0FFFF;}"),width = 3,
                                     selectizeInput("gr_variable","Response/Output variable:",choices=NULL),
                                     uiOutput("positive_class_ui"),
                                     selectizeInput("sub_group","Subgroup:",choices=NULL),
                                     uiOutput("lev_subgrp_ui"),
                                     selectizeInput("other_variables","Predictor(s):",choices=NULL,multiple=T),
                                     shinyWidgets::prettyCheckbox(inputId = "fselection",label = strong("Apply feature selection"),icon = icon("check-square-o"), status = "primary",outline = TRUE,animation = "jelly"),
                                     uiOutput("fs_options_ui"),
                                     numericInput("support","Support:",value = 0.2,min = 0.1,max = 1),
                                     numericInput("confidence","Confidence:",value = 0.5,min = 0.1,max = 1),
                                     uiOutput("disc_dd"),
                                     selectizeInput("assoc_class_algo","Classification algorithm:",choices=c("Classification-Based Association Rules"="CBA",
                                                                                                              "Regularized Class Association Rules"="RCAR")),
                                     tags$div(style="display:inline-block",numericInput(inputId = "seed",label = "Seed number:",value = sample(.Machine$integer.max,1))),
                                     tags$div(style="display:inline-block",shinyWidgets::actionBttn(inputId = "refreshseed",label=NULL, icon("refresh"),style = "jelly",color = "danger")),
                                     tags$head(tags$style(HTML('#seed{cursor: not-allowed}'))),br(),br(),
                                     tags$div(style="display: inline-block;vertical-align:top; width: 150px;",shinyWidgets::actionBttn(inputId = "analysis_left",label = "Previous", style = "jelly",color = "danger",icon=icon("hand-point-left"))),
                                     div(style="display: inline-block;vertical-align:top; width: 140px;",HTML("<br>")),
                                     tags$div(style="display: inline-block;vertical-align:top; width: 100px;",shinyWidgets::actionBttn("analysis",label = "Run",icon = icon("cogs"),style = "jelly",color = "danger")),br(),
                                     uiOutput("download_button"),br(),
                                     shinyjs::useShinyjs(),
                                     shinyjs::extendShinyjs(text = "shinyjs.refresh = function() { location.reload(); }"),
                                     column(width = 12,uiOutput("refresh_button"),align="center"),br(),br()
                                     ),
                        mainPanel(
                          column(width = 12,
                                 column(width = 12,align="center",
                                        uiOutput("stats_header")
                                 ),
                                 column(width = 6,
                                        column(width = 12,
                                               DT::dataTableOutput("confusion_matrix")
                                        ),
                                        column(width = 12,br(),
                                               uiOutput("selected_features")
                                        )
                                 ),
                                 column(width = 6,
                                        DT::dataTableOutput("metrics")
                                 ),
                                 column(width = 12,
                                        uiOutput("seperator1")
                                 ),
                                 column(width = 12,align="center",
                                        uiOutput("associations_header")
                                 ),
                                 column(width = 12,
                                        DT::dataTableOutput("DFRules")
                                 ),
                                 column(width = 12,
                                        uiOutput("seperator2")
                                 ),
                                 column(width = 12,align="center",
                                        uiOutput("network_header")
                                 ),
                                 column(width = 12,
                                        visNetwork::visNetworkOutput("graphPlot",height = "1250px")
                                 )
                          ),
                          shinysky::busyIndicator(strong("The process is running, please wait ..."),img = "progress.gif")
                          )
                        )
                      ),
             tags$head(tags$style(HTML('.navbar-nav a {cursor: not-allowed}')))
             
             #tabPanel("deneme",
             #         verbatimTextOutput("deneme")
             #)
             )
  #fixedPanel(draggable = F,bottom = "0px",left = "0px",right = "0px",height = "30px",style = "z-index: 1000000;",width = "100%",wellPanel(style = "background: #2c3e50"))
)
  
server=function(input, output, session){
  
  shinyjs::disable(selector = '.navbar-nav a')
  shinyjs::disable(selector = '#seed')
  
  observe({
    
    if(input$refreshseed){
      updateNumericInput(session,inputId = "seed",value = sample(.Machine$integer.max,1))
    }
    
  })

  output$gen_info_title=renderText({
    
    "General Info"
    
  })

  
  output$gen_info=renderText({
    
    "Associative classification (AC) is a supervised learning task that classifies based on association rules. 
    The related AC model classifies the unlabeled dataset using the formed association rules. Some of AC models 
    are CBA (Classification Based on Association Rules Algorithm), GARC (Gain based Association Rule Classification),
    RCAR (Regularized Class Association Rules), etc. There are many algorithms (Apriori, ECLAT, CARMA, etc.) that
    are used to obtain the association rules, the most common of which is the Apriori algorithm."
    
  })
  
  output$about_soft_title=renderText({
    
    "Developed Software"
    
  })
  
  output$about_soft_1=renderText({
    
    "The developed software consists of two parts. In the first part, the data file is uploaded into the software.
    Then, the types of variables ('Continuous', 'Discrete', 'Nominal', 'Ordinal') and their roles ('Predictor', 'Response/Output') are determined."
    
  })
  
  output$about_soft_2=renderText({
    
    "In the second part, the researcher can optionally apply feature selection to the dataset. 'Boruta' 
    can be used as the feature selection algorithm. In the next stage, support and trust values are determined. 
    Related parameters are set to 0.2 for support and 0.8 for confidence by default. In the next stage, there is 
    a drop-down list in which the discretization techniques can be selected depending on whether there is at least 
    one numerical variable in the dataset. At the last stage, the AC model is selected. There are two defined 
    AC models in the software. These are CBA and RCAR. Model results are given in the main panel. This panel 
    includes the confusion matrix, metrics for the classification performance of the relevant AC model, the 
    association rules formed by the Apriori algorithm and subsequently pruned, and network visualization of these association rules."
    
  })
  
  
  output$used_libraries_title=renderText({
    
    "Packages and other sources"
    
  })
  
  output$used_libraries=DT::renderDataTable(server = F,{
    mat=matrix(NA,nrow = 3,ncol = 2,dimnames = list(NULL,c("Section","Library/Site")))
    mat[,1]=c("Graphical interface", "Data upload, Data tables","Feature selection, Assciation rules, Discretization techniques, 
              Associative classification, Network visualization")
    mat[,2]=c("shiny, shinythemes, shinyWidgets, shinyLP, shinyalert, shinyjs, shinysky, https://www.flaticon.com/","foreign, readxl, DT",
              "glmnet, Boruta, arules, arulesCBA, caret, arulesViz, visNetwork")
    DT::datatable(
      mat,rownames=FALSE,
      options = list(paging = FALSE,ordering=FALSE,
                     searching=FALSE, autoWidth = TRUE,info=FALSE,scrollX=FALSE,
                     columnDefs = list(list(className = "dt-left",targets = 1)),
                     initComplete = DT::JS("function(settings, json) {",
                                       "$(this.api().table().header()).css({'background-color': '#3498db', 'color': '#fff'});",
                                       "}"))
    )
  })
  
  
  output$cita_title=renderText({
    
    "Citation"
    
  })
  
  output$cita_text=renderText({
    
    paste("Arslan, A. K., Tunç Z., Çiçek, İ. B. & Colak, C. Associative Classification Software [Web-based software]. Retrieved",format.Date(Sys.Date(),"%m/%d/%Y"),"from http://biostatapps.inonu.edu.tr/ACS/")
    
  })


  ############## Tab changing ##############
  
  observeEvent(input$intro_right,{
    updateTabsetPanel(session, "maintab",
                      selected = "datatab")
  })
  
  observeEvent(input$data_left,{
    updateTabsetPanel(session, "maintab",
                      selected = "introtab")
  })
  
  observeEvent(input$analysis_left,{
    updateTabsetPanel(session, "maintab",
                      selected = "datatab")
  })

  
  ############## Data uploading ##############
  path=reactive({input$upload_data$datapath})
  
  observe({
    if(is.null(path())){
      return(NULL)
    }
    if(any(stringr::str_detect(string = path(),pattern = c("xls","csv","txt")))){
      showModal(modalDialog(size = "m",class="row",# footer çizgisini düzenliyor
                            footer = tags$div(shinyWidgets::actionBttn("select","OK",color = "danger",style = "jelly"),`data-dismiss` = "modal"),easyClose = F,
                            title = strong(ifelse(stringr::str_detect(string = path(),pattern = "xls"),
                                                  "Select the excel sheet.",
                                                  "Make the following arrangements for the csv-txt file to be uploaded.")),
                            column(width = 12,
                                   if(stringr::str_detect(string = path(),pattern = "xls")){
                                     column(width = 12,selectizeInput("selected_tab",label = "Sheet name:",choices=readxl::excel_sheets(path())))
                                   }else{
                                     column(width = 12,
                                            checkboxInput("header","File contains header line",TRUE),
                                            radioButtons("col_sep","Column separator:",c("Comma"=",","Semicolon"=";","Tab"="\t"),selected = ","),
                                            radioButtons("dec_sep","Decimal separator:",c("Comma"=",","Dot"="."), selected = ".")
                                     )
                                   },
                                   column(width = 12,tags$div(style = 'overflow-x: scroll', tableOutput("modal_datatable"))), ## unbind_DT nesnesi ile çakıştığı için burada tableoutput kullanıldı
                                   tags$head(tags$style("#modal_datatable thead{background-color: #3498db;color: #FFFFFF;};"))
                            )
      ))
      output$modal_datatable=renderTable(bordered = T,width = "auto",striped = T,
                                         spacing = "s",hover = T,colnames = T,align = "c",{
                                           head(if(stringr::str_detect(string = path(),pattern = "xls")){
                                             readxl::read_excel(path(),sheet=input$selected_tab,col_types="guess")
                                           }else{
                                             read.csv(path(),header = input$header,sep = input$col_sep,dec = input$dec_sep,encoding = "UTF-8")
                                           },10)
                                         })
    }else{
      return(NULL)
    }
  })
  
  excel_csv_data=eventReactive(input$select,{
    if(stringr::str_detect(string = path(),pattern = "xls")){
      readxl::read_excel(path(),sheet=input$selected_tab,col_types="guess")
    }else{
      read.csv(path(),header = input$header,sep = input$col_sep,dec = input$dec_sep)
    }
  })
  
  data = reactive({
    if(is.null(path())){
      return(NULL)
    }else if(any(stringr::str_detect(string = path(),pattern = c("xls","csv","txt"))) && !is.null(excel_csv_data())){
      excel_csv_data()
    }else if(stringr::str_detect(string = path(),pattern = "sav")){
      foreign::read.spss(path(),to.data.frame = T,reencode = 'UTF-8')
    }else if(stringr::str_detect(string = path(),pattern = "arff")){
      foreign::read.arff(path())
    }else{
      return(NULL)
    }
  })
  
  raw_data=reactive({
    if(is.null(data())){
      return(NULL)
    }else{
      data=data()
      colnames(data)=stringr::str_replace_all(colnames(data)," ","")
      colnames(data)=stringr::str_replace_all(colnames(data),"-","")
      colnames(data)=stringr::str_to_lower(colnames(data),locale = "tr")
      data
    }
  })
  
  output$data_view=DT::renderDataTable(server = FALSE,{
    DT::datatable(
      raw_data(),rownames=TRUE,editable = TRUE,extensions="FixedColumns",
      options = list(paging = TRUE,ordering=TRUE,fixedColumns = list(leftColumns = 1),
                     #iDisplayLength=10,lengthMenu=seq(50,500,50),
                     searching=TRUE, autoWidth = F,info=TRUE,scrollX=TRUE,pageLength = 100, scrollY = "500px",
                     columnDefs = list(list(className = "dt-center",targets = "_all")),
                     initComplete = DT::JS("function(settings, json) {",
                                       "$(this.api().table().header()).css({'background-color': '#3498db', 'color': '#fff'});",
                                       "}"))
    )
  })
  
  
  ################ Variable type defining ################
  
  shinyValue = function(id, len) {
    unlist(lapply(seq_len(len), function(i) { 
      value = input[[paste0(id, i)]] 
      if (is.null(value)){
        return(NA)
      }else{
        return(value)
      }
    }))
  }
  
  first_type_table=reactive({
    
    session$sendCustomMessage("unbindDT","vartypetable")
    if(length(input$data_view_rows_all)>0){
      k=ifelse(is.null(path()),0,dim(raw_data())[2])
      tablo=matrix(NA, nrow = k, ncol = 1)
      colnames(tablo)=paste("Variable")
      tablo[,1]=colnames(raw_data())
      tablo=cbind.data.frame(tablo,Type=shinyInput1(selectInput,raw_data(),k,"selecter_1"),
                             Role=shinyInput2(selectInput,k,"selecter_2")
      )
      tablo
    }
    
  })
  
  output$vartypetable=DT::renderDataTable({
    
    first_type_table()
    
  },
  rownames=FALSE, selection="none", server = TRUE, escape = FALSE,
  options = list(paging = TRUE, ordering=FALSE,scrollX=TRUE,pageLength = 100, scrollY = "500px",
                 searching=TRUE, autoWidth = FALSE,info=FALSE,fixedColumns = list(leftColumns = 1),
                 columnDefs = list(list(className = "dt-center",targets = c(1:2))),
                 initComplete = DT::JS("function(settings, json) {",
                                   "$(this.api().table().header()).css({'background-color': '#f39c12', 'color': '#fff'});",
                                   "}"),
                 preDrawCallback = DT::JS('function() {
                                      Shiny.unbindAll(this.api().table().node()); }'), 
                 drawCallback = DT::JS('function() {
                                   Shiny.bindAll(this.api().table().node()); } '))
                 )
  
  
  type_table=reactive({
    
    if(length(input$data_view_rows_all)==0){
      return(NULL)
    }else{
      k=dim(raw_data())[2]
      tbl=cbind.data.frame(
        "Variable"=colnames(raw_data()),
        "Type"=shinyValue(id="selecter_1", len=k),
        "Role"=shinyValue(id="selecter_2", len=k)
      )
    }
    
  })
  
  last_data=reactive({
    
    if(is.null(raw_data())){
      return(NULL)
    }else{
      data=as.data.frame(raw_data()[,colnames(raw_data())%in%type_table()[,1]])
      
      nominals=c()
      numerics=c()
      ordinals=c()
      
      nominals=which(type_table()[,2]=="Nominal")
      numerics=which(type_table()[,2]=="Discrete" || type_table()[,2]=="Continuous")
      ordinals=which(type_table()[,2]=="Ordinal")
      
      for (i in nominals) {
        data[,i]=as.factor(data[,i])
      }
      
      for (i in numerics) {
        data[,i]=as.numeric(data[,i])
      }
      
      for (i in ordinals) {
        data[,i]=as.ordered(data[,i])
      }
      
      data
      
    }
    
  })
  
  ############## Alerts ###############
  
  observeEvent(input$data_right,{
    
    R1=length(input$data_view_rows_all)==0
    R2=any(type_table()[,3] %in% c("Response/Output")==TRUE)
    R3=type_table()[,2][which(type_table()[,3]=="Response/Output")] %in% c("Nominal", "Ordinal")
    
    if(R1==TRUE){
      
      shinyalert::shinyalert(title = "Data file is not uploaded!", text = "Upload data file to continue analysis.", type = "warning",
                             confirmButtonText = "OK",closeOnEsc = TRUE,animation = "slide-from-top",confirmButtonCol = "#F8BB86")
      
    }
    
    if(R1==FALSE && R2==FALSE){
      
      shinyalert::shinyalert(title = "Variable definitions are missing!", text = "Please specify at least one 'Response/Output' variable role.", type = "warning",
                             confirmButtonText = "OK",closeOnEsc = TRUE,animation = "slide-from-top",confirmButtonCol = "#F8BB86")
      
    }
    
    if(R1==FALSE && R2==TRUE && R3==FALSE){
      
      shinyalert::shinyalert(title = "Variable definitions are incorrect!", text = "The type of 'Response/Output' variable must be 'Nominal' or 'Ordinal'.", type = "warning",
                             confirmButtonText = "OK",closeOnEsc = TRUE,animation = "slide-from-top",confirmButtonCol = "#F8BB86")
      
    }
    
    if(R1==FALSE && R2==TRUE && R3==TRUE){
      
      updateTabsetPanel(session, "maintab",selected = "analysistab")
      
    }

  })
  
  ############## Discrization ui ##############
  
  disc_rule=reactive({
    
    any(type_table()[,2] %in% c("Discrete", "Continuous")==TRUE)
    
  })
  
  output$disc_dd=renderUI({
    
    if(disc_rule()==TRUE){
      selectizeInput("disc_methods",label="Discretization technique:",choices=c("Minimum Discription Length Principle"="mdlp", 
                                                                             "Class-Attribute Interdependence Maximization"="caim", 
                                                                             "Class-Attribute Contingency Coefficient"="cacc", 
                                                                             "Ameva"="ameva", "Chi-square"="chi2", "ChiMerge"="chimerge", 
                                                                             "Extended Chi2"="extendedchi2", "Modified Chi2"="modchi2"),selected="ameva")
      
    }else{
      NULL
    }
    
  })
  
  ############## Calling variable ##############
  
  observe({
    updateSelectizeInput(session,"gr_variable",choices=colnames(last_data()[which(type_table()[,3]=="Response/Output")]),
                         selected = colnames(last_data()[which(type_table()[,3]=="Response/Output")])[1])
    
    if(length(colnames(last_data())[na.omit(which(type_table()[,2]=="Nominal" | type_table()[,2]=="Ordinal"))])==1){
      choices="None"
    }else{
      if(is.null(type_table())){
        return(NULL)
      }else{
        f1=type_table()[which(type_table()[,3]!="Response/Output"),]
        f2=f1[which(f1[,2]=="Nominal" | f1[,2]=="Ordinal"),]
        choices=c("None",levels(f2[,1])[f2[,1]])
      }
    }
    updateSelectizeInput(session,"sub_group",choices=choices,selected = "None")
  })
  
  observe({
    
    if(input$sub_group=="None"){
      updateSelectizeInput(session,"other_variables",choices=colnames(last_data())[which(type_table()[,3]!="Response/Output")],
                           selected = colnames(last_data()[which(type_table()[,3]!="Response/Output")]))
    }else{
      updateSelectizeInput(session,"other_variables",choices=colnames(last_data()[-which(colnames(last_data())==input$sub_group)])[which(type_table()[,3]!="Response/Output")],
                           selected = colnames(last_data()[which(type_table()[,3]!="Response/Output")]))
    }

    
  })

  lev=reactive({levels(last_data()[,isolate(input$gr_variable)])})
  lev_subgrp=reactive({
    if(input$sub_group!="None"){
      levels(last_data()[,input$sub_group])
    }else{
      return(NULL)
    }
  })
  
  output$positive_class_ui=renderUI({
    
    if(length(lev())==2){
      selectizeInput("positive_class","Positive class:",choices=lev())
    }else{
      return(NULL)
    }
    
  })
  
  output$lev_subgrp_ui=renderUI({
    
    if(!is.null(lev_subgrp())){
      selectizeInput("sub_category","Subcategory:",choices=lev_subgrp())
    }else{
      return(NULL)
    }
    
  })
  

  ############## Analysis ##############
  
  observe({
    
    if(!is.null(type_table()) && NROW(type_table())<11 && input$fselection==TRUE){
      shinyalert::shinyalert(title = "Warning!", text = "It is not recommended to use this feature if the number of predictor variables is below 10.", 
                             type = "warning",confirmButtonText = "OK",closeOnEsc = TRUE,animation = "slide-from-top",confirmButtonCol = "#F8BB86")
    }
    
  })
  
  output$fs_options_ui=renderUI({
    
    if(length(lev())==2 && input$fselection==TRUE){
      selectInput("fs_options",label = "Feature selection method:",choices = c("Boruta (Random Forest)"="boruta",
                                                                               "LASSO log. regression"="lasso",
                                                                               "Elastic-net log. regression"="elasticnet"),selected = "boruta")
    }else{
      return(NULL)
    }
    
  })
  
  
  after_FS=reactiveValues(predictors=NULL)
  
  observeEvent(input$analysis,{
    
    set.seed(input$seed)
    
    if(any(c(input$gr_variable=="",is.null(input$other_variables),is.na(input$support),is.na(input$confidence)))==TRUE){
      
      shinyalert::shinyalert(title = "Blank field(s)!", text = "There is/are field(s) that no value is entered.", type = "warning",
                             confirmButtonText = "OK",closeOnEsc = TRUE,animation = "slide-from-top",confirmButtonCol = "#F27474")
      
    }else if(input$support<=0 || input$support>1){
      shinyalert::shinyalert(title = "Error!", text = "Support value must be in the range (0,1].", 
                             type = "error",confirmButtonText = "OK",closeOnEsc = TRUE, animation = "slide-from-top",confirmButtonCol = "#F27474")
    }else if(input$confidence<=0 || input$confidence>1){
      shinyalert::shinyalert(title = "Error!", text = "Confidence value must be in the range (0,1].", 
                             type = "error",confirmButtonText = "OK",closeOnEsc = TRUE, animation = "slide-from-top",confirmButtonCol = "#F27474")
    }
    else{
      
      data=na.omit(last_data())
      
      if(isolate(input$sub_group)=="None"){
        data=data
      }else{
        data=data[which(data[input$sub_group]==input$sub_category),]
      }
      
      data=data[,c(isolate(input$gr_variable),isolate(input$other_variables))]
      cls=input$gr_variable
      frml=as.formula(paste(cls,"~.",sep=" "))
      
      if(isolate(input$fselection)==TRUE && length(lev()>2)){
        FS=Boruta::Boruta(formula=frml,data=data)
        if(all(FS$finalDecision=="Rejected")==TRUE){
          data=data
        }else{
          data=data[,c(cls,names(FS$finalDecision)[which(FS$finalDecision=="Confirmed")], names(FS$finalDecision)[which(FS$finalDecision=="Tentative")])]
          after_FS$predictors=c(names(FS$finalDecision)[which(FS$finalDecision=="Confirmed")], names(FS$finalDecision)[which(FS$finalDecision=="Tentative")])
        }
      }else if(isolate(input$fselection)==TRUE && length(lev()>2)){
        if(input$fs_options=="boruta"){
          if(all(FS$finalDecision=="Rejected")==TRUE){
            data=data
          }else{
            data=data[,c(cls,names(FS$finalDecision)[which(FS$finalDecision=="Confirmed")], names(FS$finalDecision)[which(FS$finalDecision=="Tentative")])]
            after_FS$predictors=c(names(FS$finalDecision)[which(FS$finalDecision=="Confirmed")], names(FS$finalDecision)[which(FS$finalDecision=="Tentative")])
          }
        }else if(input$fs_options=="LASSO"){
          X=data.matrix(data[isolate(input$other_variables)])
          y=data[,input$gr_variable]
          cv.lasso=glmnet::cv.glmnet(X, y, alpha = 1, family = "binomial")
          model=glmnet::glmnet(X, y, alpha = 1, family = "binomial",lambda = cv.lasso$lambda.min)
          coefs=data.matrix(model$beta)
          after_FS$predictors=rownames(coefs)[which(coefs[,1]!=0)]
        }else if(input$fs_options=="elasticnet"){
          X=data.matrix(data[isolate(input$other_variables)])
          y=data[,input$gr_variable]
          alpha_vec=seq(0.1,0.9,0.1)
          cv.enet=NULL
          for (i in 1:9) {
            cv.enet[[i]]=glmnet::cv.glmnet(X, y, alpha = alpha_vec[i], family = "binomial")
          }
          k=which.min(sapply(1:9, function(i) cv.enet[[i]]$lambda.min))
          model=glmnet::glmnet(X, y, alpha = alpha_vec[k], family = "binomial",lambda = cv.enet[[k]]$lambda.min)
          coefs=data.matrix(model$beta)
          after_FS$predictors=rownames(coefs)[which(coefs[,1]!=0)]
        }
      }else{
        data=data
      }
      
      if(disc_rule()==TRUE){
        disc=arulesCBA::discretizeDF.supervised(frml,data = data,method = isolate(input$disc_methods))
      }else{
        disc=data
      }
      
      model=reactive({
        try(getFromNamespace(isolate(input$assoc_class_algo),ns = "arulesCBA")(frml, data=disc,support = isolate(input$support), confidence = isolate(input$confidence)))
      })
      
      if(inherits(model(),c("try-error", "error"))==FALSE){
        
        pred=predict(model(),disc[-1])
        pred=factor(pred,levels = lev())
        cm=caret::confusionMatrix(pred,disc[,1],positive=isolate(input$positive_class))
        
        metrics_matrix=matrix(NA, nrow = 7, ncol = 2,dimnames = list(NULL,c("Metric","Value")))
        metrics_matrix[,1]=c("Accuracy","Balanced accuracy","Sensitivity","Specificity","Positive predictive value","Negative predictive value","F1-score")
        metrics_matrix[,2]=c(round(cm$overall[[1]],3),round(cm$byClass[[11]],3),round(cm$byClass[[1]],3),
                             round(cm$byClass[[2]],3),round(cm$byClass[[3]],3),round(cm$byClass[[4]],3),round(cm$byClass[[7]],3))
        
        cm$table=rbind(cm$table,colSums(cm$table))
        cm$table=cbind(cm$table,"Total"=rowSums(cm$table))
        cm_table=cbind("Prediction"=c(lev(),"Total"),as.data.frame.matrix(cm$table))
        
        output$confusion_matrix=DT::renderDataTable({
          
          sketch = htmltools::withTags(table(class = "display",thead(tr(th(rowspan = 2, "Prediction"),th(class = "dt-center",colspan = length(lev())+1, "Reference")),
                                                                     tr(lapply(c(lev(),"Total"), th)))))
          DT::datatable(cm_table,rownames = FALSE,container = sketch,
                    options = list(paging = FALSE,ordering=FALSE,
                                   searching=FALSE, autoWidth = TRUE,info=FALSE,scrollX=FALSE,
                                   columnDefs = list(list(className = "dt-center",targets = "_all")),
                                   initComplete = DT::JS("function(settings, json) {",
                                                     "$(this.api().table().header()).css({'background-color': '#18bc9c', 'color': '#fff'});",
                                                     "}")
                                   ),
                    caption = tags$caption(
                      style = 'caption-side: top; text-align: center;',
                      tags$h5(tags$div(style="font-family: Bahnschrift;",strong("Table 1: "),"Confusion matrix"))
                      )
          )
        })
        
        output$metrics=DT::renderDataTable({
          DT::datatable(metrics_matrix,rownames = FALSE,
                    options = list(paging = FALSE,ordering=TRUE,
                                   searching=FALSE, autoWidth = TRUE,info=FALSE,scrollX=FALSE,
                                   columnDefs = list(list(className = "dt-center",targets = "_all")),
                                   initComplete = DT::JS("function(settings, json) {",
                                                     "$(this.api().table().header()).css({'background-color': '#18bc9c', 'color': '#fff'});",
                                                     "}")),
                    caption = tags$caption(
                      style = 'caption-side: top; text-align: center;',
                      tags$h5(tags$div(style="font-family: Bahnschrift;",strong("Table 2: "),"Metrics related to the classification performance of the model"))
                      )
          )
        })
        
        output$selected_features=renderUI({
          if(isolate(input$fselection)==FALSE || all(FS$finalDecision=="Rejected")==TRUE){
            return(NULL)
          }else{
            if(length(FS$finalDecision)==length(colnames(data))-1){
              wellPanel(
                tags$h5(strong("Feature selection output"),align="justify"),br(),
                tags$h5("It was determined that all predictive variables contributed to the model.",align="justify")
              )
            }else{
              wellPanel(
                
                tags$h5(tags$div(style="font-family: Bahnschrift;",strong("Feature selection output"),align="justify")),
                tags$h5(tags$div(style="font-family: Bahnschrift;",strong("Selected features: "),
                                 HTML(paste(names(FS$finalDecision)[c(which(FS$finalDecision=="Confirmed"),which(FS$finalDecision=="Tentative"))],collapse = ", ")),align="justify"))
              )
            }
          }
        })
        
        output$DFRules=DT::renderDataTable({
          rules_table=arules::DATAFRAME(model()$rules)[,c(1:4,7)]
          colnames(rules_table)=c("Right hand side rules","Left hand side rules","Support","Confidence","Frequency")
          k=NROW(rules_table)
          for (i in 1:k) {
            rules_table[i,3]=round(rules_table[i,3],3)
            rules_table[i,4]=round(rules_table[i,4],3)
          }
          DT::datatable(rules_table[,1:5],rownames = FALSE,
                    options = list(paging = FALSE,ordering=TRUE,
                                   searching=FALSE, autoWidth = TRUE,info=FALSE,scrollX=FALSE,
                                   columnDefs = list(list(className = "dt-center",targets = "_all")),
                                   initComplete = DT::JS("function(settings, json) {",
                                                     "$(this.api().table().header()).css({'background-color': '#18bc9c', 'color': '#fff'});",
                                                     "}")),
                    caption = tags$caption(
                      style = 'caption-side: top; text-align: center;',
                      tags$h5(tags$div(style="font-family: Bahnschrift;",strong("Table 3: "),"Association rules used by the classification algorithm"))
                      )
          )
        })
        
        output$graphPlot=visNetwork::renderVisNetwork({
          arulesViz:::plot.rules(model()$rules, method = "graph",shading = "confidence", engine = "htmlwidget")
        })
        
        output$stats_header=renderUI({
          
          tags$h3(tags$div(style="font-family: Bahnschrift; background-color: #f39c12;",strong("###  Classification results  ###")))
          
        })
        
        output$seperator1=renderUI({
          
          tags$hr(style="border-color: black;")
          
        })
        
        output$associations_header=renderUI({
          
          tags$h3(tags$div(style="font-family: Bahnschrift;background-color: #f39c12;",strong("###  Association rules  ###")))
          
        })
        
        output$seperator2=renderUI({
          
          tags$hr(style="border-color: black;")
          
        })
        
        output$network_header=renderUI({
          
          tags$h3(tags$div(style="font-family: Bahnschrift;background-color: #f39c12;",strong("###  Network visualization of association rules  ###")))
          
        })
        
        output$download_button=renderUI({
          
          fluidPage(
            fluidRow(
              tags$hr(style="border-color: black;"),
              column(width = 12,align="center",
                     strong("Download report as:"),br(),
                     tags$div(style="display: inline-block;vertical-align:top; width: 75px;",
                              DB(
                                outputId = "dwnl_bttn1",style = "float",no_outline = T,size="lg",
                                label=HTML(paste(tags$img(src="word.svg", width=35, height=35),tags$h6(strong("Word"))))
                              )),
                     tags$div(style="display: inline-block;vertical-align:top; width: 75px;",
                              DB(
                                outputId = "dwnl_bttn2",style = "float",no_outline = T,size="lg",
                                label=HTML(paste(tags$img(src="pdf.svg", width=35, height=35),tags$h6(strong("PDF"))))
                              )),
                     tags$div(style="display: inline-block;vertical-align:top; width: 75px;",
                              DB(
                                outputId = "dwnl_bttn3",style = "float",no_outline = T,size="lg",
                                label=HTML(paste(tags$img(src="html.svg", width=35, height=35),tags$h6(strong("HTML"))))
                              ))
                     )
            )
          )

        })
        
        output$refresh_button=renderUI({
          
          shinyWidgets::actionBttn("refresh",label = "Refresh page",icon = icon("refresh"),style = "jelly",color = "primary")
          
        })
        
        observeEvent(input$refresh, {
          shinyjs::js$refresh()
        })
        
      }else{
        
        shinyalert::shinyalert(title = "No association rule found!", text = paste("Possible solutions:", "1. Change the variable selection option.", "2. Change the classification algorithm and/or support-confidence values.",sep="\n"), 
                               type = "error",confirmButtonText = "OK",closeOnEsc = TRUE,animation = "slide-from-top",confirmButtonCol = "#F27474")
        
        output$confusion_matrix=DT::renderDataTable({NULL})
        output$selected_features=renderUI({NULL})
        output$metrics=DT::renderDataTable({NULL})
        output$DFRules=DT::renderDataTable({NULL})
        output$graphPlot=visNetwork::renderVisNetwork({NULL})
        output$stats_header=renderUI({NULL})
        output$seperator1=renderUI({NULL})
        output$seperator2=renderUI({NULL})
        output$associations_header=renderUI({NULL})
        output$network_header=renderUI({NULL})
        output$download_button=renderUI({NULL})
        
      }
      
    }

    ########### Download report #############
    
    ext=c(".docx",".pdf",".html")
    funs=c("word_document","pdf_document","html_document")
    
    lapply(1:3, function(i){
      
      output[[paste0("dwnl_bttn",i)]]=downloadHandler(
        filename = function() {
          paste(paste("associative_classification", format.Date(Sys.Date(),"%m/%d/%Y")), sep = ".",ext[i]
          )
        },
        content = function(file) {
          src = normalizePath("report.Rmd")
          owd = setwd(tempdir())
          on.exit(setwd(owd))
          file.copy(src, "report.Rmd")
          out = rmarkdown::render("report.Rmd",encoding="UTF-8",getFromNamespace(funs[i],ns = "rmarkdown")()
          )
          file.rename(out, file)
        }
      )
      
    })
    
  })
  
  parameters=reactive({
    
    mat=matrix(NA,7,2)
    mat[,1]=c("Response/Output variable","Positive class","Subgroup","Support","Confidence","Feature selection","Classification algorithm")
    
    params=c(input$gr_variable, 
             NA, 
             NA,
             input$support, 
             input$confidence,
             NA,
             input$assoc_class_algo)
    
    mat[,2]=params
    mat[2,2]=ifelse(is.null(input$positive_class)==T,"No",input$positive_class)
    mat[3,2]=ifelse(input$sub_group=="None",input$sub_group,input$sub_category)
    mat[6,2]=ifelse(input$fselection==TRUE,"Yes","No")

    
    colnames(mat)=c("Parameter","Value")
    mat

  })
  
  used_predictors=reactive({
    
    if(input$fselection==FALSE){
      paste(input$other_variables,sep = ", ")
    }else{
      paste(after_FS$predictors,sep = ", ")
    }
    
  })

  
  ############# deneme ##############
  #output$deneme=renderPrint({})
  
}

shinyApp(ui, server)