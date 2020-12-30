library(shiny)
ui=fillPage(
  tagList(tags$style(HTML("body{font-family:Bahnschrift;font-style: normal;}")),
          fixedPanel(draggable = F,top = "17px", right = "15px",style = "z-index: 1000000;",
                     shinyWidgets::radioGroupButtons("lang", NULL,selected = "<img src=\"gb.svg\" width=\"20\" height=\"15\"/> English",
                                                     choices = mapply(c("Türkçe", "English"),
                                                                      c("tr.svg",
                                                                        "gb.svg"
                                                                      ), FUN = function(country, flagUrl) {
                                                                        HTML(paste(
                                                                          tags$img(src=flagUrl, width=20, height=15),
                                                                          country
                                                                        ))
                                                                      }, SIMPLIFY = FALSE, USE.NAMES = FALSE),size = "s",status = "info"
                     )
          ),
          navbarPage(header=tags$head(tags$style(type="text/css", "body {padding-top: 70px;}",".tab-content { overflow: visible; }"),
                                      tags$style(HTML(".navbar-default .navbar-brand {color: #f39c12; font-size: 20px;font-family: Bahnschrift;font-weight: bold;}
                                                       .navbar .navbar-nav {font-size: 17px; font-family: Bahnschrift;}"
                                      )),tagList(shinyWidgets::useShinydashboard())),
                     title=tags$div(style="display: inline-block;vertical-align:top;",uiOutput("out1")),
                     windowTitle = uiOutput("out1"),
                     inverse = FALSE,position = "fixed-top",
                     theme = shinythemes::shinytheme("flatly"),id = "maintab",
                     tabPanel(uiOutput("out2"),value = "calculator",
                              fluidRow(
                                column(4,align="justify",
                                       shinyLP::panel_div("danger", div(style="font-family: Bahnschrift;",strong(uiOutput("out3"))),content = 
                                                            tags$div(style = "height:592px;",#"overflow-y: scroll;height:750px;",
                                                                     fluidRow(
                                                                       uiOutput("out4")
                                                                     )
                                                            )
                                       )
                                ),
                                column(4,
                                       column(12,
                                              shinyLP::panel_div("success", div(style="font-family: Bahnschrift;",strong(uiOutput("out5"))),content = 
                                                                   tags$div(style = "height:592px;",#"overflow-y: scroll;height:750px;",
                                                                            fluidRow(
                                                                              column(width = 12,align="justify",tags$div(style="border-bottom: groove;",tags$style(HTML("#age{height: 35px;border-color:#18bc9c;}")),
                                                                                                                         tags$div(style="display: inline-block;vertical-align:top;height:35px;margin-top:7.5px;",strong(uiOutput("out6"))),
                                                                                                                         tags$div(style="display: inline-block;vertical-align:top;float:right;margin-right:63px;",numericInput("age",label = NULL,value = NA,min = 0,width = "100px")))),
                                                                              column(width = 12,align="justify",tags$div(style="border-bottom: groove;",tags$style(HTML("#plt{height: 35px;border-color:#18bc9c;}")),
                                                                                                                         tags$div(style="display: inline-block;vertical-align:top;height:35px;margin-top:7.5px;",strong(HTML("Platelet (10<sup>3</sup>/</sup>\u03BCL):"))),
                                                                                                                         tags$div(style="display: inline-block;vertical-align:top;float:right;margin-right:63px;",numericInput("plt",label = NULL,value = NA,min = 0,width = "100px")))),
                                                                              column(width = 12,align="justify",tags$div(style="border-bottom: groove;",tags$style(HTML("#bki{height: 35px;border-color:#18bc9c;}")),
                                                                                                                         tags$div(style="display: inline-block;vertical-align:top;height:35px;margin-top:7.5px;",uiOutput("out7")),
                                                                                                                         tags$div(style="display: inline-block;vertical-align:top;float:right;margin-right:10px;",shinyWidgets::actionBttn("bki_calc",style = "simple",color = "danger",label = NULL,icon = icon("calculator"))),
                                                                                                                         tags$div(style="display: inline-block;vertical-align:top;float:right;margin-right:12px;",numericInput("bki",label = NULL,value = NA,min = 0,width = "100px"))
                                                                              )),
                                                                              column(width = 12,align="justify",tags$div(style="border-bottom: groove;",
                                                                                                                         tags$div(style="display: inline-block;vertical-align:top;height:35px;margin-top:7.5px;",strong(uiOutput("out8"))),
                                                                                                                         tags$div(style="display: inline-block;vertical-align:top;float:right;margin-right:50px;",uiOutput("out8_2")))),
                                                                              column(width = 12,align="justify",tags$div(style="border-bottom: groove;",
                                                                                                                         tags$div(style="display: inline-block;vertical-align:top;height:35px;margin-top:7.5px;",strong(uiOutput("out9"))),
                                                                                                                         tags$div(style="display: inline-block;vertical-align:top;float:right;margin-right:50px;",uiOutput("out9_2")))),
                                                                              column(12,align="center",br(),br(),br(),br(),br(),br(),uiOutput("out10"))
                                                                            )
                                                                            
                                                                   )
                                              )
                                       )
                                       
                                ),
                                column(4,shinyLP::panel_div(class_type = "warning", div(style="font-family: Bahnschrift;",strong(uiOutput("out11"))),
                                                            content = 
                                                              tags$div(style = "height:592px;",#"overflow-y: scroll;height:750px;",
                                                                       fluidRow(
                                                                         column(width = 12,align="center",br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),
                                                                                uiOutput(outputId = "risk_ui")
                                                                         )
                                                                       )
                                                              )
                                )
                                )
                              )
                     )#,tabPanel("deneme",
                      #          
                      #          verbatimTextOutput("deneme")
                      #          )
          )
  )
)
  
server=function(input, output, session){
  
  ##### translator  ##### 
  
  observe({

    output$out1=renderUI({ifelse(grepl("Türkçe",input$lang),"POAFRiskScore v.1.0","POAFRiskScore v.1.0")})
    output$out2=renderUI({ifelse(grepl("Türkçe",input$lang),"Anasayfa","Home")})
    output$out3=renderUI({ifelse(grepl("Türkçe",input$lang),"Açıklama","Introduction")})
    output$out4=renderUI({
      
      
      if(grepl("Türkçe",input$lang)){
        column(width = 12,align="justify",
               tags$div(style="font-family:Bahnschrift;font-style: normal;font-weight: bold;font-size: 20px;","Hakkında"),
               "Bu yazılım, Koroner Arter Bypass operasyonu geçiren hastalarda postoperatif Atriyal Fibrilasyon (POAF) gelişim riskini hesaplamaktadır. Bu projede;
                                                                               koroner arter bypass operasyonu geçiren hastalarda POAF ile ilişkili olan önemli faktörler belirlenerek, 
                                                                               çok sayıda lojistik regresyon tahmin modeli oluşturulmuş ve optimal modelin katsayıları kullanılarak risk tahmin aracı geliştirilmiştir.",tags$br(),tags$br(),
               tags$div(style="font-family:Bahnschrift;font-style: normal;font-weight: bold;font-size: 20px;","Uyarı"),
               tags$div(style="font-family:Bahnschrift;font-style: normal;color: #e74c3c;font-weight: bold;font-size: 15px;","Bu yazılımın sonuçlarının, uygun tıbbi ve/veya klinik 
                        eğitimi olmayanlar tarafından yorumlanması, ilgili bir sağlık profesyonelinin talebi veya bu uzmana danışılması dışında tavsiye edilmez."),tags$br(),
               tags$div(style="font-family:Bahnschrift;font-style: normal;font-weight: bold;font-size: 20px;","Teşekkür"),
               "Bu yazılım, TCD-2017-761 nolu araştırma projesi kapsamında, İnönü Üniversitesi Bilimsel Araştırma Projeleri Koordinasyon Birimi tarafından desteklenmiştir. Bu destek için 
                                                                               İnönü Üniversitesi Bilimsel Araştırma Projeleri Koordinasyon Birimi'ne proje ekibi olarak teşekkür ederiz."
               )
      }else{
        column(width = 12,align="justify",
               tags$div(style="font-family:Bahnschrift;font-style: normal;font-weight: bold;font-size: 20px;","About"),
               "This software calculates the risk of postoperative Atrial Fibrillation (POAF) development in patients who 
               have undergone Coronary Artery Bypass operation. In this project, many logistic regression estimation models were constructed by the 
               important risk factors associated with POAF in patients undergoing coronary artery bypass operation, and current risk prediction tool 
               was developed using the coefficients of the optimal model.",tags$br(),tags$br(),
               tags$div(style="font-family:Bahnschrift;font-style: normal;font-weight: bold;font-size: 20px;","Warning"),
               tags$div(style="font-family:Bahnschrift;font-style: normal;color: #e74c3c;font-weight: bold;font-size: 15px;","The interpretation of 
                        the results of this software by those without appropriate medical and / or clinical training is not recommended, except at 
                        the request of, or consultation with, a relevant healthcare professional."),tags$br(),
               tags$div(style="font-family:Bahnschrift;font-style: normal;font-weight: bold;font-size: 20px;","Acknowledgement"),
               "This software is part of the research project numbered TCD-2017-761, Inonu University Scientific Research Projects Coordination Unit
               supported by. As project team, we would like to thank Inonu University Scientific Research Projects Coordination Unit for this support."
               )
      }
    })
    
    output$out5=renderUI({ifelse(grepl("Türkçe",input$lang),"Operasyon öncesi (preoperatif) değişkenler","Preoperative variables")})
    output$out6=renderUI({ifelse(grepl("Türkçe",input$lang),"Yaş (Yıl):","Age (Year):")})
    output$out7=renderUI({strong(HTML(paste0(ifelse(grepl("Türkçe",input$lang),"Beden kitle indeksi ","Body mass index "),"(kg/m",tags$sup(2),"):")))})
    output$out8=renderUI({ifelse(grepl("Türkçe",input$lang),"Preoperatif karotis arter stenozu varlığı:","Preoperative carotid artery stenosis:")})
    
    output$out8_2=renderUI({
      if(grepl("Türkçe",input$lang)){
        shinyWidgets::radioGroupButtons(inputId = "pkasv",label = NULL,choices = c("Yok"="no", "Var"="yes"),
                                        size = "sm",status = "success",checkIcon = list(yes = tags$i(class = "fa fa-check-square"),no = tags$i(class = "fa fa-square-o")))
      }else{
        shinyWidgets::radioGroupButtons(inputId = "pkasv",label = NULL,choices = c("No"="no", "Yes"="yes"),
                                        size = "sm",status = "success",checkIcon = list(yes = tags$i(class = "fa fa-check-square"),no = tags$i(class = "fa fa-square-o")))
      }
    })
    
    output$out9=renderUI({ifelse(grepl("Türkçe",input$lang),"Kronik obstruktif akciğer hastalığı (KOAH) varlığı:","Chronic obstructive pulmonary disease (COPD):")})
    
    output$out9_2=renderUI({
      if(grepl("Türkçe",input$lang)){
        shinyWidgets::radioGroupButtons(inputId = "koah",label = NULL,choices = c("Yok"="no", "Var"="yes"),
                                        size = "sm",status = "success",checkIcon = list(yes = tags$i(class = "fa fa-check-square"),no = tags$i(class = "fa fa-square-o")))
      }else{
        shinyWidgets::radioGroupButtons(inputId = "koah",label = NULL,choices = c("No"="no", "Yes"="yes"),
                                        size = "sm",status = "success",checkIcon = list(yes = tags$i(class = "fa fa-check-square"),no = tags$i(class = "fa fa-square-o")))
      }
    })
    
    output$out10=renderUI({shinyWidgets::actionBttn(inputId = "calculate",label = ifelse(grepl("Türkçe",input$lang),"Hesapla","Calculate"),style = "simple", color = "danger",icon = icon("calculator"))})
    output$out11=renderUI({ifelse(grepl("Türkçe",input$lang),"Tahmin sonucu","Prediction result")})
    output$out12=renderUI({shinyWidgets::actionBttn("transfer",ifelse(grepl("Türkçe",input$lang),"Hesapla ve ana menüye aktar","Calculate and transfer to main menu"),color = "danger",style = "simple",block = T)})
    output$out13=renderUI({tags$div(style="font-family:Bahnschrift;",ifelse(grepl("Türkçe",input$lang),"Beden kitle indeksi hesaplayıcı","Body mass index calculator"))})
    output$out14=renderUI({ifelse(grepl("Türkçe",input$lang),"Boyunuz (metre):","Height (meter):")})
    output$out15=renderUI({ifelse(grepl("Türkçe",input$lang),"Kilonuz (kg):","Weight (kg):")})
    output$out16=renderUI({shinyWidgets::actionBttn("modal_ok",ifelse(grepl("Türkçe",input$lang),"Tamam","OK"),color = "danger",style = "simple",block = T)})
    output$out17=renderUI({tags$div(style="font-family:Bahnschrift;",ifelse(grepl("Türkçe",input$lang),"Hata!","Error!"))})
    output$out18=renderUI({tags$div(style="font-family:Bahnschrift;",ifelse(grepl("Türkçe",input$lang),"Lütfen tüm değerleri giriniz.","Please enter all the values."))})
    output$out19=renderUI({tags$div(style="font-family:Bahnschrift;",ifelse(grepl("Türkçe",input$lang),"Lütfen pozitif değerler giriniz.","Please enter positive values."))})
    output$out20=renderUI({ifelse(grepl("Türkçe",input$lang),"Operasyon sonrası atriyal fibrilasyon gelişim riski","Postoperative atrial fibrillation development risk")})
    
  })
  
  observeEvent(input$bki_calc,{
    
    showModal(modalDialog(size = "m",class="row",# footer çizgisini düzenliyor
                          footer = tags$div(uiOutput("out12"),`data-dismiss` = "modal"),easyClose = T,
                          title = strong(uiOutput("out13")),
                          fluidPage(
                            fluidRow(
                              column(width = 12,align="justify",tags$div(style="border-bottom: groove;",tags$style(HTML("#height{height: 35px;border-color:#18bc9c;}")),
                                                                         tags$div(style="display: inline-block;vertical-align:top;height:35px;margin-top:7.5px;",strong(uiOutput("out14"))),
                                                                         tags$div(style="display: inline-block;vertical-align:top;float:right;margin-right:63px;",numericInput("height",label = NULL,value = NA,min = 0,width = "100px")))),
                              column(width = 12,align="justify",tags$div(style="border-bottom: groove;",tags$style(HTML("#weight{height: 35px;border-color:#18bc9c;}")),
                                                                         tags$div(style="display: inline-block;vertical-align:top;height:35px;margin-top:7.5px;",strong(uiOutput("out15"))),
                                                                         tags$div(style="display: inline-block;vertical-align:top;float:right;margin-right:63px;",numericInput("weight",label = NULL,value = NA,min = 0,width = "100px"))))
                              )
                            )
                          )
              )
    })
  
  observeEvent(input$transfer,{
    
    updateNumericInput(session = session,inputId = "bki",label = NULL,value = round((input$weight)/((input$height)^2),digits = 3))
    
  })
  
  risk_perc=reactiveValues(val=NULL)

  observeEvent(input$calculate,{
    
    if(any(is.na(c(input$age,input$plt,input$bki)))==T){
      
      showModal(modalDialog(size = "s",class="row",# footer çizgisini düzenliyor
                            footer = tags$div(uiOutput("out16"),`data-dismiss` = "modal"),easyClose = F,
                            title = tags$div(style="font-family:Bahnschrift;",uiOutput("out17")),
                            column(width = 12,align="justify",uiOutput("out18"))
                            )
                )

    }else if(any(c(input$age,input$plt,input$bki)<0)){
      
      showModal(modalDialog(size = "s",class="row",# footer çizgisini düzenliyor
                            footer = tags$div(uiOutput("out16"),`data-dismiss` = "modal"),easyClose = F,
                            title = uiOutput("out17"),
                            column(width = 12,align="justify",uiOutput("out19"))
                            )
                )
      
      
    }else{
      
      equ.sum=-7.116+(0.878*ifelse(input$pkasv=="yes",1,0))+(0.427*ifelse(input$koah=="yes",1,0))+(0.044*input$age)-(0.004*input$plt)+(0.091*input$bki)
      risk=1/(1+exp(-equ.sum))
      risk_perc$val=round(risk*100,2)
      
    }
    
    
  })
  
  
  output$risk_ui=renderUI({
    
    if(!is.null(risk_perc$val)){
      shinydashboard::valueBox(width = 12,icon = icon("file-medical-alt"),value = ifelse(grepl("Türkçe",input$lang),paste0("%",risk_perc$val),paste0(risk_perc$val,"%")),uiOutput("out20"),color = "purple")
    }else{
      return(NULL)
    }
    
  })
  
  
  #output$deneme=renderPrint({})

}

shinyApp(ui, server)