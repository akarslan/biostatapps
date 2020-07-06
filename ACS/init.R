###########

vartype=function(x){
  x=na.omit(x)
  if((is.factor(x)==T || is.character(x))==T || (all(x%%1==0)==T && (min(x) >= 0) && (max(x) <= 4))){
    "Nominal"
  }else if((all(x%%1==0)==T && (min(x) >= 5) && (max(x) <= 10))){
    "Ordinal"
  }else if(all(floor(x)==x)==F){
    "Continuous"
  }else if(all(floor(x)==x)==T){
    "Discrete"
  }
}

###########

DB=function(outputId, label = "Download", style = "unite", color = "default", 
            size = "md", block = FALSE, no_outline = TRUE){
  btn=shinyWidgets::actionBttn(inputId = paste0(outputId, "_bttn"), 
                               label = tags$a(id = outputId, 
                                              class = "shiny-download-link", href = "", target = "_blank", 
                                              download = NA, label), color = color, style = style, 
                               size = size, block = block, no_outline = no_outline)
  btn
  
}

###########

#df=data.frame(
#  vals = c("Nitel Sınıflayıcı","Nitel Sıralayıcı","Sürekli Sayısal","Kesikli Sayısal")
#)
#
#df$imgs = c(
#  sprintf("<img src='qualitative.svg' width=15px><div class='jhr'>%s</div></img>", df$vals[1]),
#  sprintf("<img src='ordered.svg' width=15px><div class='jhr'>%s</div></img>", df$vals[2]),
#  sprintf("<img src='numeric.svg' width=15px><div class='jhr'>%s</div></img>", df$vals[3]),
#  sprintf("<img src='discrete.svg' width=15px><div class='jhr'>%s</div></img>", df$vals[4])
#)

shinyInput1 = function(FUN, data, len, id, ...) { 
  inputs = character(len) 
  types=unlist(lapply(data,vartype))
  for (i in seq_len(len)) { 
    inputs[i] = as.character(FUN(paste0(id, i), label = "", choices=c("Nominal",
                                                                      "Ordinal",
                                                                      "Discrete",
                                                                      "Continuous"),
                                 selected=types[i],width="150px"
                                 ))
  }
  inputs
}

###########

shinyInput2 = function(FUN, len, id, ...) { 
  inputs = character(len) 
  for (i in seq_len(len)) { 
    inputs[i] = as.character(FUN(paste0(id, i), label = "", choices=c("Predictor","Response/Output"),
                                 selected="Predictor",width="150px"
                                 ))
  }
  inputs
}

###########