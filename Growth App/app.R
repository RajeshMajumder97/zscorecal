library(shiny)
library(shinydashboard)
library(plotly)
library(shinythemes)
library(tidyverse)
#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


data= read.csv("All Z score.csv")

zscoretab= function(Data,age=NULL,sex=NULL,height=NULL,weight=NULL){
  
  suppressWarnings({
    require(ggplot2)
    require(plotly)
    require(reshape2)
    
    cc= function(x,m,s,l){pnorm(((x/m)^l-1)/(l*s))}
    
    agesex= paste0(round(age,digits = 0),"_",ifelse(sex=="Male",1,2))
    
    heightsex= paste0(round(height,digits = 1),"_",ifelse(sex=="Male",1,2))
    
    bmical= weight/(height/100)^2
    
    dat= subset(Data,Metric=="haz")
    hazl= ifelse(length(dat$l[which(dat$Match.ID==agesex)])==0,NA,dat$l[which(dat$Match.ID==agesex)])
    hazm= ifelse(length(dat$m[which(dat$Match.ID==agesex)])==0,NA,dat$m[which(dat$Match.ID==agesex)])
    hazs= ifelse(length(dat$s[which(dat$Match.ID==agesex)])==0,NA,dat$s[which(dat$Match.ID==agesex)])
    haz= ifelse(length(((height/hazm)^hazl-1)/(hazl*hazs))==0,NA,((height/hazm)^hazl-1)/(hazl*hazs))
    haz_risk= ifelse(length((1-pnorm(haz))*100)==0,NA,(1-pnorm(haz))*100)
    haz_Percentile= ifelse(length(cc(x=height,m=hazm,s = hazs,l = hazl)*100)==0,NA,cc(x=height,m=hazm,s = hazs,l = hazl)*100)
    
    ##--- Plotting
    
    if(sex=="Male"){
      dat= subset(dat,sex=="Male")
    }else dat= subset(dat,dat$sex=="Female")
    
    if(age<60){
      xlim_val= c(0,60);ylim_val= c(50,125)
    }else{
      xlim_val= c(60,240);ylim_val= c(95,200)
    }
    
    dat= dat[,-c(1,3,4:7)]
    dat=melt(dat,id="age_or_height")
    dat$variable= factor(dat$variable,levels = c("X2.5th","X5th","X25th","X50th","X75th","X95th","X97.5th"),
                         labels = c("2.5th","5th","25th","50th","75th","95th","97.5th"))
    names(dat)=c("Age","Centile","value")
    
    p1=ggplot(data = dat,aes(x=Age,
                             y=value,
                             color=Centile))+geom_line(size=0.3)+
      theme(legend.title=element_blank(),panel.background = element_rect(fill='transparent'),
            plot.background = element_rect(fill='transparent', color=NA),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            legend.background = element_rect(fill='transparent'),
            legend.box.background = element_rect(fill='transparent'),
            axis.line = element_line(colour = "white"),axis.text =element_text(colour = "white"),
            axis.title.x=element_text(colour = "white"),axis.title.y=element_text(colour = "white"),
            plot.title = element_text(color = "white"),legend.text = element_text(colour = "white"))+
      scale_linetype_discrete(name="GGG")+xlab("Age (Month)")+ylab("Height (cm)")+ylim(ylim_val)+xlim(xlim_val)+
      geom_point(aes(x=age,y=height),color="red",size=0.5)+ggtitle("Height for Age")
    
    #panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
    #panel.background = element_blank(),
    
    
    
    
    
    
    
    dat= subset(Data,Metric=="waz")
    wazl= ifelse(length(dat$l[which(dat$Match.ID==agesex)])==0,NA,dat$l[which(dat$Match.ID==agesex)])
    wazm= ifelse(length(dat$m[which(dat$Match.ID==agesex)])==0,NA,dat$m[which(dat$Match.ID==agesex)])
    wazs= ifelse(length(dat$s[which(dat$Match.ID==agesex)])==0,NA,dat$s[which(dat$Match.ID==agesex)])
    waz=  ifelse(length(((weight/wazm)^wazl-1)/(wazl*wazs))==0,NA,((weight/wazm)^wazl-1)/(wazl*wazs))   #((weight/wazm)^wazl-1)/(wazl*wazs)
    waz_risk= ifelse(length((1-pnorm(waz))*100)==0,NA,(1-pnorm(waz))*100)   #(1-pnorm(waz))*100
    waz_Percentile= ifelse(length(cc(x=weight,m=wazm,s = wazs,l = wazl)*100)==0,NA,cc(x=weight,m=wazm,s = wazs,l = wazl)*100)   #cc(x=weight,m=wazm,s = wazs,l = wazl)*100
    
    ##--- Plotting
    
    if(sex=="Male"){
      dat= subset(dat,sex=="Male")
    }else dat= subset(dat,dat$sex=="Female")
    
    if(age<60){
      xlim_val= c(0,60);ylim_val= c(4,25)
    }else{
      xlim_val= c(60,132);ylim_val= c(12,65)
    }
    
    dat= dat[,-c(1,3,4:7)]
    dat=melt(dat,id="age_or_height")
    dat$variable= factor(dat$variable,levels = c("X2.5th","X5th","X25th","X50th","X75th","X95th","X97.5th"),
                         labels = c("2.5th","5th","25th","50th","75th","95th","97.5th"))
    names(dat)=c("Age","Centile","value")
    
    p2=ggplot(data = dat,aes(x=Age,
                             y=value,
                             color=Centile))+geom_line(size=0.3)+
      theme(legend.title=element_blank(),panel.background = element_rect(fill='transparent'),
            plot.background = element_rect(fill='transparent', color=NA),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            legend.background = element_rect(fill='transparent'),
            legend.box.background = element_rect(fill='transparent'),
            axis.line = element_line(colour = "white"),axis.text =element_text(colour = "white"),
            axis.title.x=element_text(colour = "white"),axis.title.y=element_text(colour = "white"),
            plot.title = element_text(color = "white"),legend.text = element_text(colour = "white"))+
      scale_linetype_discrete(name="GGG")+xlab("Age (Month)")+ylab("Weight (kg)")+ylim(ylim_val)+xlim(xlim_val)+
      geom_point(aes(x=age,y=weight),color="red",size=0.5)+ggtitle("Weight for Age")
    
    
    
    
    
    dat= subset(Data,Metric=="whz")
    whzl= ifelse(length(dat$l[which(dat$Match.ID==heightsex)])==0,NA,dat$l[which(dat$Match.ID==heightsex)])
    whzm= ifelse(length(dat$m[which(dat$Match.ID==heightsex)])==0,NA,dat$m[which(dat$Match.ID==heightsex)])
    whzs= ifelse(length(dat$s[which(dat$Match.ID==heightsex)])==0,NA,dat$s[which(dat$Match.ID==heightsex)])
    whz=  ifelse(length(((weight/whzm)^whzl-1)/(whzl*whzs))==0,NA,((weight/whzm)^whzl-1)/(whzl*whzs))  #((weight/whzm)^whzl-1)/(whzl*whzs)
    whz_risk= ifelse(length((1-pnorm(whz))*100)==0,NA,(1-pnorm(whz))*100)  #(1-pnorm(whz))*100
    whz_Percentile= ifelse(length(cc(x=weight,m=whzm,s = whzs,l = whzl)*100)==0,NA,cc(x=weight,m=whzm,s = whzs,l = whzl)*100)    #cc(x=weight,m=whzm,s = whzs,l = whzl)*100
    
    ##--- Plotting
    
    if(sex=="Male"){
      dat= subset(dat,sex=="Male")
    }else dat= subset(dat,dat$sex=="Female")
    
    dat= dat[,-c(1,3,4:7)]
    dat=melt(dat,id="age_or_height")
    dat$variable= factor(dat$variable,levels = c("X2.5th","X5th","X25th","X50th","X75th","X95th","X97.5th"),
                         labels = c("2.5th","5th","25th","50th","75th","95th","97.5th"))
    names(dat)=c("Height","Centile","value")
    
    p3=ggplot(data = dat,aes(x=Height,
                             y=value,
                             color=Centile))+geom_line(size=0.3)+
      theme(legend.title=element_blank(),panel.background = element_rect(fill='transparent'),
            plot.background = element_rect(fill='transparent', color=NA),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            legend.background = element_rect(fill='transparent'),
            legend.box.background = element_rect(fill='transparent'),
            axis.line = element_line(colour = "white"),axis.text =element_text(colour = "white"),
            axis.title.x=element_text(colour = "white"),axis.title.y=element_text(colour = "white"),
            plot.title = element_text(color = "white"),legend.text = element_text(colour = "white"))+
      scale_linetype_discrete(name="GGG")+xlab("Height (cm)")+ylab("Weight (kg)")+ylim(c(2,30))+xlim(c(45,120))+
      geom_point(aes(x=height,y=weight),color="red",size=0.5)+ggtitle("Weight for height")
    
    
    
    
    
    dat= subset(Data,Metric=="baz")
    bazl= ifelse(length(dat$l[which(dat$Match.ID==agesex)])==0,NA,dat$l[which(dat$Match.ID==agesex)])
    bazm= ifelse(length(dat$m[which(dat$Match.ID==agesex)])==0,NA,dat$m[which(dat$Match.ID==agesex)])
    bazs= ifelse(length(dat$s[which(dat$Match.ID==agesex)])==0,NA,dat$s[which(dat$Match.ID==agesex)])
    baz=  ifelse(length(((bmical/bazm)^bazl-1)/(bazl*bazs))==0,NA,((bmical/bazm)^bazl-1)/(bazl*bazs))
    baz_risk= ifelse(length((1-pnorm(baz))*100)==0,NA,(1-pnorm(baz))*100)  #(1-pnorm(baz))*100
    baz_Percentile= ifelse(length(cc(x=bmical,m=bazm,s = bazs,l = bazl)*100)==0,NA,cc(x=bmical,m=bazm,s = bazs,l = bazl)*100)
    
    ##--- Plotting
    
    if(sex=="Male"){
      dat= subset(dat,sex=="Male")
    }else dat= subset(dat,dat$sex=="Female")
    
    dat= dat[,-c(1,3,4:7)]
    dat=melt(dat,id="age_or_height")
    dat$variable= factor(dat$variable,levels = c("X2.5th","X5th","X25th","X50th","X75th","X95th","X97.5th"),
                         labels = c("2.5th","5th","25th","50th","75th","95th","97.5th"))
    names(dat)=c("Age","Centile","value")
    
    p4=ggplot(data = dat,aes(x=Age,
                             y=value,
                             color=Centile))+geom_line(size=0.3)+
      theme(legend.title=element_blank(),panel.background = element_rect(fill='transparent'),
            plot.background = element_rect(fill='transparent', color=NA),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            legend.background = element_rect(fill='transparent'),
            legend.box.background = element_rect(fill='transparent'),
            axis.line = element_line(colour = "white"),axis.text =element_text(colour = "white"),
            axis.title.x=element_text(colour = "white"),axis.title.y=element_text(colour = "white"),
            plot.title = element_text(color = "white"),legend.text = element_text(colour = "white"))+
      scale_linetype_discrete(name="GGG")+xlab("Age (Month)")+ylab("BMI (kg/m^2)")+ylim(c(12,30))+xlim(c(60,240))+
      geom_point(aes(x=age,y=bmical),color="red",size=0.5)+ggtitle("BMI for Age")
    
    
    tab1= data.frame(`Metric`=c("Height for age","Weight for age","Weight for height","BMI for age"),
                     `Z score`=c(sprintf("%0.3f",haz) ,sprintf("%0.3f",waz),sprintf("%0.3f",whz),sprintf("%0.3f",baz)),
                     `Risk of growth faltaring(%)`=c(sprintf("%0.3f",haz_risk),sprintf("%0.3f",waz_risk),sprintf("%0.3f",whz_risk),sprintf("%0.3f",baz_risk)),
                     `Percentile`=c(sprintf("%0.3f",haz_Percentile),sprintf("%0.3f",waz_Percentile),sprintf("%0.3f",whz_Percentile),sprintf("%0.3f",baz_Percentile)))
    names(tab1)= c("Metric","Z Score","Risk of growth faltaring (%)","Percentile")
    
    tab2= data.frame(Metric= c("Height for age","Weight for age","Weight for height","BMI for age"),
                     L= c(sprintf("%0.3f",hazl),sprintf("%0.3f",wazl),sprintf("%0.3f",whzl),sprintf("%0.3f",bazl)),
                     M= c(sprintf("%0.3f",hazm),sprintf("%0.3f",wazm),sprintf("%0.3f",whzm),sprintf("%0.3f",bazm)),
                     S= c(sprintf("%0.3f",hazs),sprintf("%0.3f",wazs),sprintf("%0.3f",whzs),sprintf("%0.3f",bazs)))
    
    pp=cowplot::plot_grid(p1,p2,p3,p4,nrow = 2)
    
    
    
    return(list("Zscore Table"=tab1,
                "LMS Table"=tab2,
                "hazp"=suppressMessages(p1),
                "wazp"=suppressMessages(p2),
                "whzp"=p3,
                "bazp"=p4,
                "allplot"=pp))
  })
}


zscoreval= function(Data,age=NULL,sex=NULL,height=NULL,weight=NULL){
  suppressWarnings({
    cc= function(x,m,s,l){pnorm(((x/m)^l-1)/(l*s))}
    agesex= paste0(round(age,digits = 0),"_",ifelse(tolower(sex)=="male",1,2))
    heightsex= paste0(round(height,digits = 1),"_",ifelse(tolower(sex)=="Male",1,2))
    bmical= round(weight/(height/100)^2,3)
    dat= subset(Data,Metric=="haz")
    hazl= ifelse(length(dat$l[which(dat$Match.ID==agesex)])==0,NA,dat$l[which(dat$Match.ID==agesex)])
    hazm= ifelse(length(dat$m[which(dat$Match.ID==agesex)])==0,NA,dat$m[which(dat$Match.ID==agesex)])
    hazs= ifelse(length(dat$s[which(dat$Match.ID==agesex)])==0,NA,dat$s[which(dat$Match.ID==agesex)])
    haz= ifelse(length(((height/hazm)^hazl-1)/(hazl*hazs))==0,NA,((height/hazm)^hazl-1)/(hazl*hazs))
    haz_risk= ifelse(length((1-pnorm(haz))*100)==0,NA,(1-pnorm(haz))*100)
    haz_Percentile= ifelse(length(cc(x=height,m=hazm,s = hazs,l = hazl)*100)==0,NA,cc(x=height,m=hazm,s = hazs,l = hazl)*100)
    dat= subset(Data,Metric=="waz")
    wazl= ifelse(length(dat$l[which(dat$Match.ID==agesex)])==0,NA,dat$l[which(dat$Match.ID==agesex)])
    wazm= ifelse(length(dat$m[which(dat$Match.ID==agesex)])==0,NA,dat$m[which(dat$Match.ID==agesex)])
    wazs= ifelse(length(dat$s[which(dat$Match.ID==agesex)])==0,NA,dat$s[which(dat$Match.ID==agesex)])
    waz=  ifelse(length(((weight/wazm)^wazl-1)/(wazl*wazs))==0,NA,((weight/wazm)^wazl-1)/(wazl*wazs))
    waz_risk= ifelse(length((1-pnorm(waz))*100)==0,NA,(1-pnorm(waz))*100)
    waz_Percentile= ifelse(length(cc(x=weight,m=wazm,s = wazs,l = wazl)*100)==0,NA,cc(x=weight,m=wazm,s = wazs,l = wazl)*100)
    dat= subset(Data,Metric=="whz")
    whzl= ifelse(length(dat$l[which(dat$Match.ID==heightsex)])==0,NA,dat$l[which(dat$Match.ID==heightsex)])
    whzm= ifelse(length(dat$m[which(dat$Match.ID==heightsex)])==0,NA,dat$m[which(dat$Match.ID==heightsex)])
    whzs= ifelse(length(dat$s[which(dat$Match.ID==heightsex)])==0,NA,dat$s[which(dat$Match.ID==heightsex)])
    whz=  ifelse(length(((weight/whzm)^whzl-1)/(whzl*whzs))==0,NA,((weight/whzm)^whzl-1)/(whzl*whzs))
    whz_risk= ifelse(length((1-pnorm(whz))*100)==0,NA,(1-pnorm(whz))*100)
    whz_Percentile= ifelse(length(cc(x=weight,m=whzm,s = whzs,l = whzl)*100)==0,NA,cc(x=weight,m=whzm,s = whzs,l = whzl)*100)
    dat= subset(Data,Metric=="baz")
    bazl= ifelse(length(dat$l[which(dat$Match.ID==agesex)])==0,NA,dat$l[which(dat$Match.ID==agesex)])
    bazm= ifelse(length(dat$m[which(dat$Match.ID==agesex)])==0,NA,dat$m[which(dat$Match.ID==agesex)])
    bazs= ifelse(length(dat$s[which(dat$Match.ID==agesex)])==0,NA,dat$s[which(dat$Match.ID==agesex)])
    baz=  ifelse(length(((bmical/bazm)^bazl-1)/(bazl*bazs))==0,NA,((bmical/bazm)^bazl-1)/(bazl*bazs))
    baz_risk= ifelse(length((1-pnorm(baz))*100)==0,NA,(1-pnorm(baz))*100)
    baz_Percentile= ifelse(length(cc(x=bmical,m=bazm,s = bazs,l = bazl)*100)==0,NA,cc(x=bmical,m=bazm,s = bazs,l = bazl)*100)
    tab1= data.frame(`Metric`=c("Height for age","Weight for age","Weight for height","BMI for age"),
                     `Z score`=c(sprintf("%0.3f",haz) ,sprintf("%0.3f",waz),sprintf("%0.3f",whz),sprintf("%0.3f",baz)),
                     `Risk of growth faltaring(%)`=c(sprintf("%0.3f",haz_risk),sprintf("%0.3f",waz_risk),sprintf("%0.3f",whz_risk),sprintf("%0.3f",baz_risk)),
                     `Percentile`=c(sprintf("%0.3f",haz_Percentile),sprintf("%0.3f",waz_Percentile),sprintf("%0.3f",whz_Percentile),sprintf("%0.3f",baz_Percentile)))
    names(tab1)= c("Metric","Z Score","Risk of growth faltaring (%)","Percentile")
    tab2= data.frame(Metric= c("Height for age","Weight for age","Weight for height","BMI for age"),
                     L= c(sprintf("%0.3f",hazl),sprintf("%0.3f",wazl),sprintf("%0.3f",whzl),sprintf("%0.3f",bazl)),
                     M= c(sprintf("%0.3f",hazm),sprintf("%0.3f",wazm),sprintf("%0.3f",whzm),sprintf("%0.3f",bazm)),
                     S= c(sprintf("%0.3f",hazs),sprintf("%0.3f",wazs),sprintf("%0.3f",whzs),sprintf("%0.3f",bazs)))
    return(list("Zscore Table"=tab1,
                "LMS Table"=tab2,
                "BMI"=bmical))
  })
}


ui= fluidPage(theme = shinytheme("slate"),#shinythemes::themeSelector(),
              "created by Rajesh Majumder",
              
              titlePanel(title = "Z Score Calculator for Indian 0 to 19 years children"),
              #sidebarLayout(
              
              ##----- Error handeling by CSS code -----#
              
              tags$style(type="text/css",
                         ".shiny-output-error { visibility: hidden; }",
                         ".shiny-output-error:before { visibility: hidden; }"
              ),
              
              column(width = 2,
                     numericInput("year","Year",min = 1,max = 19,value = 5),
                     numericInput("month","Month",min = 0,max = 11,value = 0),
                     selectInput(inputId = "sex",label = "Sex",choices = c("Male","Female")),
                     shiny::numericInput(inputId = "h",label = "Height (cm)",value = 107.45),
                     shiny::numericInput(inputId = "w",label = "Weight (kg)",value = 17.35),
                     shiny::submitButton("Go"),
                     tags$hr(),
                     checkboxInput("bulk", "Bulk Calculation"),
                     conditionalPanel(
                       condition = "input.bulk == true",
                       helpText("Note: while the data view will show only the specified",
                                "number of observations, the summary will still be based",
                                "on the full dataset."),
                       fileInput('file1', 'Choose CSV File',
                                 accept=c('text/csv', 
                                          'text/comma-separated-values,text/plain', 
                                          '.csv')),
                       tags$hr(),
                       #checkboxInput('header', 'Header', TRUE),
                       radioButtons('sep', 'Separator',
                                    c(Comma=',',
                                      Semicolon=';',
                                      Tab='\t'),
                                    ','),
                       radioButtons('quote', 'Quote',
                                    c(None='',
                                      'Double Quote'='"',
                                      'Single Quote'="'"),
                                    '"'),
                       downloadButton('downloadData', 'Download')
                     )
              ),
              
              column(width = 5,
                     fluidRow(#h4("Personal Information"),
                       column(7,align="center",div(valueBoxOutput("year",width = 7),style="color:white;")),  #,style="background-color:gray;"
                       column(4,align="center",div(valueBoxOutput("bmi",width = 4),style="color:white;"))    #,style="background-color:gray;"
                     ),
                     #column(5,h4("Sex"),
                     #       textOutput("sex")),
                     #column(5,h4("BMI"),
                     #       )),
                     tags$hr(),
                     
                     fluidRow(h3("Z Score and Individual Risk"),
                              tableOutput("tab1")),
                     tags$hr(),
                     
                     fluidRow(h3("LMS Values"),
                              tableOutput("tab2")),
                     
                     conditionalPanel(
                       condition = "input.bulk == true",
                       tags$hr(),
                       
                       fluidRow(h3("Bulk Calculation"),
                                column(
                                  dataTableOutput(outputId = "data"), width = 12)
                       )
                     )
              ),
              
              column(width = 4,
                     fluidRow(
                       plotlyOutput("haz",width =500,height = 400)),
                     fluidRow(
                       plotlyOutput("waz",width =500,height = 400)),
                     fluidRow(
                       plotlyOutput("whz",width =500,height = 400)),
                     fluidRow(
                       plotlyOutput("baz",width =500,height = 400))
                     
              )
)


server= function(input,output){
  
  # yearval= eventReactive(input$go,{
  #   ifelse(input$year*12+input$month>=0,paste0(input$year*12+input$month," months"),"")
  # })
  # 
  # bmival= eventReactive(input$go,{
  #   ifelse(!is.na(input$w/(input$h/100)^2),input$w/(input$h/100)^2,"")
  # })
  # 
  # calculation= eventReactive(input$go,{
  #   zscoretab(Data = data,age = ifelse(is.na(input$year*12+input$month),0,input$year*12+input$month),sex = input$sex,height = input$h,weight = input$w)
  # })
  # 
  # output$year=renderText(yearval())
  # output$bmi= renderText(bmival())
  # 
  # output$tab1= renderTable(calculation()$`Zscore Table`)
  # output$tab2= renderTable(calculation()$`LMS Table`)
  # output$haz= renderPlotly({ggplotly(calculation()$hazp)})
  # output$waz= renderPlotly({ggplotly(calculation()$wazp)})
  # output$whz= renderPlotly({ggplotly(calculation()$whzp)})
  # output$baz= renderPlotly({ggplotly(calculation()$bazp)})
  
  output$year= renderValueBox({valueBox(
    value=ifelse(!is.na(input$year*12+input$month),paste0(input$year*12+input$month," months"),""),
    subtitle="Age")})  #color="gray"
  
  output$bmi= renderValueBox({valueBox(
    value=ifelse(!is.na(input$w/(input$h/100)^2),round(input$w/(input$h/100)^2,2),""),
    subtitle="BMI")})  #color="gray"
  
  # renderText(ifelse(!is.na(input$w/(input$h/100)^2),input$w/(input$h/100)^2,""))
  output$tab1= renderTable(zscoretab(Data = data,age = ifelse(is.na(input$year*12+input$month),NA,input$year*12+input$month),sex = input$sex,height = input$h,weight = input$w)$`Zscore Table`)
  output$tab2= renderTable(zscoretab(Data = data,age = ifelse(is.na(input$year*12+input$month),NA,input$year*12+input$month),sex = input$sex,height = input$h,weight = input$w)$`LMS Table`)
  output$haz= renderPlotly(zscoretab(Data = data,age = ifelse(is.na(input$year*12+input$month),NA,input$year*12+input$month),sex = input$sex,height = input$h,weight = input$w)$hazp)
  output$waz= renderPlotly(zscoretab(Data = data,age = ifelse(is.na(input$year*12+input$month),NA,input$year*12+input$month),sex = input$sex,height = input$h,weight = input$w)$wazp)
  output$whz= renderPlotly(zscoretab(Data = data,age = ifelse(is.na(input$year*12+input$month),NA,input$year*12+input$month),sex = input$sex,height = input$h,weight = input$w)$whzp)
  output$baz= renderPlotly(zscoretab(Data = data,age = ifelse(is.na(input$year*12+input$month),NA,input$year*12+input$month),sex = input$sex,height = input$h,weight = input$w)$bazp)
  
  getData <- reactive({
    
    inFile <- input$file1
    
    if (is.null(input$file1))
      return(NULL)
    
    df=read.csv(inFile$datapath, header=TRUE, sep=input$sep, 
                quote=input$quote)
    return(df)
  })
  
  
  # output$data <- renderDataTable({
  #   
  #   getData()
  #   
  # },options = list(scrollX = TRUE,pageLength = 10))
  # 
  
  
  newdata=reactive(
    {
      #stopifnot(!is.null(dim(getData())))
      getData() %>%
        add_column("BMI"= apply(getData(),1,function(x) suppressWarnings({zscoreval(Data = data,age =as.numeric(x['ageMonths']),sex = x['sex'],height = as.numeric(x['height']),weight = as.numeric(x['weight']))$BMI })),
                   "HAZ"=apply(getData(),1,function(x) suppressWarnings({zscoreval(Data = data,age =as.numeric(x['ageMonths']),sex = x['sex'],height = as.numeric(x['height']),weight = as.numeric(x['weight']))$`Zscore Table`$`Z Score`[1] })),
                   "HAZ Risk"=apply(getData(),1,function(x) suppressWarnings({zscoreval(Data = data,age =as.numeric(x['ageMonths']),sex = x['sex'],height = as.numeric(x['height']),weight = as.numeric(x['weight']))$`Zscore Table`$`Risk of growth faltaring (%)`[1] })),
                   "HAZ Percentile"=apply(getData(),1,function(x) suppressWarnings({zscoreval(Data = data,age =as.numeric(x['ageMonths']),sex = x['sex'],height = as.numeric(x['height']),weight = as.numeric(x['weight']))$`Zscore Table`$`Risk of growth faltaring (%)`[1] })),
                   "WAZ"=apply(getData(),1,function(x) suppressWarnings({zscoreval(Data = data,age =as.numeric(x['ageMonths']),sex = x['sex'],height = as.numeric(x['height']),weight = as.numeric(x['weight']))$`Zscore Table`$`Z Score`[2] })),
                   "WAZ Risk"=apply(getData(),1,function(x) suppressWarnings({zscoreval(Data = data,age =as.numeric(x['ageMonths']),sex = x['sex'],height = as.numeric(x['height']),weight = as.numeric(x['weight']))$`Zscore Table`$`Risk of growth faltaring (%)`[2] })),
                   "WAZ Percentile"=apply(getData(),1,function(x) suppressWarnings({zscoreval(Data = data,age =as.numeric(x['ageMonths']),sex = x['sex'],height = as.numeric(x['height']),weight = as.numeric(x['weight']))$`Zscore Table`$`Risk of growth faltaring (%)`[2] })),
                   "WHZ"=apply(getData(),1,function(x) suppressWarnings({zscoreval(Data = data,age =as.numeric(x['ageMonths']),sex = x['sex'],height = as.numeric(x['height']),weight = as.numeric(x['weight']))$`Zscore Table`$`Z Score`[3] })),
                   "WHZ Risk"=apply(getData(),1,function(x) suppressWarnings({zscoreval(Data = data,age =as.numeric(x['ageMonths']),sex = x['sex'],height = as.numeric(x['height']),weight = as.numeric(x['weight']))$`Zscore Table`$`Risk of growth faltaring (%)`[3] })),
                   "WHZ Percentile"=apply(getData(),1,function(x) suppressWarnings({zscoreval(Data = data,age =as.numeric(x['ageMonths']),sex = x['sex'],height = as.numeric(x['height']),weight = as.numeric(x['weight']))$`Zscore Table`$`Risk of growth faltaring (%)`[3] })),
                   "BAZ"=apply(getData(),1,function(x) suppressWarnings({zscoreval(Data = data,age =as.numeric(x['ageMonths']),sex = x['sex'],height = as.numeric(x['height']),weight = as.numeric(x['weight']))$`Zscore Table`$`Z Score`[4] })),
                   "BAZ Risk"=apply(getData(),1,function(x) suppressWarnings({zscoreval(Data = data,age =as.numeric(x['ageMonths']),sex = x['sex'],height = as.numeric(x['height']),weight = as.numeric(x['weight']))$`Zscore Table`$`Risk of growth faltaring (%)`[4] })),
                   "BAZ Percentile"=apply(getData(),1,function(x) suppressWarnings({zscoreval(Data = data,age =as.numeric(x['ageMonths']),sex = x['sex'],height = as.numeric(x['height']),weight = as.numeric(x['weight']))$`Zscore Table`$`Risk of growth faltaring (%)`[4] }))) %>%
        return(data.frame(getData()))
    }
  )
  
  output$data <- renderDataTable({
    
    newdata()
    
  },options = list(scrollX = TRUE,pageLength = 20))
  
  
  output$downloadData <- downloadHandler(
    
    filename = function() { 
      paste("data-", Sys.Date(), ".csv", sep="")
    },
    
    content = function(file) {
      
      write.csv(newdata(), file)
      
    })
  
  
  
}


shinyApp(ui=ui,server = server)


