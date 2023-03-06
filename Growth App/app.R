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


Data= read.csv("All Z score.csv")

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


ui= shinyUI(navbarPage(title = "Anthoropometric Z Score Calculator For Indian 0 to 19 Years children",
                       theme = shinytheme("slate"),
                       #theme=shinythemes::themeSelector(),
                       tabPanel("Simple Calculator",
                                tags$head(tags$script(src = "message-handler.js")),
                                #titlePanel(title = "Z Score Calculator for Indian 0 to 19 years children"),
                                #sidebarLayout(
                                
                                ##----- Error handeling by CSS code -----#
                                
                                tags$style(type="text/css",
                                           ".shiny-output-error { visibility: hidden; }",
                                           ".shiny-output-error:before { visibility: hidden; }"
                                ),
                                column(width = 2,
                                       numericInput("year","Year",min = 1,max = 19,value = 0),
                                       numericInput("month","Month",min = 0,max = 11,value = 0),
                                       selectInput(inputId = "sex",label = "Sex",choices = c("","Male","Female"),selected = NULL),
                                       shiny::numericInput(inputId = "h",label = "Height (cm)",value = 0),
                                       shiny::numericInput(inputId = "w",label = "Weight (kg)",value = 0),
                                       shiny::actionButton("go","Go")
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
                                       tags$hr(style="border-color: white;"),
                                       
                                       fluidRow(h3("Z Score and Individual Risk"),
                                                tableOutput("tab1"),
                                                span(textOutput("msg1"),style="color:red"),
                                                span(textOutput("msg2"),style="color:red"),
                                                span(textOutput("msg3"),style="color:red"),
                                                span(textOutput("msg4"),style="color:red")),
                                       tags$hr(style="border-color: white;"),
                                       
                                       fluidRow(h3("LMS Values"),
                                                tableOutput("tab2"))
                                ),
                                
                                column(width = 4,
                                       fluidRow(
                                         selectInput(inputId = "plotshow",label = "select Plot",choices = c("Height for age","Weight for age","Weight for height","BMI for age"))),
                                       tags$br(),
                                       
                                       fluidRow(
                                         plotlyOutput("haz",width =550,height = 530))
                                )
                                
                       ),
                       
                       
                       tabPanel("Bulk Calculator",
                                tags$head(tags$script(src = "message-handler.js")),
                                ##----- Error handeling by CSS code -----#
                                tags$style(type="text/css",
                                           ".shiny-output-error { visibility: hidden; }",
                                           ".shiny-output-error:before { visibility: hidden; }"
                                ),
                                
                                column(width = 3,
                                       fileInput('file1', 'Choose CSV File',
                                                 accept=c('text/csv', 
                                                          'text/comma-separated-values,text/plain', 
                                                          '.csv')),
                                       h4("Note:"),
                                       helpText("1> Age should be in Months."),
                                       helpText(div("2> Sex should be labeled as Male-Female or Boy-Girl.")),
                                       helpText("3> Height should be in cm."),
                                       helpText("4> Weight should be in kg."),
                                       tags$hr(),
                                       checkboxInput('header', 'Header', TRUE),
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
                                       tags$hr(),
                                       tags$br(),
                                       shiny::actionButton("calcu","Calculate"),
                                       tags$hr(),
                                       tags$br(),
                                       downloadButton('downloadData', 'Download'),
                                       tags$br(),tags$br(),
                                ),
                              
                                column(width = 2,
                                       h4("Specify following parameters:"),
                                       selectInput("agecol","Age",character(0)),
                                       selectInput("sexcol","Sex",character(0)),
                                       selectInput("heightcol","Hight",character(0)),
                                       selectInput("weightcol","Weight",character(0))
                                ),
                                column(width = 7,
                                       fluidRow(h3("Uploaded Data"),
                                                column(
                                                  dataTableOutput(outputId = "data"), width = 12)
                                       ),
                                       fluidRow(h3("Calculated Table"),
                                                column(
                                                  dataTableOutput(outputId = "caldata"), width = 12)
                                       )
                                )
                       )
)
)



server= function(input,output,session){
  
  
  observeEvent(input$go, {
    updateNumericInput(session, "year", value = 0)
    updateNumericInput(session, "month", value = 0)
    updateSelectInput(session, "sex",choices = c("Male","Female"),selected = FALSE)
    updateNumericInput(session, "h", value = 0)
    updateNumericInput(session, "w", value = 0)
  })
  
  
  outval=eventReactive(input$go,{
    
    suppressWarnings({
      require(ggplot2)
      require(plotly)
      require(reshape2)
      
      age=ifelse(is.na(input$year*12+input$month),NA,input$year*12+input$month)
      sex= input$sex
      height= input$h
      weight= input$w
      
      
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
      dat$variable= factor(dat$variable,levels = rev(c("X2.5th","X5th","X25th","X50th","X75th","X95th","X97.5th")),
                           labels = rev(c("2.5th","5th","25th","50th","75th","95th","97.5th")))
      names(dat)=c("Age","Centile","Height")
      
      p1=ggplot(data = dat,aes(x=Age,
                               y=Height,
                               color=Centile))+geom_line(size=0.3)+
        theme(legend.title=element_blank(),panel.background = element_rect(fill='transparent'),
              plot.background = element_rect(fill='transparent', color=NA),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              legend.background = element_rect(fill='transparent'),
              legend.box.background = element_rect(fill='transparent'),
              axis.line = element_line(colour = "white"),axis.text =element_text(colour = "white"),
              axis.title.x=element_text(colour = "white"),axis.title.y=element_text(colour = "white"),
              plot.title = element_text(color = "white"),legend.text = element_text(colour = "white"),axis.ticks = element_line(color = "white"))+
        scale_linetype_discrete(name="GGG")+xlab("Age (Year)")+ylab("Height (cm)")+
        geom_point(aes(x=age,y=height),color="yellow",size=0.5)+ggtitle("Height for Age")+
        scale_x_continuous(limits = xlim_val,breaks = seq(from=xlim_val[1],to=xlim_val[2],by=12),labels = seq(from=xlim_val[1]/12,to=xlim_val[2]/12,by=1))+
        scale_y_continuous(limits = ylim_val,breaks = seq(from=ylim_val[1],to=ylim_val[2],by=10),labels = seq(from=ylim_val[1],to=ylim_val[2],by=10))
      
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
      dat$variable= factor(dat$variable,levels = rev(c("X2.5th","X5th","X25th","X50th","X75th","X95th","X97.5th")),
                           labels = rev(c("2.5th","5th","25th","50th","75th","95th","97.5th")))
      names(dat)=c("Age","Centile","Weight")
      
      p2=ggplot(data = dat,aes(x=Age,
                               y=Weight,
                               color=Centile))+geom_line(size=0.3)+
        theme(legend.title=element_blank(),panel.background = element_rect(fill='transparent'),
              plot.background = element_rect(fill='transparent', color=NA),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              legend.background = element_rect(fill='transparent'),
              legend.box.background = element_rect(fill='transparent'),
              axis.line = element_line(colour = "white"),axis.text =element_text(colour = "white"),
              axis.title.x=element_text(colour = "white"),axis.title.y=element_text(colour = "white"),
              plot.title = element_text(color = "white"),legend.text = element_text(colour = "white"),axis.ticks = element_line(color = "white"))+
        scale_linetype_discrete(name="GGG")+xlab("Age (Year)")+ylab("Weight (kg)")+
        geom_point(aes(x=age,y=weight),color="yellow",size=0.5)+ggtitle("Weight for Age")+
        scale_x_continuous(limits = xlim_val,breaks = seq(from=xlim_val[1],to=xlim_val[2],by=12),labels = seq(from=xlim_val[1]/12,to=xlim_val[2]/12,by=1))+
        scale_y_continuous(limits = ylim_val,breaks = seq(from=ylim_val[1],to=ylim_val[2],by=5),labels = seq(from=ylim_val[1],to=ylim_val[2],by=5))
      
      
      
      
      
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
      dat$variable= factor(dat$variable,levels = rev(c("X2.5th","X5th","X25th","X50th","X75th","X95th","X97.5th")),
                           labels = rev(c("2.5th","5th","25th","50th","75th","95th","97.5th")))
      names(dat)=c("Height","Centile","Weight")
      
      p3=ggplot(data = dat,aes(x=Height,
                               y=Weight,
                               color=Centile))+geom_line(size=0.3)+
        theme(legend.title=element_blank(),panel.background = element_rect(fill='transparent'),
              plot.background = element_rect(fill='transparent', color=NA),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              legend.background = element_rect(fill='transparent'),
              legend.box.background = element_rect(fill='transparent'),
              axis.line = element_line(colour = "white"),axis.text =element_text(colour = "white"),
              axis.title.x=element_text(colour = "white"),axis.title.y=element_text(colour = "white"),
              plot.title = element_text(color = "white"),legend.text = element_text(colour = "white"),axis.ticks = element_line(color = "white"))+
        scale_linetype_discrete(name="GGG")+xlab("Height (cm)")+ylab("Weight (kg)")+ #ylim(c(2,30))+xlim(c(45,120))+
        geom_point(aes(x=height,y=weight),color="yellow",size=0.5)+ggtitle("Weight for height")+
        scale_x_continuous(limits = c(45,120),breaks = seq(45,120,length.out=10),labels = sprintf("%0.1f",seq(45,120,length.out=10)))+
        scale_y_continuous(limits = c(2,25),breaks = seq(2,25,length.out=10),labels = sprintf("%0.1f",seq(2,25,length.out=10)))
      
      
      
      
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
      dat$variable= factor(dat$variable,levels = rev(c("X2.5th","X5th","X25th","X50th","X75th","X95th","X97.5th")),
                           labels = rev(c("2.5th","5th","25th","50th","75th","95th","97.5th")))
      names(dat)=c("Age","Centile","BMI")
      
      p4=ggplot(data = dat,aes(x=Age,
                               y=BMI,
                               color=Centile))+geom_line(size=0.3)+
        theme(legend.title=element_blank(),panel.background = element_rect(fill='transparent'),
              plot.background = element_rect(fill='transparent', color=NA),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              legend.background = element_rect(fill='transparent'),
              legend.box.background = element_rect(fill='transparent'),
              axis.line = element_line(colour = "white"),axis.text =element_text(colour = "white"),
              axis.title.x=element_text(colour = "white"),axis.title.y=element_text(colour = "white"),
              plot.title = element_text(color = "white"),legend.text = element_text(colour = "white"),axis.ticks = element_line(color = "white"))+
        scale_linetype_discrete(name="GGG")+xlab("Age (Month)")+ylab("BMI (kg/m^2)")+
        geom_point(aes(x=age,y=bmical),color="yellow",size=0.5)+ggtitle("BMI for Age")+
        scale_x_continuous(limits = c(60,240),breaks = seq(60,240,12),labels = seq(60,240,12)/12)+
        scale_y_continuous(limits = c(12,30),breaks = seq(12,30,length.out=10),labels = sprintf("%0.1f",seq(12,30,length.out=10)))
      
      tab1= data.frame(`Metric`=c("Height for age","Weight for age","Weight for height","BMI for age"),
                       `Z score`=c(sprintf("%0.3f",haz) ,ifelse(age>131,NA,sprintf("%0.3f",waz)),ifelse(age>59,NA,sprintf("%0.3f",whz)),sprintf("%0.3f",baz)),
                       `Risk of growth faltaring(%)`=c(sprintf("%0.3f",haz_risk),ifelse(age>131,NA,sprintf("%0.3f",waz_risk)),ifelse(age>59,NA,sprintf("%0.3f",whz_risk)),sprintf("%0.3f",baz_risk)),
                       `Percentile`=c(sprintf("%0.3f",haz_Percentile),ifelse(age>131,NA,sprintf("%0.3f",waz_Percentile)),ifelse(age>59,NA,sprintf("%0.3f",whz_Percentile)),sprintf("%0.3f",baz_Percentile)))
      names(tab1)= c("Metric","Z Score","Risk of growth faltaring (%)","Percentile")
      
      tab2= data.frame(Metric= c("Height for age","Weight for age","Weight for height","BMI for age"),
                       L= c(sprintf("%0.3f",hazl),ifelse(age>131,NA,sprintf("%0.3f",wazl)),ifelse(age>59,NA,sprintf("%0.3f",whzl)),sprintf("%0.3f",bazl)),
                       M= c(sprintf("%0.3f",hazm),ifelse(age>131,NA,sprintf("%0.3f",wazm)),ifelse(age>59,NA,sprintf("%0.3f",whzm)),sprintf("%0.3f",bazm)),
                       S= c(sprintf("%0.3f",hazs),ifelse(age>131,NA,sprintf("%0.3f",wazs)),ifelse(age>59,NA,sprintf("%0.3f",whzs)),sprintf("%0.3f",bazs)))
      
      
      if(age>59){
        p3=NULL
      }
      
      if(age>131){
        p2=NULL
        p3=NULL
      }
      
      if(age<60){
        p4=NULL
      }
      
      return(list("Zscore Table"=tab1,
                  "LMS Table"=tab2,
                  "hazp"=suppressMessages(p1),
                  "wazp"=suppressMessages(p2),
                  "whzp"=p3,
                  "bazp"=p4,
                  "bbmi"=sprintf("%0.2f",bmical),
                  "aage"=paste0(age," months")))
    })
    
    
    
    
  })
  
  output$year= renderValueBox({valueBox(
    value=outval()$aage,
    subtitle="Age")})
  
  output$bmi= renderValueBox({valueBox(
    value=outval()$bbmi,
    subtitle="BMI")})
  
  output$tab1= renderTable(outval()$`Zscore Table`)
  output$tab2= renderTable(outval()$`LMS Table`)
  output$haz= renderPlotly({
    
    if(input$plotshow=="Height for age"){
      outval()$hazp
    }else if(input$plotshow=="Weight for age"){
      outval()$wazp
    }else if(input$plotshow=="Weight for height"){
      outval()$whzp
    }else if(input$plotshow=="BMI for age"){
      outval()$bazp
    }
    
  })
  
  
  output$msg1= renderText({
    
    if(as.numeric(outval()$`Zscore Table`$`Z Score`[1])>=6|as.numeric(outval()$`Zscore Table`$`Z Score`[1])<=(-6)){
      "Warning!!: Height for age Z score is out of plausible range"
    }
  })
  
  output$msg2= renderText({
    if(as.numeric(outval()$`Zscore Table`$`Z Score`[2])>=5|as.numeric(outval()$`Zscore Table`$`Z Score`[2])<=(-6)){
      "Warning!!: Weight for age Z score is out of plausible range"
    }
  })
  output$msg3= renderText({
    if(as.numeric(outval()$`Zscore Table`$`Z Score`[3])>=5|as.numeric(outval()$`Zscore Table`$`Z Score`[3])<=(-6)){
      "Warning!!: Weight for height Z score is out of plausible range"
    }
  })
  output$msg4= renderText({
    if(as.numeric(outval()$`Zscore Table`$`Z Score`[4])>=5|as.numeric(outval()$`Zscore Table`$`Z Score`[4])<=(-5)){
      "Warning!!: BMI for height Z score is out of plausible range"
    }
  })
  
  
  
  getData <- reactive({
    
    inFile <- input$file1
    
    if (is.null(input$file1))
      return(NULL)
    
    df=read.csv(inFile$datapath, header=input$header, sep=input$sep, 
                quote=input$quote)
    return(df)
  })
  
  
  output$data <- renderDataTable({
    
    getData()
    
  },options = list(scrollX = TRUE,pageLength = 10))
  
  
  observeEvent(getData(), {
    updateSelectInput(session = session, "agecol", choices = names(getData()))
  })
  
  observeEvent(getData(), {
    updateSelectInput(session, "sexcol", choices = names(getData()))
  })
  
  observeEvent(getData(), {
    updateSelectInput(session, "heightcol", choices = names(getData()))
  })
  
  observeEvent(getData(), {
    updateSelectInput(session, "weightcol", choices = names(getData()))
  })
  
  
  
  
  
  newdata=eventReactive(input$calcu,
                        {
                          dddta= getData()
                          
                          dddta %>%
                            add_column("BMI"= apply(dddta,1,function(x) suppressWarnings({zscoreval(Data = Data,age =as.numeric(x[input$agecol]),sex = x[input$sxcol],height = as.numeric(x[input$heightcol]),weight = as.numeric(x[input$weightcol]))$BMI })),
                                       "HAZ"=apply(getData(),1,function(x) suppressWarnings({zscoreval(Data = Data,age =as.numeric(x[input$agecol]),sex = x[input$sexcol],height = as.numeric(x[input$heightcol]),weight = as.numeric(x[input$weightcol]))$`Zscore Table`$`Z Score`[1] })),
                                       "HAZ Risk"=apply(getData(),1,function(x) suppressWarnings({zscoreval(Data = Data,age =as.numeric(x[input$agecol]),sex = x[input$sexcol],height = as.numeric(x[input$heightcol]),weight = as.numeric(x[input$weightcol]))$`Zscore Table`$`Risk of growth faltaring (%)`[1] })),
                                       "HAZ Percentile"=apply(getData(),1,function(x) suppressWarnings({zscoreval(Data = Data,age =as.numeric(x[input$agecol]),sex = x[input$sexcol],height = as.numeric(x[input$heightcol]),weight = as.numeric(x[input$weightcol]))$`Zscore Table`$`Percentile`[1] })),
                                       "WAZ"=apply(getData(),1,function(x) suppressWarnings({zscoreval(Data = Data,age =as.numeric(x[input$agecol]),sex = x[input$sexcol],height = as.numeric(x[input$heightcol]),weight = as.numeric(x[input$weightcol]))$`Zscore Table`$`Z Score`[2] })),
                                       "WAZ Risk"=apply(getData(),1,function(x) suppressWarnings({zscoreval(Data = Data,age =as.numeric(x[input$agecol]),sex = x[input$sexcol],height = as.numeric(x[input$heightcol]),weight = as.numeric(x[input$weightcol]))$`Zscore Table`$`Risk of growth faltaring (%)`[2] })),
                                       "WAZ Percentile"=apply(getData(),1,function(x) suppressWarnings({zscoreval(Data = Data,age =as.numeric(x[input$agecol]),sex = x[input$sexcol],height = as.numeric(x[input$heightcol]),weight = as.numeric(x[input$weightcol]))$`Zscore Table`$`Percentile`[2] })),
                                       "WHZ"=apply(getData(),1,function(x) suppressWarnings({zscoreval(Data = Data,age =as.numeric(x[input$agecol]),sex = x[input$sexcol],height = as.numeric(x[input$heightcol]),weight = as.numeric(x[input$weightcol]))$`Zscore Table`$`Z Score`[3] })),
                                       "WHZ Risk"=apply(getData(),1,function(x) suppressWarnings({zscoreval(Data = Data,age =as.numeric(x[input$agecol]),sex = x[input$sexcol],height = as.numeric(x[input$heightcol]),weight = as.numeric(x[input$weightcol]))$`Zscore Table`$`Risk of growth faltaring (%)`[3] })),
                                       "WHZ Percentile"=apply(getData(),1,function(x) suppressWarnings({zscoreval(Data = Data,age =as.numeric(x[input$agecol]),sex = x[input$sexcol],height = as.numeric(x[input$heightcol]),weight = as.numeric(x[input$weightcol]))$`Zscore Table`$`Percentile`[3] })),
                                       "BAZ"=apply(getData(),1,function(x) suppressWarnings({zscoreval(Data = Data,age =as.numeric(x[input$agecol]),sex = x[input$sexcol],height = as.numeric(x[input$heightcol]),weight = as.numeric(x[input$weightcol]))$`Zscore Table`$`Z Score`[4] })),
                                       "BAZ Risk"=apply(getData(),1,function(x) suppressWarnings({zscoreval(Data = Data,age =as.numeric(x[input$agecol]),sex = x[input$sexcol],height = as.numeric(x[input$heightcol]),weight = as.numeric(x[input$weightcol]))$`Zscore Table`$`Risk of growth faltaring (%)`[4] })),
                                       "BAZ Percentile"=apply(getData(),1,function(x) suppressWarnings({zscoreval(Data = Data,age =as.numeric(x[input$agecol]),sex = x[input$sexcol],height = as.numeric(x[input$heightcol]),weight = as.numeric(x[input$weightcol]))$`Zscore Table`$`Percentile`[4] }))) %>%
                            return(data.frame(dddta))
                          
                        })
  
  output$caldata <- renderDataTable({
    
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


