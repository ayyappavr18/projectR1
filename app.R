library(shinythemes)
library(ggplot2)
library(ggmosaic)
library(productplots)
library(plotly)
library(summarytools)
library(MASS) 

# function
callback <- c(
  "$.contextMenu({",
  "  selector: '#table th',", 
  "  trigger: 'right',",
  "  autoHide: true,",
  "  items: {",
  "    text: {",
  "      name: 'Enter column header:',", 
  "      type: 'text',", 
  "      value: ''", 
  "    }",
  "  },",
  "  events: {",
  "    show: function(opt){",
  "      $.contextMenu.setInputValues(opt, {text: opt.$trigger.text()});",
  "    },",
  "    hide: function(opt){",
  "      var $this = this;",
  "      var text = $.contextMenu.getInputValues(opt, $this.data()).text;",
  "      var $th = opt.$trigger;",
  "      $th.text(text);",
  "    }",
  "  }",
  "});" 
)
# outlier delection function
replace.empty <- function(a) {
  a <- as.character(a)
  a[a== ""]<- NA
  a<-as.factor(a)
  return(a)
}
# libraries
library(vcd)

library(skimr)
library(DT)
library("dplyr")
library("ggplot2")
library("tidyr")
library("gridExtra")
library(naniar)

library(shiny)
library(plotly)
library(epiDisplay)
library(epiR)
library(epitools)
options(digits=3)
library(DescTools)
library(Hmisc)
library(pastecs)
library(shiny)

library(plyr)
library(ggplot2)

library(shiny)
library(shinyjs)
library(V8)
jsCode <- 'shinyjs.winprint = function(){
window.print();
}'

library(summarytools)
library(dplyr) 
library(ggplot2)
library(ggpubr)
library(reshape2)
theme_set(theme_pubclean())
library(shinythemes)
#------
ui = navbarPage(theme = shinytheme("cerulean"),"R_shiny",
                tabPanel("Data",
                         sidebarPanel(width = 3,
                                      
                                      fileInput("file", "Choose rds File",
                                                accept = c("rds","Rdata")),
                                      downloadButton("download", "Download")
                                      
                         ),
                         useShinyjs(),
                         extendShinyjs(text = jsCode),
                         mainPanel(
                           DT::dataTableOutput("table") 
                           
                         )
                ),
                tabPanel("Data cleaning",
                         # sidebarPanel(width = 2,
                         #   actionButton("print", "PRINT")
                         # 
                         # ),
                         mainPanel(width=11,
                                   tabsetPanel(tabPanel( h5("CHOOSE DATA TYPE"),div(style="display:inline-block",uiOutput("colname_in"),width=3),div(style="display:inline-block",uiOutput("colname_in1"),width=3),verbatimTextOutput("classvari"),actionButton("change_class","change class"),tags$hr(style="border-color: black;"),
                                                         h5("RENAME COLUMN"),uiOutput("colname_in2"),uiOutput("colname_in3"),actionButton("RenameColumn","Rename Column"),tags$hr(style="border-color: black;"),
                                                         h5("RENAME FACTOR LEVELS"),uiOutput("colname_in4"),uiOutput("colname_in5"),uiOutput("colname_in6"),actionButton("RenamelevelsColumn","Rename levels"),tags$hr(style="border-color: black;")),
                                               tabPanel(h5("OUTLIERS"),uiOutput('select5'),verbatimTextOutput('summary5'),actionButton("goButton", "Remove!"),plotlyOutput("plotlier"),tags$hr(style="border-color: black;")),tabPanel( h5("MISSING VALUE"),tabsetPanel(tabPanel(h6("demographic variable"),uiOutput("miss1"),verbatimTextOutput("miss3"),plotOutput("missplot1")),
                                                                                                                                                                                                                                                                        tabPanel(h6("risk factor variable"),uiOutput("miss2"),verbatimTextOutput("miss4"),plotOutput("missplot2"))))
                                   )
                         )
                ),
                tabPanel("Univariate",
                         # # sidebarPanel(width = 2,
                         # #   
                         # #   actionButton("print", "PRINT")
                         #         
                         # ),
                         mainPanel(width=12,
                                   verbatimTextOutput('summary3'),downloadButton('report',label="Download Report")
                                   
                         )
                ),
                tabPanel("Bivariate",
                         sidebarPanel(width = 3,
                                      
                                      uiOutput('select'),
                                      uiOutput('select1')
                         ),
                         mainPanel(width=9,
                                   verbatimTextOutput("summary"),verbatimTextOutput('summary1'),verbatimTextOutput('summary2'),plotlyOutput("plot4"),tags$hr(style="border-color: black;"),plotlyOutput("plot5"),tags$hr(style="border-color: black;"),plotlyOutput("plot6")
                                   
                         )
                ),
                tabPanel("stratified",
                         mainPanel(width=11,
                                   tabsetPanel(tabPanel("stratified",
                                                        sidebarPanel(width = 3,
                                                                     
                                                                     uiOutput('select11'),
                                                                     uiOutput('select12'),
                                                                     uiOutput('select13')
                                                                     
                                                        ),
                                                        mainPanel(width=9,
                                                                  verbatimTextOutput("summary4"),verbatimTextOutput("summary7"),plotlyOutput("plot3")
                                                        )
                                   ),
                                   tabPanel("Trend chart",
                                            sidebarPanel(width = 2,
                                                         
                                                         uiOutput('select41'),
                                                         uiOutput('select42'),
                                                         uiOutput('select43')
                                                         
                                            ),
                                            mainPanel(width=10,
                                                      plotlyOutput("trend"),verbatimTextOutput("mean")
                                            )
                                   )
                                   )
                         )
                ),
                tabPanel("Risk Factor",
                         sidebarPanel(width = 3,
                                      uiOutput('select31'),
                                      uiOutput('select32'),
                                      uiOutput('select33')
                                      
                         ),
                         mainPanel(
                           verbatimTextOutput("risk1"),
                           verbatimTextOutput("risk2"),
                           verbatimTextOutput("risk3"),
                           verbatimTextOutput("risk4"),
                           verbatimTextOutput("risk5"),
                           verbatimTextOutput("risk6"),
                           plotOutput("riskplot41")
                         )
                ),
                tabPanel("Indication",
                         sidebarPanel(width = 3,
                                      uiOutput('select51'),
                                      uiOutput('select52'),
                                      uiOutput('select53')
                                      
                         ),
                         mainPanel(width=8,
                                   
                                   plotlyOutput("indiplot1"),tags$hr(style="border-color: black;"),
                                   plotlyOutput("indiplot2"),tags$hr(style="border-color: black;"),
                                   plotlyOutput("indiplot3"),tags$hr(style="border-color: black;"),
                                   textOutput("inditext1"), plotlyOutput("indiplot4"),verbatimTextOutput("indi1"),textOutput("inditext7"),plotlyOutput("indiplot7"),tags$hr(style="border-color: black;"),
                                   textOutput("inditext8"),plotlyOutput("indiplot8"),textOutput("inditext5"),plotlyOutput("indiplot5"),tags$hr(style="border-color: black;"),
                                   textOutput("inditext9"),plotlyOutput("indiplot9"),textOutput("inditext6"),plotlyOutput("indiplot6")
                         )
                ),
                tabPanel("Associate variable",
                         sidebarPanel(width = 3,
                                      uiOutput('select71'),
                                      uiOutput('select72'),
                                      uiOutput('select73')
                                      
                         ),
                         mainPanel(width=9,
                                   
                                   verbatimTextOutput("sss")
                         )
                ),
                tabPanel("Multivariate",
                         sidebarPanel(width = 3,
                                      uiOutput('select21'),
                                      uiOutput('select22'),
                                      uiOutput('select23')
                                      
                         ),
                         mainPanel(
                           verbatimTextOutput("s")
                         )
                ),
                tabPanel("MORE",
                         
                         mainPanel(width = 12,
                                   tabsetPanel(tabPanel(h6("LABELS & DEFINITIONS"),verbatimTextOutput("labels2")),
                                               tabPanel(h6("REFERENCES"),verbatimTextOutput("labels3"))
                                               
                                               
                                               
                                   )
                                   
                         )
                )
                
)
server = function(input, output,session) {
  # print function------
  # observeEvent(input$print, {
  #   js$winprint()
  # })
  
  #-----import data set --------------
  v = reactiveValues(path = NULL)
  
  observeEvent(input$file, {
    req(input$file)
    v$data <- readRDS(input$file$datapath)
  })
  #--table output views-------
  output$table <- DT::renderDataTable(
    DT::datatable(v$data, options = list(pageLength = 25))
  )
  
  # Downloadable csv of selected dataset ----
  output$download <- downloadHandler(
    filename = function() {
      req(input$file)
      paste(input$file$datapath,".csv", sep = "")
    },
    content = function(file) {
      write.csv(v$data, file, row.names = TRUE)
    }
  )
  # select variable 1st column name-------
  output$colname_in <- renderUI({
    
    selectInput(inputId = "colname",
                label = "Choose variable",
                multiple = T,
                choices = c(colnames(v$data)))
    
  })
  
  #----select variable class changes------
  output$colname_in1 <- renderUI({
    selectInput("class", "Choose type",
                choices = c(" ", "factor", "numeric", "integer", "character"),
                selected = " ")
  })
  
  observeEvent(input$change_class, {
    
    v$data <- eval(parse(text = paste0('v$data %>% mutate(',
                                       input$colname,
                                       ' = as.',
                                       input$class,
                                       '(',
                                       input$colname,
                                       '))')
    )
    )
    
  })
  #---rename select ui------
  output$colname_in2<-renderUI({
    selectInput(inputId = "OldColumnName", 
                label = "Select Column Name to rename",
                multiple = F, 
                choices = c(colnames(v$data)), 
                selected = "")
  })
  #---select---2--
  output$colname_in3<-renderUI({
    textInput(inputId = "NewColumnName", 
              label = "Enter New Column Name", "Nil")
  })
  observeEvent(input$RenameColumn, {
    req(input$NewColumnName, input$OldColumnName)
    if (input$NewColumnName != "Nil") {
      colnames(v$data)[colnames(v$data) == input$OldColumnName] <- 
        input$NewColumnName
    }
  })
  #---select ui for rename factor levels-------
  #---select----- 
  output$colname_in4<-renderUI({
    selectInput(inputId = "OldColumnName1", 
                label = "Select Column Name",
                multiple = F, 
                choices = c(colnames(v$data)), 
                selected = "")
  })
  #---select---2---  
  output$colname_in5<- renderUI({
    dff1 <- v$data[,input$OldColumnName1]
    dff1<- levels(dff1)
    selectInput("OldlevelsColumnName1", "Select Factor level Name to rename:",dff1)
  })
  #---select---3--
  output$colname_in6<-renderUI({
    textInput(inputId = "NewColumnName1", 
              label = "Enter New Factor levels Name", "Nil")
  })
  observeEvent(input$RenamelevelsColumn, {
    req(input$NewColumnName1, input$OldColumnName1,input$OldlevelsColumnName1)
    if (input$NewColumnName1 != "Nil") {
      levels(v$data[,input$OldColumnName1])[levels(v$data[,input$OldColumnName1]) == input$OldlevelsColumnName1] <- 
        input$NewColumnName1
    }
  })
  
  
  #select variable
  output$select <- renderUI({
    df<-colnames(v$data)
    selectInput("variable1", "outcome Variable(df):",df)
  })
  # this is Bivariate ui_input functions------------------------------------
  #select variable
  output$select1 <- renderUI({
    df1 <- colnames(v$data)
    selectInput("variable", "independent variable(df1):",df1)
  })
  # #select variable
  # output$select2 <- renderUI({
  #   df2 <- colnames(v$data)
  #   selectInput("variable2", "stratified variable(df2):",df2)
  # })
  
  # This is startified analysis ui input function------------------ ----------------------- 
  #select variable
  output$select11 <- renderUI({
    df11<-colnames(v$data)
    selectInput("variable11", "outcome Variable(df):",df11)
  })
  
  #select variable
  output$select12 <- renderUI({
    df12 <- colnames(v$data)
    selectInput("variable12", "independent variable(df1):",df12)
  })
  #select variable
  output$select13 <- renderUI({
    df13 <- colnames(v$data)
    selectInput("variable13", "stratified variable(df2):",df13)
  })
  
  # # This is multivariate analysis ui input functions---------------------  
  #   #select variable
  #   output$select21 <- renderUI({
  #     df<-colnames(v$data)
  #     selectInput("variable1", "outcome Variable(df):",df)
  #   })
  #   
  #   #select variable
  #   output$select22 <- renderUI({
  #     df1 <- colnames(v$data)
  #     selectInput("variable", "independent variable(df1):",df1)
  #   })
  #   #select variable
  #   output$select23 <- renderUI({
  #     df2 <- colnames(v$data)
  #     selectInput("variable2", "stratified variable(df2):",df2)
  #   })
  #-----------------------------------------------------------------------------------  
  
  #Risk factor--select variable
  output$select31 <- renderUI({
    df31 <- colnames(v$data[40:55])
    selectInput("variable31", "Medical Risk factor(df1):",df31)
  })
  # -- Risk factor--select variable2
  output$select32 <- renderUI({
    df32 <- colnames(v$data[56:70])
    selectInput("variable32", "Obstetric Risk factor(df2):",df32)
  })
  # --Risk factor--select variable2
  output$select33 <- renderUI({
    df33 <- colnames(v$data)
    selectInput("variable33", "Outcome variable(df3):",df33)
  })
  #----indications--select-1-ui---
  output$select51 <- renderUI({
    df51 <- colnames(v$data)[126:145]
    selectInput("variable51", "Induction indication(df1):",df51)
  })
  #----indications--select-2-ui---
  output$select52 <- renderUI({
    df52 <- colnames(v$data)[86:105]
    selectInput("variable52", "Primary indication(df2):",df52)
  })
  #----indications--select-3-ui---
  output$select53 <- renderUI({
    df53 <- colnames(v$data)[106:125]
    selectInput("variable53", "Secondary indication(df3):",df53)
  })
  
  # outlier variable
  output$select5 <- renderUI({
    df5 <- colnames(v$data)
    selectInput("variable5", "choose variable:",df5)
  })
  #---missing select--ui----- 
  output$miss1<-renderUI({
    mi1<-colnames(v$data[1:39])
    selectInput("misvariable1","demograph:",mi1)
  })
  
  output$miss2<-renderUI({
    mi2<-colnames(v$data[40:71])
    selectInput("misvariable2","risk factor:",mi2)
  })
  
  # chosse data type
  output$classvari<-renderPrint({
    dfg<-v$data[,input$colname]
    print(class(dfg))
    #print(class([,input$colname]))
    print(input$class)
    
  })
  
  # trend chart select variable--------------
  
  #select variable
  output$select41 <- renderUI({
    
    selectInput(
      inputId = "variable41", 
      label = "outcome Variable(df)",
      choices = c(colnames(v$data)))
  })  
  
  #select variable
  output$select42 <- renderUI({
    df42 <- v$data
    df42<- colnames(df42["YEAR"])
    selectInput("variable42", "time Variable(df):",df42)
  })
  
  #select variable
  output$select43 <- renderUI({
    df43 <- v$data
    df43 <- colnames(df43["unit1"])
    selectInput("variable43", "stratified Variable(df):",df43)
  })
  # associate variable select variable--------------
  output$select71<- renderUI({
    df71<- v$data
    df72<-data.frame(df71$BABY.WEIGHT1,df71$AGE1,df71$MOTHER.HEIGHT1,df71$MOTHER.HEIGHT,df71$PLACENTA.WEIGHT.Gms.1,df71$GEST.WEEKS1)
    names(df72)<-c("BABY.WEIGHT1","AGE1","MOTHER.HEIGHT1","MOTHER.WEIGHT1","PLACENTA.WEIGHT.Gms.1","GEST.WEEKS1")
    df73 <- colnames(df72)
    
    selectInput("variable73", "stratified Variable(df):",df73)
  })
  
  
  #---outliers remove
  observeEvent(input$gobutton,{
    df5<-v$data
    df6<-df5[,input$variable5]
    
    #df5[-which(df6 %in% outliers),]
  })
  
  #summary--------------
  output$summary<-renderPrint({
    df<-v$data
    df<- df[,input$variable1]
    #df<- paste(names(df[,input$variable1]))
    # df<- names(antental1[input$variable1])
    #df<- paste(antental1[input$variable])
    if(is.factor(df)){
      describe(df,paste(input$variable1[1]))
    }else {
      Desc(df,maxrows = NULL, ord = NULL, plotit = F,sep = NULL,paste(input$variable1[1]))
      
    }
  })
  # #summary-------------------------
  output$summary1<-renderPrint({
    df1<- v$data
    df1<- df1[,input$variable]
    if(is.integer(df1)){
      Desc(df1,maxrows = NULL, ord = NULL, plotit = F,sep = NULL,paste(input$variable[1]))
    }else {
      describe(df1,paste(input$variable[1]))
    }
  })
  ## #summary-----------------------
  output$summary2<- renderPrint({
    options(digits=3)
    df<- v$data
    df1<- v$data
    df<- df[,input$variable1]
    df1<- df1[,input$variable]
    nlevels<-length(levels(df1))
    if ((is.integer(df)|is.numeric(df)) & is.factor(df1)){
      if (nlevels == 2){
        t.test(df~df1)
      } 
      else if (nlevels >= 3){
        g<-lm(df~df1)
        anova(g)        
      } 
    }
    else if ((is.integer(df)|is.numeric(df)) & (is.integer(df1) | is.numeric(df1))){
      cor(df,df1,use="complete.obs")
    }
    else if (is.factor(df) & is.factor(df1)) {
      tab<-table(df,df1,dnn =c(paste(input$variable1[1]),paste(input$variable[1])))
      tab1<-t(tab)
      r<-oddsratio(tab1, method = "wald")
      print(r,digits=5)
      
    }
    else {
      print("NULL")
      #table(df,df1,dnn =c(paste(input$variable1[1]),paste(input$variable[1])))
    }
  })  
  
  # Generate a summary of the dataset ----
  output$summary3 <- renderPrint({
    dfSummary(v$data)
    #Desc(v$data,main = NULL, maxrows = NULL, ord = NULL, plotit = F,sep = NULL,digits = NULL)
  })
  #---risk factor summary--1----
  output$risk1<-renderPrint({
    df31<-v$data
    df1<- df31[,input$variable31]
    if (is.factor(df1)){
      describe(df1)
    }
    else {
      print("NULL")
    }
  })
  #---risk factor summary--2----
  output$risk2<-renderPrint({
    df32<-v$data
    df2<- df32[,input$variable32]
    if (is.factor(df2)){
      describe(df2)
    }
    else {
      print("NULL")
    }
  })
  #---risk factor summary---3----
  output$risk3<-renderPrint({
    df31<-v$data
    df32<-v$data
    df1<- df31[,input$variable31]
    df2<- df32[,input$variable32]
    table(df1,df2,dnn =c(paste(input$variable31[1]),paste(input$variable32[1])))
  })
  #---risk factor summary---4----
  output$risk4<-renderPrint({
    options(digits=3)
    df31<-v$data
    df33<-v$data
    df1<- df31[,input$variable31]
    df3<- df33[,input$variable33]
    if((is.integer(df3)| is.numeric(df3)) & (is.factor(df1))){
      aggregate(df3 ~ df1,data=v$data,mean)
      # with(v$data, tapply(df31 ,df32, mean))
    }
    else {
      print("NULL")
    }
  })
  #---risk factor summary---5----
  output$risk5<-renderPrint({
    options(digits=3)
    df32<-v$data
    df33<-v$data
    df2<- df32[,input$variable32]
    df3<- df33[,input$variable33]
    if((is.integer(df3)| is.numeric(df3)) & (is.factor(df2))){
      aggregate(df3 ~ df2,data=v$data,mean)
      # with(v$data, tapply(df31 ,df32, mean))
    }
    else {
      print("NULL")
    }
  })
  #---risk factor summary--INDICATION----
  output$risk6<-renderPrint({
    df31<-v$data
    df32<-v$data
    df333<-v$data
    df1<- df31[,input$variable31]
    df2<- df32[,input$variable32]
    df422<- v$data[83]
    df423<- v$data[84]
    df424<- v$data[85]
    d1<-data.frame(df422,df1)
    d2<-data.frame(df423,df1)
    d3<-data.frame(df424,df1)
    names(d1)<-c("INDUCTION.INDICATIONS","risk.factor1")
    names(d2)<-c("PRIMARY.INDICATIONS","risk.factor2")
    names(d3)<-c("SECONDARY.INDICATIONS","risk.factor3")
    l1<-subset(d1,risk.factor1=="1")
    l2<-subset(d2,risk.factor2=="1")
    l3<-subset(d3,risk.factor3=="1")
    f1<- names(sort(table(l1$INDUCTION.INDICATIONS),decreasing=TRUE)[1:10])
    f2<- names(sort(table(l2$PRIMARY.INDICATIONS),decreasing=TRUE)[1:10])
    f3<- names(sort(table(l3$SECONDARY.INDICATIONS),decreasing=TRUE)[1:10])
    a<-data.frame(f1,f2,f3)
    names(a)<-c("df3:INDUCTION_INDICATION","df3:PRIMARY_INDICATION","df3:SECONDARY_INDICATION")
    a
    # cat("INDUCTION_INDICATION:",f1, sep = "\n")
    # cat("PRIMARY_INDICATION:",f2, sep = "\n")
    # cat("SECONDARY_INDICATION:",f3, sep = "\n")
  })
  #---polt----------------
  output$indiplot1<-renderPlotly({
    df63<-v$data[126:145]
    mylist <- data.frame(mapply(table, df63))
    a1<-mylist[2,]
    r2<-sort(a1,decreasing = T)
    r3<-names(r2)
    r4<-as.numeric(r2)
    p<-sum(r4)
    p1<-(r4/p)*100
    da<-data.frame(r3,p1)
    da$r3<-factor(da$r3,levels = unique(da$r3)[order(da$p1,decreasing = T)])
    plot_ly(da, x = ~r3, y = ~p1, type = "bar")%>%
      layout(xaxis = list(title = 'induction_indication'),yaxis = list(title = 'patients(percentage)'),title = "induction indication(df1)")
    
  })
  #---polt----------------
  output$indiplot2<-renderPlotly({
    df63<-v$data[86:105]
    mylist <- data.frame(mapply(table, df63))
    a1<-mylist[2,]
    r2<-sort(a1,decreasing = T)
    r3<-names(r2)
    r4<-as.numeric(r2)
    p<-sum(r4)
    p1<-(r4/p)*100
    da<-data.frame(r3,p1)
    da$r3<-factor(da$r3,levels = unique(da$r3)[order(da$p1,decreasing = T)])
    plot_ly(da, x = ~r3, y = ~p1, type = "bar")%>%
      layout(xaxis = list(title = 'primary_indication'),yaxis = list(title = 'patients(percentage)'),title = "Primary indication(df2)")
    
  })
  #---polt----------------
  output$indiplot3<-renderPlotly({
    df63<-v$data[106:125]
    mylist <- data.frame(mapply(table, df63))
    a1<-mylist[2,]
    r2<-sort(a1,decreasing = T)
    r3<-names(r2)
    r4<-as.numeric(r2)
    p<-sum(r4)
    p1<-(r4/p)*100
    da<-data.frame(r3,p1)
    da$r3<-factor(da$r3,levels = unique(da$r3)[order(da$p1,decreasing = T)])
    plot_ly(da, x = ~r3, y = ~p1, type = "bar")%>%
      layout(xaxis = list(title = 'secondary_indication'),yaxis = list(title = 'patients(percentage)'),title = "secondary indication(df3)")
    
  })
  #---indi text output--------------
  output$inditext1<-renderText({
    df62<-v$data[,input$variable51]
    p<-paste0("selected indication is:", input$variable51)
    p
  })
  
  #---indication plot--medical-----
  output$indiplot4<-renderPlotly({
    df61<-v$data[73:76]
    df62<-v$data[,input$variable51]
    da<-data.frame(df61,df62)
    names(da)<-c("risk1","risk2","risk3","risk4","df1")
    d1<-subset(da,df1=="1")
    d3<-d1
    rows<-seq(1:nrow(d3))
    d2<-data.frame(rows,d3)
    d2$df1<-NULL
    dat3 <- melt(d2, id.var = "rows")
    
    
    df3 <- dat3 %>% 
      group_by(variable, value) %>% 
      tally() %>% 
      complete(value, fill = list(n = 0)) %>% 
      mutate(percentage = n / sum(n) * 100)
    p<-ggplot(df3,aes(x=variable,y=percentage,fill=value)) + 
      geom_bar(stat = "identity",position="fill")+
      scale_fill_manual(values = c("Anemia Mild"="green", "Anemia severe" = "green","Anemia Moderate"="green","BIOHAZARD-HCV"="skyblue","BIOHAZARD-HBsAg"="skyblue","BIOHAZARD-HIV"="skyblue",
                                   "BRONCHIAL ASTHMA" = "slateblue1",  "CARDIAC DISEASE-CONGENITAL HEART DISEASE" = "purple3","CARDIAC DISEASE-RHEUMATIC HEART DISEASE" ="purple3","ESSENTIAL/CHRONIC HYPERTENSION"="blue2",
                                   "GDM Diet control" = "firebrick4",  "GDM Insulin"="firebrick4", "GDM OHA"="firebrick4",
                                   "GHTN Drugs" = "coral2", "GHTN Home BP" = "coral2", "LIVER DISORDER-AFLP" = "palevioletred", "LIVER DISORDER-HELLP" = "palevioletred","None" = "black",
                                   "Preeclampsia Mild" = "violetred" ,"Preeclampsia Severe" = "violetred" ,"Eclampsia"= "violetred" , "PREGESTATIONAL DIABETES MELLITUS"="red2", "SEIZURE DISORDERS" = "springgreen2",
                                   "THYROID DISORDERS-HYPERTHYROIDISM" = "darkgoldenrod2", "THYROID DISORDERS-HYPOTHYROIDISM" = "darkgoldenrod2"), 
                        na.value ="grey")+ggtitle(paste("INDUCTION_INDICATION FOR MEDICAL RISK FACTOR",colnames(v$data[,input$variable51])))
    
    
    ggplotly(p)
    
  })
  
  #---indi text output--------------
  output$inditext7<-renderText({
    df62<-v$data[,input$variable51]
    p<-paste0("selected indication is:", input$variable51)
    p
  })
  
  #---indication plot-obs--
  output$indiplot7<-renderPlotly({
    df61<-v$data[78:81]
    df62<-v$data[,input$variable51]
    da<-data.frame(df61,df62)
    names(da)<-c("risk1","risk2","risk3","risk4","df1")
    d1<-subset(da,df1=="1")
    d3<-d1
    rows<-seq(1:nrow(d3))
    d2<-data.frame(rows,d3)
    d2$df1<-NULL
    dat3 <- melt(d2, id.var = "rows")
    
    df3 <- dat3 %>% 
      group_by(variable, value) %>% 
      tally() %>% 
      complete(value, fill = list(n = 0)) %>% 
      mutate(percentage = n / sum(n) * 100)
    p<-ggplot(df3,aes(x=variable,y=percentage,fill=value)) + 
      geom_bar(stat = "identity",position="fill")+
      scale_fill_manual(values = c("APH-ABRUPTION GRADE 1"="pink1", "APH-ABRUPTION GRADE 2" = "pink1","APH-ABRUPTION GRADE 3"="pink1","APH-PLACENTA PREVIA"="pink1","APH-UNCLASSIFIED" ="pink1","BIOHAZARD-HCV"="violet","BREECH"="firebrick4",
                                   "ELDERLY PRIMI" = "slateblue1",  "Fibroid complicating pregnancy" = "yellow","GRAND MULTIPARA" ="purple3",
                                   "IUGR"  = "green",
                                   "IVF PREGNANCY" =  "blue2", "MACROSOMIA" = "navyblue", "None" = "black",
                                   "Multiple pregnancy Triplets"  = "tomato" ,"Multiple pregnancy Twins"= "coral2"  , "PPROM"="palevioletred", "Preterm labour"  = "violetred" ,
                                   "PREVIOUS LSCS" =  "red2", "PREVIOUS NEONATAL DEATH" = "springgreen2", "PREVIOUS PRETERM DELIVERY" = "yellowgreen", "PREVIOUS STILLBIRTH" = "palegreen4",
                                   "PRIMARY INFERTILITY" ="darkgoldenrod2","Rh isoimmunisation"="tan" ,"SHORT STATURE"="tan3"),
                        na.value = "grey")+ggtitle("INDUCTION_INDICATION FOR OBSTETRICS RISK FACTOR")
    
    
    ggplotly(p)
  })    
  #---indi text output--------------
  output$inditext8<-renderText({
    df62<-v$data[,input$variable51]
    p<-paste0("selected indication is:", input$variable52)
    p
  })
  
  #---indication plot---medical------
  output$indiplot8<-renderPlotly({
    df61<-v$data[78:81]
    df62<-v$data[,input$variable52]
    da<-data.frame(df61,df62)
    names(da)<-c("risk1","risk2","risk3","risk4","df1")
    d1<-subset(da,df1=="1")
    d3<-d1
    rows<-seq(1:nrow(d3))
    d2<-data.frame(rows,d3)
    d2$df1<-NULL
    
    dat3 <- melt(d2, id.var = "rows")
    df3 <- dat3 %>% 
      group_by(variable, value) %>% 
      tally() %>% 
      complete(value, fill = list(n = 0)) %>% 
      mutate(percentage = n / sum(n) * 100)
    p<-ggplot(df3,aes(x=variable,y=percentage,fill=value)) + 
      geom_bar(stat = "identity",position="fill")+
      scale_fill_manual(values = c("APH-ABRUPTION GRADE 1"="pink1", "APH-ABRUPTION GRADE 2" = "pink1","APH-ABRUPTION GRADE 3"="pink1","APH-PLACENTA PREVIA"="pink1","APH-UNCLASSIFIED" ="pink1","BIOHAZARD-HCV"="violet","BREECH"="firebrick4",
                                   "ELDERLY PRIMI" = "slateblue1",  "Fibroid complicating pregnancy" = "yellow","GRAND MULTIPARA" ="purple3",
                                   "IUGR"  = "green",
                                   "IVF PREGNANCY" =  "blue2", "MACROSOMIA" = "navyblue", "None" = "black",
                                   "Multiple pregnancy Triplets"  = "tomato" ,"Multiple pregnancy Twins"= "coral2"  , "PPROM"="palevioletred", "Preterm labour"  = "violetred" ,
                                   "PREVIOUS LSCS" =  "red2", "PREVIOUS NEONATAL DEATH" = "springgreen2", "PREVIOUS PRETERM DELIVERY" = "yellowgreen", "PREVIOUS STILLBIRTH" = "palegreen4",
                                   "PRIMARY INFERTILITY" ="darkgoldenrod2","Rh isoimmunisation"="tan" ,"SHORT STATURE"="tan3"),
                        na.value = "grey")+ggtitle("PRIMARY_INDICATION FOR OBSTETRICS RISK FACTOR")
    
    
    ggplotly(p)
    
  })   
  #---indi text output--------------
  output$inditext5<-renderText({
    df62<-v$data[,input$variable51]
    p<-paste0("selected indication is:", input$variable52)
    p
  })
  
  output$indiplot5<-renderPlotly({
    df61<-v$data[73:76]
    df62<-v$data[,input$variable52]
    da<-data.frame(df61,df62)
    names(da)<-c("risk1","risk2","risk3","risk4","df1")
    d1<-subset(da,df1=="1")
    d3<-d1
    rows<-seq(1:nrow(d3))
    d2<-data.frame(rows,d3)
    d2$df1<-NULL
    dat3 <- melt(d2, id.var = "rows")
    # g<- ggplot(data = dat3) +
    #   geom_mosaic(aes(x = product(variable), fill = value)) +
    #   labs(x='', title='mosaic plot') 
    # 
    df3 <- dat3 %>% 
      group_by(variable, value) %>% 
      tally() %>% 
      complete(value, fill = list(n = 0)) %>% 
      mutate(percentage = n / sum(n) * 100)
    p<-ggplot(df3,aes(x=variable,y=percentage,fill=value)) + 
      geom_bar(stat = "identity",position="fill")+
      scale_fill_manual(values = c("Anemia Mild"="green", "Anemia severe" = "green","Anemia Moderate"="green","BIOHAZARD-HCV"="skyblue","BIOHAZARD-HBsAg"="skyblue","BIOHAZARD-HIV"="skyblue",
                                   "BRONCHIAL ASTHMA" = "slateblue1",  "CARDIAC DISEASE-CONGENITAL HEART DISEASE" = "purple3","CARDIAC DISEASE-RHEUMATIC HEART DISEASE" ="purple3","ESSENTIAL/CHRONIC HYPERTENSION"="blue2",
                                   "GDM Diet control" = "firebrick4",  "GDM Insulin"="firebrick4", "GDM OHA"="firebrick4",
                                   "GHTN Drugs" = "coral2", "GHTN Home BP" = "coral2", "LIVER DISORDER-AFLP" = "palevioletred", "LIVER DISORDER-HELLP" = "palevioletred","None" = "black",
                                   "Preeclampsia Mild" = "violetred" ,"Preeclampsia Severe" = "violetred" ,"Eclampsia"= "violetred" , "PREGESTATIONAL DIABETES MELLITUS"="red2", "SEIZURE DISORDERS" = "springgreen2",
                                   "THYROID DISORDERS-HYPERTHYROIDISM" = "darkgoldenrod2", "THYROID DISORDERS-HYPOTHYROIDISM" = "darkgoldenrod2"), 
                        na.value ="grey")+ggtitle(paste("PRIMARY_INDICATION FOR MEDICAL RISK FACTOR",colnames(v$data[,input$variable51])))
    
    
    ggplotly(p)
    
  })
  
  #---indi text output--------------
  output$inditext9<-renderText({
    df62<-v$data[,input$variable51]
    p<-paste0("selected indication is:", input$variable53)
    p
  })
  
  #---indication plot---
  output$indiplot9<-renderPlotly({
    df61<-v$data[78:81]
    df62<-v$data[,input$variable53]
    da<-data.frame(df61,df62)
    names(da)<-c("risk1","risk2","risk3","risk4","df1")
    d1<-subset(da,df1=="1")
    d3<-d1
    rows<-seq(1:nrow(d3))
    d2<-data.frame(rows,d3)
    d2$df1<-NULL
    dat3 <- melt(d2, id.var = "rows")
    
    df3 <- dat3 %>% 
      group_by(variable, value) %>% 
      tally() %>% 
      complete(value, fill = list(n = 0)) %>% 
      mutate(percentage = n / sum(n) * 100)
    p<-ggplot(df3,aes(x=variable,y=percentage,fill=value)) + 
      geom_bar(stat = "identity",position="fill")+
      scale_fill_manual(values = c("APH-ABRUPTION GRADE 1"="pink1", "APH-ABRUPTION GRADE 2" = "pink1","APH-ABRUPTION GRADE 3"="pink1","APH-PLACENTA PREVIA"="pink1","APH-UNCLASSIFIED" ="pink1","BIOHAZARD-HCV"="violet","BREECH"="firebrick4",
                                   "ELDERLY PRIMI" = "slateblue1",  "Fibroid complicating pregnancy" = "yellow","GRAND MULTIPARA" ="purple3",
                                   "IUGR"  = "green",
                                   "IVF PREGNANCY" =  "blue2", "MACROSOMIA" = "navyblue", "None" = "black",
                                   "Multiple pregnancy Triplets"  = "tomato" ,"Multiple pregnancy Twins"= "coral2"  , "PPROM"="palevioletred", "Preterm labour"  = "violetred" ,
                                   "PREVIOUS LSCS" =  "red2", "PREVIOUS NEONATAL DEATH" = "springgreen2", "PREVIOUS PRETERM DELIVERY" = "yellowgreen", "PREVIOUS STILLBIRTH" = "palegreen4",
                                   "PRIMARY INFERTILITY" ="darkgoldenrod2","Rh isoimmunisation"="tan" ,"SHORT STATURE"="tan3"),
                        na.value = "grey")+ggtitle("SECONDARY_INDICATION FOR OBSTETRICS RISK FACTOR")
    
    
    ggplotly(p)
  })
  #---indi text output--------------
  output$inditext6<-renderText({
    df62<-v$data[,input$variable51]
    p<-paste0("selected indication is:", input$variable53)
    p
  })
  
  output$indiplot6<-renderPlotly({
    df61<-v$data[73:76]
    df62<-v$data[,input$variable53]
    da<-data.frame(df61,df62)
    names(da)<-c("risk1","risk2","risk3","risk4","df1")
    d1<-subset(da,df1=="1")
    d3<-d1
    rows<-seq(1:nrow(d3))
    d2<-data.frame(rows,d3)
    d2$df1<-NULL
    dat3 <- melt(d2, id.var = "rows")
    # g<- ggplot(data = dat3) +
    #   geom_mosaic(aes(x = product(variable), fill = value)) +
    #   labs(x='', title='mosaic plot') 
    # 
    df3 <- dat3 %>% 
      group_by(variable, value) %>% 
      tally() %>% 
      complete(value, fill = list(n = 0)) %>% 
      mutate(percentage = n / sum(n) * 100)
    p<-ggplot(df3,aes(x=variable,y=percentage,fill=value)) + 
      geom_bar(stat = "identity",position="fill")+
      scale_fill_manual(values = c("Anemia Mild"="green", "Anemia severe" = "green","Anemia Moderate"="green","BIOHAZARD-HCV"="skyblue","BIOHAZARD-HBsAg"="skyblue","BIOHAZARD-HIV"="skyblue",
                                   "BRONCHIAL ASTHMA" = "slateblue1",  "CARDIAC DISEASE-CONGENITAL HEART DISEASE" = "purple3","CARDIAC DISEASE-RHEUMATIC HEART DISEASE" ="purple3","ESSENTIAL/CHRONIC HYPERTENSION"="blue2",
                                   "GDM Diet control" = "firebrick4",  "GDM Insulin"="firebrick4", "GDM OHA"="firebrick4",
                                   "GHTN Drugs" = "coral2", "GHTN Home BP" = "coral2", "LIVER DISORDER-AFLP" = "palevioletred", "LIVER DISORDER-HELLP" = "palevioletred","None" = "black",
                                   "Preeclampsia Mild" = "violetred" ,"Preeclampsia Severe" = "violetred" ,"Eclampsia"= "violetred" , "PREGESTATIONAL DIABETES MELLITUS"="red2", "SEIZURE DISORDERS" = "springgreen2",
                                   "THYROID DISORDERS-HYPERTHYROIDISM" = "darkgoldenrod2", "THYROID DISORDERS-HYPOTHYROIDISM" = "darkgoldenrod2"), 
                        na.value ="grey")+ggtitle(paste("SECONDARY_INDICATION FOR MEDICAL RISK FACTOR",colnames(v$data[,input$variable51])))
    
    
    ggplotly(p)
  })  
  #download page----------------
  
  output$report <- downloadHandler(
    # For PDF output, change this to "report.pdf"
    filename = "report.html",
    content = function(file) {
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      tempReport <- file.path(tempdir(), "input.Rmd")
      file.copy("input.Rmd", tempReport, overwrite = TRUE)
      
      # Set up parameters to pass to Rmd document
      para<- dfSummary(v$data)
      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app).
      rmarkdown::render(tempReport, output_file = file,
                        para = para,
                        envir = new.env(parent = globalenv())
      )
    }
  )
  #-----------
  # output$summary4<- renderPrint({
  #   df11<-v$data
  #   df11<-df11[,input$variable11]
  #   summary(df11)
  # })
  # #summary-------------------------
  output$summary4<-renderPrint({
    df11<- v$data
    df12<-v$data
    df13<-v$data
    df11<- df11[,input$variable11]
    df12<- df12[,input$variable12]
    df13<- df13[,input$variable13]
    
    if ((is.factor(df11)) & (is.factor(df12) | is.numeric(df12) | is.integer(df12)) & (is.factor(df13) | is.numeric(df13) | is.integer(df13))){
      
      table(df11,df12,df13,dnn =c(paste(input$variable11[1]),paste(input$variable12[1]),paste(input$variable13[1])))
      # mhor(df,df1,df2)
      # epi.2by2(table(df,df1,df2))
      #crosstab(da,row.vars = c("df","df1"),col.vars = "df2",type = "f")
    }
    else if ((is.integer(df11) | is.numeric(df11)) & (is.factor(df12) | is.numeric(df12) | is.integer(df12)) & (is.factor(df13) | is.numeric(df13) | is.integer(df13))){
      library(data.table)
      #df11<- v$data[,input$variable11]
      #df12<- v$data[,input$variable12]
      #df13<- v$data[,input$variable13]
      #aggregate(df11 ~ df12 + df13, data=v$data, mean)
      with(v$data, tapply(df11, list(df1=df12,df2=df13), mean) )
    }
  })
  
  
  # # # #summary-------------------------
  output$summary7<-renderPrint({
    df11<- v$data
    df12<-v$data
    df13<-v$data
    df11<- df11[,input$variable11]
    df12<- df12[,input$variable12]
    df13<- df13[,input$variable13]
    df<-df11
    df1<-df12
    df2<-df13
    if (is.factor(df) & is.factor(df1) & is.factor(df2)){
      
      # df<- names((input$variable1[1])
      # df1<-paste(input$variable[1])
      # table(df,df1,df2,dnn =c(paste(input$variable1[1]),paste(input$variable[1]),paste(input$variable[1])))
      mhor(df,df1,df2)
      # epi.2by2(table(df,df1,df2))
      #crosstab(da,row.vars = c("df","df1"),col.vars = "df2",type = "f")
    }
    else {
      print("NULL")
    }
  })
  
  
  #outlier
  output$summary5<- renderPrint({
    df5<-v$data
    df6<-df5[,input$variable5]
    if (is.integer(df6) |is.numeric(df6) ){
      outliers<-boxplot(df6, plot=FALSE)$out
      length(outliers)  
    }
    else {
      print("NULL")
    }
    #remove outliers
    #df5[-which(df6 %in% outliers),]
  })
  #----missing value---summary------
  output$miss3<- renderPrint({
    df7<-v$data[1:39]
    df7<-df7[,input$misvariable1]
    if (is.integer(df7) | is.numeric(df7)){
      
      count<-sum(is.na(df7))
      Total=length(df7)
      percentage<-(count/length(df7))*100
      percent<-paste(round(percentage,2),"%",sep="")
      data.frame(count,percent)
      
    }
    else if(is.factor(df7)){
      df7<-replace.empty(df7)
      count<-sum(is.na(df7))
      Total=length(df7)
      percentage<-(count/length(df7))*100
      percent<-paste(round(percentage,2),"%",sep="")
      data.frame(count,percent)
    } 
  })
  #missing value
  output$miss4<- renderPrint({
    df7<-v$data[40:71]
    df7<-df7[,input$misvariable2]
    if (is.integer(df7) | is.numeric(df7)){
      
      count<-sum(is.na(df7))
      Total=length(df7)
      percentage<-(count/length(df7))*100
      percent<-paste(round(percentage,2),"%",sep="")
      data.frame(count,percent)
      
    }
    else if(is.factor(df7)){
      df7<-replace.empty(df7)
      count<-sum(is.na(df7))
      Total=length(df7)
      percentage<-(count/length(df7))*100
      percent<-paste(round(percentage,2),"%",sep="")
      data.frame(count,percent)
    } 
  })
  
  # #removing variable
  # output$summary8<-renderPrint({
  #   df8<-myData()
  #   df9<-df8[,input$variable8]
  #   df9<-NULL
  #   df9
  # })
  
  #Trend chart summary------------------------  
  output$mean<-renderPrint({
    options(digits=4)
    df41<- v$data
    df42<- v$data
    df43<-v$data
    df41<- df41[,input$variable41]
    df42<- df42[,input$variable42]
    df43<- df43[,input$variable43]
    
    if((is.integer(df41) | is.numeric(df41)) & (is.integer(df42) | is.numeric(df42)|is.factor(df42) | is.character(df43)) & (is.integer(df43) | is.numeric(df43)|is.factor(df43)| is.character(df43))){
      
      ds<-aggregate(df41 ~ df42+df43,data=v$data,mean,na.rm=T)
      names(ds)<-c("year","unit1","df")
      ds
    }
    else if((is.integer(df41) | is.numeric(df41)| is.factor(df41)| is.character(df41)) & (is.integer(df42) | is.numeric(df42)|is.factor(df42)| is.character(df43)) & (is.integer(df43) | is.numeric(df43)|is.factor(df43)| is.character(df43))){
      df<-data.frame(df41,df42,df43)
      names(df)<-c("df1","year","unit1")
      
      a<-as.data.frame(table(df[,c("df1","year","unit1")]))
      a
    }
    
  })  
  
  # #----labels---
  #   output$labels1 <- renderPrint({
  #     l1<-v$data[40:70]  
  #     l2<-v$data[86:145]
  #     l<-cbind(l1,l2)
  #     l5<-label(l)
  #     transform(l5)
  #  })
  #----labels---
  # output$labels2 <- renderPrint({
  #   
  #   input <- c("M.anemia",
  #              "decrease in the total red blood cells or hemoglobin in the blood during pregnancy or in the period following pregnancy.",
  #              "M.HBSG",
  #              "he presence of HBsAg indicates ongoing HBV infection, and in newly infected persons, HBsAg is the only serologic marker detected during the first 3--5 weeks after infection.",
  #              "M.HCV",
  #              " ",
  #              "M.HIV",
  #              "HIV can pass through the placenta and infect the fetus. During labor and delivery, the baby may be exposed to the virus in the mother's blood and other fluids",
  #              "M.ASTHMA",
  #              "chronic inflammatory disease of the airways that is characterized by increased responsiveness of the tracheobronchial tree to multiple stimuli.",
  #              "M.CONGENIT",
  #              
  #              "congenital heart disease (CHD) at risk of pregnancy is growing because over 90% of them are grown-up into adulthood.",
  #              "M.RHEUMAT",
  #              
  #              "Rheumatic heart disease (RHD) is the most common acquired heart disease in pregnancy RHD is a chronic acquired heart disorder resulting from acute rheumatic fever..",
  #              "M.ECLAMPSIA",
  #              "Eclampsia is a condition that causes a pregnant woman, usually previously diagnosed with preeclampsia (high blood pressure and protein in the urine), to develop seizures",
  #              "M.CHTN",
  #              "Chronic hypertension is high blood pressure that was present before you became pregnant or that occurs in the first. half (before 20 weeks) of your pregnancy",
  #              "M.GDM",
  #              "Gestational diabetes mellitus (GDM) is defined as any degree of glucose intolerance with onset or first recognition during pregnancy.",
  #              "M.GHTN",
  #              "new onset of hypertension after 20 weeks' gestation in the absence of proteinuria or systemic findings such as thrombocytopenia or impaired liver function",
  #              "M.LIVER",
  #              "",
  #              "M.PECLAMPSIA",
  #              " ",
  #              "M.PGDM",
  #              " ",
  #              "M.SEIZUR",
  #              "Slowing of the fetal heart rate. Decreased oxygen to the fetus. Fetal injury, premature separation of the placenta from the uterus (placental abruption) or miscarriage due to trauma, such as a fall, during a seizure.",
  #              "M.THYROID",
  #              "Hypothyroidism is a condition marked by an underactive thyroid gland and may be present during pregnancy.Having low thyroid hormone levels may even interfere with becoming pregnant or be a cause of miscarriage.",
  #              "O.APH",
  #              "",
  #              "O.BREECH",
  #              "breech pregnancy occurs when the baby is positioned head-up in the woman's uterus.",
  #              "O.ELDERLY",
  #              "The elderly primigravida is defined as a woman who goes into pregnancy for the first time at the age of 35 years or older.",
  #              "O.FIBROID",
  #              " ",
  #              "O.PARA",
  #              " ",
  #              "O.IUGR",
  #              "Intrauterine growth restriction (IUGR) refers to a condition in which an unborn baby is smaller than it should be because it is not growing at a normal rate inside the womb. Delayed growth puts the baby at risk of certain health problems during pregnancy, delivery, and after birth,Low birth weight.",
  #              "O.IVF",
  #              "",
  #              "O.MULTIPRE",
  #              " ",
  #              "O.PPROM",
  #              "Preterm premature rupture of membranes (PPROM) is ROM prior to 37 weeks' gestation.",
  #              "O.PRETERM",
  #              " ",
  #              "O.LSCS",
  #              "Women, with one previous lower segment caesarean section (LSCS), live pregnancy with haemoglobin ≥8 g/dl. ... TOL refers to trial for vaginal delivery, which may end as successful vaginal birth after caesarean (VBAC) or failed TOL resulting in repeat section.",
  #              "O.NEONATAL",
  #              " ",
  #              "O.STILBIRTH",
  #              "",
  #              "O.INFERTILITY ",
  #              "Primary infertility refers to couples who have not become pregnant after at least 1 year having sex without using birth control methods. Secondary infertility refers to couples who have been able to get pregnant at least once, but now are unable",
  #              "O.STATURE",
  #              "Uterine rupture usually occurs at the site of deficient cesarean scars.",
  #              "P.DESCENT",
  #              "The head of the fetus is in the same place in the birth canal during the first and second examinations, which your doctor performs one hour apart.",
  #              "P.DILATATION",
  #              "arrest of dilation is when the cervix is 6 centimeters dilated during the first and second examinations, which your doctor performs one to two hours apart.",
  #              "P.BREECH",
  #              "",
  #              
  #              "P.CORD",
  #              "A cord presentation (also known as a funic presentation) is a variation in the fetal presentation where the umbilical cord points towards the internal cervical os or lower uterine segment.",
  #              "P.CPD",
  #              "",
  #              
  #              "P.DCDA",
  #              "DCDA.TWINS",
  #              "P.DMC",
  #              "Rheumatic heart disease (RHD) is the most ",
  #              "P.FAIL",
  #              "Failure to progress (FTP) happens when labor slows and delays delivery of the baby. Before labor starts, the cervix thins out and starts to open.",
  #              "P.IUGR",
  #              "",
  #              "P.NRFS",
  #              "Nonreassuring fetal status (NRFS) is a term that may be used to describe a baby's health late in the pregnancy or during labor. baby may not be getting enough oxygen.",
  #              "P.PLACENTA.PREVIA",
  #              "Placenta previa is a problem of pregnancy in which the placenta grows in the lowest part of the womb (uterus) and covers all or part of the opening to the cervix.",
  #              "P.PPROM",
  #              "",
  #              "P.LSCS",
  #              "",
  #              "P.PLSCS",
  #              "",
  #              "PBREECH",
  #              "breech presentation who delivered preterm.",
  #              "PDILATATION",
  #              "Protracted labor is abnormally slow cervical dilation or fetal descent during active labor.Active labor usually occurs after the cervix dilates to ≥ 4 cm.",
  #              "P.SCAR",
  #              "",
  #              "P.SPE",
  #              "Superimposed pre-eclampsia can occur without severe features when there is new onset or a sudden worsening of proteinuria or blood pressure (to <160 mm Hg systolic and 105 mm Hg diastolic).",
  #              "P.TWINS",
  #              "",
  #              "P.CERVIX",
  #              "The cervix is said to be unfavourable for induction of labour if the Bishop's score is 6 or less. These women will need to undergo cervical ripening which can be by means of a prostaglandin based gel/controlled release or a transcervical foley catheter.",
  #              "S.DOPPLER",
  #              "Doppler ultrasound uses sound waves to detect the movement of blood in vessels. It is used in pregnancy to study blood circulation in the baby, uterus and placenta.",
  #              "S.BREECH",
  #              "",
  #              "S.CAT.II",
  #              "",
  #              "S.CAT.III",
  #              "",
  #              "S.CHRIOAMNI",
  #              "Chorioamnionitis is a bacterial infection that occurs before or during labor. The name refers to the membranes surrounding the fetus the chorion (outer membrane) and the amnion.",
  #              "S.CPD",
  #              "",
  #              "S.FECV",
  #              "explains external cephalic version (ECV), which tries to turn breech babies to the head-down position ready for a normal vaginal birth.",
  #              "S.IUGR ",
  #              "",
  #              "S.MSAF",
  #              "Neonatology. Meconium aspiration syndrome (MAS) also known as neonatal aspiration of meconium is a medical condition affecting newborn infants. It describes the spectrum of disorders and pathophysiology of newborns born in meconium-stained amniotic fluid (MSAF).",
  #              "S.NRFS",
  #              "",
  #              "S.OLIGOHY",
  #              "Oligohydramnios is the condition of having too little amniotic fluid. ... About 8% of pregnant women can have low levels of amniotic fluid, with about 4% being diagnosed with oligohydramnios.",
  #              "S.PLACENTA ",
  #              "",
  #              "S.PPROM",
  #              "",
  #              "S.LSCS",
  #              "",
  #              "S.PBREECH",
  #              "",
  #              "S.PROM",
  #              "Premature rupture of membranes (PROM) refers to a patient who is beyond 37 weeks' gestation and has presented with rupture of membranes (ROM) prior to the onset of labor.",
  #              "S.RLSCS",
  #              "",
  #              "S.SPE",
  #              "",
  #              "S.TWINS",
  #              "",
  #              "S.CERVIX",
  #              "",
  #              "I.CHTN",
  #              "",
  #              "I.ELDERLY",
  #              "",
  #              "I.GDM",
  #              "",
  #              "I.GHTN",
  #              "",
  #              "I.HYDROCEPHA",
  #              "Hydrocephalus is a condition associated with a buildup of cerebrospinal fluid (CSF) in or around the brain. If left untreated, this can lead to brain tissue stretching, significantly affecting your child’s growth and development.",
  #              "I.THYROID",
  #              "",
  #              "I.INFERTILITY",
  #              "",
  #              "I.IUD",
  #              "IUD (intrauterine contraceptive device): A device inserted into the uterus (womb) to prevent conception (pregnancy). The IUD can be a coil, loop, triangle, or T in shape made of plastic or metal",
  #              "I.IUGR",
  #              "",
  #              "I.Low.AFI",
  #              "Low amniotic fluid (oligohydramnios) is a condition in which the amniotic fluid measures lower than expected for a baby's gestational age. No treatment has been proved effective long term. But short-term improvement of amniotic fluid is possible and might be done in certain circumstances.",
  #              "I.MILD.PE",
  #              "Pulmonary embolisms (PE) typically occur during or shortly after the labor and delivery, and may be fatal for the mother if not treated immediately.",
  #              "I.OBESE",
  #              "Obesity is defined as having an excessive amount of body fat",
  #              "I.POST.DATES",
  #              "Postterm pregnancy is dethat has extended to or beyond 42 weeks of gestation (294 days).",
  #              "I.PPROM ",
  #              "",
  #              "I.PREVIOUS.IUD",
  #              "",
  #              "I.PROM",
  #              "",
  #              "I.RH.NEGATIVE ",
  #              "Rhesus (Rh) factor is an inherited protein found on the surface of red blood cells. If your blood has the protein, you're Rh positive. If your blood lacks the protein, you're Rh negative.",
  #              "I.SGA",
  #              "SGA babies usually have birthweights below the 10th percentile for babies of the same gestational age.",
  #              "I.SPE",
  #              "",
  #              "I.TWINS ",
  #              ""
  #   )
  #   
  #   library(lubridate)
  #   df <- matrix(input, ncol = 2, byrow = TRUE) %>% 
  #     as_tibble() %>% 
  #     mutate(V1 = (V1), V2 = (V2))
  #   p<-seq(1:91)
  #   a<-data.frame(p,df$V2)
  #   w<-df$V1
  #   name.width <- max(sapply(names(a), nchar))
  #   a2<-format(a, width = name.width, justify = "left")
  #   l1<-an2[40:70]  
  #   l2<-an2[86:145]
  #   l<-cbind(l1,l2)
  #   l5<-label(l)
  #   r3<-transform(l5)
  #   r3<-data.frame(r3)
  #     
  #   a1<-r3$X_data
  #   a3<-cbind(w,a1,a2)
  #   a3$p<-NULL
  #   names(a3)<-c("variable_name","labels","definitions")
  #  
  #   a3
  #   
  # })
  #----labels---
  output$labels3 <- renderPrint({
    cat("link:","https://www.ncbi.nlm.nih.gov/pmc/articles.", sep = "\n")
    cat("link(2):","https://emedicine.medscape.com/article.", sep = "\n")
    cat("link(3):","https://www.ncbi.nlm.nih.gov/pubmed/25185379.", sep = "\n")
    cat("link(4):","https://care.diabetesjournals.org/content/26/suppl_1/s103.", sep = "\n")
    cat("link(5):","https://www.hopkinsmedicine.org/health/conditions-and-diseases/staying-healthy-during-pregnancy/hypothyroidism-and-pregnancy.", sep = "\n")
    cat("link(6):","https://www.medicinenet.com/script/main/art.asp?articlekey=4063.", sep = "\n")
    cat("link(7):","https://www.webmd.com/baby/iugr-intrauterine-growth-restriction#1.", sep = "\n")
    cat("link(8):","https://www.mayoclinic.org/healthy-lifestyle/pregnancy-week-by-week/expert-answers/low-amniotic-fluid/faq-20057964.", sep = "\n")
    cat("link(9):","https://www.urmc.rochester.edu/encyclopedia/content.aspx?ContentTypeID=90&ContentID=P02496.", sep = "\n")
    cat("link(10):","https://emedicine.medscape.com/article/261137-overview.", sep = "\n")
    cat("link(11):","https://www.ncbi.nlm.nih.gov/pubmed/12972014.", sep = "\n")
    
  })
  
  #outliers
  eventReactive(input$goButton, {
    df5<-data.frame(v$data)
    df6<-df5[,input$variable5]
    input$variale5 <- df5[-which(df6 %in% outliers),]
  })
  # outliers plot-------------------------
  output$plotlier <- renderPlotly({
    df5<-v$data
    df6<-df5[,input$variable5]
    
    if (is.integer(df6) |is.numeric(df6) ){
      r<- ggplot(v$data, aes(x = "", y = df6)) +geom_boxplot(fill="lightblue")
      r<- ggplotly(r)
      r  
    }
    else {
      df6<-df5[,input$variable5]
      plot_ly(x=names(table(df6)),y= as.numeric(table(df6)),type="bar")
    }
  })
  #missing plot1---------
  output$missplot1<-renderPlot({
    df7<-v$data[1:39]
    # df7<-df7[,input$misvariable1]
    # df7<-data.frame(df7)
    gg_miss_var(df7)
  })
  #missing plot--2---------
  output$missplot2<-renderPlot({
    df17<-v$data[40:71]
    # df17<-df17[,input$misvariable2]
    # df17<-data.frame(df17)
    gg_miss_var(df17)
  })
  
  
  #
  output$plot4<-renderPlotly({
    df<- v$data
    df<- df[,input$variable1]
    if(is.integer(df) |is.numeric(df) ){
      plot_ly(x=df,type="histogram")%>%
        layout(xaxis = list(title = 'df'),yaxis = list(title = 'Frequency'),title = "outcome variable(df)")
      
    }
    else {
      plot_ly(x=names(table(df)),y= as.numeric(table(df)),type="bar")%>%
        layout(xaxis = list(title = 'df'),yaxis = list(title = 'Count'),title = "outcome variable(df)")
    }
  })
  
  output$plot5<-renderPlotly({
    df1<- v$data
    df1<- df1[,input$variable]
    if(is.factor(df1)){
      plot_ly(x=names(table(df1)),y= as.numeric(table(df1)),type="bar")%>%
        layout(xaxis = list(title = 'df1'),yaxis = list(title = 'Count'),title = "independent variable(df1)")
    }
    else {
      plot_ly(x=df1,type="histogram")%>%
        layout(xaxis = list(title = 'df1'),yaxis = list(title = 'Frequency'),title = "independent variable(df1)")
    }
  })
  #
  output$plot6 <- renderPlotly({
    df<-v$data
    df1<-v$data
    df <- df[,input$variable1]
    df1<- df1[,input$variable]  
    if(is.factor(df) & is.factor(df1)){
      #mosaic(~ df1+df, data = v$data)
      an<-data.frame(df,df1)
      names(an)<-c("df","df1")
      g<- ggplot(data = an) +
        geom_mosaic(aes(x = product(df), fill = df1), na.rm=TRUE) +
        labs(x='', title='mosaic plot') 
      g<- ggplotly(g)
      g
      
    }
    else if (is.factor(df) & (is.integer(df1)|is.numeric(df1))){
      wr<- data.frame(df,df1)
      names(wr)<-c("df","df1")
      p <- ggplot(wr, aes(x=df, y=df1, fill=df)) + geom_boxplot()
      p <- ggplotly(p)
      p
    }
    else if ((is.integer(df)|is.numeric(df))& (is.integer(df1) | is.numeric(df1))){
      wr<- data.frame(df,df1)
      names(wr)<-c("df","df1")
      plot_ly(wr,x = ~df,y= ~df1,type = "scatter",mode = 'markers')%>%
        layout(title = "scatter plot")
      
    }
    else if ((is.integer(df)|is.numeric(df)) & is.factor(df1)) {
      wr<- data.frame(df,df1)
      names(wr)<-c("df","df1")
      p <- ggplot(wr, aes(x=df1, y=df, fill=df1)) + geom_boxplot()
      p <- ggplotly(p)
      p
      
    }
  })
  
  output$plot3<-renderPlotly({
    df11<- v$data
    df11<- df11[,input$variable11]
    df12<- v$data
    df12<- df12[,input$variable12]
    df13<- v$data
    df13<- df13[,input$variable13]
    if(is.factor(df11) & is.factor(df12) & is.factor(df13)){
      w<-data.frame(df11,df12,df13)
      names(w)<-c("df","df1","df2")
      a<- ggplot(data = w) +
        geom_mosaic(aes(x = product(df, df1), fill=df2), na.rm=TRUE) +
        labs(x = "", title='mosaic plot')+theme(axis.text.x = element_text(angle = 55, hjust = 1))
      a<-ggplotly(a)
      a
    }
    else if (is.factor(df11) & is.factor(df12) & (is.integer(df13) | is.numeric(df13)) ){
      
      w<-data.frame(df11,df12,df13)
      f<-na.omit(w)
      
      names(f)<-c("df","df1","df2")
      t<- ggplot(f, aes(x = df, y = df2, fill = df1) ) +
        geom_bar(stat="identity", position = 'dodge') +theme(axis.text.x = element_text(angle = 55, hjust = 1))
      t<-ggplotly(t)
      t
    }
    else if (is.factor(df11) & (is.numeric(df12) | is.integer(df12)) & is.factor(df13)) {
      
      ae<-data.frame(df11,df12,df13)
      f<-na.omit(ae)
      names(f)<-c("df","df1","df2")
      pq<- ggplot(f, aes(df, df1)) +
        geom_boxplot(aes(color = df2 ))+
        facet_wrap(~df2)
      pq<-ggplotly(pq)
      pq
    }
    else if ((is.integer(df11)| is.numeric(df11)) & is.factor(df12) & is.factor(df13)){
      aa<-data.frame(df11,df12,df13)
      f<-na.omit(aa)
      names(f)<-c("df","df1","df2")
      pq<- ggplot(f, aes(df1, df)) +
        geom_boxplot(aes(color = df2 ))+
        facet_wrap(~df2)
      pq<-ggplotly(pq)
      pq
    }
    
    else if ((is.integer(df11) | is.numeric(df11)) & (is.integer(df12) | is.numeric(df12)) & is.factor(df13)){
      e<-data.frame(df11,df12,df13)
      f<-na.omit(e)
      names(f)<-c("df","df1","df2")
      q <- ggplot(f, aes(df, df1)) +
        geom_point(aes(color = df2), size = 3, alpha = 0.6) +
        facet_wrap(~df2)
      q<-ggplotly(q)
      q
      
    }
    else if((is.integer(df11) | is.numeric(df11)) & is.factor(df12) & (is.integer(df13) | is.numeric(df13))){
      w<-data.frame(df11,df12,df13)
      f<-na.omit(w)
      names(f)<-c("df","df1","df2")
      
      p<-plot_ly(f, x = ~df, y = ~interaction(df1, df2)) %>%
        add_boxplot(color = ~df1) %>%
        layout(yaxis = list(title = ""))
      p
    }
    else if (is.factor(df11) & (is.integer(df12) | is.numeric(df12)) & (is.integer(df13) | is.numeric(df13))){
      w<-data.frame(df11,df12,df13)
      f<-na.omit(w)
      names(f)<-c("df","df1","df2")
      
      p<-plot_ly(f, x = ~df2, y = ~interaction(df1, df)) %>%
        add_boxplot(color = ~df) %>%
        layout(yaxis = list(title = ""))
      p
    }
    else {
      print("NULL")
    }
  })
  #   else if ((is.integer(df11) | is.numeric(df11)) & (is.integer(df12) | is.numeric(df12)) & (is.integer(df13) | is.numeric(df13))){
  #     w<-data.frame(df11,df12,df13)
  #     names(w)<-c("df","df1","df2")
  #     p <- plot_ly(w, x = ~df, y = ~df1, z = ~df2, colors = c('#BF382A', '#0C4B8E')) %>%
  #       add_markers() %>%
  #       layout(title = "scatter plot")
  #     p
  #     
  #     
  #   }
  #trend plot-------------------------------
  output$trend<-renderPlotly({
    options(digits=4)
    df41<- v$data
    df42<- v$data
    df43<-v$data
    df41<- df41[,input$variable41]
    df42<- df42[,input$variable42]
    df43<- df43[,input$variable43]
    
    if((is.integer(df41) | is.numeric(df41)) & (is.integer(df42) | is.numeric(df42)|is.factor(df42)| is.character(df43)) & (is.integer(df43) | is.numeric(df43)|is.factor(df43)| is.character(df43))){
      
      ds<-aggregate(df41~df42+df43,data=v$data,mean)
      names(ds)<-c("year","unit1","df")
      p<- ggplot(ds, aes(x = year, y = df)) + geom_line(aes(color = unit1), size = 1)
      p<-ggplotly(p)
      p
      
    }
    else if((is.integer(df41) | is.numeric(df41)| is.factor(df41)| is.character(df41)) & (is.integer(df42) | is.numeric(df42)|is.factor(df42)| is.character(df43)) & (is.integer(df43) | is.numeric(df43)|is.factor(df43)| is.character(df43))){
      
      df<-data.frame(df41,df42,df43)
      names(df)<-c("df1","year","unit1")
      
      a<-as.data.frame(table(df[,c("df1","year","unit1")]))
      q <- ggplot(a,aes(x=year, y=Freq, colour=df1, group=df1)) +
        geom_line() +
        facet_wrap(~unit1, scales="free_y")
      
      q<-ggplotly(q)
      q
      
    }
  })
  #--risk plot----  
  output$riskplot1<-renderPlotly({
    df31<- v$data
    df32<- v$data
    df31<- df31[,input$variable31]
    df32<- df32[,input$variable32]
    ee<-data.frame(df31,df32)
    names(ee)<-c("df1","df2")
    g<- ggplot(data = ee) +
      geom_mosaic(aes(x = product(df1), fill = df2), na.rm=TRUE) +
      labs(x='', title='mosaic plot') 
    g<- ggplotly(g)
    g
  })
  output$sss<-renderPrint({
    dg<-v$data
    df71<-v$data
    options(digits = 5)
    df72<-data.frame(df71$BABY.WEIGHT1,df71$AGE1,df71$MOTHER.HEIGHT1,df71$MOTHER.HEIGHT,df71$PLACENTA.WEIGHT.Gms.1,df71$GEST.WEEKS1)
    names(df72)<-c("BABY.WEIGHT1","AGE1","MOTHER.HEIGHT1","MOTHER.WEIGHT1","PLACENTA.WEIGHT.Gms.1","GEST.WEEKS1")
    
    df73<-df72[,input$variable73]
    
    t<-t.test(df73~dg$BABY.SEX)
    t1<-t.test(df73~dg$BOOKED)
    t2<-t.test(df73~dg$ONSET.OF.LABOUR)
    t3<-t.test(df73~dg$LSCS.TYPE)
    p<-data.frame(A = round(t$estimate[[1]], digits = 4),
                  B = round(t$estimate[[2]], digits = 4),
                  CI = paste('(',round(t$conf.int[[1]], 
                                       digits = 4),', ',
                             round(t$conf.int[[2]], 
                                   digits = 4), ')',
                             sep=""), pvalue = t$p.value,
                  A1 = round(t1$estimate[[1]], digits = 4),
                  B1 = round(t1$estimate[[2]], digits = 4),
                  CI1 = paste('(',round(t1$conf.int[[1]], 
                                        digits = 4),', ',
                              round(t1$conf.int[[2]], 
                                    digits = 4), ')',
                              sep=""), pvalue1 = t1$p.value,
                  
                  A2 = round(t2$estimate[[1]], digits = 4),
                  B2 = round(t2$estimate[[2]], digits = 4),
                  CI2 = paste('(',round(t2$conf.int[[1]], 
                                        digits = 4),', ',
                              round(t2$conf.int[[2]], 
                                    digits = 4), ')',
                              sep=""), pvalue2 = t2$p.value,
                  A3 = round(t3$estimate[[1]], digits = 4),
                  B3 = round(t3$estimate[[2]], digits = 4),
                  CI3 = paste('(',round(t3$conf.int[[1]], 
                                        digits = 4),', ',
                              round(t3$conf.int[[2]], 
                                    digits = 4), ')',
                              sep=""), pvalue3 = t3$p.value)
    
    
    p$CI<-as.character(p$CI)
    p$CI1<-as.character(p$CI1)
    p$CI2<-as.character(p$CI2)
    p$CI3<-as.character(p$CI3)
    
    variable1<-c("SEX","","BOOKED","","ONSET.OF.LABOUR","","LSCS.TYPE","")
    label1<-c("M","F","I","O","INDUCED","SPONTANEOUS","ELECTIVE","EMERGENCY")
    
    mean1<-c(1,2,3,4,5,6,7,8)
    
    pvalue2<-c(1,2,3,4,5,6,7,8)
    
    diff<-c("","","","","","","","")
    
    confidence.interval1<-c("_","_","_","_","_","_","_","_")
    
    adj_pvalue<-c("_","_","_","_","_","_","_","_")
    
    df11<-data.frame(variable1,label1,mean1,pvalue2,diff,confidence.interval1,adj_pvalue)
    names(df11)<-c("Variable","Levels","mean","Pvalue","Difference","Confidence.interval","Adj_pvalue")
    
    df11$mean<-c(p$A,p$B,p$A1,p$B1,p$A2,p$B2,p$A3,p$B3)
    
    df11$Pvalue<-c(p$pvalue,"",p$pvalue1,"",p$pvalue2,"",p$pvalue3,"")
    
    df11$Confidence.interval<-c(p$CI,"",p$CI1,"",p$CI2,"",p$CI3,"")
    
    #df11<-data.frame(variable1,label1,mean1,pvalue2,diff,confidence.interval1,adj_pvalue)
    
    df22<-df11
    
    #---
    
    p1<-aov(df73~dg$MODE.OF.INDUCTION)
    p2<-summary(p1)[[1]][["Pr(>F)"]]
    a<-aggregate(df73~dg$MODE.OF.INDUCTION,FUN="mean")
    names(a)<-c("mode.of.induction","weight")
    variable1<-c("MODE.OF.INDUCTION","","","")
    
    label1<-c("FOLEYS","NONE","PGE1","PGE2")
    
    mean1<-c(a$weight[1],a$weight[2],a$weight[3],a$weight[4])
    
    pvalue2<-c(p2[1],"","","")
    
    diff<-c("","","","")
    
    confidence.interval1<-c("_","_","_","_")
    adj_pvalue<-c("_","_","_","_")
    
    df2<-data.frame(variable1,label1,mean1,pvalue2,diff,confidence.interval1,adj_pvalue)
    names(df2)<-c("Variable","Levels","mean","Pvalue","Difference","Confidence.interval","Adj_pvalue")
    #-----
    p1<-aov(df73~dg$unit1)
    
    p2<-summary(p1)[[1]][["Pr(>F)"]]
    
    p22<-round(p2[1],digits = 4)
    t<-TukeyHSD(p1)
    
    p3<-aov(df73~dg$BLOOD.LOSS)
    
    p4<-summary(p3)[[1]][["Pr(>F)"]]
    
    p44<-round(p4[1],digits = 4)
    t1<-TukeyHSD(p3)
    a<-aggregate(df73~dg$unit1,FUN="mean")
    names(a)<-c("unit","weight")
    
    a1<-aggregate(df73~dg$BLOOD.LOSS,FUN="mean")
    names(a1)<-c("blood","weight")
    
    Cf = paste('(',round(t$`dg$unit1`[4], 
                         digits = 4),', ',
               round(t$`dg$unit1`[7], 
                     digits = 4), ')',
               sep="")
    
    Cf1 = paste('(',round(t$`dg$unit1`[5], 
                          digits = 4),', ',
                round(t$`dg$unit1`[8], 
                      digits = 4), ')',
                sep="")
    
    Cf2 = paste('(',round(t$`dg$unit1`[6], 
                          digits = 4),', ',
                round(t$`dg$unit1`[9], 
                      digits = 4), ')',
                sep="")
    
    
    variable1<-c("UNIT","","","BLOOD LOSS","","")
    
    label1<-c("OG3","OG4","OG5","< 500 M","> 1000 ML","500 - 1000 ML")
    
    mean1<-c(a$weight[1],a$weight[2],a$weight[3],a1$weight[1],a1$weight[2],a1$weight[3])
    
    pvalue2<-c(p22,"","",p44,"","")
    l1<-round(t$`dg$unit1`[1],digits = 4)
    l2<-round(t$`dg$unit1`[2],digits = 4)
    l3<-round(t$`dg$unit1`[3],digits = 4)
    l4<-round(t$`dg$unit1`[10],digits = 4)
    l5<-round(t$`dg$unit1`[11],digits = 4)
    l6<-round(t$`dg$unit1`[12],digits = 4)
    
    diff<-c("_","_","_","_","_","_")
    
    confidence.interval1<-c("_","_","_","_","_","_")
    adj_pvalue<-c("_","_","_","_","_","_")
    
    df1<-data.frame(variable1,label1,mean1,pvalue2,diff,confidence.interval1,adj_pvalue)
    names(df1)<-c("Variable","Levels","mean","Pvalue","Difference","Confidence.interval","Adj_pvalue")
    #---
    p1<-aov(df73~dg$PERINIUM)
    p2<-summary(p1)[[1]][["Pr(>F)"]]
    a<-aggregate(df73~dg$PERINIUM,FUN="mean")
    names(a)<-c("perineum","weight")
    variable1<-c("PERINEUM","","","","","")
    
    label1<-c("Degree of tear I","Degree of tear II","Degree of tear III","Degree of tear IV","Intact","RMLE")
    
    mean1<-c(a$weight[1],a$weight[2],a$weight[3],a$weight[4],a$weight[5],a$weight[6])
    
    pvalue2<-c(p2[1],"","","","","")
    
    diff<-c("","","","","","")
    
    confidence.interval1<-c("_","_","_","_","_","_")
    adj_pvalue<-c("_","_","_","_","_","_")
    
    df3<-data.frame(variable1,label1,mean1,pvalue2,diff,confidence.interval1,adj_pvalue)
    names(df3)<-c("Variable","Levels","mean","Pvalue","Difference","Confidence.interval","Adj_pvalue")
    #----
    #-----
    p1<-aov(df73~dg$STILLBORN)
    p1
    p2<-summary(p1)[[1]][["Pr(>F)"]]
    a<-aggregate(df73~dg$STILLBORN,FUN="mean")
    names(a)<-c("STILLBORN","weight")
    variable1<-c("STILLBORN","","","","","")
    
    label1<-c("0","1","2","3","4","5")
    
    mean1<-c(a$weight[1],a$weight[2],a$weight[3],a$weight[4],a$weight[5],a$weight[6])
    
    pvalue2<-c(p2[1],"","","","","")
    
    diff<-c("","","","","","")
    
    confidence.interval1<-c("_","_","_","_","_","_")
    adj_pvalue<-c("_","_","_","_","_","_")
    
    df4<-data.frame(variable1,label1,mean1,pvalue2,diff,confidence.interval1,adj_pvalue)
    names(df4)<-c("Variable","Levels","mean","Pvalue","Difference","Confidence.interval","Adj_pvalue")
    #-----
    p1<-aov(df73~dg$ABORTION)
    p2<-summary(p1)[[1]][["Pr(>F)"]]
    a<-aggregate(df73~dg$ABORTION,FUN="mean")
    names(a)<-c("ABORTION","weight")
    variable1<-c("ABORTION","","","","","","","","")
    
    label1<-c("0","1","2","3","4","5","6","7","8")
    
    mean1<-c(a$weight[1],a$weight[2],a$weight[3],a$weight[4],a$weight[5],a$weight[6],a$weight[7],a$weight[8],a$weight[9])
    
    pvalue2<-c(p2[1],"","","","","","","","")
    
    diff<-c("","","","","","","","","")
    
    confidence.interval1<-c("_","_","_","_","_","_","_","_","_")
    adj_pvalue<-c("_","_","_","_","_","_","_","_","_")
    
    df5<-data.frame(variable1,label1,mean1,pvalue2,diff,confidence.interval1,adj_pvalue)
    names(df5)<-c("Variable","Levels","mean","Pvalue","Difference","Confidence.interval","Adj_pvalue")
    #-----
    p1<-aov(df73~dg$DELIVERY.TYPE)
    p2<-summary(p1)[[1]][["Pr(>F)"]]
    a<-aggregate(df73~dg$DELIVERY.TYPE,FUN="mean")
    names(a)<-c("DELIVERY.TYPE","weight")
    variable1<-c("DELIVERY.TYPE","","","","","","","","","","")
    
    label1<-c("Assisted breech","Breech extraction","Hysterotomy","Laparotomy","Low mid forceps","Normal","Outlet Forceps","Primary LSCS","Repeat LSCS","Sponteneous Breech","Vaccum")
    
    mean1<-c(a$weight[1],a$weight[2],a$weight[3],a$weight[4],a$weight[5],a$weight[6],a$weight[7],a$weight[8],a$weight[9],a$weight[10],a$weight[11])
    
    pvalue2<-c(p2[1],"","","","","","","","","","")
    
    diff<-c("","","","","","","","","","","")
    
    confidence.interval1<-c("_","_","_","_","_","_","_","_","_","_","_")
    adj_pvalue<-c("_","_","_","_","_","_","_","_","_","_","_")
    
    df6<-data.frame(variable1,label1,mean1,pvalue2,diff,confidence.interval1,adj_pvalue)
    names(df6)<-c("Variable","Levels","mean","Pvalue","Difference","Confidence.interval","Adj_pvalue")
    #-----
    p1<-aov(df73~dg$LIVING)
    p2<-summary(p1)[[1]][["Pr(>F)"]]
    
    a<-aggregate(df73~dg$LIVING,FUN="mean")
    names(a)<-c("LIVING","weight")
    variable1<-c("LIVING","","","","","","","","","","")
    
    label1<-c("0","1","2","3","4","5","6","7","8","9","13")
    
    mean1<-c(a$weight[1],a$weight[2],a$weight[3],a$weight[4],a$weight[5],a$weight[6],a$weight[7],a$weight[8],a$weight[9],a$weight[10],a$weight[11])
    
    pvalue2<-c(p2[1],"","","","","","","","","","")
    
    diff<-c("","","","","","","","","","","")
    
    confidence.interval1<-c("_","_","_","_","_","_","_","_","_","_","_")
    adj_pvalue<-c("_","_","_","_","_","_","_","_","_","_","_")
    
    df7<-data.frame(variable1,label1,mean1,pvalue2,diff,confidence.interval1,adj_pvalue)
    names(df7)<-c("Variable","Levels","mean","Pvalue","Difference","Confidence.interval","Adj_pvalue")
    #-----
    p1<-aov(df73~dg$PARA)
    p2<-summary(p1)[[1]][["Pr(>F)"]]
    a<-aggregate(df73~dg$PARA,FUN="mean")
    names(a)<-c("PARA","weight")
    variable1<-c("PARA","","","","","","","","","","")
    
    label1<-c("0","1","2","3","4","5","6","7","8","9","13")
    
    mean1<-c(a$weight[1],a$weight[2],a$weight[3],a$weight[4],a$weight[5],a$weight[6],a$weight[7],a$weight[8],a$weight[9],a$weight[10],a$weight[11])
    
    pvalue2<-c(p2[1],"","","","","","","","","","")
    
    diff<-c("","","","","","","","","","","")
    
    confidence.interval1<-c("_","_","_","_","_","_","_","_","_","_","_")
    adj_pvalue<-c("_","_","_","_","_","_","_","_","_","_","_")
    
    df8<-data.frame(variable1,label1,mean1,pvalue2,diff,confidence.interval1,adj_pvalue)
    names(df8)<-c("Variable","Levels","mean","Pvalue","Difference","Confidence.interval","Adj_pvalue")
    #-----
    p1<-aov(df73~dg$GRAVIDA)
    p2<-summary(p1)[[1]][["Pr(>F)"]]
    a<-aggregate(df73~dg$GRAVIDA,FUN="mean")
    names(a)<-c("GRAVIDA","weight")
    variable1<-c("GRAVIDA","","","","","","","","","","","")
    
    label1<-c("1","2","3","4","5","6","7","8","9","10","11","13")
    
    mean1<-c(a$weight[1],a$weight[2],a$weight[3],a$weight[4],a$weight[5],a$weight[6],a$weight[7],a$weight[8],a$weight[9],a$weight[10],a$weight[11],a$weight[12])
    
    pvalue2<-c(p2[1],"","","","","","","","","","","")
    
    diff<-c("","","","","","","","","","","","")
    
    confidence.interval1<-c("_","_","_","_","_","_","_","_","_","_","_","_")
    adj_pvalue<-c("_","_","_","_","_","_","_","_","_","_","_","_")
    
    df9<-data.frame(variable1,label1,mean1,pvalue2,diff,confidence.interval1,adj_pvalue)
    names(df9)<-c("Variable","Levels","mean","Pvalue","Difference","Confidence.interval","Adj_pvalue")
    #---
    variable1<-c("variable")
    
    label1<-c("label")
    mean1<-c("mean")
    pvalue2<-c("pvalue")
    diff<-c("correlation")
    confidence.interval1<-c("confidence.interval")
    adj_pvalue<-c("adj_pvalue")
    df10<-data.frame(variable1,label1,mean1,pvalue2,diff,confidence.interval1,adj_pvalue)
    names(df10)<-c("Variable","Levels","mean","Pvalue","Difference","Confidence.interval","Adj_pvalue")
    
    r<-cor.test(df73,dg$AGE1)
    k<-mean(dg$AGE1,na.rm=T)
    k1<-round(r$estimate[[1]],digits = 4)
    variable1<-"MOTHERS AGE"
    label1<-""
    mean1<-k
    pvalue2<-r$p.value
    k1<-cor(df73,dg$AGE1,use="complete.obs")
    
    diff<-paste('(',round(k1, 
                          digits = 4),')',sep="")
    CI2 = paste('(',round(r$conf.int[[1]], 
                          digits = 4),', ',
                round(r$conf.int[[2]], 
                      digits = 4), ')',
                sep="")
    
    confidence.interval1<-CI2
    adj_pvalue<-""
    df11<-data.frame(variable1,label1,mean1,pvalue2,diff,confidence.interval1,adj_pvalue)
    names(df11)<-c("Variable","Levels","mean","Pvalue","Difference","Confidence.interval","Adj_pvalue")
    
    #---
    r<-cor.test(df73,dg$MOTHER.HEIGHT1)
    k<-mean(dg$MOTHER.HEIGHT1,na.rm=T)
    k1<-round(r$estimate[[1]],digits = 4)
    variable1<-"MOTHERS HEIGHT"
    label1<-""
    mean1<-k
    pvalue2<-r$p.value
    
    k1<-cor(df73,dg$MOTHER.HEIGHT1,use="complete.obs")
    
    diff<-paste('(',round(k1, 
                          digits = 4),')',sep="")
    
    CI2 = paste('(',round(r$conf.int[[1]], 
                          digits = 4),', ',
                round(r$conf.int[[2]], 
                      digits = 4), ')',
                sep="")
    
    confidence.interval1<-CI2
    adj_pvalue<-""
    df12<-data.frame(variable1,label1,mean1,pvalue2,diff,confidence.interval1,adj_pvalue)
    names(df12)<-c("Variable","Levels","mean","Pvalue","Difference","Confidence.interval","Adj_pvalue")
    #---
    r<-cor.test(df73,dg$MOTHER.WEIGHT1)
    k<-mean(dg$MOTHER.WEIGHT1,na.rm=T)
    k1<-round(r$estimate[[1]],digits = 4)
    variable1<-"MOTHERS WEIGHT"
    label1<-""
    mean1<-k
    pvalue2<-round(r$p.value,digits = 10)
    
    k1<-cor(df73,dg$MOTHER.WEIGHT1,use="complete.obs")
    
    diff<-paste('(',round(k1, 
                          digits = 4),')',sep="")
    CI2 = paste('(',round(r$conf.int[[1]], 
                          digits = 4),', ',
                round(r$conf.int[[2]], 
                      digits = 4), ')',
                sep="")
    
    confidence.interval1<-CI2
    adj_pvalue<-""
    df13<-data.frame(variable1,label1,mean1,pvalue2,diff,confidence.interval1,adj_pvalue)
    names(df13)<-c("Variable","Levels","mean","Pvalue","Difference","Confidence.interval","Adj_pvalue")
    #---
    r<-cor.test(df73,dg$PLACENTA.WEIGHT.Gms.1)
    k<-mean(dg$PLACENTA.WEIGHT.Gms.1,na.rm=T)
    
    variable1<-"PLACENTA.WEIGHT"
    label1<-""
    mean1<-k
    pvalue2<-round(r$p.value,digits = 10)
    k1<-cor(df73,dg$PLACENTA.WEIGHT.Gms.1,use="complete.obs")
    
    diff<-paste('(',round(k1, 
                          digits = 4),')',sep="")
    
    CI2 = paste('(',round(r$conf.int[[1]], 
                          digits = 4),', ',
                round(r$conf.int[[2]], 
                      digits = 4), ')',
                sep="")
    
    confidence.interval1<-CI2
    adj_pvalue<-""
    df14<-data.frame(variable1,label1,mean1,pvalue2,diff,confidence.interval1,adj_pvalue)
    names(df14)<-c("Variable","Levels","mean","Pvalue","Difference","Confidence.interval","Adj_pvalue")
    #---
    
    r<-cor.test(df73,dg$GEST.WEEKS1)
    k<-mean(dg$GEST.WEEKS1,na.rm=T)
    
    variable1<-"GESTATIONAL WEEKS"
    label1<-""
    mean1<-k
    pvalue2<-round(r$p.value,digits = 10)
    k1<-cor(df73,dg$GEST.WEEKS1,use="complete.obs")
    
    diff<-paste('(',round(k1, 
                          digits = 4),')',sep="")
    
    CI2 = paste('(',round(r$conf.int[[1]], 
                          digits = 4),', ',
                round(r$conf.int[[2]], 
                      digits = 4), ')',
                sep="")
    
    confidence.interval1<-CI2
    adj_pvalue<-""
    df15<-data.frame(variable1,label1,mean1,pvalue2,diff,confidence.interval1,adj_pvalue)
    names(df15)<-c("Variable","Levels","mean","Pvalue","Difference","Confidence.interval","Adj_pvalue")
    
    
    new <- rbind(df22,df1,df2,df3,df4,df5,df6,df7,df8,df9,df10,df11,df12,df13,df14,df15)
    knitr::kable(new)
    
    
  })
  
  
} 

shinyApp(ui, server)
