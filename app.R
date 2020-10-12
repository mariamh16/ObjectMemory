#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#



library(shiny)
library(shinyWidgets)
library(gridExtra)
library(png)
library(grid)
library(jpeg)
library(ggimage)
library(readr)
library(dplyr)
library(rpivotTable)
library(DT)



setwd("/Volumes/Seagate_Portable_SWD/DinoLabObjects")
cats<-read.csv("data/categories_hitrates.csv")
catss<-read.csv("data/cat_task_hr_domain.csv")
pics<-read.csv("data/pics.csv")
all_mds<-read.csv("data/MDS_150sub_new.csv")
pf<-read.csv("data/2featsPF.csv")

# Define UI for application
ui <- fluidPage(
  
  # Application title
  titlePanel("Dino Lab Object Database"),
  
  navbarPage("Dino Lab Property Norms Application",
             tabPanel("Items & Categories",
               sidebarPanel(
                 selectInput(catss$task,inputId = "task", label = "Choose a task", selected = "lexical"),
                 selectInput(catss$category,inputId = "category", label = "Choose a category to display", selected = "appliances")),
                 mainPanel(plotOutput("plot1"),
                           imageOutput("image1"))
               ),
             tabPanel("Create MDS Plots", 
               sidebarPanel(
                 fileInput(inputId="file",label="Upload CSV file",accept = c("text/csv","text/comma-separated-values,text/plain",".csv")),
                 tags$hr(),
                 checkboxInput("header", "Header", TRUE),
                 tags$hr(),
                 uiOutput("list1"),
                 uiOutput("list2"),
                 uiOutput("list3"),
                 actionButton("mdsType", "Create MDS plot")),
                 mainPanel(tableOutput("rawData"), plotOutput("mdsPlot"))
               ),
             tabPanel("Prodution Frequency", 
                      sidebarPanel(
                        selectInput(inputId = "item", label = "Choose an item to display", choice = split(pf$Concept, pf$category))),
                        mainPanel(tableOutput("pfdata"))#rpivotTableOutput("pivot")
                      )
             
                  )
      
      )
  



# Define server
server <- function(input, output) {
  
  output$plot1<-renderPlot({
    if  (input$task == "visual") { 
      ggplot(data=subset(catss, task != "verbal"),  aes(x=reorder(category, -hit_rate), y=hit_rate)) + geom_bar(aes(fill=domain),stat="identity")+ scale_fill_brewer(palette="RdPu") + theme( axis.text.x=element_text(angle=45,hjust=0.6,vjust=0.6),panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "grey")) + labs(x="categories", y="hit rate", title="Visual Task") 
      }
    else {
      ggplot(data=subset(catss, task != "visual"), aes(x=reorder(category, -hit_rate), y=hit_rate)) + geom_bar(aes(fill=domain),stat="identity")+ scale_fill_brewer(palette="RdPu") + theme( axis.text.x=element_text(angle=45,hjust=0.6,vjust=0.6),panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "grey")) + labs(x="categories", y="hit rate", title="Lexical Task")
    }
  })

  
  output$image1 <- renderPlot({
    if (is.null(input$category)) {
      return(NULL)
    }
  cat <- input$category
  pics <- pics[which(pics$category == cat),]
    filename <- normalizePath(file.path("/Volumes/Seagate_Portable_SWD/DinoLabObjects/www", paste0(pics$filename, sep = "")))
    filename <- filename[file.exists(filename)]
    pngs = lapply(filename, readJPEG)
    asGrobs = lapply(pngs, rasterGrob)
    p <- grid.arrange(grobs=asGrobs, nrow = 4)
  }, width = 1000)
  
  #plot the values of mdscale from Matlab with pictures 
  
  # rawData <- eventReactive(input$file, {
  #   #read.csv(input$file$datapath)
  #   
  #   
  # })
  
  rawData <- eventReactive(input$file, {
    read.csv(input$file$datapath)
  })
  
  output$rawData <- renderTable({
    if (input$header == TRUE){
      rawData() %>% head
    } else {
      return(NULL)
    }
    
  })
  
  
  # output$rawData<- renderTable({
  #   
  #   mds_file<-input$file
  #   
  #   if (is.null(mds_file))
  #     return(NULL)
  #   
  #   mds_data<-read.csv(mds_file$datapath, header = input$header)
  #   
  #   if (input$header == TRUE){
  #     head(mds_data, n=10)
  #   } else {
  #     return(NULL)
  #   }
  # 
  # })

  output$list1 <- renderUI({
    selectInput(inputId = "columnX", 
                       label = "Select column with x values", 
                       choices = names(rawData()))
  })
  
  output$list2 <- renderUI({
    selectInput(inputId = "columnY", 
                label = "Select column with y values", 
                choices = names(rawData()))
  })
  
  output$list3 <- renderUI({
    selectInput(inputId = "columnImagePath", 
                label = "Select column with path to images", 
                choices = names(rawData()))
  })
  # df_sel <- reactive({
  #   req(input$select_var)
  #   df_sel <- rawData() %>% select(input$select_var)
  # })
  
  
  output$mdsPlot<- renderPlot({
    vars <- names(rawData())
    
    if (input$mdsType == TRUE){
      mds<-ggplot(data=rawData(), aes(x=.data[[input$columnX]], y=.data[[input$columnY]]))
      mds  + geom_image(aes(image=.data[[input$columnImagePath]]),size=.08, image_fun= function(.) magick::image_transparent(., "white"))+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                                                                                                           panel.background = element_blank(),axis.title.x=element_blank(),axis.title.y=element_blank(),axis.text=element_blank(),axis.ticks=element_blank())#+ xlim(-.5, .5) + ylim(-0.7, 0.7)
    }else{
      return(NULL)
    }
    
  })
  
  # output$pivot<-renderRpivotTable({
  #   rpivotTable(data = pf, rows = pf$Feature,
  #                         vals = pf$Feature,inclusions = input$item,
  #                         width="100%", height="500px")#cols=c(pf$Feature, length(pf$Feature))
  # })

   output$pfdata <- renderTable(pf)
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)

