#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/



#library(rsconnect)
#rsconnect::deployApp("~/Desktop/DinoLabObjects")

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
library(rsconnect)
library(stats)
library(MASS)



catss<-read.csv("data/cat_task_hr_dom.csv")
catss_vis<-read.csv("data/catss_vis.csv")
catss_lex<-read.csv("data/catss_lex.csv")
pics<-read.csv("data/pics.csv")
all_mds<-read.csv("data/mds_subset_10520.csv")
stampMDS_sem<-read.csv("data/STAMP_VGG_MDS_shiny_sem.csv")
stampMDS_viz<-read.csv("data/STAMP_VGG_MDS_shiny_viz.csv")
conceptFreq<-read.csv("data/featurematrix.csv")
concept_HR<-read.csv("data/concept_hitrates.csv")



# Define UI for application
ui <- fluidPage(
  
  titlePanel(title=div(img(src="https://sites.duke.edu/electricdino/files/2020/05/web_header_1920x1200.png", height = 140, width=180), "DinoLab Object Database"),windowTitle = "DinoLab Object Database"),
  
  
  navbarPage("Property Norms Application",
             tabPanel("Welcome",
                      titlePanel("Welcome to the DinoLab Object Database"),
                        mainPanel(
                                h3("This database represents normative visual and semantic feature information on 1000 object images, selected to represent a wide range of real-world concepts organized by category."),
                                tags$br(),
                                tags$img(src="kitten_exemplar1.jpg",height = 120, width=140),tags$img(src="dinosaur_exemplar1.jpg",height = 120, width=140),tags$img(src="seahorse_exemplar1.jpg",height = 120, width=140),tags$img(src="rhinoceros_exemplar1.jpg", height = 120, width=140),
                                h3("Here you will be able to interact with data from two studies from our lab focusing on the role of individual object features on the memory for those items."),
                                tags$br(),
                                tags$img(src="doberman_exemplar1.jpg", height = 120, width=140),tags$img(src="cardinal_exemplar1.jpg", height = 120, width=140),tags$img(src="acoustic_guitar_exemplar1.jpg", height = 120, width=140),tags$img(src="octopus_exemplar1.jpg", height = 120, width=140),
                                h3("We also enable the visualization of multidimensional scale (MDS) plots we used in published work, in order to visualize the latent organization of these images. We also have enabled the creation of new MDS plots based on customizable analysis."),
                                tags$br(),
                                tags$img(src="gramophone_exemplar1.jpg", height = 120, width=140),tags$img(src="rabbit_exemplar1.jpg", height = 120, width=140),tags$img(src="saxophone_exemplar1.jpg",height = 120, width=140),tags$img(src="magnifyingglass_exemplar1.jpg", height = 120, width=140),
                                tags$div(
                                  h3("Details on the construction and composition of the database can be found ",
                                  tags$a(href="https://psyarxiv.com/nqmjt", 
                                         "here"),
                                  "and the associated fMRI study",
                                  tags$a(href="https://academic.oup.com/cercor/advance-article-abstract/doi/10.1093/cercor/bhaa269/5906195?redirectedFrom=fulltext", 
                                         "here."))
                                ),
                                tags$br(),
                                uiOutput("tab"),
                                h5("Created by Mariam Hovhannisyan",
                                tags$a(href="https://osf.io/r7gux/", "(OSF),"),"Electric Dinosaur Laboratory, Department of Neurology & Center for Cognitive Neuroscience, Duke University"))),
             tabPanel("Items & Categories",
               sidebarPanel(
                 h5("Select a task to view hit rates across category."),
                 selectInput(unique(catss$task),inputId = "task", label = "Choose a task", selected = "lexical"),
                 h5("Select a category to display the object images within it"),
                 selectInput(unique(catss$category),inputId = "category", label = "Choose a category to display", selected = "appliances")),
                 mainPanel(plotOutput("plot1"), imageOutput("image1"))
               ),
             tabPanel("Display our MDS plots",
               sidebarPanel(
                 h4("Here you can choose MDS plots to display from our Object Memory Studies."),
                 tags$hr(),
                 selectInput("plotmds", "Display an ObjMem MDS Plot",  c( "---" =1,
                                                                                    "Early DNN" = 2,
                                                                                    "Middle DNN" = 3,
                                                                                    "Late DNN" = 4,
                                                                                    "Semantic Features" = 5)),
                 tags$hr(),
                 selectInput("plotSTAMPV", "Display a STAMP Visual MDS Plot",  c( "---" =1,
                                                                                    "Early visual RDM" = 2,
                                                                                    "Middle visual RDM" = 3,
                                                                                    "Late visual RDM" = 4)),
                 tags$hr(),
                 selectInput("plotSTAMPS", "Display a STAMP Semantic MDS Plot", c("---" = 1, "Observed Visual" = 2,
                                                                                  "Taxonomic" = 3,
                                                                                  "Encyclopaedic" = 4)),
                 tags$hr(),
                 h6("ObjMem = behavioral study, STAMP = fMRI study"),
                 h6("To zoom in: Create a box with your cursor around the area of interest and double-click. To zoom out, double-click.")),
                  mainPanel(plotOutput("ourMDS",height = 400,dblclick = "plot1_dblclick",brush = brushOpts(id = "plot1_brush",resetOnNew = TRUE)),
                            span(textOutput("amtText")),
                            tags$hr(),
                            plotOutput("stampMDSV",height = 400,dblclick = "plot1_dblclick",brush = brushOpts(id = "plot1_brush",resetOnNew = TRUE)),
                            span(textOutput("stampMDSVText")),
                            tags$hr(),
                            plotOutput("stampMDSS",height = 400,dblclick = "plot1_dblclick",brush = brushOpts(id = "plot1_brush",resetOnNew = TRUE)),
                            span(textOutput("stampMDSSText")))
               
             ),
             tabPanel("Create MDS Plots", 
               sidebarPanel(
                 h4("Here you can create your own MDS plots in one of two ways"),
                 tags$hr(),
                 h5("1. If you have MDS values already (x and y coordinates) for your images, you can upload a CSV by using the browse button below."), 
                 h5("This requires that your x,y values as well as the path to your images are in one CSV file."),
                 tags$br(),
                 fileInput(inputId="file",label="Upload your CSV here",accept = c("text/csv","text/comma-separated-values,text/plain",".csv")),
                 tags$br(),
                 checkboxInput("header", "Header", TRUE),
                 tags$br(),
                 uiOutput("list1"),
                 uiOutput("list2"),
                 uiOutput("list3"),
                 actionButton("mdsType", "Create MDS plot"),
                 tags$hr(),
                 tags$hr(),
                 h5("2. To calculate MDS values using a similarity matrix, follow instructions in the order presented below:"),
                 tags$br(),
                 h5("1.Choose the type of MDS you want to calculate."),
                 h5("2.Upload a similarity matrix."),
                 h5("3.Upload a separate file with the path to your images (with only 1 column indicating the path to your images.)"),
                 tags$br(),
                 selectInput("mdsFunc","To calculate MDS values first choose the MDS type", c("---"=1, "Classical MDS"=2,"Non-metric MDS" =3)),
                 fileInput(inputId="mdsfile",label="Upload a simalilarity matrix (csv files only)",accept = c("text/csv","text/comma-separated-values,text/plain",".csv")),
                 fileInput(inputId="path2images",label="Upload a file with the path to your images (csv files only)",accept = c("text/csv","text/comma-separated-values,text/plain",".csv")),
                 tags$br(),
                 actionButton("compMDS", "Compute MDS & Display PLot"),
                 tags$hr(),
                 h6('To zoom in: Create a box with your cursor around the area of interest and double-click. To zoom out, double-click.'),
                 tags$br(),
                 uiOutput("mdsURL")),
                 hr(),
                 mainPanel(tableOutput("rawData"), plotOutput("mdsPlot",height = 300,
                                                              dblclick = "plot1_dblclick",
                                                              brush = brushOpts(
                                                                id = "plot1_brush",
                                                                resetOnNew = TRUE)),plotOutput("computeMDS",height = 300,
                                                                                                                      dblclick = "plot1_dblclick",
                                                                                                                      brush = brushOpts(
                                                                                                                      id = "plot1_brush",
                                                                                                                      resetOnNew = TRUE)))
               ),
             tabPanel("Downloads",
                      mainPanel(
                        downloadButton("downloadImages", "Click to download object images"),
                        tags$hr(),
                        downloadButton("downloadCFM", "Click to download the concept x frequency matrix"),
                        tags$hr(),
                        downloadButton("downloadMem", "Click to download memory data for concepts"))
                      )
                    )
                  )
  



# Define server
server <- function(input, output) {
  
  url1 <- a("MDS functions in R", href="http://www.sthda.com/english/articles/31-principal-component-methods-in-r-practical-guide/122-multidimensional-scaling-essentials-algorithms-and-r-code/")
  output$mdsURL <- renderUI({
    tagList("Click here for an explanation of the MDS functions that are being used to calculate MDS values in this app:", url1)
  })
  
  url <- a("Electric Dino Lab Homepage", href="https://sites.duke.edu/electricdino/")
  output$tab <- renderUI({
    tagList("", url)
  })

  
  output$plot1<-renderPlot({
    if  (input$task == "visual") { 
      ggplot(data=subset(catss_vis),  aes(x=reorder(category, -hit_rate), y=hit_rate)) + geom_bar(aes(fill=domain),stat="identity")+ scale_fill_brewer(palette="RdPu") + theme( axis.text.x=element_text(angle=45,hjust=0.6,vjust=0.6),panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "grey")) + labs(x="categories", y="hit rate", title="Visual Task") 
      }
    else if (input$task == "lexical"){
      ggplot(data=subset(catss_lex), aes(x=reorder(category, -hit_rate), y=hit_rate)) + geom_bar(aes(fill=domain),stat="identity")+ scale_fill_brewer(palette="RdPu") + theme( axis.text.x=element_text(angle=45,hjust=0.6,vjust=0.6),panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "grey")) + labs(x="categories", y="hit rate", title="Lexical Task")
    }
  }, width = 1000)
  
  
  output$image1 <- renderPlot({
    cat <- input$category
    pics <- pics[which(pics$category == cat),]
    filename <- file.path("www", paste0(pics$filename1, sep = ""))#normalizePath
    filename <- filename[file.exists(filename)]
    pngss = lapply(filename, readJPEG)
    asGrobs = lapply(pngss, rasterGrob)
    p <- grid.arrange(grobs=asGrobs, nrow = 4)
  }, width = 1000)
  

  
  
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
  

  
  output$mdsPlot<- renderPlot({
    vars <- names(rawData())
    
    if (input$mdsType == TRUE){
      mds<-ggplot(data=rawData(), aes(x=.data[[input$columnX]], y=.data[[input$columnY]]))
      mds  + geom_image(aes(image=.data[[input$columnImagePath]]),size=.08, image_fun= function(.) magick::image_transparent(., "white"))+ coord_cartesian(xlim = ranges$x, ylim = ranges$y, expand = FALSE)+ theme_minimal()
    }else{
      return(NULL)
    }
    
  }, height = 400, width = 600)
  
  #plot MDS from Obj Mem
  output$ourMDS<- renderPlot({
    
    if (input$plotmds == 1){
      return(NULL)
    }
    else if (input$plotmds == 2){
      x=all_mds$x3
      y=all_mds$y3
        output$amtText <- renderText({
          paste0("The early visual MDS plot (Layer 3 in AlexNet) organizes concepts largely by shape (thin, vertically oriented objects are distributed on the left side, with square objects mostly in the middle, and circular objects to the right side).")
        })
        cap<-labs(title = "Early DNN")
    } else if (input$plotmds == 3){
      x=all_mds$x6
      y=all_mds$y6
        output$amtText <- renderText({
        paste0("The middle visual MDS plot (Layer 6 in AlexNet), suggests more complex frequency and orientation information.")
      })
        cap<-labs(title = "Middle DNN")
    } else if (input$plotmds == 4){
      x=all_mds$x8
      y=all_mds$y8
        output$amtText <- renderText({
        paste0("The late visual MDS plot (Layer 8 in AlexNet) retains some of this complex configural information, but also begins to group items of similar categories together in loose clusters.")
      })
        cap<-labs(title = "Late DNN")
    } else if (input$plotmds == 5){
      x=all_mds$semenc_x
      y=all_mds$semenc_y
        output$amtText <- renderText({
        paste0("MDS plots for semantic feature information groups items in a configuration roughly consistent with their categorical organization. Living things are mostly organized on the right side, while non-living things on the left side, as well as animals in the bottom left corner, foods in the top left corner.")
      })
        cap<-labs(title = "Observed Semantic")
    }

    fd<-ggplot(data=all_mds, aes(x=x, y=y))
    fd  + geom_image(aes(image=filename),size=.08, image_fun= function(.) magick::image_transparent(., "white")) + coord_cartesian(xlim = ranges$x, ylim = ranges$y, expand = FALSE) +cap+ theme_minimal()
    
  }, height = 400, width = 600)
  
  #plot MDS from STAMP
  output$stampMDSV<- renderPlot({
    
    if (input$plotSTAMPV == 1){
      return(NULL)
    }
      else if (input$plotSTAMPV == 2){
      x=stampMDS_viz$L2_x
      y=stampMDS_viz$L2_y
      output$stampMDSVText <- renderText({
        paste0("The MDS plot for the Early visual RDM appears to represent largely color saturation (horizontal axis)")
      })
      cap<-labs(title = "Early visual RDM")
    } else if (input$plotSTAMPV == 3){
      x=stampMDS_viz$L12_x
      y=stampMDS_viz$L12_y
      output$stampMDSVText <- renderText({
        paste0("The MDS plot for the Middle visual RDM seems to code for shape-orientation combinations (e.g., round objects towards the top, square objects at the top-left, thin-oblique objects at the bottom)")
      })
      cap<-labs(title = "Middle visual RDM")
    } else if (input$plotSTAMPV == 4){
      x=stampMDS_viz$L22_x
      y=stampMDS_viz$L22_y
      output$stampMDSVText <- renderText({
        paste0("The MDS plot for for the Late visual RDM codes for more complex feature combinations that approximate object categories (e.g., animals at the top-left, round colorful fruits to the bottom, squarish furniture on the top-right, musical instruments to the right)")
      })
      cap<-labs(title = "Late visual RDM")
    } 
    
    image_file<- file.path("STAMP_pngs", paste0(stampMDS_viz$filename, sep = ""))
    
    fd<-ggplot(data=stampMDS_viz, aes(x=x, y=y))
    fd  + geom_image(aes(image=image_file),size=.08, image_fun= function(.) magick::image_transparent(., "white")) + cap + coord_cartesian(xlim = ranges$x, ylim = ranges$y, expand = FALSE) + theme_minimal()
    
  }, height = 400, width = 600)
  
  
  output$stampMDSS<- renderPlot({
    
    if (input$plotSTAMPS == 1){
      return(NULL)
    }
    else if (input$plotSTAMPS == 2){
      x=stampMDS_sem$semobs_x
      y=stampMDS_sem$semobs_y
      output$stampMDSSText <- renderText({
        paste0("The MDS plot for the Observed semantic RDM codes for the complex combinations of visual features that can distinguish some categories of objects. \nFor example, colorful roundish objects (e.g., vegetable and fruits) can be seen on the top right, squarish darker objects (e.g., some furniture and buildings) on the left/bottom-left, and furry animals at the bottom, and birds clustered tightly in the bottom right (birds have highly correlated observable features like “has a beak” and “has feathers”). \nNotably, despite the obvious visual nature of these distinctions, the Observed semantic RDM was created using verbal descriptions of the objects (e.g., “is round”, “is square”, “has fur”) and not the visual properties of the images themselves (RGB values, luminance, etc.).")
      })
      cap<-labs(title = "Observed semantic RDM")
    } else if (input$plotSTAMPS == 3){
      x=stampMDS_sem$semtax_x
      y=stampMDS_sem$semtax_y
      output$stampMDSSText <- renderText({
        paste0("The MDS plot for the Taxonomic visual RDM, not surprisingly, groups objects into more abstract semantic categories (e.g., edible items to the bottom-left, mammals to the top, and vehicles to the bottom-right).")
      })
      cap<-labs(title = "Taxonomic visual RDM")
    } else if (input$plotSTAMPS == 4){
      x=stampMDS_sem$semenc_x
      y=stampMDS_sem$semenc_y
      output$stampMDSSText <- renderText({
        paste0("The MDS plot for the Encyclopedic visual RDM three clear groupings are apparent, such that food/fruits/vegetables are clustered in the top right, animals in the bottom-right, and non-living objects in the left side of the image.")
      })
      cap<-labs(title = "Encyclopedic visual RDM")
    } 
    
    image_file<- file.path("STAMP_pngs", paste0(stampMDS_sem$filename, sep = ""))
    
    fd<-ggplot(data=stampMDS_sem, aes(x=x, y=y))
    fd  + geom_image(aes(image=image_file),size=.08, image_fun= function(.) magick::image_transparent(., "white")) + coord_cartesian(xlim = ranges$x, ylim = ranges$y, expand = FALSE) + cap + theme_minimal()
    
  }, height = 400, width = 600)
  
  
  

  rawMDS <- reactive({
    mdsInput<-input$mdsfile
    if (is.null(mdsInput))
      return(NULL)
    read.csv(mdsInput$datapath, header = FALSE)
  })
  
  mdsFile <- reactive({
    inFile<-input$path2images
    if (is.null(inFile))
      return(NULL)
    read.csv(inFile$datapath, header = FALSE)
  })
  
  
  output$computeMDS<-renderPlot({
    
    if (input$compMDS == TRUE){
      mdsfile_1<-rawMDS()
      dis_matrix<-1-mdsfile_1
      d <- dist(dis_matrix) # euclidean distances between the rows
      
      if (input$mdsFunc== 2){
        fit2 <- cmdscale(d,eig=TRUE, k=2)
      } else if(input$mdsFunc== 3) {
        fit2 <- isoMDS(d,k=2)
      } else if (input$mdsFunc== 1){
        return(NULL)
      }
      
      # plot solution
      x<-fit2$points[,1]
      y<-fit2$points[,2]
  
      mds_user<-ggplot(data=rawMDS(), aes(x=x, y=y))
      mds_user + geom_image(aes(image=mdsFile()[,1]),size=.08, image_fun= function(.) magick::image_transparent(., "white"))+ coord_cartesian(xlim = ranges$x, ylim = ranges$y, expand = FALSE)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                                                                                                                                                                                      panel.background = element_blank(),axis.title.x=element_blank(),axis.title.y=element_blank(),axis.text=element_blank(),axis.ticks=element_blank())
    }
    else {
      return(NULL)
    }
},height = 400, width = 600)

 output$downloadCFM<- downloadHandler(
   filename = function() {
     paste0("conceptFreqMatrix", ".csv", sep="")
   },
   content = function(file) {
     write.csv(conceptFreq, file)
   }
 )
  
  output$downloadMem<- downloadHandler(
    filename = function() {
      paste0("memData", ".csv", sep="")
    },
    content = function(file) {
      write.csv(concept_HR, file)
    }
  )
  
  
  output$downloadImages<- downloadHandler(
    filename = function() {
      paste0("images", ".zip", sep="")
    },
    content = function(file) {
      file.copy("www.zip", file)
    },
    contentType = "application/zip"
  )
  
  # Single zoomable plot. 
  ranges <- reactiveValues(x = NULL, y = NULL)
  
  # When a double-click happens, check if there's a brush on the plot.
  # If so, zoom to the brush bounds; if not, reset the zoom.
  observeEvent(input$plot1_dblclick, {
    brush <- input$plot1_brush
    if (!is.null(brush)) {
      ranges$x <- c(brush$xmin, brush$xmax)
      ranges$y <- c(brush$ymin, brush$ymax)
      
    } else {
      ranges$x <- NULL
      ranges$y <- NULL
    }
  })
  
  
}


# Run the application 
shinyApp(ui = ui, server = server)

