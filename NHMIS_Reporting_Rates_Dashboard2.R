# Load packages ####
library(shiny)
library(dplyr,warn.conflicts = FALSE)
library(ggplot2)
library(grid)
library(scales)
library(lubridate)
library(tibble)
library("stringr")

# Prior set-up ####
# setwd("~/R_projects/reportingRates/")

if(FALSE) {
  gs_auth()
  nhmis2017Gsheet <- gs_key(x = "1WeYGTSV8WJIOXoF36F0QWtFWkP-8PoWe-CGKKGW5g-Y")
  periodData <- nhmis2017Gsheet %>% gs_read(ws = "Sheet1", range = "B2:AA14" )
  orgUnitData <- nhmis2017Gsheet %>% gs_read(ws = "Sheet1", range = "B103:AA147")
  write.csv(x = periodData,file = "periodData.csv", row.names = FALSE)
  write.csv(x = orgUnitData, file = "orgUnitData.csv", row.names = FALSE)
}

pData <- 
  read.csv(file = "data/dummy_data.csv",as.is = TRUE)
# Rename names of columns ####
pData <- add_column(.data = pData, Period = dmy_hm(pData$Date), .before = 1)

pData$Months <- format(pData$Period, "%B")
pData$Months <- factor(pData$Months, levels = c("January","February","March","April","May","June","July","August","September","October","November","December"))

pData$Years <- format(pData$Period, "%Y")
pData$Years <- as.factor(pData$Years)
pData$Day <- format(pData$Period, "%e")
nr <- nrow(pData)
pData$Period2 <- Sys.Date() - 1:nr
pData <- add_column(.data = pData, FormattedDate = dmy(format(pData$Period, "%e/%m/%Y")))

# pData$FormattedDate <- format(pData$Period, "%a,%e-%b %y")
pData$mthlabel <- paste0("(",pData$Day,"-",str_sub(pData$Months,1,3),"-",pData$Years,")")

pDataNames <- names(pData[,3:ncol(pData) - 3])
orgUnits <- names(pData[,3:ncol(pData) - 3])

colours <- c("blue","red","black","orange","yellow","purple","brown", "pink")
allObjs <- c("state","y_ind","omit_period","fullPanel","lineType_2017","linecolor_2017","lineSize_2017")

input.brushDirection <- "xy"

# Define ui component ####
ui <- function(request) {
  fluidPage(
    sidebarLayout(
      sidebarPanel(
        width = 2,
        checkboxInput('expandBookMarkOpts','Check to show bookmark options', value = FALSE),
        # Conditional panel ####
        conditionalPanel(condition = "input.expandBookMarkOpts == true", 
                         # Client side selectizeInput
                         selectizeInput(inputId = "bookmarkType",label = "Type of bookmark", choices = c("server","url"),options = list(create = TRUE,placeholder = "Select type of bookmark")),
                         selectizeInput(inputId = "objBookmarks", label = "Objects",
                                        choices = allObjs,selected = NULL, multiple = TRUE),
                         selectInput("includeExcluedBk","Include or Exclude Bookmark",choices = c("Include to bookmark","Exclude from bookmark")),
                         bookmarkButton(title = "Save the current state of the dashboard", label = "Share dashboard")
        ),
        # Continuation of user input ####
        selectInput(inputId = "state", label = "Select state", choices = orgUnits, selected = orgUnits[26]),
        selectInput('chartType',"Type of chart",c("Stacked Bar chart","Un-stacked Bar chart","Line chart"), selected = "Line chart"),
        selectInput('year', "Select year", unique(pData$Years), selected = 2018),
        actionButton(inputId = "updateBrush", label = "Update brushed data"),
        selectInput("brushDirection", "Select brush direction", c("x","y","xy"), selected = "xy"),
        checkboxInput('fullPanel','Check to show full panel', value = FALSE),
        # Conditional panel ON CHART appearance ####
        conditionalPanel(condition = "input.fullPanel == true", 
                         checkboxInput("show_detailed_label", "Show detailed label", FALSE),
                         checkboxInput("expandChart", "Expand Chart?", FALSE),
                         selectInput('clip', 'Do you want to clip?', c("on","off"),selected = "on"),
                         checkboxInput("lim_x_items", "Check to limit number of elements on the x-axis", TRUE),
                         conditionalPanel("input.lim_x_items == true",
                                          numericInput("noOfxElements","No. of x-data elements",value = 70, min = 0, max = 99999,step = 1)),
                         conditionalPanel(condition = "input.overwriteClip == true", 
                                          numericInput("myClipZoomLevel","Custom Clip zoom level",value = 100, min = 0, max = 99999)),
                         checkboxInput("overwriteClip", "Check to overwrite clip zoom value of max?", FALSE),
                         selectInput("theme","Select Theme",choices = c("bw","gray","linedraw","light","dark","minimal","classic","void")),
                         selectInput("legendDirection","Legend Direction",c("horizontal","vertical")),
                         numericInput("positionDodgeWidth","Position dodge width",value = 100, min = 0, max = 99999),
                         numericInput("width","Width of bar",value = 0.5, min = 0, max = 99999),
                         checkboxInput("reverseOrder","Reverse Order?",value = FALSE),
                         numericInput("padding","Padding",value = 0.2, min = 0, max = 99999),
                         # Other conditional panel functions ####
                         numericInput('legendxpos',"legend x-position",value = 1, min = 0, max = 99999),
                         numericInput('legendypos',"legend y-position",value = 1, min = 0, max = 99999),
                         selectInput('legendPosition',"Legend position",c("none","left","right","bottom","top","custom position"), selected = "left"),
                         selectInput('arrow_ends','Select arrow ends', c("first","last","both","none"), selected = "both"),
                         selectInput('arrowType', 'Select arrow type', c("closed","open"), selected = "closed"),
                         numericInput('arrowAngle', 'Select arrow angle', value = 15, min = 0, max = 90, step = 5)
        )
      ),
      # Main panel ####
      mainPanel(
        
        # plotOutput(outputId = 'plot1',width = "1100px",height = "630px",brush = brushOpts(id = "plot_brush", fill = "#9cf", stroke = "#FFF", clip = TRUE, direction = "xy", delayType = "debounce"))
        textOutput("interactions"),
        plotOutput(outputId = 'zoom',width = "1100px",height = "500px"),
        plotOutput(outputId = 'overall',width = "1100px",height = "300px",brush = brushOpts(id = "plot_brush", fill = "#9cf", stroke = "#FFF", clip = TRUE, direction = "xy", delayType = "debounce"))
        
        ,verbatimTextOutput(outputId = "table2")
        ,tableOutput(outputId = "table1"),width = 10, height = 5000
        ,textOutput(outputId = "myprint"),br(),textOutput(outputId = "lastSaved"))
    )
  )
}

# Define server component ####
server <- function(input, output, session) {
  # Some Reactive stuffs ####
  vals <- reactiveValues(savedTime = NULL)
  output$lastSaved <- renderText({
    if(!is.null(vals$savedTime))
      paste("Last saved at", vals$savedTime)
    else
      ""
  })
  onBookmark(function(state) {
    vals$savedTime <- Sys.time()
    # state is a mutable reference object, and we can add arbitrary values to it.
    state$values$time <- vals$savedTime
  })
  onRestore(function(state){
    vals$savedTime <- state$values$time
  })
  
  my_state <- reactive({
    input$state
  })
  
  output$myprint <- renderPrint({
    cat("Items to bookmark are below\n")
    print(setdiff(allObjs,bookmarkToOmit()))
  })
  
  bookmarkToOmit <- reactive({
    if(input$includeExcluedBk == "Include to bookmark") {
      setdiff(x = allObjs, y = input$objBookmarks)
    } else if(input$includeExcluedBk == "Exclude from bookmark") {
      input$objBookmarks
    }
  })
  
  selectedData_ind <- reactive({
    enableBookmarking(store = input$bookmarkType)
    setBookmarkExclude(names = bookmarkToOmit())
    selectCols <- pData[,c("Period","Date","Months","Years","FormattedDate","mthlabel", input$state)]
    selectedCols <- selectCols[!is.na(selectCols[,input$state]),]
    selectedCols2 <- selectedCols[selectedCols$Years == input$year,]
    selectedCols2[,input$state] <- selectedCols2[,input$state] / 100
    selectedCols2
  })

  # Plotmaker ####
  # output$plot1 <- renderPlot({
  plotBuilder <- reactive({
    # Prior charts ####

    max1 <- max(selectedData_ind()[,input$state],na.rm = TRUE)
    myX <- "FormattedDate"
    myY <- input$state
    myG <- "Months"
    padding = input$padding
    myTheme <- input$theme
    date_breaks <- "2 weeks"
    myD <- selectedData_ind()
    
    if(input$legendPosition == "custom position") {
      legendPosition <- c(input$legendxpos,input$legendypos)
    } else {
      legendPosition <- input$legendPosition
    }
    maxY <- 1
    breaks <- seq(0,1,0.1)
      nudge_y <- 0.01
      nudge_x <- 0.01
      perlabel <- round(selectedData_ind()[,input$state],4)
      mthlabel2 <- if(input$show_detailed_label == TRUE){
        selectedData_ind()[,"mthlabel"]
      } else {NULL} 
    if(input$overwriteClip == TRUE) {
      clipZoom <- input$myClipZoomLevel
    } else if(input$overwriteClip == FALSE) {
      clipZoom <- maxY
    }
      no_of_x_elem <- if(input$lim_x_items == TRUE) {
        input$noOfxElements
      } else {nrow(selectedData_ind()[!is.na(selectedData_ind()[,input$state]),])}
# Line chart ####
    if(input$chartType == 'Line chart') {

      p <- ggplot(data = selectedData_ind(), mapping = aes_string(x = myX, y = myY, group = myG)) +
        geom_line() +
        geom_point() +
        # scale_x_date(date_breaks = date_breaks,position = "bottom", name = "Dates",date_labels = "%a %e-%b '%y") +
        scale_x_date(position = "bottom", name = "Dates",date_labels = "%a %e-%b '%y") +
        
        geom_text(nudge_y = nudge_y, nudge_x = nudge_x, check_overlap = TRUE, aes(label = paste0(
          percent(perlabel),mthlabel2))) +
        labs(y = input$state, title = paste(input$state, "Reporting rates")) +
        scale_y_continuous(breaks = breaks, labels = percent) 
        # coord_cartesian(ylim = c(0,clipZoom), expand = input$expandChart,clip = input$clip) +
      if(myTheme == "bw") {p <- p + theme_bw() } else if (myTheme == "gray") {p <- p + theme_gray()} else if (myTheme == "linedraw") {p <- p + theme_linedraw()} else if (myTheme == "light") {p <- p + theme_light()} else if (myTheme == "dark") {p <- p + theme_dark()} else if (myTheme == "minimal") {p <- p + theme_minimal()} else if (myTheme == "classic") {p <- p + theme_classic()} else if (myTheme == "void") {p <- p + theme_void()}
      p + theme(legend.position = legendPosition,legend.direction = input$legendDirection, axis.text.x = element_text(angle = 90, face = "italic",size = 12))
    }
    # Bar charts ####
    # else if(input$chartType == 'Un-stacked Bar chart' || input$chartType == 'Stacked Bar chart') {
    #   p <- ggplot(data = myD,
    #               mapping = aes_string(x = myX, y = myY)) +
    #     geom_col(width = input$width, position = 
    #                if(input$chartType == 'Un-stacked Bar chart') {
    #                  position_dodge2(width = input$positionDodgeWidth,preserve = "total",padding = padding,reverse = input$reverseOrder)
    #                } else if(input$chartType == 'Stacked Bar chart') {
    #                  position = position_stack(reverse = input$reverseOrder)}) +
    #     geom_point() +
    #     geom_text(nudge_y = nudge_y, nudge_x = nudge_x, check_overlap = TRUE, aes(label = paste(
    #       perlabel,"^(",myD[,c("Months")],")",sep = "")), parse = TRUE) +
    #     labs(x = 'formattedDate', y = input$state, title = paste(input$state, "in 2016 and 2017. Indicator type is", "percentage")) +
    #     scale_y_continuous(breaks = breaks, labels = percent) +
    #     coord_cartesian(ylim = c(0,clipZoom), expand = input$expandChart,clip = input$clip,xlim = c(0,input$noOfxElements))
    #   
    #   if(myTheme == "bw") {p <- p + theme_bw() } else if (myTheme == "gray") {p <- p + theme_gray()} else if (myTheme == "linedraw") {p <- p + theme_linedraw()} else if (myTheme == "light") {p <- p + theme_light()} else if (myTheme == "dark") {p <- p + theme_dark()} else if (myTheme == "minimal") {p <- p + theme_minimal()} else if (myTheme == "classic") {p <- p + theme_classic()} else if (myTheme == "void") {p <- p + theme_void()}
    #   p + theme(legend.position = legendPosition,legend.direction = input$legendDirection, axis.text.x = element_text(angle = 90, face = "bold",size = 14))
    # }
    
  })
  
  output$zoom <- renderPlot({
    if(!is.null(input$plot_brush)) {
      plotBuilder() + xlim(lubridate::as_date(input$plot_brush$xmin), lubridate::as_date(input$plot_brush$xmax))
    } else {
      plotBuilder() + xlim(dmy("01-04-2018"),dmy("01-06-2018"))
    }
    
  })
  
  output$interactions <- renderPrint({
    print(input$plot_brush$xmin)
    print(class(input$plot_brush$xmin))
    print(lubridate::as_date(input$plot_brush$xmin))

    
    cat("\n\n\n")
    
    print(input$plot_brush$xmax)
  })
  
  output$overall <- renderPlot({
    plotBuilder()
  })
  
  output$table1 <- renderTable({
    # head(x = selectedData_ind(), n = 9999)
    head(x = brushedData1(), n = 9999)
  })
  # Brush of plot1 handling ####
  brushedData1 <- eventReactive(input$updateBrush, {
    brushedPoints(df = selectedData_ind(), brush = input$plot_brush, allRows = FALSE)
  })

  output$table2 <- renderPrint({
    rows <- brushedPoints(df = selectedData_ind(), brush = input$plot_brush, allRows = FALSE)
    cat("Brushed points:\n")
    rows <- select(rows,c("FormattedDate", input$state))
    rows[,"FormattedDate"] <- format(rows[,"FormattedDate"],"%A %e-%B %Y")
    rows[,input$state] <- percent(rows[,input$state])
    print(rows)
  })
 
  
  
  
  
}
# Line
# Colour
# type
# size
# 
# Point
# colour
# type
# size


# Option to activate delayed reactions
shinyApp(ui, server)
