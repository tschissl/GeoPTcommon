#### GeoPT common Shiny App for GeoPT participants ###
# 20190813
# Thomas Meisel


## future changes: analyte list needs to be coupled with round selection

library(tidyverse)
library(shiny)
library(data.table)
library(googlesheets) # for loading the GeoPT data from Google drive
library(stringr) #for combining letters and numbers for lab selection
#library(plotflow) # needed for reorder in plots
#library(readxl) # for Excel import
library(readr) # delimited csv import


loadData1 <- function() {
  # Grab the Google Sheet
  sheet <- gs_title("GeoPTall") 
  # Read the data
  gs_read_csv(sheet, col_types = cols(
    lab = col_character(),
    analyte = col_character(),
    method = col_character(),
    prep = col_character(),
    mass = col_double() ,
    measurand = col_double(),
    round = col_character()
  ))
}

loadData2 <- function() {
  # Grab the Google Sheet
  sheet <- gs_title("GeoPT_assigned_values") 
  # Read the data
  gs_read_csv(sheet, col_types = cols(
    analyte = col_character(),
    assigned = col_double(),
    round = col_character()
  ))
}

GeoPT <- loadData1() # file in Google Drive needs to be named GeoPTall and not GeoPTall.csv
# need to check if headers are available
# need to upload latest version of "~/GitHub/GeoPT02app/GeoPTall.xlsx" then open with Google Sheets
# and rename GeoPTall.csv to GeoPTall
#GeoPT <- read_excel("C:/Daten/projects/GeoPT/GeoPT_all/GeoPTall.xlsx")
#GeoPT <- read_delim("C:/Daten/projects/GeoPT/GeoPT_all/GeoPTall.csv",";", escape_double = FALSE, trim_ws = TRUE)
#GeoPT<- read.csv("C:/Daten/projects/GeoPT/GeoPT_all/GeoPTall.xlsx")
#GeoPT_assigned_values <- read_delim("C:/Daten/projects/GeoPT/GeoPT_all/GeoPT_assigned_values.csv",";", escape_double = FALSE, trim_ws = TRUE)
GeoPT_assigned_values <- loadData2()

GeoPT <- as.data.table(GeoPT)
GeoPT_assigned_values <- as.data.table(GeoPT_assigned_values)
mort <- c("SiO2", "TiO2", "Al2O3", "Fe2O3T", "MnO", "MgO", "CaO", "Na2O", "K2O", "P2O5", "LOI", "Fe(II)O", "CO2", "H2O+")  # needed for assigning T for trace or M for major element as is used for unit and z-score calcuations

GeoPT$MorT <- "T" # setting all to trace element "T
GeoPT[analyte %in% mort, MorT := "M"] # exchanging the major element analytes with "M" for major

GeoPT$unit <- "mg/kg" # assigning unit used for ggplot2
GeoPT$unit[GeoPT$MorT == "M"] <- "g/100g" # assigning unit used for ggplot2

GeoPT_assigned_values$MorT <- "T" # setting all to trace element "T
GeoPT_assigned_values[analyte %in% mort, MorT := "M"] # exchanging the major element analytes with "M" for major

GeoPT_assigned_values$unit <- "mg/kg" # assigning unit used for ggplot2
GeoPT_assigned_values$unit[GeoPT_assigned_values$MorT == "M"] <- "g/100g" # assigning unit used for ggplot2


## introduced to simplify the digestion and measurement methods ##

setkey(GeoPT, prep)


GeoPT[prep == "FM+AD", prep := "FM_AD"] #need to change for scale_fill_manual colours
GeoPT[prep == "AD+FM", prep := "AD_FM"]  #need to change for scale_fill_manual colours
GeoPT[method == "ICP-OES/AES",method := "ICP"]  #need to change for scale_fill_manual colours
GeoPT[method == "ICP-MS", method := "ICP_MS"]  #need to change for scale_fill_manual colours
GeoPT[method == "GRAV+VOL", method := "GRAV_VOL"] #need to change for scale_fill_manual colours


anal <- unique(GeoPT$analyte) # needed for ui selector
rnd <- unique(GeoPT$round) # needed for ui selector


ui = fluidPage(
  navbarPage("Chart Selection",
             tabPanel("Sigmoidal",
                      pageWithSidebar(
                        headerPanel('GeoPT charts'),
                        sidebarPanel(
                          
                          selectInput("round.select", "GeoPT Round", rnd, selected=rnd[[49]]),
                          #uiOutput("listAnalyte"),
                          selectInput("xcol", "analyte", anal, selected=anal[[1]]),
                          actionButton(inputId = "go", label = "Update calculations",
                                       style = "background-color:#E69F00"),
                          actionButton("exclude_reset", "Reset"),
                          helpText("Note: Press Reset button before changing the analyte. In case an error msg occurs, press Reset button"),
                          radioButtons("sig_colour", "select point grouping", 
                                       c("quality" = "quality",
                                         "prep" = "prep", 
                                         "method" ="method"), selected = "quality"),
                          numericInput("num", label = "type lab. code number for highlighting", value = 0),
                          width = 2 # ajusting the width of the sidebar panel
                          
                        ),
                        # Plot is placed in main panel
                        mainPanel(
                          h4("sigmoidal chart plot"),
                          plotOutput('plot_sig', height = "600px",
                                     click = "plot1_click"),
                          h4("consensus value and uncertainty"), # table below sigmoidal plot
                          tableOutput("table1")
                        )
                      )
             ),
             tabPanel("Bar",
                      h4("bar chart plot"),
                      plotOutput('plot_bar', height = "600px",
                                 click = "plot1_click")
             ),
             tabPanel("Violin",
                      h4("Violin plot"),
                      plotOutput('plot_violin1', height = "600px"
                      ),
                      plotOutput('plot_violin2', height = "600px"
                      )
             ),
             tabPanel("analyte",
                      h4("consensus value and uncertainty"),
                      tableOutput("tab2")
             )
  )
)



server = function(input, output){

  ##20190105 trying to work with reactive GeoPT data
  
  GeoPTSubset = reactive({
    GeoPT[round == input$round.select, measurand, by = .(analyte, lab, quality, round, method, prep, MorT, unit)]
    })
  
  ##20190105 trying to use reactive output as input for analyte that are available for a selected round 
  
  output$listAnalyte = renderUI({
    
    selectInput("xcol", "Pick analyte", 
               choices = unique(GeoPTSubset()[,analyte])
    )
   })
  
  ##20190105 for testing and making selection visible
  
  output$tab2 <- renderTable({
    #filter(GeoPTSubset(), round == input$round.select, analyte == "In")
    unique(GeoPTSubset()[,analyte])
      })

  
  output$plot_sig <- renderPlot({
    
    #selectedData <- GeoPT[round == "2" & analyte == "SiO2", measurand, by = .(lab, analyte, quality, round, method, prep, MorT, unit)][order(round, measurand)]
    selectedData <- GeoPT[round == input$round.select & analyte == input$xcol, measurand, by = .(lab, analyte, quality, round, method, prep, MorT, unit)][order(round, measurand)]
    
    # converting into data frame
    df <- as.data.frame(selectedData)
    
    # picking the reference line for plots based on GeoPT reports
    estimators <- GeoPT_assigned_values[round == input$round.select & analyte == input$xcol, assigned,] 
    #estimators <- GeoPT_assigned_values[round == "2" & analyte == "SiO2", assigned,]
    
    df$rank <- 1:length(df$measurand)
    
    ## horwitz value calculation
    MorT <- unique(df$MorT)
    Horwitz <- ifelse(MorT == "T", 1*0.01*(estimators/100/10000)^0.8495*10000,
                      0.01*(estimators/100)^0.8495)
    horwitz <- Horwitz*100 #getting the horwitz s for plotting range
    
    
    # getting the y-axis label
    unit <- unique(selectedData$unit)
    
    method <- df$method
    
    # needs to be selected before ggplot is executed. "select" is added to ggplot. Point removal does not work with print("plot)
    
    select <- switch(input$sig_colour,
                     "method" = geom_point(aes(fill=method), size=6, alpha=1, shape=21) ,
                     "prep" = geom_point(aes(fill=prep), size=6, alpha=1, shape=21) , 
                     "quality" = geom_point(size=6, alpha=1, aes(fill = quality), shape=21)
    )
    select.col <- switch(input$sig_colour,
                         "method" = scale_fill_manual(values=c(CSAN="#fdae61",
                                                               CVAAS="gray80",
                                                               EDXRF="#d73027",
                                                               ETAAS="#ffffbf",
                                                               FLAAS="#66bd63",
                                                               GFAAS="#56B4E9",
                                                               GRAV="#f46d43",
                                                               GRAV_VOL="black",
                                                               HYAAS ="#d9ef8b",
                                                               ICP="#1a9850", 
                                                               ICP_MS="#006837",
                                                               INAA="#dfc27d",
                                                               IRS="#a6d96a",
                                                               ISE="darkorchid2",
                                                               other="#80cdc1",
                                                               SPHOT="#8073ac",
                                                               UVS="gray50",
                                                               VOL="#66bd63",
                                                               WDXRF="#a50026")) ,
                         "prep" = scale_fill_manual(values=c(AD="#a50026",
                                                             AD_FM="#abd9e9",
                                                             CB="#fee090",
                                                             FA="black",
                                                             FD="#313695", 
                                                             FM="#4575b4",
                                                             FM_AD="#74add1",
                                                             NO="#dfc27d",
                                                             other="#80cdc1",
                                                             PF="gray50",
                                                             PP="#d73027",
                                                             PY="gray50",
                                                             SD="#f46d43",
                                                             SI="#fdae61")),
                         "quality" = scale_fill_manual(values=c(Applied ="mediumpurple",
                                                                Pure = "darkorange"))
    )
    
    # finding the position for lab highlighting
    
    round.ini.let <- substr(df$lab[1], 1,1) # getting the inital letter assigned to this round
    lab <- paste(round.ini.let, input$num, sep="") # pasting the initial letter with the lab number
    
    position <- df$rank[df$lab == lab] # finding the position for the plot
    
    ## for plot label: name = of sample during or for GeoPT round, sample = rock type
    
    round <- GeoPT_assigned_values[round == input$round.select & analyte == input$xcol, GeoPTround,]
    analyte <- GeoPT_assigned_values[round == input$round.select & analyte == input$xcol, analyte,]
    name <- GeoPT_assigned_values[round == input$round.select & analyte == input$xcol, name,]
    sample <- GeoPT_assigned_values[round == input$round.select & analyte == input$xcol, sample,]
    
    ##
    
    if(lab %in% df$lab){
      rect <- geom_rect(xmin = position - 0.5, xmax = position +0.5, alpha = 0.9, ymin = -Inf, ymax = Inf, fill = "gray90")
    } else {
      rect <-   NULL
    }
    
    x <- estimators # assigned value from repots
    
    ggplot(df, aes(reorder(x=lab, rank), measurand)) +
      rect + # adding rectangle for selected lab (input)
      geom_point(size=1, alpha=.1) + # need to have this line for "rect" otherwise error msg
      annotate("rect", xmin=-Inf, xmax=+Inf, ymin= x - 4*horwitz, ymax=x + 4*horwitz,
               fill="gray", alpha=0.2) + # gray box for horwitz range
      geom_hline(yintercept = x, colour="darkgreen") + # assigned value
      geom_hline(yintercept =  x + 2*horwitz, colour = "gray20", linetype="solid") + # horwitz
      geom_hline(yintercept =  x - 2*horwitz, colour="gray20", linetype="solid") + # horwitz
      geom_hline(yintercept =  x + 4*horwitz, colour = "gray50", linetype="dashed") + # horwitz*2
      geom_hline(yintercept =  x - 4*horwitz, colour="gray50", linetype="dashed") + # horwitz*2
      annotate("text", x=-Inf, y=Inf , hjust= 0, vjust= 1.5, label = round, size=8, colour="grey40") +        #round
      annotate("text", x=-Inf, y=Inf , hjust= 0, vjust= 3.0, label = analyte, size=10, colour="darkgreen") +  #"analyte"
      annotate("text", x=-Inf, y=Inf , hjust= 0, vjust= 8, label = sample, size=5, colour="grey50") +         #sample
      annotate("text", x=-Inf, y=x+4*horwitz, hjust= -0.2, vjust= -0.3, label = "z'= 2", size=3) +
      annotate("text", x=-Inf, y=x+2*horwitz, hjust= -0.2, vjust= -0.35, label = "z = 2", size=3) +
      annotate("text", x=Inf, y=x-2*horwitz, hjust= +1.25, vjust= -0.3, label = "z = -2", size=3) +
      annotate("text", x=Inf, y=x-4*horwitz, hjust= +1.2, vjust= -0.4, label = "z' = -2", size=3) +
      ggtitle(name) +             #name of RM over the plot
      xlab("laboratory code") +
      ylab(unit) +
      theme_bw() +
      select + # the selection of the groupings (quality, prep, method)
      select.col + 
      theme(legend.position = c(1,0), legend.justification = c(1,-0.05)) +
      theme(plot.title = element_text(colour = "blue", size = 25)) +  #format name of RM over the plot
      theme(axis.text.x = element_text(angle=90, hjust=1, vjust=.4, size = rel(1.4))) 
    
  })
  
  output$plot_bar <- renderPlot({                         # barchart plot
    
    selectedData <- GeoPT[round == input$round.select & analyte == input$xcol, measurand, by = .(lab, analyte, quality, round, method, prep, MorT, unit)][order(round, measurand)]
    
    # converting into data frame
    df <- as.data.frame(selectedData)
    
    # picking the reference line for plots based on GeoPT reports
    estimators <- GeoPT_assigned_values[round == input$round.select & analyte == input$xcol, assigned,] 
    #estimators <- GeoPT_assigned_values[round == "38A" & analyte == "SiO2", assigned,]
    
    df$rank <- 1:length(df$measurand)
    
    ## horwitz value calculation
    MorT <- unique(df$MorT)
    Horwitz <- ifelse(MorT == "T", 1*0.01*(estimators/100/10000)^0.8495*10000,
                      0.01*(estimators/100)^0.8495)
    horwitz <- Horwitz*100 #getting the horwitz s for plotting range
    
    # for plot label
    round <- GeoPT_assigned_values[round == input$round.select & analyte == input$xcol, GeoPTround,]
    analyte <- GeoPT_assigned_values[round == input$round.select & analyte == input$xcol, analyte,]
    name <- GeoPT_assigned_values[round == input$round.select & analyte == input$xcol, name,]
    sample <- GeoPT_assigned_values[round == input$round.select & analyte == input$xcol, sample,]
    
    
    # getting the y-axis label
    unit <- unique(selectedData$unit)
    
    method <- df$method
    
    x <- estimators # assigned value from report
    
    ## horwitz value calculation
    df$hor[df$MorT == "T"] <- 1*0.01*(x/100/10000)^0.8495*10000
    df$hor[df$MorT == "M"] <- 0.01*(x/100)^0.8495
    df$z <- (df$measurand-x)/horwitz
    
    
    ## range testing
    df$colour <- "gray40"
    df$colour[df$z > 8] <- "gray80" # changing colour in plot if out of range
    df$colour[df$z < - 8] <- "gray80" # changing colour in plot if out of range
    #df$colour[df$lab == lab] <- "red"
    #df$z[df$z > 8] <- 8 # setting maximum to 8 if out of range
    #df$z[df$z < - 8] <- -8 # setting minimum to 8 if out of range
    #####
    
    #all.data <- list(anal, nx, med.sig, median_se, # 1,2,3,4
    #                 Mode, mode_lientz_data, mode_lientz_mean,  #5,6,7
    #                 mode_lientz_med, mode_lientz_sd, mode_lientz_se, #8,9,10
    #                 robhub, robhubs, robhubse, Mike.mode[1], Mike.mode[2]) #11,12,13,14,15
    
    
    
    colour <- df$colour # getting the colours for barcharts
    
    # getting the y-axis label
    unit <- unique(selectedData$unit)
    
    
    ggplot(df, aes(reorder(x=lab, measurand), z)) +
      geom_hline(yintercept = 0, colour="darkgreen") + # assigned value as reference
      geom_hline(yintercept =  + 2, colour = "gray20", linetype="solid") + # horwitz "tramline"
      geom_hline(yintercept =  - 2, colour="gray20", linetype="solid") + # horwitz
      geom_hline(yintercept =  + 4, colour = "gray70", linetype="solid") + # horwitz*2 "tramline for applied labs"
      geom_hline(yintercept =  - 4, colour="gray70", linetype="solid") + # horwitz*2
      geom_bar(stat="identity", position="identity", width=0.6, colour=colour, fill=colour) +
      annotate("rect", xmin=-Inf, xmax=+Inf, ymin=  - 4, ymax= + 4, fill="gray", alpha=0.2) + # gray box for horwitz range
      annotate("text", x=-Inf, y=Inf , hjust= 0, vjust= 1.5, label = round, size=8, colour="grey40") +        #round
      annotate("text", x=-Inf, y=Inf , hjust= 0, vjust= 3.0, label = analyte, size=10, colour="darkgreen") +  #"analyte"
      annotate("text", x=-Inf, y=Inf , hjust= 0, vjust= 8, label = sample, size=5, colour="grey50") +         #sample
      ylab("z-score") +
      xlab("laboratory code") +
      annotate("text", x=0, y=4, hjust= -0.2, vjust= -0.3, label = "z'= 2", size=3) +
      annotate("text", x=0, y=2, hjust= -0.2, vjust= -0.35, label = "z = 2", size=3) +
      annotate("text", x=Inf, y=-4, hjust= +1.2, vjust= -0.3, label = "z'= -2", size=3) +
      annotate("text", x=Inf, y=-2, hjust= +1.2, vjust= -0.4, label = "z = -2", size=3) +
      geom_hline(yintercept = + 8, linetype="dotted", colour="gray70") +
      geom_hline(yintercept = - 8, linetype="dotted", colour="gray70") +
      scale_y_continuous(breaks = c(-8, -4,-2,2,4,8)) +
      ggtitle(name) +
      theme(plot.title = element_text(colour = "blue", size = 25)) +  #format name of RM over the plot
      theme_bw() +
      theme(axis.text.x = element_text(angle=90, hjust=1, vjust=.4, size = rel(.9))) +
      theme(plot.title = element_text(colour = "blue", size = 25))  #format name of RM over the plot
    
  })
  
  output$plot_violin1 <- renderPlot({                    #### violin method plot
    
    selectedData <- GeoPT[round == input$round.select & analyte == input$xcol, measurand, 
                          by = .(lab, analyte, quality, round, method, prep, MorT, unit)][order(round, measurand)]
    
    # converting into data frame
    df <- as.data.frame(selectedData)
    
    # picking the reference line for plots based on GeoPT reports
    estimators <- GeoPT_assigned_values[round == input$round.select & analyte == input$xcol, assigned,] 
    
    
    df$rank <- 1:length(df$measurand)
    
    x <- estimators # estimator for assigned value as basis for horwitz tramline
    
    ## horwitz value calculation
    MorT <- unique(df$MorT)
    Horwitz <- ifelse(MorT == "T", 1*0.01*(x/100/10000)^0.8495*10000, 0.01*(x/100)^0.8495)
    horwitz <- Horwitz*100 #getting the horwitz s for plotting range
    

    
    # getting the y-axis label
    unit <- unique(selectedData$unit)
    round <- GeoPT_assigned_values[round == input$round.select & analyte == input$xcol, GeoPTround,]
    analyte <- GeoPT_assigned_values[round == input$round.select & analyte == input$xcol, analyte,]
    name <- GeoPT_assigned_values[round == input$round.select & analyte == input$xcol, name,]
    sample <- GeoPT_assigned_values[round == input$round.select & analyte == input$xcol, sample,]
    
  ggplot(df, aes(reorder(method, measurand), measurand)) + ##"reorder_by" does not work with version 3.6.1
    #annotate("text", x=-Inf, y=Inf , hjust= 0, vjust= 1.5, label = round, size=8, colour="grey40") +        #round
    #annotate("text", x=-Inf, y=Inf , hjust= 0, vjust= 3.0, label = analyte, size=10, colour="darkgreen") +  #"analyte"
    #annotate("text", x=-Inf, y=Inf , hjust= 0, vjust= 8, label = sample, size=5, colour="grey50") +         #sample
    geom_hline(yintercept =  x + 2*horwitz, colour = "gray20", linetype="solid") + # horwitz
    geom_hline(yintercept =  x -  2*horwitz, colour="gray20", linetype="solid") + # horwitz
    geom_hline(yintercept =  x + 4*horwitz, colour = "gray70", linetype="solid") + # horwitz*2
    geom_hline(yintercept =  x - 4*horwitz, colour="gray70", linetype="solid") + # horwitz*2
    geom_hline(yintercept = x, colour="aquamarine3") + # median
    geom_violin(scale = "count") +
    geom_jitter(aes(fill=prep), size = 5, alpha = .8, 
                position = position_jitter(width = .15), shape=21) +
    stat_summary(fun.y=median, geom="point", fill="white", shape=21, size=3.5) +
    ggtitle(name) +
    ylab(unit) + 
    labs(colour = "prep") +
    scale_fill_manual(values=c(AD="#a50026",
                               AD_FM="#abd9e9",
                               CB="#fee090",
                               FA="black",
                               FD="#313695", 
                               FM="#4575b4",
                               FM_AD="#74add1",
                               NO="#dfc27d",
                               other="#80cdc1",
                               PF="gray50",
                               PP="#d73027",
                               PY="gray50",
                               SD="#f46d43",
                               SI="#fdae61")) +
    theme_bw() +
    theme(axis.text.x = element_text(angle=45, hjust=1, vjust=1, size = rel(1.2))) +
    theme(plot.title = element_text(colour = "blue", size = 25)) + # for title appearance
    theme(legend.text=element_text(size=rel(1.2))) +
    theme(legend.position="top") +
    theme(plot.title = element_text(colour = "blue", size = 25)) # for format of title
  
})
  
  output$plot_violin2 <- renderPlot({                 #### violin prep plot
    
    selectedData <- GeoPT[round == input$round.select & analyte == input$xcol, measurand, 
                          by = .(lab, analyte, quality, round, method, prep, MorT, unit)][order(round, measurand)]
    
    # converting into data frame
    df <- as.data.frame(selectedData)
    
    # picking the reference line for plots based on GeoPT reports
    estimators <- GeoPT_assigned_values[round == input$round.select & analyte == input$xcol, assigned,] 
    
    
    df$rank <- 1:length(df$measurand)
    
    x <- estimators # estimator for assigned value as basis for horwitz tramline
    
    ## horwitz value calculation
    MorT <- unique(df$MorT)
    Horwitz <- ifelse(MorT == "T", 1*0.01*(x/100/10000)^0.8495*10000, 0.01*(x/100)^0.8495)
    horwitz <- Horwitz*100 #getting the horwitz s for plotting range
    
    
    # getting the y-axis label
    unit <- unique(selectedData$unit)
    round <- GeoPT_assigned_values[round == input$round.select & analyte == input$xcol, GeoPTround,]
    analyte <- GeoPT_assigned_values[round == input$round.select & analyte == input$xcol, analyte,]
    name <- GeoPT_assigned_values[round == input$round.select & analyte == input$xcol, name,]
    sample <- GeoPT_assigned_values[round == input$round.select & analyte == input$xcol, sample,]
    
    ggplot(df, aes(reorder(method, measurand), measurand)) +
      geom_hline(yintercept =  x + 2*horwitz, colour = "gray20", linetype="solid") + # horwitz
      geom_hline(yintercept =  x -  2*horwitz, colour="gray20", linetype="solid") + # horwitz
      geom_hline(yintercept =  x + 4*horwitz, colour = "gray70", linetype="solid") + # horwitz*2
      geom_hline(yintercept =  x - 4*horwitz, colour="gray70", linetype="solid") + # horwitz*2
      geom_hline(yintercept = x, colour="aquamarine3") + # median
      geom_violin(scale = "count") +
      geom_jitter(aes(fill=method), size = 5, alpha = .8, position = position_jitter(width = .15), shape=21) +
      stat_summary(fun.y=median, geom="point", fill="white", shape=21, size=3.5) +
      ggtitle(name) +
      ylab(unit) + 
      labs(colour = "prep") +
      scale_fill_manual(values=c(CSAN="#fdae61",
                                 CVAAS="gray80",
                                 EDXRF="#d73027",
                                 ETAAS="#ffffbf",
                                 FLAAS="#66bd63",
                                 GFAAS="#56B4E9",
                                 GRAV="#f46d43",
                                 GRAV_VOL="black",
                                 HYAAS ="#d9ef8b",
                                 ICP="#1a9850", 
                                 ICP_MS="#006837",
                                 INAA="#dfc27d",
                                 IRS="#a6d96a",
                                 ISE="darkorchid2",
                                 other="#80cdc1",
                                 SPHOT="#8073ac",
                                 UVS="gray50",
                                 VOL="#66bd63",
                                 WDXRF="#a50026")) +
      theme_bw() +
      theme(axis.text.x = element_text(angle=45, hjust=1, vjust=1, size = rel(1.2))) +
      theme(legend.text=element_text(size=rel(1.2))) +
      theme(legend.position="top") +
      theme(plot.title = element_text(colour = "blue", size = 25)) # for format of title
    
      })
  
  output$table1 <- renderTable({
    est.tab <- GeoPT_assigned_values[round == input$round.select & analyte == input$xcol, ]
    #est.tab <- GeoPT_assigned_values[round == "42" & analyte == "SiO2", ]
    
    tab.u <- cbind(est.tab$GeoPTround,
                   est.tab$name,
                   est.tab$analyte,
                   est.tab$unit,
                   est.tab$assigned,
                   est.tab$uncertainty,
                   est.tab$`Horwitz Target value`,
                   est.tab$`Number of reported results`,
                   est.tab$status,
                   est.tab$type)
    
    tab.u <- as.data.frame(tab.u)
    names(tab.u) <- c("round", "name", "analyte", "unit", "consensus", "uncertainty","Horwitz target value" ,"n", "status", "type")
    tab.u
    })
  
  
} # final curly bracket for server

shinyApp(ui = ui, server = server, 
         options = list(height = 800))
