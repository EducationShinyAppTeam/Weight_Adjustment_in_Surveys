#Load Packages 
library(shiny)
library(shinydashboard)
library(shinyBS)
library(treemap)
library(shinyWidgets)
library(boastUtils)
library(RColorBrewer)
library(ggplot2)
library(readr)

# Define UI for App ----
ui <- list(
    ## Create the app page---
    dashboardPage(
        skin = "blue",
        ### Create the app header ----
        dashboardHeader(
            title = "Weight Adjustment On Sampling",
            titleWidth = 250,
            tags$li(class = "dropdown", actionLink("info", icon("info"))),
            tags$li(class = "dropdown", 
                    tags$a(
                        target = "_blank", icon("comments"),
                        href = "https://pennstate.qualtrics.com/jfe/form/SV_7TLIkFtJEJ7fEPz?appName=Weight_Adjustment_in_Surveys"
                    )),
            tags$li(class = "dropdown", tags$a(href = 'https://shinyapps.science.psu.edu/', icon("home")))
        ),
        
        ### Create the sidebar/left navigation menu ----
        dashboardSidebar(
            sidebarMenu(
                id = "pages",
                menuItem("Overview", tabName = "overview", icon = icon("dashboard")),
                menuItem('Prerequisites', tabName = 'preq', icon = icon('book')),
                menuItem("Easy Level", tabName = "easy", icon = icon("gamepad")),
                menuItem("Hard Level", tabName = "hard", icon = icon("gamepad"))), 
            tags$div(
                class = "sidebar-logo", 
                boastUtils::sidebarFooter()
            ) 
        ),
        
        ### Create the content ----
        dashboardBody(
            tabItems(
                #### Set up the Overview Page ----
                tabItem(tabName = "overview",
                        fluidPage(
                            h1("Weight Adjustment in Surveys"),
                            tags$a(href = 'http://stat.psu.edu/',tags$img(src = 'logo.png', align = "left", width = 180)),
                            br(),
                            br(),
                            br(),
                            h3(strong("About:")),
                            h4("Explore how weighting adjustment affects the predicted results in survey analysis."),br(),
                            h3(strong("Instructions:")),
                            h4(tags$li("Move the sliders around to explore how the weighting adjustment affects the results.")),
                            h4(tags$li("Use your bestjudgement to find out the correct adjustment weight for each scenario.")),
                            h4(tags$li("Notice that the summation bar should never be larger than one because the weighted sample should never be larger than the population.")),
                            div(style = "text-align: center",
                                bsButton("go","G O !",icon("bolt"),style = "danger",size = "medium",class = "circle grow")),
                            #bsButton("start", "GO", icon("bolt"),size = "large", style = "warning")),
                            br(),
                            h3(strong("Acknowledgements:")),
                            h4("This app was developed and coded by Yuxin Zhang and updated by Luxin Wang and Thomas McIntyre. The exit poll data set was extracted from", 
                               tags$a(href = "https://www.nytimes.com/interactive/2016/11/08/us/politics/election-exit-polls.html","Election 2016: Exit Polls.", style = "text-decoration: underline; color: #cd3333"),"on July 20, 2017.")
                        )
                ), 
                #### Set up the Prerequisites Page ----
                tabItem(tabName = 'preq',
                        h3(strong('Background: Weight Adjustment On Sampling')),br(),
                        h4(tags$li("A selected sample may not be a good representation of a population due to many reasons. 
                         Non-response rate is one of the biggest challenges.")),
                        h4(tags$li("When some variables measured in the survey are 
                         under- or over-represented, statisticians use a weighting adjustment as a common correction technique. ")),
                        h4(tags$li("Each survey respondent gets an adjustment weight. Subjects in underrepresented group get a weight more than one, 
                         and subjects in overrepresented group get a weight smaller than one.")),
                        br(),
                        div(style = "text-align: center",bsButton("start","Go to the overview",icon("bolt"),style = "danger",size = "medium",class = "circle grow"))
                ),
                #### Set up the Easy Level Page ----
                tabItem(tabName = "easy",
                        fluidPage(
                            #div(style="display: inline-block;vertical-align:top;",
                            #tags$a(href='https://shinyapps.science.psu.edu/',tags$img(src='homebut.PNG', width = 19))
                            #),
                            #div(style="display: inline-block;vertical-align:top;",
                            #circleButton("info",icon = icon("info"), status = "myClass",size = "xs")
                            #),
                            theme = "theme.css",
                            
                            tags$head(tags$style("#successM{color: red;
                                     font-size: 12px;
                                     font-style: italic;
                                     }"
                            )),
                            tags$head(tags$style("#successF{color: red;
                                     font-size: 12px;
                                     font-style: italic;
                                     }"
                            )),
                            tags$head(tags$style("#successO{color: red;
                                     font-size: 20px;
                                     font-style: italic;
                                     }"
                            )),
                            tags$head(tags$style("#successA{color: red;
                                     font-size: 20px;
                                     font-style: italic;
                                     }"
                            )),
                            tags$head(tags$style("#successH{color: red;
                                     font-size: 20px;
                                     font-style: italic;
                                     }"
                            )),
                            tags$head(tags$style("#successB{color: red;
                                     font-size: 20px;
                                     font-style: italic;
                                     }"
                            )),
                            tags$head(tags$style("#successW{color: red;
                                     font-size: 20px;
                                     font-style: italic;
                                     }"
                            )),
                            
                            tags$style(HTML(".js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar {background:#AED6F1}")),
                            tags$style(HTML(".js-irs-1 .irs-single, .js-irs-1 .irs-bar-edge, .js-irs-1 .irs-bar {background:#C0392B}")),
                            
                            titlePanel("Weighting adjustment with one auxiliary variable"),
                            
                            fluidPage(
                                fluidRow(
                                    wellPanel(h4("In order to find out between The Ellen Show and The Late Night Show which one is more 
                                 popular in our campus, we did a survey on 100 students. However, this sample cannot 
                                 represent the population well because the proportion of females in this sample is much 
                                 larger than the proportion of males in the population. Therefore, we need weighting adjustment
                                 to the data we got. Based on the following table and proportion graph, can you guess what is the 
                                 correct weight? Try playing around with both sliders following the instruction."),
                                              fluidRow(column(6,img(src = "image1.png", width = 200)),column(6,img(src = "image2.png", width = 300))))
                                ),
                                
                                fluidRow(
                                    wellPanel(
                                        #fluidRow(h3("Left is the treemap of gender proportion in population.")), 
                                        #fluidRow(h3("Right is the treemap of gender proportion in the sample.")),br(),
                                        fluidRow(h3("The Bar Plot showing the gender proportion")),
                                        #fluidRow(img(src = "arrow5.png", align = "right",width = 80)),
                                        fluidRow(plotOutput("barPopSample"))
                                        #class = "col-lg-4 col-md-6 col-sm-12 col-xs-12"
                                        #fluidRow(column(6,h3("Left is the treemap of gender proportion in population."), column(6,h3("Right is the treemap of gender proportion in the sample."))))
                                    )
                                ),
                                
                                fluidRow(
                                    uiOutput("warning"),
                                    uiOutput("progress"),
                                    div(style = "position: relative; top:-15px", div(style = "float: left", print("0")),div(style = "float:right", print("n"))),
                                    
                                    
                                    bsTooltip(id = 'male', 'Use the weight 48/30, population divided by sample', placement = 'top', trigger = 'click'),
                                    bsTooltip(id = 'female', 'Use the weight 52/70, population divided by sample', placement = 'top', trigger = 'click', options = NULL),
                                    
                                    wellPanel(
                                        sliderInput("male","Weight for Male:", min = 0, value = 1, max = 2, step = 0.1),
                                        textOutput("hintM"),
                                        #conditionalPanel("input.male == 1.6", textOutput("successM")),
                                        br(),
                                        sliderInput("female","Weight for Female", min = 0, value = 1, max = 2, step = 0.02),
                                        textOutput("hintF"),
                                        #conditionalPanel("input.female == 0.74", textOutput("successF"))
                                        class = "col-lg-4 col-md-6"),
                                    
                                    wellPanel(plotOutput("samplePop"), class = "wellBorder col-lg-3 col-md-6 col-sm-12 col-xs-12"),
                                    wellPanel(plotOutput("bar"), class = "wellBorder col-lg-4 col-md-6 col-sm-12 col-xs-12")
                                ),
                                
                                fluidRow(
                                    
                                    column(12,conditionalPanel(condition = "(input.male == 1.6) & (input.female == 0.74)",
                                                               wellPanel(h1(textOutput("Congrats")), 
                                                                         h5("The proportion of female is larger than the proportion of male in the sample, which does not
                                                            represent the population well. Before the weighting adjustment, the supporting rate of
                                                            The Ellen Show is much higher than that of The Late Night Show, but after the weighting adjustment,
                                                            the supporting rate of The Ellen Show is almost the same with that of the Late Night Show."),
                                                                         h5("This is a simple example of weighting adjustment with one auxiliary variable.
                                                            The population distribution is available so we can compare the response distribution of
                                                            sample with the population distribution."),
                                                                         h5("We can make the response representative with respect to gender. The weight is
                                                            obtained by dividing the population percentage by the corresponding response percentage.
                                                            The weight for male is 48 / 30 = 1.6 . The weight for female is 52 / 70 = 0.74 ."),
                                                                         h5("If you understand the weighting adjustment with the population distribution known,
                                                            please go to the hard level to explore the weighting technique with the population 
                                                            distribution unknown."))))
                                    
                                )))
                ),
                #### Set up the Hard Level page ---- 
                tabItem(tabName = "hard",
                        fluidPage(
                            #div(style="display: inline-block;vertical-align:top;",
                            #tags$a(href='https://shinyapps.science.psu.edu/',tags$img(src='homebut.PNG', width = 19))
                            #),
                            #div(style="display: inline-block;vertical-align:top;",
                            #circleButton("info1",icon = icon("info"), status = "myClass",size = "xs")
                            #),
                            theme = "sliderColor.css",
                            titlePanel("Weighting adjustment with unknown population"),
                            
                            fluidPage(
                                fluidRow(
                                    wellPanel(
                                        fluidRow(
                                            column(5,h4("In order to predict the result of an election correctly, statisticians need to use a weighting adjustment to deal with
                                    problems like non-response bias in analyzing samples. This is the exit poll data from 2016 election broken down by race/ethnicity.
                                    Try playing around with the sliders to see how big a difference do weights make.")),
                                            column(6,offset = 1,img(src = "electionRace.jpg", width = 300))), class = "well1"),
                                    column(6,div(style = "height:260px;",plotOutput("elePopEW"))),
                                    column(6, 
                                           column(5,plotOutput("elePopWBar")),
                                           column(5,div(style = "position: relative; left: 20px; top: 30px",
                                                        h5("Weights for each variable:"),
                                                        div(style = "height:35px", sliderInput("other",label = NULL, min = 0, value = 0.5, max = 1, step = 0.3)),
                                                        div(style = "height:35px", sliderInput("asian",label = NULL, min = 0, value = 0.5, max = 1, step = 0.2)),
                                                        div(style = "height:35px", sliderInput("hispanic",NULL, min = 0, value = 0.5, max = 1, step = 0.01)),
                                                        div(style = "height:35px", sliderInput("black",NULL, min = 0, value = 0.5, max = 1, step = 0.3)),
                                                        div(style = "height:35px", sliderInput("white",NULL, min = 0, value = 1, max = 2, step = 0.7))
                                           ),
                                           column(3,
                                                  div(style = "position: relative; left: 243px; bottom: 133px", h5("Other")),
                                                  div(style = "position: relative; left: 243px; bottom: 121px", h5("Asian")),
                                                  div(style = "position: relative; left: 243px; bottom: 111px", h5("Latino")),
                                                  div(style = "position: relative; left: 243px; bottom: 100px", h5("Black")),
                                                  div(style = "position: relative; left: 243px; bottom: 89px", h5("White"))
                                           )
                                           )),
                                    div(style = "position:relative; top:-140px",
                                        column(7,uiOutput("warningB")), column(5,div(style = "margin-top:7px",img(src = "legend.png", width = 400))),
                                        column(12,uiOutput("progressB")),
                                        div(style = "position: relative; top:-15px", div(style = "float: left", print("0")),div(style = "float:right", print("n"))))
                                    
                                ),
                                fluidRow(column(8, offset = 2,
                                                div(style = "position:relative; top:-420px;",
                                                    conditionalPanel(condition = "(input.white == 1.4) & (input.black == 0.6)
                                                      & (input.hispanic == 0.73) & (input.asian == 0.4) & (input.other == 0.6)",
                                                                     wellPanel(h1(textOutput("Congradulation")), class = "transparentpanel"))))
                                )
                                
                            )
                        )
                        
                ), 
                #### Set up the References Page ----
                tabItem( 
                    tabName = "references",
                    withMathJax(),
                    h2("References"),
                    p("You'll need to fill in this page with all of the appropriate
            references for your app."),
                    p(
                        class = "hangingindent",
                        "Bailey, E. (2015). shinyBS: Twitter bootstrap components for shiny.
              (v0.61). [R package]. Available from
              https://CRAN.R-project.org/package=shinyBS"
                    ),
                    br(),
                    br(),
                    br(),
                    boastUtils::copyrightInfo()
                )
            )
        )
    )
)


# Define server logic ----
server <- function(input, output, session) {
    ## Set up Info Button ---- 
    observeEvent(input$info,{
        sendSweetAlert(
            session = session,
            title = "Instructions:",
            text = "Move the sliders to explore how the weighting adjustment affects the results.",
            type = "info"
        )
    })
    
    observeEvent(input$info1,{
        sendSweetAlert(
            session = session,
            title = "Instructions:",
            text = "Move the sliders to explore how the weighting adjustment affects the results.",
            type = "info"
        )
    })
    
    ###UPDATE: adding the go button 
    
    observeEvent(input$go,{
        updateTabItems(session,"tabs","easy")
    })
    observeEvent(input$start,{
        updateTabItems(session,"tabs","overview")
    })}

##############################read in the data set ################################################
originaldata <- read_csv("originalDataset.csv", TRUE)
dataf = data.frame(originaldata)
population <- read_csv("population.csv",TRUE)
datafP = data.frame(population)
sample <- read_csv("sample.csv", TRUE)
datafS = data.frame(sample)
electionPopulationEW <- read_csv("electionPopulationRace.csv",TRUE)
eleDatafEW = data.frame(electionPopulationEW)


output$dataTable <- renderTable(read.csv("dataTable.csv"))
output$populationRatio <- renderTable(read.csv("PopulationRatio.csv"))



inputs = reactive({
    value = c(input$male,input$female,input$white*0.5,input$black*0.2,input$hispanic*0.15,input$asian*0.10,input$other*0.05)
})

#############################Easy level###########################################################


###UPDATE###
#create the static bar chart for Easy level (sample and pop)
output$barPopSample <- renderPlot({
    barTable <- matrix(c(52,70,48,30),ncol = 2, byrow = TRUE)
    colnames(barTable) <- c("Poplulation", "Sample")
    barTable <- as.table(barTable)
    
    barplot(barTable, main = "Gender Proportion",
            col = c("#FBB4AE","#B3CDE3"),
            width = 0.8, xlim = c(0,2),cex.names = 1, cex.main = 1.3)
    par(lwd = 2)
    legend("topright", c("Female","Male"), fill = c("#FBB4AE","#B3CDE3"))
    
}, width = 370, height = 320)



##create the interactive treemap for easy level
output$samplePop <- renderPlot({
    
    value = inputs()
    dataf[c(1,3),"Population"] = dataf[c(1,3),"Population"] *  value[1]
    dataf[c(2,4),"Population"] = dataf[c(2,4),"Population"] *  value[2]
    
    treemap(dataf, index = c("Gender","TVshow"), vSize = "Population", type = "index", 
            palette =  colorRampPalette(brewer.pal(4, "Pastel1"))(4), 
            title = "Use Sample to Represent Population", fontsize.title = 14, fontsize.labels = 16)
}, width = 300, height = 260)


##create the bar chart for Easy level
output$bar <- renderPlot({
    
    value = inputs()
    dataf[c(1,3),"Population"] = dataf[c(1,3),"Population"] *  value[1]
    dataf[c(2,4),"Population"] = dataf[c(2,4),"Population"] *  value[2]
    
    ellen = c(dataf[2,"Population"], dataf[1,"Population"])
    late = c(dataf[4,"Population"], dataf[3,"Population"])
    dataframe = data.frame("The Ellen Show" = ellen, "The Late Night Show" = late)
    matrix = as.matrix(dataframe)
    par(lwd = 2)
    barplot(matrix, col = c("#FBB4AE","#B3CDE3"), main = "Supporting Rate of Both Show", 
            width = 0.8, xlim = c(0,2),cex.names = 1, cex.main = 1.3, ylim = c(0,60))
    par(lwd = 2)
    legend("topright", c("Female","Male"), fill = c("#FBB4AE","#B3CDE3"))
    
}, width = 370, height = 320)

################################Hard level##############################################################
##horizontal bar plot
output$elePopEW <- renderPlot({
    value = inputs()
    
    barplot(prop.table(rbind(c(eleDatafEW[1,4],eleDatafEW[4,4],eleDatafEW[7,4],eleDatafEW[10,4],eleDatafEW[13,4]),
                             c(eleDatafEW[2,4],eleDatafEW[5,4],eleDatafEW[8,4],eleDatafEW[11,4],eleDatafEW[14,4]),
                             c(eleDatafEW[3,4],eleDatafEW[6,4],eleDatafEW[9,4],eleDatafEW[12,4],eleDatafEW[15,4])))
            ,horiz = TRUE, col = c("#002868","azure1","#BF0A30"), names.arg = c("White","Black","Latino","Asian","Other")
            , main = "Comparison of Two Candidates", las = 1
            , width = c(value[3],value[4],value[5],value[6],value[7])
    )
},width = 500, height = 300)

##vertical bar plot
output$elePopWBar <- renderPlot({
    value = inputs()
    
    barplot(prop.table(rbind(c(eleDatafEW[1,4] * value[3],eleDatafEW[3,4] * value[3]),
                             c(eleDatafEW[4,4] * value[4],eleDatafEW[6,4] * value[4]),
                             c(eleDatafEW[7,4] * value[5],eleDatafEW[9,4] * value[5]),
                             c(eleDatafEW[10,4] * value[6],eleDatafEW[12,4] * value[6]),
                             c(eleDatafEW[13,4] * value[7],eleDatafEW[15,4] * value[7])))
            , names.arg = c("Clinton","Trump")
            , col = brewer.pal(8, "YlOrBr")
    )
},width = 250, height = 300)

#####################################################################################################  
##Hints
output$hintM <- renderText(
    if (input$male == 1.4 | input$male == 1.8) {print("You are close to the right answer.")}
    else if (input$male == 1.6) {print("Congratulations! You got the correct weight for male.")}
    else {print("Move the slider to reach the correct weight.")}
)
output$hintF <- renderText(
    if (input$female == 0.74) {print("Congratulations! You got the correct weight for female.")}
    else if (input$female >= 0.7 & input$female <= 0.8) {print("You are close to the right answer.")}
    else {print("Move the slider to reach the correct weight.")}
)
output$Congrats <- renderText(
    print("Congrats!") 
)

output$Solutions <- renderText(
    print("Finding the correct weight is hard, especially when the population proportion is unknown. ")
)

##progress bar
output$progress <- renderUI({
    if (sum(round(input$male * 30),round(input$female * 70)) <= 100) {
        tags$div(
            'class' = "progress progress-striped active",
            tags$div('class' = "progress-bar progress-bar-info", 'style' = paste0("width:",round(input$male * 30),"%",sep = '')),
            tags$div('class' = "progress-bar progress-bar-default", 'style' = paste0("width:",round(input$female * 70),"%",sep = ''))
        )
    }else{
        tags$div(
            'class' = "progress progress-striped active",
            tags$div('class' = "progress-bar progress-bar-info", 'style' = paste0("width:",round(input$male * 30),"%",sep = '')),
            tags$div('class' = "progress-bar progress-bar-default", 'style' = paste0("width:",100 - round(input$male * 30),"%",sep = ''))
        )
    }
    
})
output$progressB <- renderUI({
    value = inputs()
    
    if (sum(value[3], value[4], value[5], value[6], value[7]) <= 1) {
        tags$div(
            'class' = "progress progress-striped active",
            tags$div('class' = "progress-bar progress-bar-success", 'style' = paste0("width:",value[7] * 100,"%",sep = '')),
            tags$div('class' = "progress-bar progress-bar-warning", 'style' = paste0("width:",value[6] * 100,"%",sep = '')),
            tags$div('class' = "progress-bar progress-bar-danger", 'style' = paste0("width:",value[5] * 100,"%",sep = '')),
            tags$div('class' = "progress-bar progress-bar-info", 'style' = paste0("width:",value[4] * 100,"%",sep = '')),
            tags$div('class' = "progress-bar progress-bar-default", 'style' = paste0("width:",value[3] * 100,"%",sep = ''))
        )
    }else{
        tags$div(
            'class' = "progress progress-striped active",
            tags$div('class' = "progress-bar progress-bar-success", 'style' = paste0("width:",value[7] * 100,"%",sep = '')),
            tags$div('class' = "progress-bar progress-bar-warning", 'style' = paste0("width:",value[6] * 100,"%",sep = '')),
            tags$div('class' = "progress-bar progress-bar-danger", 'style' = paste0("width:",value[5] * 100,"%",sep = '')),
            tags$div('class' = "progress-bar progress-bar-info", 'style' = paste0("width:",value[4] * 100,"%",sep = '')),
            tags$div('class' = "progress-bar progress-bar-default", 'style' = paste0("width:",(100 - sum(value[4], value[5], value[6], value[7])),"%",sep = ''))
        )
    }
    
})

##Feedbacks
output$warning <- renderUI({
    if (input$male * 30 + input$female * 70 <= 100) {
        h4("Notice that the summation bar should never be larger or smaller than one because the weighted sample should have the same sample size as the real sample.")
    }else{
        h4("Warning: The summation is now larger than n.",style = "color: red")
    }
})

output$warningB <- renderUI({
    value = inputs()
    
    if (sum(value[3], value[4], value[5], value[6], value[7]) <= 1) {
        h4("Notice that the summation bar should never be larger or smaller than one because the weighted sample should have the same sample size as the real sample.")
    }else{
        h4("Warning: The summation is now larger than n.",style = "color: red")
    }
})

output$Congradulation <- renderText(
    print("Congratulations! This is the result of 2016 Exit Polls.")
)


# Boast App Call ----

boastUtils::boastApp(ui = ui, server = server) 
