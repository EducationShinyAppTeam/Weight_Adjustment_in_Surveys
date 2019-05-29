library(shiny)
library(treemap)
library(RColorBrewer)
library(ggplot2)
library(shinyBS)
library(shinydashboard)
library(shinyWidgets)

ui <- dashboardPage(
  dashboardHeader(title = "Weight Adjustment On Sampling", titleWidth = 300,
                  tags$li(class = "dropdown", 
                          tags$a(href = "https://shinyapps.science.psu.edu/", 
                                 icon("home", lib = "font-awesome"))),
                  tags$li(class = "dropdown", 
                          actionLink("info", icon("info"), class = "myClass"))),
  dashboardSidebar(width = 180,
    sidebarMenu(id='tabs',
      menuItem('Prerequisites', tabName='preq', icon=icon('book')),
      menuItem("Overview", tabName = "overview", icon = icon("dashboard")),
      menuItem("Easy Level", tabName = "easy", icon = icon("gamepad")),
      menuItem("Hard Level", tabName = "hard", icon = icon("gamepad"))
    )
  ),
  dashboardBody(
    tags$head( 
      tags$link(rel = "stylesheet", type = "text/css", href = "Feature.css")
    ),
    tabItems(
      tabItem(tabName = 'preq',
              
                h3(strong('Background: Weight Adjustment On Sampling')),br(),
                
                h4(tags$li("A selected sample may not be a good representation of a population due to many reasons. 
                   Non-response rate is one of the biggest challenges.")),
                
                h4(tags$li("When some variables measured in the survey are 
                   under- or over-represented, statisticians use a weighting adjustment as a common correction technique. ")),

                 h4(tags$li("Each survey respondent gets an adjustment weight. Subjects in underrepresented group get a weight more than one, 
                   and subjects in overrepresented group get a weight smaller than one.")),
                br(),
                div(style = "text-align: center",bsButton("start","Go to the overview",icon("bolt"),style = "danger",size = "medium",class="circle grow"))
              
        
      ),
      tabItem(tabName = "overview",
              
              fluidPage(
                tags$a(href='http://stat.psu.edu/',tags$img(src='logo.png', align = "left", width = 180)),
                br(),br(),br(),
                h3(strong("About:")),
                h4("Explore how weighting adjustment affects the predicted results in survey analysis."),br(),
                h3(strong("Instructions:")),
                h4(tags$li("Move the sliders around to explore how the weighting adjustment affects the results.")),
                h4(tags$li("Use your best judgement to find out the correct adjustment weight for each scenario.")),
                h4(tags$li("Notice that the summation bar should never be larger than one because the weighted sample should never be larger than the population.")),
                div(style = "text-align: center",
                    bsButton("go","G O !",icon("bolt"),style = "danger",size = "medium",class = "circle grow")),
                br(),
                h3(strong("Acknowledgements:")),
                h4("This app was developed and coded by Yuxin Zhang and updated by Luxin Wang and Thomas McIntyre. The exit poll data set was extracted from", 
                   tags$a(href = "https://www.nytimes.com/interactive/2016/11/08/us/politics/election-exit-polls.html","Election 2016: Exit Polls.", style = "text-decoration: underline; color: #cd3333"),"on July 20, 2017.")
                )
               ),
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
                              fluidRow(column(4,img(src = "image1.png", width = 200)),column(4,img(src = "image2.png", width = 300))))
                    ),
                  
                  fluidRow(
                   wellPanel(
                      fluidRow(h3("Left is the treemap of gender proportion in population.")), 
                      fluidRow(h3("Right is the treemap of gender proportion in the sample.")),
                      br(),
                      fluidRow(h3("Area represents the proportion.")),
                      fluidRow(img(src = "arrow5.png", align = "right",width = 80))
                      , class = "col-lg-4 col-md-6 col-sm-12 col-xs-12"),
                    wellPanel(plotOutput("population"), class = "wellBorder col-lg-4 col-md-6 col-sm-12 col-xs-12"),
                    wellPanel(plotOutput("sample"), class = "wellBorder col-lg-4 col-md-6 col-sm-12 col-xs-12")
                  ),
                  fluidRow(
                    uiOutput("warning"),
                    uiOutput("progress"),
                    div(style = "position: relative; top:-15px", div(style = "float: left", print("0")),div(style = "float:right", print("n"))),
                
                    
                    bsTooltip(id='male', 'Use the weight 48/30, population divided by sample', placement='top', trigger='click'),
                    bsTooltip(id='female', 'Use the weight 52/70, population divided by sample', placement='top', trigger='click',option=NULL),
                    
                    wellPanel(
                      sliderInput("male","Weight for Male:", min = 0, value = 1, max = 2, step = 0.02),
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
                              #Labels for the sliders
                              column(3,
                                div(style = "position: relative; left: 243px; bottom: 133px", h5("Other")),
                                div(style = "position: relative; left: 243px; bottom: 121px", h5("Asian")),
                                div(style = "position: relative; left: 243px; bottom: 111px", h5("Latino")),
                                div(style = "position: relative; left: 243px; bottom: 100px", h5("Black")),
                                div(style = "position: relative; left: 243px; bottom: 89px", h5("White"))
                              )
                            )),
                            
                            div(style = "position:relative; top:-140px",
                            column(7,uiOutput("warningB")), 
                            column(5,div(style = "",img(src = "legend.png", width = 400))),
                            column(12,uiOutput("progressB")),
                            div(style = "position: relative; top:-15px", div(style = "float: left", print("0")),div(style = "float:right", print("n"))))
                          
                          ),
                          fluidRow(column(8, offset = 2,
                            div(style = "position:relative; top:-450px;",
                                      conditionalPanel(condition = "(input.white == 1.4) & (input.black == 0.6)
                                                      & (input.hispanic == 0.73) & (input.asian == 0.4) & (input.other == 0.6)",
                                                      wellPanel(h1(textOutput("Congragulations!")), class = "transparentpanel"))))
                          )
                         
                            ))
              )
    )
  )
)

