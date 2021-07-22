# Load Packages ----
library(shiny)
library(shinydashboard)
library(shinyBS)
library(treemap)
library(shinyWidgets)
library(boastUtils)
library(ggplot2)
library(DT)
library(RColorBrewer)


# Load additional dependencies and setup functions
originaldata <- read.csv("originalDataset.csv", TRUE)
dataf = data.frame(originaldata)

population <- read.csv("population.csv",TRUE)
datafP = data.frame(population)

sample <- read.csv("sample.csv", TRUE)
datafS = data.frame(sample)

electionPopulationEW <- read.csv("electionPopulationRace.csv",TRUE)
eleDatafEW = data.frame(electionPopulationEW)

table1 <- read.csv("table_1.csv", TRUE)
table2 <- read.csv("table_2.csv", TRUE)
table3 <- read.csv("table_3.csv", TRUE)

# source("global.R")   

# Define UI for App ----
ui <- list(
  ## Create the app page ----
  dashboardPage(
    skin = "red",
    ### Create the app header ----
    dashboardHeader(
      title = "Weight Adjustment", 
      titleWidth = 250,
      tags$li(class = "dropdown", actionLink("info", icon("info"))),
      tags$li(class = "dropdown",
        boastUtils::surveyLink(name = "Weight_Adjustment_in_Surveys")
      ),
      tags$li(
        class = "dropdown",
        tags$a(href = 'https://shinyapps.science.psu.edu/',
               icon("home")
        )
      )
    ),
    
    ### Create the sidebar/left navigation menu ----
    dashboardSidebar(
      sidebarMenu(
        id = "pages",
        menuItem("Overview", tabName = "overview", icon = icon("dashboard")),
        menuItem("Prerequisites", tabName = "prerequisites", icon = icon("book")),
        menuItem("Explore", tabName = "explore", icon = icon("gamepad")),
        menuItem("Challenge", tabName = "challenge", icon = icon("gamepad")), 
        menuItem("References", tabName = "references", icon = icon("leanpub"))
        ),
      tags$div(
        class = "sidebar-logo",
        boastUtils::sidebarFooter()
      )
    ),
    
    
    ### Create the content ----
    dashboardBody(
      tabItems(
        #### Set up the Overview Page ----
        tabItem(
          tabName = "overview",
          withMathJax(),
          h1("Weight Adjustment in Surveys"), # This should be the full name.
          p("Explore how weighting adjustment affects the predicted results in survey analysis."),
          h2("Instructions"),
          tags$ol(
            tags$li("Move the sliders abound to explore how the weighting adjustment affects the results"),
            tags$li("Use your best judgement to find out the correct adjustment weight for each scenario."),
            tags$li("Notice that the summation bar should extend to n because the weighted sample should have the same n as the original sample.")
          ),
          br(),
          ##### Go Button--location will depend on your goals ----
          div(
            style = "text-align: center",
            bsButton(
              inputId = "go1",
              label = "GO!",
              size = "large",
              icon = icon("bolt"),
              style = "default"
            )
          ),
          ##### Create two lines of space ----
          br(),
          br(),
          h2("Acknowledgements"),
          p(
            "This app was developed and coded by Yuxin Zhang in 2017 and updated by Luxin Wang and Thomas McIntyre in 2018.",
            br(),
            br(),
            br(),
            br(),
            br(),
            div(class = "updated", "Last Update: 06/17/2021 by Qiaojuan Tu.")
          )
        ),
        #### Set up the Prerequisites Page ----
        tabItem(
          tabName = "prerequisites",
          withMathJax(),
          h2("Prerequisites"),
          p("In order to get the most out of this app, please review the
            following:"),
          tags$ul(
            tags$li("A selected sample may not be a good representation of a population due to many reasons. 
                         Non-response rate is one of the biggest challenges."),
            tags$li("When some values of a variable measured in the survey are 
                         under- or over-represented, statisticians use a weighting adjustment as a common correction technique in an attempt to remove bias in a survey."),
            tags$li("Each survey respondent gets an adjustment weight. Subjects in underrepresented groups get a weight larger than one, 
                         and subjects in overrepresented groups get a weight smaller than one.")
          ), 
          br(),
          br(), 
          div(
            style = "text-align: center",
            bsButton(
              inputId = "explore1",
              label = "Explore!",
              size = "large",
              icon = icon("bolt"),
              style = "default"
            )
          )
        ),
        
        #### Set up an Explore Page ----
        tabItem(
          tabName = "explore",
          withMathJax(),
          h2("Weight adjustment with known population"), 
          br(), 
          p(
            tags$strong(
              "Context:"
            )
          ),
          p("In order to find out whether The Ellen Show or The Late Night Show
            is more popular in our campus, we did a survey of 100 students.
            However, the sample was unrepresentative of the population because 
            the proportion of females in the sample was much larger than the
            proportion of females in the population. Therefore, we need to 
            apply a weighting adjustment to the data we got. Based on the 
            following table and the proportion graph, can you calculate the
            correct weights for both males and females? "), 
          fluidRow(
            column(
              width = 6,
              DT::DTOutput(
                "table1_PopSample", 
                width = "50%", 
                height = "auto"
              )
            ), 
            column(
              width = 6,
              DT::DTOutput("table2_sample", 
                           width = "50%", 
                           height = "auto"
              )
            )
          ),
          fluidRow(
            column(width = 6,  
                   br(),
                   br(),
                   wellPanel(
                     p(
                       "Answering the following questions according to",
                       tags$strong("Table 1."),
                       "and the proportion bar plot on the right:"),
                     br(),
                     p("1. What is the weight for Males to make the sample align with the population? "), 
                     numericInput(
                       inputId = "male_weight", 
                       label = NULL, 
                       value = "", 
                       min = 0,
                       step = 0.1,
                       max = 10, 
                       width = "50%"
                     ), 
                     actionButton(
                       inputId = "submit1",
                       label = "Submit", 
                       size = "small", 
                       style = "default",
                     ), 
                     uiOutput("question1"), 
                     uiOutput("question1_hint"),
                     br(), 
                     br(), 
                     p("2. What is the weight for Females to make the sample align with the population?"), 
                     numericInput(
                       inputId = "female_weight", 
                       label = NULL, 
                       value = "", 
                       min = 0, 
                       step = 0.1, 
                       max = 10, 
                       width = "50%"
                     ), 
                     actionButton(
                       inputId = "submit2", 
                       label = "submit", 
                       size = "small", 
                       style = "default", 
                     ), 
                     uiOutput("question2"), 
                     uiOutput("question2_hint")
                   ), 
            ), 
            br(),
            br(), 
            br(), 
            column(width = 6, 
                   plotOutput("barPopSample")
            )
          ),
          br(), 
          br(), 
          p(
            tags$strong("Explore more about this question based on Table 2.:"
            )
          ),
          br(),
          fluidRow(
            column(
              width = 4, 
              wellPanel(
                p("Instruction: Move the slider bar and watch how weighting 
                  adjustment affects the graphs and the summation bar. 
                  The explanation of this question will pop up at the end when
                  you get to the correct weights for both Males and Females. "
                ),
                br(),
                sliderInput(
                  "male", 
                  "Weight for Males",
                  min = 0,
                  value = 1, 
                  max = 2, 
                  step = 0.1
                ), 
                textOutput("hintM"),
                br(),
                sliderInput(
                  "female", 
                  "Weight for Females", 
                  min = 0, 
                  value = 1, 
                  max = 2, 
                  step = 0.01
                ), 
                textOutput("hintF"), 
                br(),
                
                uiOutput("warning"),
                plotOutput("HorizontalBar",
                           width = "100%", 
                           height = "250px"
                )
              )
            ), 
            column(
              width = 4, 
              plotOutput("bar")
            ), 
            plotOutput("samplePop"),
            column(width = 8,
                   conditionalPanel(
                     condition = "(input.male == 1.6) & (input.female == 0.74)", 
                     wellPanel(
                       h2("Congrats!"
                       ), 
                       p("The proportion of females is much larger than the 
                         proportion of males in the sample, which does not
                         represent the population well. Before the weighting
                         adjustment, the rate of support for The Ellen Show is
                         then overestimated because the females preferred that 
                         show. But after the weighting adjustment, the rate of 
                         support for The Ellen Show is almost the same as for 
                         the Late Night Show."
                       ), 
                       p("We can make the response representative with respect to gender. The weight is
                                                            obtained by dividing the population percentage by the corresponding response percentage."
                       ), 
                       p("If you understand the weighting adjustment with the population distribution known,
                                                            please go to the challenge page to explore the weighting technique with the population 
                                                            distribution unknown."
                       )
                     ) 
                   )
            )
            
          ),
          div(
            style = "text-align: center",
            bsButton(
              inputId = "challenge",
              label = "Challenge!",
              size = "large",
              icon = icon("bolt"),
              style = "default"
            )
          )
        ),
        
        #### Set up the Challenge Page ----
        tabItem(
          tabName = "challenge",
          withMathJax(),
          h2("Weight adjustment with unknown population"), 
          br(), 
          p(
            tags$strong(
              "Context:"
            )
          ),
          fluidRow(
            column(width = 6,
                   p("In order to examine the preferences of different
                     demographic groupings in an election, statisticians
                     conduct an exit poll on election day  (or by phone for
                     those who already voted).  Table 3 gives exit poll data 
                     from the 2020 election broken down by race/ethnicity. Often
                     pollsters will over-sample smaller demographic groups in 
                     order to get enough responses to have a reasonable Margin
                     of Error for describing their opinions. Suppose for example
                     , that each of the five groups in Table 3 had the same
                     number of people sample.  That would make weighting very
                     important for statements about the electorate as a whole 
                     since the groups are clearly not of equal size amongst all
                     voters. Draw on what you learned in the previous explore
                     page, and try to find weights for each group that make this
                     exit pollgive the actual final election results. You can
                     explore the different weights by moving slider bars for
                     each group below, and confirm your answers when weights
                     give the true results."
                   ), 
                   wellPanel(
                     p("Question:"), 
                     p("What is the weight for Biden to match the example unweighted
                       total align with the actual election results?"), 
                     selectInput(
                       inputId = "white_election", 
                       label = NULL, 
                       choices = list("","More than one", "Less than one"),
                       selected = FALSE,
                       multiple = FALSE, 
                       width = "100%"
                     ), 
                     actionButton(
                       inputId = "white_submit", 
                       label = "Submit", 
                       size = "small", 
                       style = "defalt"
                     ), 
                     uiOutput("white_question")
                   )
            ), 
            column(
              width = 6,
              DT::DTOutput("table3_election", 
                           width = "80%", 
                           height = "auto"
              )
            )
          ),
          br(), 
          br(), 
          p(
            tags$strong(
              "Matching Weights:"
            )
          ), 
          fluidRow(
            column(
              width = 4,
              wellPanel(
                p("Instruction: Play around the sliders to see how each affects the graph on the right. Try to calculate the weights for each ethnicity."), 
                sliderInput("white", label = "White", min = 0, value = 1, max = 2, step = 0.01),
                sliderInput("black", label = "Black", min = 0, value = 1, max = 2, step = 0.01),
                sliderInput("hispanic", label = "Hispanic/Latino", min = 0, value = 1, max = 2, step = 0.01),
                sliderInput("asian", label = "Asian", min = 0, value = 1, max = 2, step = 0.01),
                sliderInput("other", label = "Other", min = 0, value = 1, max = 2, step = 0.01)
              )
            ), 
            column(width = 8, 
                   plotOutput(
                     "elePopWBar"
                   ) 
            ), 
            br(),
            br(), 
            br(), 
            br(), 
            br(),
            br(), 
            br(), 
            br(), 
            br(),
            conditionalPanel(
              condition = "(input.white + input.black + input.hispanic + input.asian + input.other == 5)
              & (0.57*input.white/(input.white + input.black + input.hispanic + input.asian + input.other) +
                 0.12*input.black/(input.white + input.black + input.hispanic + input.asian + input.other) +
                 0.32*input.hispanic/(input.white + input.black + input.hispanic + input.asian + input.other) +
                 0.31*input.asian/(input.white + input.black + input.hispanic + input.asian + input.other) +
                 0.04*input.other/(input.white + input.black + input.hispanic + input.asian + input.other) == 0.47) 
              & (0.01*input.white/(input.white + input.black + input.hispanic + input.asian + input.other) +
                 0.01*input.black/(input.white + input.black + input.hispanic + input.asian + input.other) +
                 0.02*input.hispanic/(input.white + input.black + input.hispanic + input.asian + input.other) +
                 0.06*input.asian/(input.white + input.black + input.hispanic + input.asian + input.other) +
                 0.02*input.other/(input.white + input.black + input.hispanic + input.asian + input.other) == 0.02)
              & (0.42*input.white/(input.white + input.black + input.hispanic + input.asian + input.other) +
                 0.87*input.black/(input.white + input.black + input.hispanic + input.asian + input.other) +
                 0.66*input.hispanic/(input.white + input.black + input.hispanic + input.asian + input.other) +
                 0.63*input.asian/(input.white + input.black + input.hispanic + input.asian + input.other) +
                 0.58*input.other/(input.white + input.black + input.hispanic + input.asian + input.other) == 0.51)", 
              textOutput("Congratulation")
            )
          )
        ),
        
        #### Set up the References Page ----
        tabItem(
          tabName = "references",
          withMathJax(),
          h2("References"),
          p(
            class = "hangingindent",
          "Bailey, E. (2015). shinyBS: Twitter bootstrap components for shiny.
            (v0.61). [R package]. Available from
            https://CRAN.R-project.org/package=shinyBS"
          ),
          p(
            class = "hangingindent", 
            "Carey, R. (2019), boastUtils: BOAST Utilities, R Package. Available from https://github.com/EducationShinyAppTeam/boastUtils"
          ), 
          p(
            class = "hangingindent", 
            "Chang, W. and Borges Ribeio, B. (2018), shinydashboard: Create 
            dashboards with 'Shiny,' R Package. Available from https://CRAN.
            R-project.org/package=shinydashboard"
          ), 
          citation("ggplot2"), 
          p(
            class = "hangingindent", 
            "Chang, W., Cheng, J., Allaire, J., Xie, Y., and McPherson, J. (2021),
            shiny: Web application framework for R, R Package. Available from
            https://CRAN.R-project.org/package=shiny"
          ), 
          p(
            class = "hangingindent",
            "Chang W, Cheng J, Allaire J, Xie Y and Mcpherson J (2017). 
          shiny: Web Application Framework for R. R package version 1.0.3"
          ),
          p(
            class = "hangingindent", 
            "Neuwirth E. (2014), RColorBrewer: ColorBrewer Palettes, R Package.
            Available from https://CRAN.R-project.org/package=RColorBrewer"
          ), 
          p(
            class = "hangingindent", 
            "Perrier, V., Meyer, F., Granjon, D., Fellows, I., and Davis, W. (2020),
            shinyWidgets: Custom Inputs Widgets for Shiny, R package. Available from
            https://cran.r-project.org/web/packages/shinyWidgets/index.html"
          ), 
          p(
            class = "hangingindent", 
            "Tennekes M. and Ellis P. (2017), treemap: Treemap visualization, R Package.
            Available from https://CRAN.R-project.org/package=treemap"
          ), 
          p(
            class = "hangingindent", 
            "Xie Y., Cheng J., Tan X., and Allaire J. (2021), DT: A Wrapper of the 
            JavaScript Library 'DataTables'. R Package. Available from  
            https://CRAN.R-project.org/package=DT"
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
  ## Set up Info button ----
  observeEvent(
    eventExpr = input$info,
    handlerExpr = {
      sendSweetAlert(
        session = session,
        type = "info",
        title = "Information",
        text = "Move the sliders to explore how the weight adjustments affect the result. "
      )
    }
  )
  
  ## Set up the Go Button ----
  observeEvent(
    eventExpr = input$go1, 
    updateTabItems(session, "pages", "prerequisites")
  )
 
  ## Set up the Explore Button ----
  observeEvent(
    eventExpr = input$explore1, 
    updateTabItems(session, "pages", "explore")
  )
  
  ## Set up the Challenge Button ----
  observeEvent(
    eventExpr = input$challenge, 
    updateTabItems(session, "pages", "challenge")
  )
  
  ## Set up the Explore Page Server ----
  ### create table1 for explore page
  names(table1) <- c("", "Males", "Females")
  output$table1_PopSample <- DT::renderDT(
    expr = table1, 
    caption = "Table 1. The percentage of male and female in both sample and population", 
    autoHideNavigation = TRUE, 
    rownames = FALSE,
    style = "bootstrap4",
    options = list(
      ordering = FALSE,
      scrollX = FALSE, 
      paging = FALSE, 
      searching = FALSE, 
      info = FALSE, 
      responsive = FALSE 
    )
  )
  
  ### create table2 for explore page 
  names(table2) <- c("", "Males", "Females", "Total")
  output$table2_sample <- DT::renderDT(
    expr = table2, 
    caption = "Table 2. The sample results on 100 students",
    autoHideNavigation = TRUE, 
    rownames = FALSE, 
    style = "bootstrap4", 
    options = list(
      ordering = FALSE, 
      scrollX = FALSE, 
      paging = FALSE, 
      searching = FALSE, 
      info = FALSE, 
      responsive = FALSE
    )
  )
  
  ### update question 1 button 
  observeEvent(
    eventExpr = input$submit1, 
    updateTabItems(
      session = session,
      inputId = "submit1",
      output$question1 <- renderIcon(
        if (input$male_weight == 1.6) {condition = "correct"}
        else {condition = "incorrect"}
      )
    )
  )
  
  observeEvent(
    eventExpr = input$submit1, 
    updateTabItems(
      session = session, 
      inputId = "submit1", 
      output$question1_hint <- renderText(
        if (input$male_weight == 1.6) {print("Correct! The weight for Males is 48/30 = 1.6.")}
        else {print("Hint: The weight is used to match the sample Males with the population Males.")
        }
      )
    )
  )
  
  ### update question 2 button 
  observeEvent(
    eventExpr = input$submit2, 
    updateTabItems(
      session = session, 
      inputId = "submit2",
      output$question2 <- renderIcon(
        if (input$female_weight == 0.74 | input$female_weight == 0.743) {condition = "correct"}
        else {condition = "incorrect"}
      )
    )
  )
  
  observeEvent(
    eventExpr = input$submit2, 
    updateTabItems(
      session = session, 
      inputId = "submit2", 
      output$question2_hint <- renderText(
        if (input$female_weight == 0.74 | input$female_weight == 0.743) {
          print(
            "Correct! The weight for Females is 52/70 = 0.74."
          )
        }
        else {
          print(
            "Hint: The weight is used to match the sample Females with the 
            population Females."
          )
        }
      )
    )
  ) 
  
  ### Gender Proportion Bar Plot 
  output$barPopSample <- renderPlot(
    {
      barTable <- matrix(c(52,70,48,30),ncol = 2, byrow = TRUE)
      colnames(barTable) <- c("Poplulation", "Sample")
      barTable <- as.table(barTable)
      barplot(barTable, main = "Gender Proportion Bar Plot",
              col = c("#FBB4AE","#B3CDE3"),
              width = 0.8, 
              xlim = c(0,2),
              cex.names = 1, 
              cex.main = 1.3
      )
      par(lwd = 2)
      legend("topright", 
             c("Female","Male"),
             fill = c("#FBB4AE","#B3CDE3")
      )
    }, 
    width = 370, 
    height = 320)
  
  ### Horizontal Proportion Bar
  output$HorizontalBar <- renderPlot(
    {
      #### Form Data Vector 
      barTable <- matrix(c(70 * input$female, 30 * input$male), byrow = TRUE)
      colnames(barTable) <- c("")
      barTable <- as.table(barTable)
      #### Set line width and plot margins
      par(
        lwd = 3,
        mar = c(4,2,2,2)
      )
      #### Make background frame
      barplot(
        height = c(100.6),
        border = "black",
        col = "white",
        offset = 0.1,
        xlim = c(0, 100.6),
        ylim = c(0, 0.3),
        width = 0.2,
        horiz = TRUE,
        xlab = "Sample Size"
      )
      #### Make Weighted Bar
      barplot(
        barTable,
        border = NA,
        horiz = TRUE,
        add = TRUE,
        col = c("#FBB4AE","#B3CDE3"),
        width = 0.17,
        offset = 0.6,
        space = 0.325,
        xlim = c(0, 100),
        ylim = c(0, 0.3)
      )
      #### Thin frame for legend
      par(lwd = 1)
      #### Make legend
      legend(
        x = "top",
        inset = 0,
        horiz = TRUE,
        legend = c("Female","Male"),
        fill = c("#FBB4AE","#B3CDE3")
      )
    }
  )
  
  ### Supporting Rate of Both Show Graph
  output$bar <- renderPlot(
    {
      value = inputs()
    dataf[c(1,3),"Population"] = dataf[c(1,3),"Population"] *  value[1]
    dataf[c(2,4),"Population"] = dataf[c(2,4),"Population"] *  value[2]
    ellen = c(dataf[2,"Population"], dataf[1,"Population"])
    late = c(dataf[4,"Population"], dataf[3,"Population"])
    dataframe = data.frame("The Ellen Show" = ellen, "The Late Night Show" = late)
    matrix = as.matrix(dataframe)
    par(lwd = 2)
    barplot(matrix,
            col = c("#FBB4AE","#B3CDE3"),
            main = "Supporting Rate of Both Show", 
            width = 0.8,
            xlim = c(0,2),
            cex.names = 1,
            cex.main = 1.3,
            ylim = c(0,60)
    )
    par(lwd = 2)
    legend("topright",
           c("Female","Male"),
           fill = c("#FBB4AE","#B3CDE3")
    )
  },
  width = 370, 
  height = 320)
  
  ### Use Sample to Represent Population interactive Graph
  output$samplePop <- renderPlot({
    value = inputs()
    dataf[c(1,3),"Population"] = dataf[c(1,3),"Population"] *  value[1]
    dataf[c(2,4),"Population"] = dataf[c(2,4),"Population"] *  value[2]
    treemap(dataf,
            index = c("Gender","TVshow"),
            vSize = "Population",
            type = "index", 
            palette =  colorRampPalette(brewer.pal(4, "Pastel1"))(4), 
            title = "Use Sample to Represent Population",
            fontsize.title = 14,
            fontsize.labels = 16 
    )
  }, 
  width = 300, 
  height = 250)
  
   ### Hints for the slider bars panel (Explore Page)
   output$hintM <- renderText(
     if (input$male == 1.4 | input$male == 1.8) {print("You are close to the right answer.")}
     else if (input$male == 1.6) {print("Congratulations! You got the correct weight for male.")}
     
   )
   output$hintF <- renderText(
     if (input$female == 0.74 ) {print("Congratulations! You got the correct weight for female.")}
     else if (input$female >= 0.7 & input$female <= 0.8) {print("You are close to the right answer.")}
   )
   
  
  
  ## Set up the Challenge page server ----
   ### create table3 for the challenge page
   names(table3) <- c("Race/Ethnicity", "Trump", "Others","Biden","% of two party vote for Biden","Total")
   output$table3_election <- DT::renderDT(
    expr = table3, 
    caption = "Table 3. 2020 Election Exit Poll Results for Race/Ethnicity Groups.",
    autoHideNavigation = TRUE, 
    rownames = FALSE, 
    style = "bootstrap4", 
    options = list(
      ordering = FALSE, 
      scrollX = FALSE, 
      paging = FALSE, 
      searching = FALSE, 
      info = FALSE, 
      responsive = FALSE
    )
  )
 
   ### Percentage of the two party vote Graph 
   output$elePopWBar <- renderPlot({
     value = inputs()
     barplot(
       prop.table(
         rbind(
           c(eleDatafEW[1,4] * value[3],eleDatafEW[3,4] * value[3]),
           c(eleDatafEW[4,4] * value[4],eleDatafEW[6,4] * value[4]),
           c(eleDatafEW[7,4] * value[5],eleDatafEW[9,4] * value[5]),
           c(eleDatafEW[10,4] * value[6],eleDatafEW[12,4] * value[6]),
           c(eleDatafEW[13,4] * value[7],eleDatafEW[15,4] * value[7])
         )
       ),
       names.arg = c("Biden","Trump"), 
       main = "Percentage of the two party vote",
       col = brewer.pal(8, "YlOrBr")
     )
   },
   width = 400, 
   height = 400)
   
   ### update hint question submit button 
   observeEvent(
     eventExpr = input$white_submit, 
     updateTabItems(
       session = session, 
       inputId = "white_submit",
       output$white_question <- renderIcon(
         if (input$white_election == "More than one") {condition = "correct"}
         else {condition = "incorrect"}
       )
     )
   )
   ### Feedbacks for the slider bars panel 
   output$warning <- renderUI(
     if (input$male * 30 + input$female * 70 <= 100) {
       p("Note: Notice that the summation bar should never be larger or smaller than n=100 because the weighted sample should have the same sample size as the real sample.")
     }
     else{
       p("Warning: The summation is now larger than n.", style = "color: red")
     }
   )
   
   output$warningB <- renderUI({
     value = inputs()
     if (sum(value[3], value[4], value[5], value[6], value[7]) <= 1) {
       h5("Notice that the summation bar should never be larger or smaller than n=100 because the weighted sample should have the same sample size as the real sample.")
     }
     else{
       h5("Warning: The summation is now larger than n.", style = "color: red")
     }
   }
   )
   
   output$Congratulation <- renderText(
     print("Congratulations! You have successfully matched the sample unweighted results to the actual election results in 2016!"
     )
   )

   inputs = reactive(
     {
       value = c(input$male,input$female,input$white*0.5,input$black*0.2,input$hispanic*0.15,input$asian*0.10,input$other*0.05)
     }
   )
   ##progress bar
   ###progress bar for the explore page
   output$progress <- renderUI(
     if (
       sum(round(input$male * 30),
           round(input$female * 70)
       ) <= 100) 
     {
       tags$div(
         'class' = "progress progress-striped active",
         tags$div('class' = "progress-bar progress-bar-info",
                  'style' = paste0(
                    "width:",
                    round(input$male * 30),
                    "%",
                    sep = ''
                  )
         ),
         tags$div('class' = "progress-bar progress-bar-default", 
                  'style' = paste0(
                    "width:", 
                    round(input$female * 70),
                    "%",
                    sep = '')
         )
       )
     }
     else{
       tags$div(
         'class' = "progress progress-striped active",
         tags$div(
           'class' = "progress-bar progress-bar-info",
           'style' = paste0(
             "width:",
             round(input$male * 30),
             "%", 
             sep = '')
         ),
         tags$div(
           'class' = "progress-bar progress-bar-default", 
           'style' = paste0(
             "width:",
             100 - round(input$male * 30),
             "%", 
             sep = ''
           )
         )
       )
     }
   )

   ###progress bar for the challenge page
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
  }
  )
}

# Boast App Call 
boastUtils::boastApp(ui = ui, server = server)

