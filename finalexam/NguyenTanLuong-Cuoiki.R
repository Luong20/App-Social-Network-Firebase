library(shiny)
library(shinydashboard)
library(DT)
library(plotly)
library(shiny)
library(shinydashboard)
library(tidyverse)
library(ggplot2)
library(plotly)
library(dplyr)
# Define UI for application that draws a histogram
ui <- dashboardPage(skin = "red",
                    dashboardHeader(title = "NBA Stat"),
                    dashboardSidebar(
                      sidebarMenu(
                        menuItem("Dashboard", tabName = "Dashboard", icon = icon("tachometer-alt")),
                        menuItem("Team", tabName = "Team", icon = icon("user-friends")),
                        menuItem("Model",tabName = "DetailAnalysis1", icon = icon("rust")),
                        menuItem("Statistical",tabName = "DetailAnalysis2",icon = icon("chart-bar")),
                        menuItem("RawData",tabName = "DetailAnalysis3",icon = icon("table"))
                      )),
                    dashboardBody(
                      tabItems(
                        tabItem(tabName = "Dashboard",
                                fluidRow(
                                  valueBoxOutput("all_Player"),
                                  valueBoxOutput("all_Team"),
                                  valueBoxOutput("all_Salary")
                                ),
                                fluidRow(
                                  valueBoxOutput("all_Age"),
                                  valueBoxOutput("all_weight"),
                                  valueBoxOutput("all_Height")
                                ),
                                fluidRow(
                                  valueBoxOutput("all_Point"),
                                  valueBoxOutput("all_Assit"),
                                  valueBoxOutput("all_Rebounds")
                                ),
                                fluidRow(
                                  fluidPage(
                                    sidebarLayout(sidebarPanel(
                                      selectInput(
                                        inputId = "position",
                                        label = "Select Postion",
                                        choices = levels(xPosition),
                                        selected = "C"
                                      ),
                                      selectInput(
                                        inputId = "age",
                                        label = "Select Age",
                                        choices = levels(xAge),
                                        selected = "22"
                                      ),
                                      radioButtons(
                                        inputId = "all_characterstic",
                                        label = "Select data field",
                                        choices = c(
                                          
                                          "Height" = "Height_i",
                                          "Weight" = "Weight",
                                          "Salary" = "Salary",
                                          "Points" = "Points",
                                          "Rebounds"="Rebounds",
                                          "Assists" = "Assists"
                                        ),
                                        selected = "Height_i"
                                      )
                                    ),
                                    mainPanel(tabsetPanel(
                                      tabPanel("Summary", verbatimTextOutput("all_mysummary")),
                                      tabPanel("Piechart", plotlyOutput("all_pieChart")),
                                      tabPanel("Data", DT::dataTableOutput("dat"),style = "height:500px; overflow-y: scroll;overflow-x: scroll;")
                                    ))))
                                ),
                        ),
                        tabItem(tabName = "Team",
                                fluidRow(
                                  valueBoxOutput("Player"),
                                  valueBoxOutput("Team"),
                                  valueBoxOutput("Age")
                                ),
                                fluidRow(
                                  valueBoxOutput("Height"),
                                  valueBoxOutput("Point"),
                                  valueBoxOutput("Assit")
                                ),
                                fluidPage(
                                  sidebarPanel( 
                                    selectInput(
                                      inputId = "team",
                                      label = "Select Team",
                                      choices = levels(x),
                                      selected = "Atlanta Hawks"
                                    ),
                                    radioButtons(
                                      inputId = "characterstic",
                                      label = "Select data field",
                                      choices = c(
                                        "Age" = "Age",
                                        "Height" = "Height_i",
                                        "Weight" = "Weight",
                                        "Salary" = "Salary",
                                        "Points" = "Points",
                                        "Assists" = "Assists"
                                      ),
                                      selected = "Height_i"
                                    )
                                  ),
                                  
                                  mainPanel(
                                    tabBox(width = 12,
                                           tabPanel("Summary", verbatimTextOutput("mysummary")),
                                           tabPanel("Piechart", plotlyOutput("pieChart")),
                                           tabPanel("Data", DT::dataTableOutput("data"),style = "height:500px; overflow-y: scroll;overflow-x: scroll;")
                                    )
                                    
                                  )),
                                fluidRow(box(plotOutput("player_Position")),box(plotOutput("player_dif")))
                        ),
                        tabItem(tabName = "DetailAnalysis1",
                                fluidPage(
                                  sidebarPanel( 
                                    radioButtons(
                                      inputId = "model_choice",
                                      label = "Select data field",
                                      choices = c(
                                        "Height" = "Height_i",
                                        "Weight" = "Weight",
                                        "Salary" = "Salary",
                                        "Points" = "Points",
                                        "Assists" = "Assists"
                                      ),
                                      selected = "Points"
                                    )
                                  ),
                                  
                                  mainPanel(
                                    tabBox(width = 12,
                                           tabPanel("plot", plotOutput("all_ANA"),verbatimTextOutput("lmSummary"))
                                           
                                    )
                                    
                                  )
                                )
                                
                        ),
                        tabItem(tabName = "DetailAnalysis2",
                                fluidRow(
                                  box(width = 6,title = "Top 10 Best Player",status = "warning",solidHeader = TRUE,
                                      plotOutput("bestplot")),
                                  box(width = 6,title = "Top 10 Best Team",status = "primary",solidHeader = TRUE,
                                      plotOutput("bestTeam")),
                                  box(width = 6,title = "Top 10 Heighest Player",status = "danger",solidHeader = TRUE,
                                      plotOutput("highplot")),
                                  box(width = 6,title = "Top 10 Best Assists",status = "info",solidHeader = TRUE,
                                      plotOutput("Assistsplot")),
                                  box(width = 12,title = "Number of players on the team",status = "success",solidHeader = TRUE,
                                      plotOutput("teamplot")),
                                )
                        ),
                        tabItem(tabName = "DetailAnalysis3",
                                fluidRow(
                                  box( title = "Table Data", status = "primary", height = 
                                         "800",width = "12",solidHeader = T, 
                                       column(width = 12,
                                              DT::dataTableOutput("table"),
                                              style = "height:700px; overflow-y: scroll;overflow-x: scroll;"
                                       )
                                  )
                                )
                        )
                        
                      )
                      
                    )
)
server <- function(input, output) {
  
  dt=read.csv("D:\\Dataset\\players.csv")
  #PreProcess
  dt$College[which(dt$College=="")] <- "Unknow"
  dt$Salary[which(is.na(dt$Salary))] = mean(dt$Salary, na.rm=T)
  dt$Salary<-format(round(dt$Salary, 0))
  dt$Points[which(is.na(dt$Points))] <- mean(dt$Points, na.rm=T)
  dt$Points<-format(round(dt$Points, 2))
  dt$Rebounds[which(is.na(dt$Rebounds))] <-mean(dt$Rebounds, na.rm=T)
  dt$Rebounds<-format(round(dt$Rebounds, 2))
  dt$Assists[which(is.na(dt$Assists))] <- mean(dt$Assists, na.rm=T)
  dt$Assists<-format(round(dt$Assists, 2))
  
  #Add Column
  dt$Height_cm <-(dt$Height_i*30.48)
  dt$Height_cm<-format(round(dt$Height_cm, 0))
  
  dt$Weight_kg <- (dt$Weight*0.45359237)
  dt$Weight_kg<-format(round(dt$Weight_kg, 0))
  
  dt <- mutate(dt, Salary = as.numeric(Salary),
               Points = as.numeric(Points),
               Rebounds = as.numeric(Rebounds),
               Assists = as.numeric(Assists),
               Height_cm = as.numeric(Height_cm),
               Weight_kg = as.numeric(Weight_kg)
  )
  #factor
  x<-factor(dt$Team)
  xPosition <- factor(dt$Position)
  xAge <- factor(dt$Age)
  summary(dt$Height_cm)
  #all-Team
  output$all_mysummary = renderPrint({
    test <- dt[dt$Position == input$position,]
    summary(test[, input$all_characterstic][test$Age == input$age])
  })
  
  output$table <- renderDataTable({
    datatable(cbind(dt))
  })
  output$all_pieChart <- renderPlotly({
    test <- dt[dt$Position == input$position,]
    x1 <- factor(test[,input$all_characterstic][test$Age == input$age])
    varicount <- reactive(
      summary(x1)
    )
    pie_data <- data.frame(
      Categorie = levels(x1),
      Count = varicount()
    )
    plot_ly(
      pie_data,
      labels =  ~ Categorie,
      values =  ~ Count,
      type = 'pie'
    )
  })
  output$dat<-renderDataTable ({
    df <- dt[dt$Position == input$position,]
    dat <- data.frame(
      Name = df$Name[df$Age == input$age],
      Position = df$Position[df$Age == input$age],
      Age = input$age,
      Team = df$Team[df$Age == input$age],
      attribute_index = df[,input$all_characterstic][df$Age == input$age]
    )
    dat
  })
  output$all_Player <- renderValueBox({
    valueBox(
      length(dt$Name), "Player", icon = icon("user"),
      color = "aqua"
    )
  })
  output$all_Team <- renderValueBox({
    valueBox(
      length(table(dt$Team)), "Team", icon = icon("user-friends"),
      color = "light-blue"
    )
  })
  output$all_Salary <- renderValueBox({
    valueBox(
      paste0(max(dt$Salary)," $"), "Highest salary ", icon = icon("money-bill-alt"),
      color = "yellow"
    )
  })
  output$all_Age <- renderValueBox({
    valueBox(
      paste0(max(dt$Age),"-year-old"), "Oldest player", icon = icon("calendar"),
      color = "lime"
    )
  })
  output$all_weight <- renderValueBox({
    valueBox(
      paste0(max(dt$Weight), " lbs"), "Highest weight", icon = icon("weight"),
      color = "orange"
    )
  })
  output$all_Height <- renderValueBox({
    valueBox(
      paste0(max(dt$Height_i)," ft"), "Tallest height", icon = icon("ruler-vertical"),
      color = "green"
    )
  })
  output$all_Point <- renderValueBox({
    valueBox(
      max(dt$Points), "Highest point ", icon = icon("star"),
      color = "red"
    )
  })
  output$all_Assit <- renderValueBox({
    valueBox(
      max(dt$Assists), "Highest assists ", icon = icon("hands-helping"),
      color = "purple"
    )
  })
  output$all_Rebounds <- renderValueBox({
    valueBox(
      max(dt$Rebounds), "Highest rebound", icon = icon("hands-helping"),
      color = "maroon"
    )
  })
  #Team
  output$mysummary = renderPrint({
    summary(dt[,input$characterstic][dt$Team == input$team])
  })
  output$data<-renderDataTable ({
    team_data <- data.frame(
      Name = dt$Name[dt$Team == input$team ],
      Team = input$team,
      attribute_index = dt[,input$characterstic][dt$Team == input$team]
    )
    team_data
  })
  output$pieChart <- renderPlotly({
    
    x2 <- factor(dt[, input$characterstic][dt$Team == input$team])
    varicount <- reactive(
      summary(x2)
    )
    pie_data <- data.frame(
      Categorie = levels(x2),
      Count = varicount()
    )
    plot_ly(
      pie_data,
      labels =  ~ Categorie,
      values =  ~ Count,
      type = 'pie'
    )
  })
  output$Player <- renderValueBox({
    valueBox(
      length(dt$Name[dt$Team == input$team]), "Player", icon = icon("user"),
      color = "aqua"
    )
  })
  
  output$Team <- renderValueBox({
    valueBox(
      paste0(max(dt$Salary[dt$Team == input$team])," $"), "Highest salary ", icon = icon("money-bill-alt"),
      color = "yellow"
    )
  })
  output$Age <- renderValueBox({
    valueBox(
      paste0(max(dt$Age[dt$Team == input$team]),"-year-old"), "Oldest player", icon = icon("calendar"),
      color = "green"
    )
  })
  output$Height <- renderValueBox({
    valueBox(
      paste0(max(dt$Height_i[dt$Team == input$team])," ft"), "Tallest", icon = icon("thumbs-up"),
      color = "green"
    )
  })
  output$Point <- renderValueBox({
    valueBox(
      max(dt$Points[dt$Team == input$team]), "Highest point ", icon = icon("star"),
      color = "blue"
    )
  })
  output$Assit <- renderValueBox({
    valueBox(
      max(dt$Assists[dt$Team == input$team]), "Highest assists ", icon = icon("heart"),
      color = "purple"
    )
  })
  output$player_Position <- renderPlot({
    
    dt %>% 
      filter(Team == input$team) %>% 
      select(Position) %>% 
      drop_na() %>% 
      count(Position) %>% 
      mutate(Position = fct_reorder(Position, n)) %>% 
      ggplot(aes(x = n, y = Position, fill = Position)) + 
      geom_col() + 
      ylab("") + 
      labs(title = "Position") + 
      theme(legend.position = "none")
  })
  
  output$player_dif <- renderPlot({
    dt %>% 
      filter(Team == input$team) %>% 
      select(College) %>% 
      drop_na() %>% 
      count(College) %>% 
      mutate(College = fct_reorder(College, n)) %>% 
      ggplot(aes(x = n, y = College, fill = College)) + 
      geom_col() + 
      ylab("") + 
      labs(title = "Country") + 
      theme(legend.position = "none")
  })
  #statistical
  output$bestplot <- renderPlot({
    best =  dt %>% 
      group_by(Name)%>%
      summarise(Points)%>%
      arrange(desc(Points))
    g_genre <- ggplot(data = best[1:10,], aes(x = Name, y = Points, fill = Points));
    g_genre + geom_col(color="black")+theme(axis.text.x = element_text(angle = 30)) + xlab("Player") + ylab("Points") 
    
  })
  output$bestTeam <- renderPlot({
    bestteam =  dt %>% 
      group_by(Team)%>%
      summarise(sum_Points = sum(Points))%>%
      arrange(desc(sum_Points))          
    
    g_genre1 <- ggplot(data = bestteam[1:10,], aes(x = Team, y = sum_Points, fill = sum_Points));
    g_genre1 + geom_col(color = "black") +theme(axis.text.x = element_text(angle = 30))+ xlab("Team") + ylab("Points") 
  })
  output$highplot <- renderPlot({
    high =  dt %>% 
      group_by(name = Name)%>%
      summarise(high = Height_i)%>%
      arrange(desc(high))          
    
    g_genre1 <- ggplot(data = high[1:10,], aes(x = name, y = high, fill = high));
    g_genre1 + geom_col(color = "black") +theme(axis.text.x = element_text(angle = 30))+ xlab("Name") + ylab("Height")
  })
  output$Assistsplot <- renderPlot({
    assists =  dt %>% 
      group_by(name = Name)%>%
      summarise(Assists)%>%
      arrange(desc(Assists))          
    
    g_genre1 <- ggplot(data = assists[1:10,], aes(x = name, y = Assists, fill = Assists));
    g_genre1 + geom_col(color = "black") +theme(axis.text.x = element_text(angle = 30))+ xlab("Name") + ylab("Assists") 
  })
  output$teamplot <- renderPlot({
    teamplayer =  dt %>% 
      group_by(Team)%>%
      summarise(Player = length(Team))%>%
      arrange(desc(Player))          
    
    g_genre1 <- ggplot(data = teamplayer, aes(x = Team, y = Player, fill = Player));
    g_genre1 + geom_col(color = "black") +theme(axis.text.x = element_text(angle = 30))+ xlab("Team") + ylab("Player") 
  })
  #Model
  output$lmSummary <- renderPrint({
    #summary
    req(lm(dt$Age~dt[,input$model_choice], data= dt))
    summary(lm(dt$Age~dt[,input$model_choice], data= dt))
  })
  output$all_ANA <- renderPlot({
    ggplot(dt,aes(x=Age,y=dt[,input$model_choice],fill = dt[,input$model_choice]))+
      geom_line()
    plot(dt$Age~dt[,input$model_choice])
    abline(lm(dt$Age~dt[,input$model_choice]),col ="red")
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
