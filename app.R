# Load packages ----
require(easyr)

begin(load=c())
# pkgs <- c("shiny", "dqshiny", "tidyverse", "formattable", "DT", "plotly", 
#           "reshape2", "ggplot2", "rchess", "ggstatsplot", "lubridate", 
#           "shinydashboard")
# lapply(pkgs, libary, character.only=TRUE)
library(shiny)
library(shinydashboard)
library(dqshiny)
library(tidyverse)
library(formattable)
library(DT)
library(plotly)
library(reshape2)
library(ggplot2)
library(rchess)
library(lubridate)
library(fst)
library(purrr)

# Source helpers ----
source("helpers.R")

# Load data sets ----
# load("./R_Data/players.rds")
load("./R_Data/player_level.rds")
# load("./R_Data/player_opening_level.rds")
# load("./R_Data/player_tmt_level.rds")
# play_opp_level <- read_fst("./R_Data/play_opp_level.fst")
twic_clean <- read_fst("./R_Data/twic_clean.fst")
players <- read_fst("./R_Data/fide_players_clean.fst")
ratings_prog <- read_fst("./R_Data/ratings_prog.fst")
ratings_pop <- read_fst("./R_Data/ratings_pop.fst")

# Global inputs ----
players_list <- player_level %>% distinct(player) %>% .$player
players_list2 <- ratings_prog %>% distinct(Name) %>% .$Name

# Run on sublime text
# R < app.R  --no-save

# User interface ----
ui <- dashboardPage(
  dashboardHeader(title = "Chess Stats Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Home",
               tabName = "home_tab",
               icon = icon("dashboard")),
      menuItem("Player Lookup",
               tabName = "player_tab",
               icon = icon("dashboard")),
      menuItem("Head to Head",
               tabName = "head_tab",
               icon = icon("dashboard")),
      menuItem("Opening Explorer",
               tabName = "opening_tab",
               icon = icon("dashboard"))
    )
  ),
  dashboardBody(
    shiny::tags$style(".small-box.bg-yellow { background-color: #FFFFFF !important; color: #000000 !important; }"),
    shiny::tags$style(".small-box.bg-teal { background-color: #D9D9D9 !important; color: #000000 !important; }"),
    shiny::tags$style(".small-box.bg-black { background-color: #595959 !important; color: #FFFFFF !important; }"),
    tabItems(
      tabItem(
        tabName = "home_tab",
        fluidRow(box(uiOutput("rating_selectorH")),
                 box(uiOutput("rating_goal_selectorH"))),
        # fluidRow(box(dataTableOutput("player_desc"), width = 12, column(align = "center", width = 12))),
        fluidRow(
          valueBoxOutput("road_time_card"),
          valueBoxOutput("road_game_card"),
          valueBoxOutput("road_success_card")
        ),
        fluidRow(
          box(plotlyOutput("road_to_scatter"), width = 12, column(align = "center", width = 12))
        ),
        fluidRow(
          box(dataTableOutput("road_to_desc"), width = 12, column(align = "center", width = 12))
        )
      ),
      tabItem(
        tabName = "player_tab",
        fluidRow(box(uiOutput("player_selector")), box(uiOutput("color_selector"))),
        fluidRow(box(dataTableOutput("player_desc"), width = 12, column(align = "center", width = 12))),
        fluidRow(
          box(plotlyOutput("p_opening_radar"), width = 6, column(align = "center", width = 6)),
          box(dataTableOutput("p_opening_desc"), width = 6, column(align = "center", width = 6))
        ),
        fluidRow(
          box(plotlyOutput("p_tmt_radar"), width = 6, column(align = "center", width = 6)),
          box(dataTableOutput("p_tmt_desc"), width = 6, column(align = "center", width = 6))
        )
        # ideas: ratin progress chart + road to 1800/2000/2200, etc..
        #        top 5 tournament performances (by rating performance)
      ),
      tabItem(tabName = "head_tab",
              fluidRow(box(uiOutput("player_selector2")), box(uiOutput("opponent_selector"))),
              fluidRow(
                box(valueBoxOutput("player_total_card"),
                           valueBoxOutput("player_white_card"),
                           valueBoxOutput("player_black_card")), 
                box(valueBoxOutput("opponent_total_card"),
                           valueBoxOutput("opponent_white_card"),
                           valueBoxOutput("opponent_black_card"))),
              fluidRow(box(sliderInput("p_elo_input",
                                       label = "Player ELO Range",
                                       min = 1000,
                                       max = 2900,
                                       step = 50,
                                       value = c(1000, 2900))),
                       box(sliderInput("o_elo_input",
                                       label = "Opponent ELO Range",
                                       min = 1000,
                                       max = 2900,
                                       step = 50,
                                       value = c(1000, 2900)))),
              fluidRow(box(sliderInput("date_input",
                                      label = "Date Range",
                                      min = as.Date("2012-01-01", "%Y-%m-%d"),
                                      max = as.Date("2022-10-01", "%Y-%m-%d"),
                                      value = c(as.Date("2012-01-01", "%Y-%m-%d"),
                                                as.Date("2022-10-01", "%Y-%m-%d")),
                                      timeFormat = "%b %Y"), width = 12)),
              fluidRow(
                box(dataTableOutput("p_opp_desc"), width = 12, column(align = "center", width = 12))
              )),
      tabItem(tabName = "opening_tab",
              fluidRow(box(uiOutput("opening_selector"))))
              # box(plotlyOutput("elo_timeseries")),
              # box(plotlyOutput("elo_dist")),
              # box(dataTableOutput("top_5_table")),
              # box(uiOutput("weight_class_selector_1")),
              # box(sliderInput(inputId = "v_k_1",
              #                 label = "K for ELO",
              #                 min = 1, 
              #                 max = 100,
              #                 value = 20))
    )
  )
)

server <- function(input, output) {
  
  # Drop downs ----
  output$player_selector <- renderUI({
    autocomplete_input(id = "v_player_1",
                       label = "Player",
                       options = players_list,
                       value = "Carlsen,M",
                       max_options = 1000)
  })
  output$color_selector <- renderUI({
    selectInput(inputId = "v_color_1",
                label = "Color",
                choices = c("White", "Black"))
  })
  output$player_selector2 <- renderUI({
    autocomplete_input(id = "v_player_2",
                       label = "Player",
                       options = players_list,
                       value = "Carlsen,M",
                       max_options = 1000)
  })
  output$opponent_selector <- renderUI({
    autocomplete_input(id = "v_opponent_1",
                       label = "Opponent",
                       options = players_list,
                       value = "Caruana,F",
                       max_options = 1000)
  })
  output$rating_selectorH <- renderUI({
    numericInput(
      inputId = "v_rating_H",
      label = "Current Rating",
      value = 2000, min = 1000, max = 2900)
  })
  output$rating_goal_selectorH <- renderUI({
    numericInput(
      inputId = "v_rating_goal_H",
      label = "Rating Goal",
      value = 2200, min = input$v_rating_H, max = 2900,
      step = 100)
  })
  
  # Road to XXXX Data Table ----
  # To Do: add average games per month
  roadToDesc <- reactive({
    a <- ratings_prog %>%
      filter(rating_from >= plyr::round_any(input$v_rating_H, 100, f=round) & 
        rating_to <= input$v_rating_goal_H) %>%
      group_by(Name) %>%
      summarise(
        n=n(),
        nmonths=sum(nmonths), 
        ngames=sum(ngames),
        is_start=any(rating_from == plyr::round_any(input$v_rating_H, 100, f=round)),
        is_end=any(rating_at_end >= input$v_rating_goal_H),
        start_date=min(start_date),
        end_date=max(end_date),
        start_rating=min(rating_at_start),
        end_rating=max(rating_at_end),
        start_age=min(start_age)
      ) %>%
      ungroup() %>%
      mutate(is_success=(is_start & is_end),
        start_age=ifelse(start_age>150, NA, start_age),
        age_group=plyr::round_any(start_age, 10, f=floor),
        age_group=paste(age_group, "-", age_group+10, sep=""))
  })

  # Road to XXXX Metrics ----
  roadToMetrics <- reactive({
    roadToDesc() %>%
      group_by() %>%
      summarise(
        nmonths=round(mean(nmonths[is_success])),
        ngames=round(mean(ngames[is_success])),
        nsuccess=sum(is_success)
      ) %>%
      ungroup() %>%
      mutate(
        nmonths=ifelse(nmonths<12, paste(nmonths, "months"), 
          paste(nmonths%/%12, "years", nmonths%%12, "months")),
        success_rate=(nsuccess/
          (ratings_pop %>% 
            filter(rating_group==plyr::round_any(input$v_rating_H, 100, f=round)) %>%
            .$nplayers)*100),
        success_rate=round(success_rate, digits=1)
      )
  })
  output$road_time_card <- renderValueBox({
    valueBox(
      value = roadToMetrics() %>% .$nmonths,
      subtitle = paste("Average Time to reach", input$v_rating_goal_H),
      color = "orange",
      icon = icon("hourglass-start")
    )
  })
  output$road_game_card <- renderValueBox({
    valueBox(
      value = paste(roadToMetrics() %>% .$ngames, "Games"),
      subtitle = paste("Average Games played to reach", input$v_rating_goal_H),
      color = "blue",
      icon = icon("chess-board")
    )
  })
  output$road_success_card <- renderValueBox({
    valueBox(
      value = paste(roadToMetrics() %>% .$success_rate, "%", sep=""),
      subtitle = paste(plyr::round_any(input$v_rating_H, 100, f=round), 
        "rated players reach", input$v_rating_goal_H),
      color = "green",
      icon = icon("plus-minus")
    )
  })

  # Road to XXXX fastest players ----
  roadToTable <- reactive({
    roadToDesc() %>%
      filter(is_success) %>%
      arrange(nmonths, ngames) %>%
      mutate(nyears=ifelse(nmonths<12, paste(nmonths, "months"), 
          paste(nmonths%/%12, "years", nmonths%%12, "months"))) %>%
      select(-is_start, -is_end, -is_success, -n, -age_group) %>%
      relocate(Name, nyears, nmonths, ngames, start_age, start_rating, start_date, end_rating, end_date) %>%
      rename(`Time (Years)`=nyears, `Time (Months)`=nmonths, `Games`=ngames, `Start Age`=start_age, 
        `Start Rating`=start_rating, `Start Date`=start_date, `End Rating`=end_rating, 
        `End Date`=end_date) %>%
      formattable(.) %>%
      as.datatable(escape = FALSE,
                   options = list(scrollX = TRUE),
                   rownames = FALSE) %>%
      formatNumeric(c("Time (Months)", "Games", "Start Age", "Start Rating", "End Rating"))
  })
  output$road_to_desc<- renderDataTable({
    roadToTable()
  })
  
  # Road to XXXX Scatter Plot ----
  output$road_to_scatter <- renderPlotly({
    roadToDesc() %>%
      filter(is_success) %>%
      plot_ly(
        data = .,
        x = ~ngames,
        y = ~nmonths,
        color = ~age_group,
        colors = "Set1",
        text = ~paste("Player: ", Name, "<br>",
                      "Starting Age: ", start_age, "<br>",
                      "Time to ", input$v_rating_goal_H, ": ", 
                      ifelse(nmonths<12, 
                        paste(nmonths, "months"),
                        paste(nmonths%/%12, "years", nmonths%%12, "months")), "<br>",
                      "Games Played: ", ngames, "<br>", 
                      start_rating, " on ", start_date, "<br>", 
                      end_rating, " on ", end_date, "<br>", sep=""),
        marker = list(symbol = "circle-open")
      ) %>%
      layout(title = paste("Road from", input$v_rating_H, "to", input$v_rating_goal_H))
  })
  
  # Player Summary Data Table ----
  filterPlayer1 <- reactive({
    twic_clean %>%
      filter(White==input$v_player_1 | Black==input$v_player_1) %>%
      mutate(
        player=input$v_player_1,
        color=if_else(White==input$v_player_1, "White", "Black"),
        opponent=if_else(color=="White", Black, White),
        player_rating=if_else(color=="White", WhiteElo, BlackElo),
        opp_rating=if_else(color=="White", BlackElo, WhiteElo),
        player_title=if_else(color=="White", WhiteTitle, BlackTitle),
        opp_title=if_else(color=="White", BlackTitle, WhiteTitle)
      ) %>%
      select(-White, -Black, -WhiteElo, -BlackElo)
      # group_by(player, rating, Opening, EvalDate) %>%
      # summarise(
      #   ngames=n(), nscore=sum(nscore), nmoves=mean(NMoves),
      #   ntourneys=n_distinct(Event), opp_elo=mean(rating, na.rm=TRUE)
      # ) %>%
      # ungroup()
  })
  filterColor1 <- reactive({
    filterPlayer1() %>%
      filter(color==input$v_color_1)
  })
  playerDesc <- reactive({
    filterPlayer1() %>%
      # filter(player==input$v_player_1) %>%
      drop_na(nscore) %>%
      group_by(player) %>%
      summarise(
        title = player_title[which.max(EvalDate)],
        curr_rating = player_rating[which.max(EvalDate)],
        peak_rating = max(player_rating, na.rm=TRUE),
        score_perc = sum(nscore)/n(),
        rating_perf = 0,
        tmts = n_distinct(Event),
        games = n(),
        openings = n_distinct(Opening),
        moves = mean(NMoves),
        # inputs for rating_perf calc - to drop later
        opp_elo = mean(opp_rating, na.rm=TRUE),
        nscore = sum(nscore)
        # other metrics to add - last active date, fed, age, 
        # title achieved date, last tournament played
      ) %>%
      ungroup() %>%
      mutate(
        rating_perf = opp_elo - if_else(nscore==games, -800, 
                                       ifelse(nscore==0, 800,
                                              log(games/nscore - 1)/log(10)*400)),
        rating_perf = round(rating_perf),
        moves = round(moves)
      ) %>%
      select(-player, -opp_elo, -nscore) %>%
      rename(`Title`=title, `Rating`=curr_rating, `Peak Rating`=peak_rating,
             `Score`=score_perc, `Rating Performance`=rating_perf, 
             `Tournaments`=tmts, `Games`=games, `Openings`=openings,
             `Average Moves`=moves) %>%
      formattable(.) %>%
      as.datatable(escape = FALSE,
                   options = list(scrollX = TRUE),
                   rownames = FALSE) %>%
      formatPercentage(c("Score"))
  })
  output$player_desc <- renderDataTable({
    playerDesc()
  })
  
  # Top 5 Openings Radar Chart + Data Table ----
  pOpeningRadar <- reactive({
    filterColor1() %>%
      # filter(player==input$v_player_1, color==input$v_color_1) %>%
      drop_na(nscore) %>%
      group_by(Opening) %>%
      summarise(
        last_played = max(EvalDate, na.rm=TRUE),
        score_perc = sum(nscore)/n(),
        rating_perf = 0,
        tmts = n_distinct(Event),
        games = n(),
        moves = mean(NMoves),
        # inputs for rating_perf calc - to drop later
        opp_elo = mean(opp_rating, na.rm=TRUE),
        nscore = sum(nscore)
      ) %>%
      ungroup() %>%
      slice_max(games, n = 5) %>%
      mutate(
        rating_perf = opp_elo - if_else(nscore==games, -800,
                                        ifelse(nscore==0, 800,
                                               log(games/nscore - 1)/log(10)*400)),
        rating_perf = round(rating_perf),
        moves = round(moves)
      ) %>%
      select(-opp_elo, -nscore)
  })
  pOpeningDesc <- reactive({
    pOpeningRadar() %>%
      rename(`Last Played`=last_played, `Score`=score_perc,
             `Rating Performance`=rating_perf, `Tournaments`=tmts,
             `Games`=games, `Average Moves`=moves) %>%
      formattable(.) %>%
      as.datatable(escape = FALSE,
                   options = list(scrollX = TRUE),
                   rownames = FALSE) %>%
      formatPercentage(c("Score"))
  })
  output$p_opening_radar <- renderPlotly({
    pOpeningRadar() %>%
      plot_ly(
        type = "scatterpolar",
        r = .$score_perc,
        theta = .$Opening,
        fill = "toself"
      ) %>%
      layout(title = paste(input$v_player_1, "Most Played",
                           input$v_color_1, "Openings"),
             polar = list(radialaxis = list(range = c(0, 1))))
  })
  output$p_opening_desc <- renderDataTable({
    pOpeningDesc()
  })
  
  # Top 5 Tournaments Radar Chart + Data Table ----
  pTmtRadar <- reactive({
    filterPlayer1() %>%
      # filter(ngames>=5) %>%
      drop_na(nscore) %>%
      group_by(Event) %>%
      summarise(
        date = max(EvalDate, na.rm=TRUE),
        score_perc = sum(nscore)/n(),
        rating_perf = 0,
        games = n(),
        moves = mean(NMoves),
        openings = n_distinct(Opening),
        # inputs for rating_perf calc - to drop later
        opp_elo = mean(opp_rating, na.rm=TRUE),
        nscore = sum(nscore)
      ) %>%
      ungroup() %>%
      filter(games>=5) %>%
      mutate(
        rating_perf = opp_elo - if_else(nscore==games, -800, 
                                        ifelse(nscore==0, 800,
                                               log(games/nscore - 1)/log(10)*400)),
        rating_perf = round(rating_perf),
        moves = round(moves)
      ) %>%
      select(-opp_elo, -nscore) %>%
      slice_max(rating_perf, n = 5)
  })
  pTmtDesc <- reactive({
    pTmtRadar() %>%
      rename(`Event`=Event, `Date`=date, `Score`=score_perc, `Rating Performance`=rating_perf, 
             `Games`=games, `Average Moves`=moves, `Openings`=openings) %>%
      formattable(.) %>%
      as.datatable(escape = FALSE,
                   options = list(scrollX = TRUE),
                   rownames = FALSE) %>%
      formatPercentage(c("Score"))
  })
  output$p_tmt_radar <- renderPlotly({
    pTmtRadar() %>%
      plot_ly(
        type = "scatterpolar",
        r = .$rating_perf,
        theta = .$Event,
        fill = "toself"
      ) %>% 
      layout(title = paste("Top 5 Tournaments"),
             polar = list(radialaxis = list(range = c(1000, 3500))))
  })
  output$p_tmt_desc <- renderDataTable({
    pTmtDesc()
  })
  
  # Head-to-Head Summary Data Table + Probability Boxes ----
  filterPlayerOpp <- reactive({
    a <- twic_clean %>%
      filter(
        White %in% c(input$v_player_2, input$v_opponent_1), 
        Black %in% c(input$v_player_2, input$v_opponent_1)
      ) %>%
      mutate(
        player=input$v_player_1,
        color=if_else(White==input$v_player_1, "White", "Black"),
        opponent=if_else(color=="White", Black, White),
        player_rating=if_else(color=="White", WhiteElo, BlackElo),
        opp_rating=if_else(color=="White", BlackElo, WhiteElo),
        player_title=if_else(color=="White", WhiteTitle, BlackTitle),
        opp_title=if_else(color=="White", BlackTitle, WhiteTitle)
      ) %>%
      select(-WhiteElo, -BlackElo)
  })
  pOppData <- reactive({
    filterPlayerOpp() %>%
      # filter(White %in% c(input$v_player_2, input$v_opponent_1), 
      #        Black %in% c(input$v_player_2, input$v_opponent_1)) %>%
      drop_na(nscore) %>%
      filter(
        between(player_rating, input$p_elo_input[1], input$p_elo_input[2]),
        between(opp_rating, input$o_elo_input[1], input$o_elo_input[2]),
        between(EvalDate, input$date_input[1], input$date_input[2])
      )
  })
  pOppDesc <- reactive({
    pOppData() %>%
      group_by(White, Black) %>%
      summarise(
        w_score_perc = sum(nscore)/n(),
        # b_score_perc = sum(bscore)/sum(ngames),
        tmts = n_distinct(Event),
        games = n(),
        openings = n_distinct(Opening),
        moves = mean(NMoves)
      ) %>%
      ungroup() %>%
      mutate(
        moves = round(moves),
        b_score_perc = 1-w_score_perc
      ) %>%
      relocate(White, Black, w_score_perc, b_score_perc) %>%
      rename(`White Score`=w_score_perc, `Black Score`=b_score_perc,
             `Tournaments`=tmts, `Games`=games, `Openings`=openings,
             `Average Moves`=moves) %>%
      formattable(.) %>%
      as.datatable(escape = FALSE,
                   options = list(scrollX = TRUE),
                   rownames = FALSE) %>%
      formatPercentage(c("White Score", "Black Score"))
  })
  pOppTotal <- reactive({
    pOppData() %>%
      group_by() %>%
      summarise(
        pwscore=sum(nscore[White==input$v_player_2]),
        owscore=sum(nscore[White==input$v_opponent_1]),
        pwgames=sum(White==input$v_player_2),
        owgames=sum(White==input$v_opponent_1)
      ) %>%
      ungroup() %>%
      mutate(
        p_wscore_perc=round(pwscore/pwgames*100),
        o_wscore_perc=round(owscore/owgames*100),
        p_bscore_perc=100-o_wscore_perc,
        o_bscore_perc=100-p_wscore_perc,
        # p_bscore_perc=round((pbgames-pwscore)/pbgames*100),
        p_score_perc=round((pwscore+owgames-owscore)/(pwgames+owgames)*100),
        # o_wscore_perc=round((pwgames-pwscore)/pwgames*100),
        # o_bscore_perc=round((pbgames-pbscore)/pbgames*100),
        o_score_perc=round((owscore+pwgames-pwscore)/(owgames+pwgames)*100)
      )
  })
  output$player_total_card <- renderValueBox({
    valueBox(
      value = paste(pOppTotal() %>% .$p_score_perc, "%", sep = ""),
      subtitle = "total score",
      color = "teal",
      icon = icon("chess-king")
    )
  })
  output$player_white_card <- renderValueBox({
    valueBox(
      value = paste(pOppTotal() %>% .$p_wscore_perc, "%", sep = ""),
      subtitle = "white score",
      color = "yellow",
      icon = icon("chess-king")
    )
  })
  output$player_black_card <- renderValueBox({
    valueBox(
      value = paste(pOppTotal() %>% .$p_bscore_perc, "%", sep = ""),
      subtitle = "black score",
      color = "black",
      icon = icon("chess-king")
    )
  })
  output$opponent_total_card <- renderValueBox({
    valueBox(
      value = paste(pOppTotal() %>% .$o_score_perc, "%", sep = ""),
      subtitle = "total score",
      color = "teal",
      icon = icon("chess-king")
    )
  })
  output$opponent_white_card <- renderValueBox({
    valueBox(
      value = paste(pOppTotal() %>% .$o_wscore_perc, "%", sep = ""),
      subtitle = "white score",
      color = "yellow",
      icon = icon("chess-king")
    )
  })
  output$opponent_black_card <- renderValueBox({
    valueBox(
      value = paste(pOppTotal() %>% .$o_bscore_perc, "%", sep = ""),
      subtitle = "black score",
      color = "black",
      icon = icon("chess-king")
    )
  })
  output$p_opp_desc <- renderDataTable({
    pOppDesc()
  })
}

shinyApp(ui = ui, server = server)