#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(DT)
library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)
source("R/parse_notes.R")

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  
  notes = parse_notes("data/player notes.csv") 
    # mutate(Player = gsub(intToUtf8(160), " ", Player)) %>%
    # mutate(Player = gsub(" [A-Z][A-Z]* $", "", Player)) %>%
    # mutate(Player = gsub(" $", "", Player)) %>%
    # mutate(Player = gsub(".DEN$", "", Player)) %>%
    # mutate(Player = gsub("JAC$", "", Player))
  player_table_file <- 'data/FantasyPros_2019_Draft_Overall_Rankings.csv'
  player.table.raw = reactiveFileReader(1000, session, player_table_file, read.csv)
  baseline_ranks <-
    read.csv(player_table_file) %>% 
    head(100) %>% 
    mutate(PosRank = Pos) %>%
    mutate(Pos = gsub("[0-9]", "", Pos)) %>%
    count(Pos) %>% 
    rename(BaseRank = n)
  
  prefix = "data/FantasyPros_Fantasy_Football_Projections_%s.csv"
  proj_pts <- 
    data.frame(Pos = c("QB", "RB", "TE", "WR")) %>% 
    mutate(proj = map(Pos, ~read.csv(sprintf(prefix, .x), stringsAsFactors=F))) %>% 
    mutate(proj = map(proj, ~select(.x, Player, FPTS))) %>% 
    select(-Pos) %>% 
    unnest() %>% 
    filter(!is.na(FPTS)) %>% 
    rename(ProjPts = FPTS)
             
  vor <-
    read.csv(player_table_file) %>% 
    mutate(Pos = gsub("[0-9]", "", Pos)) %>%
    group_by(Pos) %>% 
    left_join(proj_pts) %>% 
    ungroup() %>% 
    nest(-Pos) %>% 
    left_join(baseline_ranks) %>% 
    mutate(vor = map2(data, BaseRank, ~.x$ProjPts - .x$ProjPts[.y])) %>% 
    unnest() %>% 
    arrange(Avg) %>% 
    select(Player, VOR = vor)
  
  
    
  
  #player.table.raw = reactiveFileReader(1000, session, 'data/riel.csv', read.csv)
  
  player.table = reactive({
    blah = player.table.raw() %>%
      mutate(player.fac = factor(Player, levels=Player)) %>%
      mutate(player.fac.shift = c(player.fac[4:n()], NA, NA, NA)) %>%
      mutate(PosRank = Pos) %>%
      mutate(Pos = factor(gsub("[0-9]", "", Pos))) %>%
      mutate(Team = factor(Team)) %>%
      left_join(vor) %>% 
      mutate(Drafted = factor(Drafted, levels = c("X", ""))) %>% 
      filter(Drafted == "") %>% # Checkboxes not supported so characters work better than logical, trust me
      {.}
    print(class(blah$Drafted))
    blah
  })
    
   
  output$player.table = DT::renderDataTable({
    player.table() %>% 
    select(Rank, Player, Team, PosRank, Pos, Best, Worst, Avg, Std.Dev, ADP, vs..ADP, VOR, Drafted) %>%
    datatable(filter = 'top',
              options = list(pageLength = 6,
                             autoWidth = TRUE,
                             fillContainer = TRUE
              ),
              rownames = FALSE,
              selection = 'single',
              editable = T
    )} #%>% 
      #formatRound(c("Score","Event_FF", "FF", "FFXY"), 3) %>%
      #formatPercentage("Chromfrac", 2) %>%
      #formatStyle(columns = c(2:13), 'text-align' = 'center')  
  )
  
  output$rankPlot = renderPlot({
    player.table() %>%
      head(25) %>%
      ggplot(aes(player.fac, Avg, color=Pos)) +
      theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
      #geom_vline(data = data.frame(x=c(6, 19, 30, 43, 54, 67, 78, 91, 102, 115, 126, 139, 150, 163, 174)),
      #geom_vline(data = data.frame(x=c(9, 16, 33, 40, 57, 64, 81, 88, 105, 115, 126, 139, 150, 163, 174)),
      geom_vline(data = data.frame(x=sort(c(seq(7, 147, 20), c(14, 147, 20)))),
                 aes(xintercept = x), color = 'grey', size=2) +
      geom_segment(aes(xend = player.fac, y=Best, yend=Worst, color=Pos), size=1) +
      geom_segment(aes(xend = player.fac, y=Avg+Std.Dev, yend=Avg-Std.Dev, color=Pos), size=3) +
      geom_point(color='black') +
      geom_segment(aes(xend=player.fac.shift, yend=Avg)) +
      scale_x_discrete() +
      NULL
  })
  
  output$vorPlot = renderPlot({
    player.table() %>%
      head(25) %>%
      ggplot(aes(player.fac, VOR, color=Pos)) +
      theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
      #geom_vline(data = data.frame(x=c(6, 19, 30, 43, 54, 67, 78, 91, 102, 115, 126, 139, 150, 163, 174)),
      #geom_vline(data = data.frame(x=c(9, 16, 33, 40, 57, 64, 81, 88, 105, 115, 126, 139, 150, 163, 174)),
      geom_vline(data = data.frame(x=sort(c(seq(1, 150, 24), seq(24, 150, 24)))), 
                 aes(xintercept = x), color = 'grey', size=2) +
      geom_point() +
      scale_x_discrete() +
      NULL
  })
  output$notes = renderText({
    #"test"
    #as.character(
    player = player.table()[input$player.table_rows_selected, "Player"]
    if (length(player) > 0) {
      quick.notes = notes %>% 
        filter(Player == player) %>%
        .$Notes
      #sprintf("%s<br><br>%s", player, quick.notes)
      quick.notes
    } else {
      ""
    }
    #player.table()[input$player.table_rows_selected, "Player"]
  })
  
})
