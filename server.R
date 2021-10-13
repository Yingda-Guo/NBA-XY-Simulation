# Server.R
library(shiny)

server <- function(input, output, session) {
  
  # initialise then start the guide
  guide$init()$start()
  
  # Player Photo
  output$player_img <- renderUI({
    box(width = 2, title = input$player_select, solidHeader = T, status = "primary", height = "230px", 
        div(tags$img(src = paste0(input$player_select, ".png"),width = "160px",height = "160px"), style="text-align: center;"))
  })
  
  
# VALUEBOX: Accel Count
  output$vb_accel <- renderValueBox({
    shiny::req(real_time_all_data())
    value <- round(real_time_all_data()[name == input$player_select & timebin == time_counter - 1, accel] + runif(1, min = -10, max = 10),1)
    shinydashboard::valueBox(value = tags$p(value, style = "font-size: 60%;"),
             #width = 3,
             color = "yellow",
             subtitle = tags$p("# of Accels", style = "font-size: 100%;"))}
    )
  
  
# VALUEBOX: Speed
  output$vb_cur_speed <- renderValueBox({
    shiny::req(real_time_all_data())
    value <- round(real_time_all_data()[name == input$player_select & timebin == time_counter - 1, speed],1)
        shinydashboard::valueBox(value = tags$p(value, style = "font-size: 60%;"),
             #width = 3,
             color = "yellow",
             subtitle = tags$p("Speed (m/s)", style = "font-size: 100%;"))})
    
 
  
# VALUEBOX: Decel count
  output$vb_decel <- renderValueBox({
    shiny::req(real_time_all_data())
    value <- round(real_time_all_data()[name == input$player_select & timebin == time_counter - 1, accel] + runif(1, min = -10, max = 10),1)
    shinydashboard::valueBox(value = tags$p(value, style = "font-size: 60%;"),
           #width = 3,
           color = "yellow",
           subtitle = tags$p("# of Decels", style = "font-size: 100%;"))})
  
  
# VALUEBOX: real-time Accel 
  output$vb_cur_acc <- renderValueBox({
    shiny::req(real_time_all_data())
    value <- round(real_time_all_data()[name == input$player_select & timebin == time_counter - 1, accel],1)
    #value <- round(cal_data()[player.id == input$player_select, mean_accel],1)
    shinydashboard::valueBox(value = tags$p(value, style = "font-size: 60%;"),
           #width = 3,
           color = "yellow",
           subtitle = tags$p("Real-time Accel", style = "font-size: 100%;"))})
  
  
  
# VALUEBOX: Cumulative Distance
  output$vb_total_dis <- renderValueBox({
    shiny::req(real_time_all_data())
    value <- round(real_time_all_data()[name == input$player_select & timebin == time_counter - 1, cul_dis],1)
    #value <- round(cal_data()[player.id == input$player_select, total_distance],1)
    shinydashboard::valueBox(value = tags$p(value, style = "font-size: 60%;"),
           color = "yellow",
           subtitle = tags$p("Distance (m)", style = "font-size: 100%;"))})
  
  
  # VALUEBOX: Physioload
  output$vb_physioload <- renderValueBox({
    shiny::req(real_time_all_data())
    value <- round(real_time_all_data()[name == input$player_select & timebin == time_counter - 1, speed]  + runif(1, min = -10, max = 10),1)
    #value <- round(cal_data()[player.id == input$player_select, physioload],1)
    shinydashboard::valueBox(value = tags$p(value, style = "font-size: 60%;"),
             #  width = 3,
             color = "yellow",
             subtitle = tags$p("Load (physio)", style = "font-size: 100%;"))})
  
  # VALUEBOX: Mechload
  output$vb_mechload <- renderValueBox({
    shiny::req(real_time_all_data())
    value <- round(real_time_all_data()[name == input$player_select & timebin == time_counter - 1, accel]  + runif(1, min = -10, max = 10),1)
    #value <- round(cal_data()[player.id == input$player_select, mechload],1)
    shinydashboard::valueBox(value = tags$p(value, style = "font-size: 60%;"),
             #  width = 3,
             color = "yellow",
             subtitle = tags$p("Load (mech)", style = "font-size: 100%;"))})

  
  # Render Energy Socre Gauge
  output$gauge1 <- renderGauge({
    shiny::req(real_time_all_data())
    value = round(runif(1, min = 50, max = 100), 1)
    gauge(value, min = 0, max = 100, gaugeSectors(
      success = c(79, 100), warning = c(66, 78), danger = c(0, 65)
    ))
  })
  
  
  # Render Energy Socre Gauge for team blue
  output$gauge2 <- renderGauge({
    shiny::req(real_time_all_data())
    #value <- round(cal_data()[player.id %in% c("038", "026","047", "044", "020", "017"), sum(energy_score)],1)
    value = round(runif(1, min = 500, max = 550), 1)
    gauge(value, min = 400, max = 550, gaugeSectors(
      success = c(501, 550), warning = c(451, 500), danger = c(400, 450)
    ))
  })
  
  # Render Energy Socre Gauge for team red
  output$gauge3<- renderGauge({
    shiny::req(real_time_all_data())
    #value <- round(cal_data()[player.id %in% c("108", "093","090", "084", "081", "075"), sum(energy_score)],1)
    value = round(runif(1, min = 500, max = 550),1)
    gauge(value, min = 400, max = 550, gaugeSectors(
      success = c(501, 550), warning = c(451, 500), danger = c(400, 450)
    ))
  })

  
 # Real time raw data flow
  output$table <- renderTable({
    shiny::req(real_time_all_data())
    real_time_all_data()[name == input$player_select & timebin == time_counter - 1,.(name, timebin, X, Y)]
  })
  
  # Event Table
  # output$table_event <- renderTable({
  #   shiny::req(cum_result$cal_dt)
  #   shiny::req(event_dt())
  #   dt <- event_dt()[2:time_counter,.("Time(s)" = as.integer(nanoseconds_seq), Event = puck_possession)]
  #   dt
  # })
  
 # Real time metric calculation
  output$table_metric <- renderTable({
    shiny::req(real_time_all_data())
    real_time_all_data()[name == input$player_select,.(player_id, timebin, total_distance = cul_dis)]
  })
  
  
  ################## Calculate the result ----------
  # Create a reactive time counter
  time_counter <- 0
  
  # control the click event
  start_again <- reactiveValues(yes_or_no = 0)
  
  # Set the frame length
  time_gap <- 0.3
  
  # Create a reactive value to store the calculated result
  cur_result <- reactiveValues( cal_dt = NA, last_obs = NA )
  
  # Create a reactive value to store cumulative result
  cum_result <- reactiveValues(cal_dt = NA)
  
  onclick("plot",  {
    start_again$yes_or_no <- start_again$yes_or_no + 1
    time_counter <- 0
    cur_result$cal_dt <- NA
    cur_result$last_obs <- NA
  })
  
  # Create all player data
  all_player_data <- reactive({
    
    # There will be multiple events, so remove event id 
    setkey(dt_event, player_id, game_clock)
    
    # There will be over lapping between events, so we only find unique game time for each player
    dt_event <- unique(dt_event)
    dt_event <- dt_event[order(player_id, game_clock),]
    
    # Count the number of frames
    dt_count <- dt_event[,.(count = .N), by =player_id]
    
    # Stop if not all players have the same number of frames
    stopifnot(all(dt_count$count == dt_count$count[1]))
    
    # Then, find all unique game clocks
    clocktimes <- rev(sort(unique(dt_event$game_clock)))
    
    # Find the start time
    start_time <- min(clocktimes)
    end_time <- max(clocktimes)
    
    # Set frame length in seconds
    gap <- time_gap
    # Another way to set frame lenght
    #gap <- max(dt_event[,.(max_diff = max(diff(game_clock))), by = player_id]$max_diff) * 20
    
    # Generate a new time sequence
    time_sequence <- seq(from = start_time, to = end_time, by = gap)
    
    # The game clock will decrease as time goes, so we need to use the maximal time to subtract the current game clock to get increasing timebin
    dt_event[, timebin := (end_time - game_clock)%/%gap ]
    
    # Group all the data within the same time segment and calculate the mean
    dt_animation <- dt_event[, .(X = mean(x_loc), Y = mean(y_loc),
                                 vx = velocity_one_direction(x_loc),
                                 vy = velocity_one_direction(y_loc),
                                 game_clock = round(mean(game_clock), 1),
                                 name = name[1],
                                 ID = team_id[1], 
                                 jersey = jersey[1], 
                                 Dist = travelDist(x_loc, y_loc), 
                                 speed = mean(velocity(x_loc, y_loc)),
                                 accel = mean(acceleration(x_loc, y_loc))
    ), by = .(player_id, timebin)][order(player_id, timebin)][, cul_dis := cumsum(Dist), by = player_id]
    
    # Create color for the team
    dt_animation[, team_color := ifelse(ID == "1610612737", "red", "blue")]
    
  })
  
  # Create another reactive value to store calculated results
  real_time_all_data <- reactive({
    
    # Make sure it will start to generate after click
    shiny::req(start_again$yes_or_no > 0)
    
    # Make sure the data is valid
    shiny::req(all_player_data())
    
    # First load the data for all players
    dt_all <- all_player_data()
    num_seconds <- max(all_player_data()$timebin)
    
    # Simulate the real-time data flow
    if(time_counter < num_seconds){
      dt <- dt_all[timebin %in% c(0:time_counter),]
      # Re-execute this reactive expression after 500 milliseconds
      invalidateLater(0.5 * 1000, session)
      time_counter <<- time_counter + 1
    }else{
      dt <- dt_all[timebin %in% c(0:time_counter),]
    }
    
    dt
  })
  
  # Calculate Event
  event_dt <- reactive({
    dt2 <- all_player_data()
    shiny::req(nrow(dt2) > 0)

    # Observe the vector relationship of puck and all players
    dt_analysis <- dt2[, {
      # Calculate the KNN
      print(.SD[, unique(timebin)])
      dt_players <- .SD[player_id != "-1", ]
      dt_puck <- .SD[player_id == "-1", ]
      knn <- get.knnx(dt_players[, .(X, Y)], query = dt_puck[, .(X, Y)], k = 3)
      index_1 = knn$nn.index[1]
      index_2 = knn$nn.index[2]
      index_3 = knn$nn.index[3]
      
      list(dis_1 = knn$nn.dist[1], player_1 = dt_players[index_1, jersey], p1_angle_with_puck = angle_vectors(dt_puck[, .(vx, vy)], dt_players[index_1, .(vx,vy)]), p1_velocity = dt_players[index_1, speed],
           dis_2 = knn$nn.dist[2], player_2 = dt_players[index_2, jersey], p2_angle_with_puck = angle_vectors(dt_puck[, .(vx, vy)], dt_players[index_2, .(vx,vy)]), p2_velocity = dt_players[index_2, speed],
           dis_3 = knn$nn.dist[3], player_3 = dt_players[index_3, jersey], p3_angle_with_puck = angle_vectors(dt_puck[, .(vx, vy)], dt_players[index_3, .(vx,vy)]), p3_velocity = dt_players[index_3, speed],
           puck_velocity = dt_puck[, speed])
    }, by = timebin]
    
    # Based on analysis result
    dt_analysis[, puck_possession := ifelse((((dis_1 <= 3 & dis_2 > 3)| (dis_2 / dis_1 > 2)) & (
      (puck_velocity / p1_velocity <= 5 | abs(puck_velocity - p1_velocity) <= 5) &
        p1_angle_with_puck <= 100 ) | (dis_2 / dis_1 >= 2 & (puck_velocity / p1_velocity <= 1.5 | abs(puck_velocity - p1_velocity) <= 8) & p1_angle_with_puck <= 100)),paste0("Player ",player_1," is in possession of the basketball"), "passing or shooting")]
    
  })
  
  
  # Output visulization
  output$plot <- renderPlotly({
    
    playdt <- copy(all_player_data())
    
    # Highlight the selected player
    playdt[name == input$player_select, team_color := "green"]
    
    # Sort the playdt by timebin and player id
    playdt <- playdt[order(timebin, player_id),]
    
    
    # Observe the vector relationship of puck and all players
    dt_analysis <- event_dt()
    
    # Draw dynmaic graphs
    t <- list(
      family = "sans serif",
      size = 12,
      color = "white")
    
    t2 <- list(
      family = "sans serif",
      size = 14,
      color = "green")
    
    # Set text font
    t3 <- list(
      family = "sans serif",
      size = 20,
      color = "black",
      face="bold")
    
    t4 <- list(
      family = "sans serif",
      size = 18,
      color = "black",
      face="bold")
    
    # polygon <- playdt[,{chull_plot2(.SD)}, by = timebin]
    
    p <- playdt %>% plot_ly(frame = ~timebin, showlegend = F, type = 'scatter',mode = 'markers') %>% config(displayModeBar = F) %>% 
      add_trace(x= ~X, y=~Y, data = playdt[is.na(ID),], marker = list(size = 12, color = "#ff6005")) %>% 
      add_markers(x = ~X, y = ~Y, data = playdt[ID == "1610612737",], opacity = 0.6, marker = list(size = 22, color = ~team_color)) %>%
      add_text(x = ~X, y = ~Y, data = playdt[ID == "1610612737",], text = ~jersey, textfont = t) %>%
      add_markers(x = ~X, y = ~Y, data = playdt[ID == "1610612765",], opacity = 0.6, marker = list(size = 22, color = ~team_color)) %>%
      add_text(x = ~X, y = ~Y, data = playdt[ID == "1610612765",], text = ~jersey, textfont = t) %>%
      # add_polygons(x = ~X, y = ~Y, data = polygon[ID == 1,], line=list(width=2,color="red")) %>%
      # add_polygons(x = ~X, y = ~Y, data = polygon[ID == 2,], line=list(width=2,color="blue")) %>%
      layout(yaxis = list(range = c(0, 50), title = "", zeroline = F, showline = F, showticklabels = F, showgrid = F), 
             xaxis = list( range = c(0, 94), title = "", zeroline = F, showline = F, showticklabels = F, showgrid = F),
             shapes = list(
               # Outer boundary
               list(type = "rect",
                    fillcolor = "white", line = list(color = "black", width = 4), opacity = 0.2,
                    x0 = 0, x1 = 94, xref = "x",
                    y0 = 0, y1 = 50, yref = "y"),
               # Left backboard
               list(type = "line",
                    line = list(color = "black", width = 4), opacity=0.4,
                    x0 = 4, x1 = 4, xref = "x",
                    y0 = 22, y1 = 28, yref = "y"),
               # Right backboard
               list(type = "line",
                    line = list(color = "black", width = 4), opacity=0.4,
                    x0 = 90, x1 = 90, xref = "x",
                    y0 = 22, y1 = 28, yref = "y"),
               # Left outer box
               list(type = "rect",
                    line = list(color = "black", width = 2), opacity=0.4,
                    x0 = 0, x1 = 19, xref = "x",
                    y0 = 17, y1 = 33, yref = "y"),
               # Left inner box
               list(type = "rect",
                    line = list(color = "black", width = 2), opacity=0.4,
                    x0 = 0, x1 = 19, xref = "x",
                    y0 = 19, y1 = 31, yref = "y"),
               # Right outer box
               list(type = "rect",
                    line = list(color = "black", width = 2), opacity=0.4,
                    x0 = 75, x1 = 94, xref = "x",
                    y0 = 17, y1 = 33, yref = "y"),
               # Right inner box
               list(type = "rect",
                    line = list(color = "black", width = 2), opacity=0.4,
                    x0 = 75, x1 = 94, xref = "x",
                    y0 = 19, y1 = 31, yref = "y"),
               # Left corner a
               list(type = "rect",
                    fillcolor = "white", line = list(color = "black", width = 2), opacity = 0.4,
                    x0 = 0, x1 = 14, xref = "x",
                    y0 = 47, y1 = 47, yref = "y"),
               # Left corner b
               list(type = "rect",
                    fillcolor = "white", line = list(color = "black", width = 2), opacity = 0.4,
                    x0 = 0, x1 = 14, xref = "x",
                    y0 = 3, y1 = 3, yref = "y"),
               # Right corner a
               list(type = "rect",
                    fillcolor = "white", line = list(color = "black", width = 2), opacity = 0.4,
                    x0 = 80, x1 = 94, xref = "x",
                    y0 = 47, y1 = 47, yref = "y"),
               # Left corner b
               list(type = "rect",
                    fillcolor = "white", line = list(color = "black", width = 2), opacity = 0.4,
                    x0 = 80, x1 = 94, xref = "x",
                    y0 = 3, y1 = 3, yref = "y"),
               # Half court
               list(type = "line",
                    line = list(color = "black", width = 3), opacity = 0.3,
                    x0 = 47, x1 = 47, xref = "x",
                    y0 = 0, y1 = 50, yref = "y"),
               # Left hoop
               list(type = 'circle',
                    xref = 'x', x0 = 6.1, x1 = 4.6,
                    yref = 'y', y0 = 25.75, y1 = 24.25,
                    line = list(color = 'black', width = 2),
                    opacity = 0.4),
               # Right hoop
               list(type = 'circle',
                    xref = 'x', x0 = 89.4, x1 = 87.9,
                    yref = 'y', y0 = 25.75, y1 = 24.25,
                    line = list(color = 'black', width = 2),
                    opacity = 0.4),
               # Left free throw circle
               list(type = 'circle',
                    xref = 'x', x0 = 25, x1 = 13,
                    yref = 'y', y0 = 31, y1 = 19,
                    line = list(color = 'black', width = 2),
                    opacity = 0.4),
               # Right free throw circle
               list(type = 'circle',
                    xref = 'x', x0 = 81, x1 = 69,
                    yref = 'y', y0 = 31, y1 = 19,
                    line = list(color = 'black', width = 2),
                    opacity = 0.4),
               # Center big circle
               list(type = 'circle',
                    xref = 'x', x0 = 53, x1 = 41,
                    yref = 'y', y0 = 31, y1 = 19,
                    line = list(color = 'black', width = 2),
                    opacity = 0.4),
               # Center small circle
               list(type = 'circle',
                    xref = 'x', x0 = 49, x1 = 45,
                    yref = 'y', y0 = 27, y1 = 23,
                    line = list(color = 'black', width = 2),
                    opacity = 0.4),
               # Left arc shape 
               list(type = 'path',
                    path =  'M 14,47 Q 45,25 14,3',
                    line = list(color = 'black', width = 2),
                    opacity = 0.4),
               # Right arc shape
               list(type = 'path',
                    path =  'M 80,47 Q 49,25 80,3',
                    line = list(color = 'black', width = 2),
                    opacity = 0.4)
             ))
    

    p
    
  })
  
  # Event Table
  output$table_event <- renderTable({
    shiny::req(nrow(event_dt()) > 0)
    # Re-execute this reactive expression after 500 milliseconds
    invalidateLater(0.5 * 1000, session)
    dt <- event_dt()[time_counter + 1,.("Time(s)" = as.integer(timebin + 1), Event = puck_possession)]
    dt
  })
  
  # Data dictionary
  output$metric_dic_table <- DT::renderDataTable({ 
    datatable(metric_dic, options = list(paging = FALSE, scrollX = TRUE))
  })
  
}