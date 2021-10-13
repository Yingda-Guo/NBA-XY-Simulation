# Ui.R
library(shiny)
library(shinycssloaders)
library(tableHTML)


# Use shiny dashboard
ui <- dashboardPage(
  skin = "black",
  # Header
  dashboardHeader(title = "Hawks VS Pistons", titleWidth = 200),
  # Sidebar
  dashboardSidebar(
    width = 200,
    collapsed = FALSE,
    sidebarMenu(
      selectInput(
        "player_select",
        label = "Select Athlete :",
        choices = player_names,
        selected = "Kyle Korver"
      ),
      menuItem("Streaming Data ", tabName = "live_stream"),
      menuItem("Metric Dictionary",  tabName = "metric_dictionary")
    )
  ),
  
  # Body
  dashboardBody(
    use_cicerone(), # include dependencies
    useShinyjs(),
    ### CSS
    
    # box padding
    tags$head(tags$style(
      HTML('.box {margin-left: 15x;margin-top: -3px;}')
    )),
    # valuebox paddings
    tags$head(tags$style(
      HTML(
        ".small-box {padding: -3px;
                                 margin-top: 3px;
                                 margin-left: 0px;
                                 margin-right: 0px;
                                 margin-bottom: 3px;}; "
      )
    )),
    
    # Valueboxs color and size
    tags$head(tags$style(HTML(
      ".small-box {height: 65px}"
    ))),
    tags$style(
      ".small-box.bg-yellow { background-color: #FFFFFF !important; color: #000000 !important; }"
    ),
    
    # Background
    tags$head(tags$style(
      HTML(
        '/* body */ .content-wrapper, .right-side {background-color: #FFFFFF; }'
      )
    )),
    
    # Box Colors
    tags$style(
      HTML(
        " .box.box-solid.box-primary>.box-header {color:#FFFFFF; background:#222D32}.box.box-solid.box-primary{ border-bottom-color:#FFFFFF; border-left-color:#FFFFFF; border-right-color:#FFFFFF; border-top-color:#989898;}"
      )
    ),
    
    # table text colors
    #tags$style(make_css(list('.box', c('font-size', 'font-family', 'color'),  c('14px', 'arial', 'red')))),
    
    # box background color
    tags$style(
      HTML(
        ".box.box-solid.box-primary>.box-header {}.box.box-solid.box-primary{background:#FFFFFF}"
      )
    ),
    
    
    tabItems(
      tabItem(
        "live_stream",
        #animation
        
        fluidRow(
          box(
            id ="box_plot",
            width = 8,
            title = "Animation",
            status = "primary",
            solidHeader = TRUE,
            height = "550px",
            plotlyOutput("plot", width = "700px", height = "500px") %>% withSpinner(color = "#000000")
          ),
          
          uiOutput("player_img"),
          
          box(
            id ="box_energy_single",
            width = 2,
            title = "Energy Score",
            status = "primary",
            solidHeader = TRUE,
            height = "230px",
            br(),
            flexdashboard::gaugeOutput("gauge1", width = "100%", height = "200px")
          ),
          
          box(
            id = "box_metrics",
            width = 4,
            title = "Real-Time Metrics",
            status = "primary",
            solidHeader = TRUE,
            height = "300px",
            br(),
            fluidRow(
              valueBoxOutput("vb_accel"),
              valueBoxOutput("vb_physioload"),
              valueBoxOutput("vb_cur_speed")
            ),
            br(),
            fluidRow(
              valueBoxOutput("vb_decel"),
              valueBoxOutput("vb_mechload"),
              valueBoxOutput("vb_total_dis")
            )
          )
          
          
        ),
        
        fluidRow(
          box(
            id = "box_data",
            title = "Real-time Data Flow",
            status = "primary",
            solidHeader = T,
            width = 4,
            height = "230px",
            tableOutput('table')
          ),
          
          box(
            id = "box_event",
            title = "Event",
            status = "primary",
            solidHeader = T,
            width = 4,
            height = "230px",
            tableOutput("table_event")
          ),
          
          box(
            width = 2,
            title = "Energy Score for Hawks",
            status = "primary",
            solidHeader = T,
            height = "230px",
            br(),
            flexdashboard::gaugeOutput("gauge2", width = "100%", height = "200px")
          ),
          
          box(
            width = 2,
            title = "Energy Score for Pistons",
            status = "primary",
            solidHeader = T,
            height = "230px",
            br(),
            flexdashboard::gaugeOutput("gauge3", width = "100%", height = "200px")
          )
        )
        
      ),
      
      tabItem(
        "metric_dictionary",
        DT::dataTableOutput("metric_dic_table")
      )
    )#tabitems
  )#dashboardbody
)#dashboardpage
