# !diagnostics suppress=., .TITLE,

source("constants.R")
ui <- shinydashboard::dashboardPage(
    title = .TITLE,
    header = shinydashboard::dashboardHeader(
        title = .TITLE,
        titleWidth = 230
    ),
    
    # Sidebar ----
    sidebar = shinydashboard::dashboardSidebar(
        shinydashboard::sidebarMenu( 
            shinydashboard::menuItem(
                text = "Overview",
                tabName = "overview_tab",
                selected = TRUE
            ), 
            shinydashboard::menuItem(
                text = "Word Cloud",
                tabName = "wordcloud_tab",
                selected = FALSE
            ),
            shinydashboard::menuItem(
                text = "Health habits",
                tabName = "health_tab",
                selected = FALSE
            )
        )
    ),
    
    body = shinydashboard::dashboardBody(
        shinydashboard::tabItems(

            shinydashboard::tabItem(
                tabName = "wordcloud_tab",
                shinydashboard::tabBox(
                    tabPanel( 
                        title = "Words used by occupation",
                        fluidRow(
                            column(
                                selectInput("selection", "Choose an occupation:",
                                            choices = jobs),
                                actionButton("update", "Change"),
                                hr(),
                                sliderInput("freq",
                                            "Minimum Frequency:",
                                            min = 1,  max = 50, value = 15),
                                sliderInput("max",
                                            "Maximum Number of Words:",
                                            min = 1,  max = 300,  value = 100),
                                width = 2
                            ),

                            column(
                                plotOutput(
                                    outputId = 'wordcloud',
                                    height = '500px'
                                ),
                                width = 10
                            )
                        )
                    ),
                    
                    width = NULL
                )
            ),
            
            shinydashboard::tabItem(
                tabName = "health_tab",
                shinydashboard::tabBox(
                    tabPanel( 
                        title = "Stack Of Vices",
                        fluidRow(
                            column(
                                selectInput("stack_x", "Choose first variable:",
                                            choices = stack_x),
                                selectInput("stack_y", "Choose second variable:",
                                            choices = stack_y),
                                hr(),
                                checkboxInput("exclude_na", "Hide NA values",
                                              TRUE),
                                width = 2
                            ),
                            
                            column(
                                plotOutput(
                                    outputId = 'stack_plot',
                                    height = '500px'
                                ),
                                width = 10
                            )
                        )
                    ),
                    width = NULL
                )
            ),
            
            shinydashboard::tabItem(
                tabName = "overview_tab",
                shinydashboard::tabBox(
                    tabPanel( 
                        title = "Who's using Ok Cupid?",
                        fluidRow(
                            column(
                                selectInput("over_bar_x", "Choose variable:",
                                            choices = over_bar_x),
                                hr(),
                                checkboxInput("over_exclude_na", 
                                              "Hide NA values",
                                              TRUE),
                                width = 2
                            ),
                            
                            column(
                                plotOutput(
                                    outputId = 'bar_plot',
                                    height = '500px'
                                ),
                                width = 10
                            )
                        )
                    ),
            
                    tabPanel( 
                        title = "Density distributions by age",
                        fluidRow(
                            column(
                                selectInput("density_x", "Choose variable:",
                                            choices = density_x),
                                hr(),
                                checkboxInput("density_exclude_na", 
                                              "Hide NA values",
                                              TRUE),
                                width = 2
                            ),
                            
                            column(
                                plotOutput(
                                    outputId = 'density_plot',
                                    height = '500px'
                                ),
                                width = 10
                            )
                        )
                    ),
                    
                    tabPanel( 
                        title = "Gender insights",
                        fluidRow(
                            column(
                                selectInput("select_plot", "Choose desired plot:",
                                            choices = gender_plot_select),
                                width = 2
                            ),
                            
                            column(
                                plotOutput(
                                    outputId = 'gender_plot',
                                    height = '500px'
                                ),
                                width = 10
                            )
                        )
                    ),
                    
                    width = NULL
                )
            )
        )
    )
)


