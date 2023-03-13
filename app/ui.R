library(DT, warn.conflicts = FALSE)
library(shiny, warn.conflicts = FALSE)
library(shinydashboard, warn.conflicts = FALSE, verbose = FALSE)

# Define the UI layout
shinyUI(dashboardPage(skin = 'black',
  dashboardHeader(title = 'Dataset compare'),
  dashboardSidebar(fluidPage(
    sidebarMenu(id = 'menu',
      menuItem('Data',
        tabName = 'nav_data',
        icon = icon('file')
      ),
      menuItem('Stats',
        tabName = 'nav_stats',
        icon = icon('table')
      ),
      menuItem('Visualisations',
        tabName = 'nav_visual',
        icon = icon('chart-simple')
      )
    )
  )),
  # Main panel
  dashboardBody(fluidPage(tabItems(
    tabItem(tabName='nav_data',
      box(width = 6, title = 'Data Selection',
        fileInput('file_data1', 'Dataset 1',
          multiple = FALSE,
          accept = c('text/csv', 'text/comma-separated-values', '.csv')
        ),
        dataTableOutput('dt_sample_data1')
      ),
      box(width = 6, title = 'Data Selection',
        fileInput('file_data2', 'Dataset 2',
          multiple = FALSE,
          accept = c('text/csv', 'text/comma-separated-values', '.csv')
        ),
        dataTableOutput('dt_sample_data2')
      )
    ),
    tabItem(tabName='nav_stats',
      box(width = 6, title = 'File 1', collapsible = TRUE,
        dataTableOutput('dt_summary1')
      ),
      box(width = 6, title = 'File 2', collapsible = TRUE,
        dataTableOutput('dt_summary2')
      ),
      box(width = 12, title = 'Distribution changes',
        dataTableOutput('dt_psi')
      )
    ),
    tabItem(tabName='nav_visual',
      box(width = 12, title = 'Feature Selection',
          uiOutput('ui_sel_column'),
          plotlyOutput('plot_histogram'),
          plotlyOutput('plot_density')
      )
    )
  )))
))
