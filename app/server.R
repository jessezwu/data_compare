library(DT, warn.conflicts=FALSE)
library(shiny, warn.conflicts=FALSE)
library(shinydashboard, warn.conflicts=FALSE, verbose=FALSE)
library(tidyverse)
library(creditmodel)

options(warn=-1, shiny.maxRequestSize=1024^3)

shinyServer(function(input, output, clientData, session) {

  ################################################################################
  # Data reading
  ################################################################################
  data1 <- reactive({
    file <- input$file_data1
    if(is.null(file)) return(NULL)
    tryCatch({
      read_csv(file$datapath)
    }, error = raise_error)
  })
  data2 <- reactive({
    file <- input$file_data2
    if(is.null(file)) return(NULL)
    tryCatch({
      read_csv(file$datapath)
    }, error = raise_error)
  })

  output$dt_sample_data1 <- renderDataTable({
    req(dt <- data1())
    dt %>%
      head(n = 10) %>%
      datatable(rownames=FALSE, options=list(dom='t', scrollX='t'))
  })
  output$dt_sample_data2 <- renderDataTable({
    req(dt <- data2())
    dt %>%
      head(n = 10) %>%
      datatable(rownames=FALSE, options=list(dom='t', scrollX='t'))
  })


  ################################################################################
  # Data cleaning
  ################################################################################

  exclude_cols <- reactive({
    req(d1 <- data1())
    exclude_cols = d1 %>%
      select_if(negate(is.numeric)) %>%
      apply(2, n_distinct)
    names(exclude_cols)[exclude_cols > 100]
  })
  columns <- reactive({
    req(d1 <- data1())
    req(d2 <- data2())
    setdiff(intersect(colnames(d1), colnames(d2)), exclude_cols())
  })
  output$ui_sel_column <- renderUI({
    req(columns())
    selectizeInput('sel_column', 'Column selection',
      choices = columns()
    )
  })


  ################################################################################
  # Stats
  ################################################################################
  output$dt_summary1 <- renderDataTable({
    req(d1 <- data1())
    summary(d1) %>% as.data.frame() %>% 
      transmute(Feature = Var2, Stats = Freq) %>%
      filter(!is.na(Stats)) %>%
      datatable(rownames=FALSE)
  })
  output$dt_summary2 <- renderDataTable({
    req(d2 <- data2())
    summary(d2) %>% as.data.frame() %>%
      transmute(Feature = Var2, Stats = Freq) %>%
      filter(!is.na(Stats)) %>%
      datatable(rownames=FALSE)
  })

  output$dt_psi <- renderDataTable({
    req(d1 <- data1())
    req(d2 <- data2())
    withProgress(message = 'calculating PSI', {
      get_psi_all(
        dat = d1,
        dat_test = d2,
        ex_cols = exclude_cols(),
        parallel = TRUE,
        as_table = TRUE
      )
    }) %>%
      datatable(rownames=FALSE, options=list(scrollX='t'))
  })

  ################################################################################
  # Plots
  ################################################################################

  data_plot <- reactive({
    req(column <- input$sel_column)
    req(d1 <- data1())
    req(d2 <- data2())
    bind_rows(
      d1 %>% select(one_of(column)) %>% mutate(origin='File 1'),
      d2 %>% select(one_of(column)) %>% mutate(origin='File 2')
    )
  })
  output$plot_histogram <- renderPlotly({
    req(df <- data_plot())
    req(column <- input$sel_column)
    plot_ly(df, x=as.formula(paste0('~', column)), color=~origin) %>%
      add_histogram()
  })
  output$plot_density <- renderPlotly({
    req(df <- data_plot())
    req(column <- input$sel_column)
    p <- tryCatch({
      dens1 <- df %>% filter(origin == 'File 1') %>% pull(column) %>%
        density(kernel = 'gaussian', na.rm = TRUE)
      dens2 <- df %>% filter(origin == 'File 2') %>% pull(column) %>%
        density(kernel = 'gaussian', na.rm = TRUE)
      plot_ly() %>%
        add_lines(x=dens1$x, y=dens1$y, name='File 1') %>%
        add_lines(x=dens2$x, y=dens2$y, name='File 2')
    }, error = function(x) {
      p <- ggplot(df, aes_string(x=column, fill='origin', color='origin')) +
        geom_density(alpha=0.5) +
        theme_minimal()
      ggplotly(p)
    })
    p
  })


})
