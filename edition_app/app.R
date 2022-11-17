#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(knitr)
library(here)
library(tidyverse)
library(keyring)
library(gt)
library(DBI)
library(RMariaDB)
library(IRanges)
library(plotly)
library(sortable)
options(scipen = 999999)

con <- dbConnect(
  drv = MariaDB(),
  host = "vm3065.kaj.pouta.csc.fi",
  dbname = "hpc-hd",
  user = Sys.getenv("DB_USER") ,
  password =  Sys.getenv("DB_PASS"),
  bigint = "integer",
  load_data_local_infile = TRUE,
  autocommit = TRUE,
  reconnect = TRUE
)


get_ranges = function(ecco_id, id, reuse_df){
  tryCatch({
    ra = reuse_df %>% 
      filter(ecco_id.x == id & ecco_id.y == ecco_id) %>% 
      arrange(t2_start, t2_end) %>% 
      select(start = t2_start, end = t2_end)
    
    ir1 = IRanges(start = ra$start, end = ra$end)
    
    islands = ir1 %>% reduce() %>% 
      as.data.frame() %>% 
      as_tibble() %>% 
      mutate(type = 'island')
    
    gaps = gaps(ir1) %>% 
      as.data.frame() %>% 
      as_tibble() %>% 
      mutate(type = 'gap')
    
    rbind(islands, gaps) %>% 
      mutate(doc_id = ecco_id)
    
    
  }, error = function(e) {
    
  })
  
}

idmap <- tbl(con,dbplyr::in_schema("hpc-hd","idmap_a"))
full_reuses <- tbl(con,dbplyr::in_schema("hpc-hd","textreuses_2d_a"))
estc_core = tbl(con,dbplyr::in_schema("hpc-hd","estc_core_a"))




ui <- fluidPage(
  
  
  titlePanel("Edition Comparer"),
  
  sidebarLayout(
    sidebarPanel(tabsetPanel(tabPanel("Groups",
      textInput("bins", "Work ID:", "2179-fable of bees or private vices public benefits"),
      actionButton('click', 'Generate'),
      actionButton('add', 'Add to list'),
      actionButton('reset', 'Reset'), textOutput('current'), textOutput('seed'),
      uiOutput("groups")),tabPanel("Plot",
      plotlyOutput("distPlot", width = '30vw'))
    )),
    
    # Show a plot of the generated distribution
    mainPanel(tags$head(
      tags$style(
        HTML("width: 1000px"))),
      htmlOutput("frame")
    )
  )
)

server <- function(input, output) {
  
  
  # output$compare = renderUI({
  #   
  # 
  #     
  #    # selectInput('which_to_compare', 'Seed: ', choices = work_ids)
  #   }) 
  # })
  
  
  output$groups = renderUI({
    
    withProgress(message = 'Generating editions from work ID', value = 0, {
      
      x = input$bins
      
      work_ids_df = idmap %>% filter(work_id == x) %>% 
        inner_join(estc_core %>% 
                     select(estc_id, publication_year, short_title)) %>% mutate(publication_year = as.integer(publication_year)) %>% 
        #mutate(publication_year = paste0(ecco_id, " (", publication_year, ")"))  %>% 
        select(ecco_id, publication_year, short_title) %>% mutate(title = paste0(short_title, " (", publication_year, ")"))
      
      work_ids = work_ids_df %>% 
        arrange(publication_year)%>% pull(title)
      
      names(work_ids) = work_ids_df %>% 
        arrange(publication_year)%>% pull(ecco_id)
    
    bucket_list(
    header = "",
    group_name = "rank_groups",
    orientation = "horizontal",
    
    add_rank_list(
      text = h5("Seed: "),
      input_id = "seed"),
    
    add_rank_list(
      text = h5("Compare: "),
      input_id = "which_to_compare"),
    
    add_rank_list(
      text = h5("Editions"),
      input_id = "delete",
      labels = work_ids))
    
    })
    
    

    
    })
  
  
  values = reactiveValues()
  
  observeEvent(input$add, {
    
    values$keep = unique(c(values$keep, input$which_to_compare))
    
    
    
  })
  
  
  seed = reactiveValues()
  
  observe({
    
    seed$keep = unique(c(seed$keep, input$seed))  
    
    
    
  })
  
  
  output$current = renderText({
    
    paste0("Current:", paste0(unique(values$keep), collapse = ' ;'))
    
  })
  
  
  output$seed = renderText({
    
    paste0("Seed:", paste0(unique(seed$keep)), collapse  = ' ;')
    
  })
  
  
  observeEvent(input$reset,{
    
   seed$keep = NULL
  values$keep = NULL
    
  } )
  
  hypo = eventReactive(input$click, {
    withProgress(message = 'Gathering reuses', value = 0, {
      print(values$keep)
      #i = input$bins
      
      # all_ids = estc_core %>%
      #   filter(work_id == i) %>%
      #   left_join(idmap, by = 'estc_id') %>%
      #   arrange(publication_year) %>% filter(!is.na(ecco_id)) %>%
      #   pull(ecco_id)
      
      x = seed$keep 
      y = values$keep
      
      print(x)
      print(y)
      
      ids = idmap %>% filter(ecco_id %in% y)
      
      first_id = idmap %>% filter(ecco_id == x)
      
      r1 = full_reuses %>% 
        inner_join(first_id, by = c('t1_id' = 't_id'))%>% 
        inner_join(ids, by = c('t2_id' = 't_id')) %>% 
        left_join(estc_core %>% select(estc_id, publication_year, short_title), by= c('estc_id.y' = 'estc_id')) %>% 
        collect()
      
      r2 = r1 %>% select( -publication_year, -short_title)
      
      
      
      #y = all_ids[! all_ids == x]
      
      withProgress(message = 'Calculating Missing Ranges', value = 0, {
        
        t = lapply(y, get_ranges, id = x, reuse_df = r2) %>% 
          data.table::rbindlist() %>% 
          mutate(gap_link_text = paste0("<a href='https://a3s.fi/octavo-reader/index.html#?doc=",doc_id,"&startOffset=",start,"&endOffset=", end, "'>", doc_id, "</a><br>"))%>% 
          mutate(gap_link = paste0("https://a3s.fi/octavo-reader/index.html#?doc=",doc_id,"&startOffset=",start,"&endOffset=", end)) %>% 
          mutate(gap_link = paste0(gap_link, "&doc=", x, "&startOffset=",start,"&endOffset=", end)) %>%
          left_join(r1 %>% 
                      distinct(ecco_id.y, short_title, publication_year, work_id.y), by = c('doc_id' = 'ecco_id.y'))
        
        
        t
      })
      
    })
    
  })
  
  
  
  output$distPlot <- renderPlotly({
    
    req(hypo())
    
    p = hypo()  %>% filter(type == 'gap') %>% 
      arrange(desc(publication_year)) %>% 
      mutate(doc_id = factor(doc_id, levels = unique(doc_id))) %>% 
      arrange(work_id.y, publication_year) %>% ungroup() %>% 
      #mutate(sort_level = 1:nrow(.)) %>% 
      ggplot(aes(text = paste0(short_title, " (", publication_year, ")"))) + 
      geom_segment(aes(color = doc_id, x = start, xend = end, y = doc_id, yend = doc_id), size = 10) + theme_void() + 
      theme(legend.position = 'none') +
      scale_color_viridis_d()
    
    
    
    gp = plotly::ggplotly(p)
    
    gp%>%
      event_register("plotly_selecting")
    
  })
  
  
  src = reactive({
    req(event_data("plotly_click"))
    d <- event_data("plotly_click")
    i = as.numeric(d['x'])
    
    hypo() %>% filter(type == 'gap') %>% filter(end == i) %>% head(1) %>% 
      pull(gap_link)
  })
  
  
  output$frame <- renderUI({
    
    my_test <- tags$iframe(src=src(), height=600, width=1200)
    print(my_test)
    my_test
    
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
