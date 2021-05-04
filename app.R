library(shinythemes)
library(plotly)
library(DT)
library(tidyverse)
library(shiny)

source("algo_scratch.R")

colores <- RColorBrewer::brewer.pal(8, "Dark2") %>% 
  set_names(c("Precision", "Recall", "Avg precision", "CG", "DCG", "nDCG", "RBP(p=0.8)", "ERR"))

ggplotter <- function(data) {
  p <- ggplot(data, aes(position, value, color = metric)) +
    geom_point(size = 4) +
    geom_path(size = 1) +
    scale_color_manual(values = colores) +
    scale_x_continuous(breaks = 1:10) +
    theme_minimal(base_size = 16) +
    labs(y = "Value", x = "@ Position")
  
  ggplotly(p) %>% config(displayModeBar = FALSE)
}


# Define UI for application that draws a histogram
ui <- fluidPage(theme = shinytheme("paper"),
                
                # Application title
                titlePanel("Visualizing search metrics"),
                
                # Sidebar with a slider input for number of bins 
                sidebarLayout(
                  sidebarPanel(
                    actionButton("help_btn", "Info", icon = icon("info-circle"), style = "margin:10px"),
                    DTOutput("judgmentsTable"),
                    actionButton("resample_btn", "Resample Documents", icon = icon("vial"), style = "margin:10px;"),
                  ),
                  
                  # Show a plot of the generated distribution
                  mainPanel(
                    fluidRow(
                      column(width = 3, offset = 1,
                             checkboxGroupInput("set_metrics", "Set-based metrics:",
                                                choices = c("Precision*" = "Precision", "Recall*" = "Recall", "CG"),
                                                selected = c("Precision")
                             )
                      ),
                      column(width = 3, offset = 1,
                             checkboxGroupInput("rank_metrics", "Rank-based metrics:",
                                                choices = c("Avg precision*" = "Avg precision", "DCG", "nDCG"),
                                                selected = c("Avg precision")
                             )
                      ),
                      column(width = 3, offset = 1,
                             checkboxGroupInput("user_metrics", "User-based metrics:",
                                                choices = c("RBP(p=0.8)", "ERR")
                             )
                      )
                    ),
                    fluidRow(
                      plotlyOutput("setPlot")
                    ),
                    fluidRow(
                      a(href = "https://opensourceconnections.com/",
                        style = "float:right;padding:25px",
                        img(src = "https://avatars1.githubusercontent.com/u/339001?s=200&v=4",
                            alt = "OSC logo",
                            width = 50,
                            height = 50)
                      )
                    )
                  )
                )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  modalHelp <-  modalDialog(
    p("Re-arrange the document results to see how different rank metrics
          perform at differnt positions. Click-and-drag in the 'Position' column or use the sorting arrows next to the column title."),
    p("For metrics that operate on binary relevance grades (marked with *), grades of 4 and 3 are considered 'Relevant' and 2, 1, and 0 are considerd 'Not-relevant'."),
    a("Learn more about these metrics on Wikipedia.", href = "https://en.wikipedia.org/wiki/Evaluation_measures_(information_retrieval)"),
    a(href = "https://opensourceconnections.com/",
      img(src = "https://avatars1.githubusercontent.com/u/339001?s=200&v=4",
          alt = "OSC logo",
          width = 50,
          height = 50
      )
    ),
    title = "Visualizing search metrics",
    easyClose = TRUE
  )
  
  showModal(modalHelp)
  
  observeEvent(input$help_btn, {
    showModal(modalHelp)
  })
  
  rv <- reactiveValues()
  
  observeEvent(input$resample_btn, {
    docs <- sample(0:4, 10, replace = TRUE, prob = c(.2, .2, .2, .2, .2))
    rv$docs <- tibble(doc_id = paste0("doc", 1:length(docs)),
                      judgment = docs,
                      judgment2 = ifelse(docs > 2, "relevant", "not relevant"))
  }, ignoreNULL = FALSE)
  
  output$judgmentsTable <- renderDT(server = FALSE, {
    datatable(
      rv$docs,
      colnames = c("Position" = 1, "Doc ID" = 2, "Graded (0-4) Relevance" = 3, "Binary Relevance" = 4),
      extensions = "RowReorder",
      options = list(
        rowReorder = TRUE,
        order = list(list(0, 'asc')),
        dom = "t",
        pageLength = input$n
      ),
      callback=JS(
        "table.on('row-reorder', function(e, details, changes) {
                    Shiny.onInputChange('table_row_reorder', JSON.stringify(details));
                });"
      ),
      selection = "none"
    ) %>% 
      formatStyle(
        "Position",
        backgroundColor = "#2196F3",
        color = "white"
      )
  }
  )
  
  output$setPlot <- renderPlotly({
    req(rv$docs)
    req(input$judgmentsTable_rows_current)
    
    docs <- rv$docs$judgment[input$judgmentsTable_rows_current]
    
    dat <- tibble(n = 1:10) %>% 
      rowwise() %>% 
      mutate(
        Precision = precision(n, docs > 2),
        Recall = recall(n, docs > 2),
        `Avg precision` = avg_precision(n, docs > 2),
        CG  = cg(n, docs),
        DCG = dcg(n, docs),
        nDCG = ndcg(n, docs),
        ERR = err(n, docs),
        sat = satisfied_at(n, docs),
        `RBP(p=0.8)` = rbp(n, docs, .8)
      ) %>% 
      pivot_longer(-n, "metric") %>% 
      mutate(value = round(value, 3)) %>% 
      rename(position = n)
    
    dat %>%
      filter(metric %in% c(input$set_metrics, input$rank_metrics, input$user_metrics)) %>% 
      ggplotter()
    
  })
}

shinyApp(ui = ui, server = server)
