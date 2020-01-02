library(plotly)
library(tidyverse)
library(shiny)

source("algo_scratch.R")

colores <- RColorBrewer::brewer.pal(8, "Dark2") %>% 
    set_names(c("precision", "recall", "avg_precision", "cg", "dcg", "ndcg", "rbp", "err"))

ggplotter <- function(data) {
    p <- ggplot(data, aes(n, value, color = measure)) +
        geom_path(size = 3) +
        scale_color_manual(values = colores) +
        scale_x_continuous(breaks = 1:10) +
        theme_minimal()
    
    ggplotly(p)
}


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Visualizing ranking metrics"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            actionButton("resample_btn", "Resample Documents", icon = icon("vial")),
            DT::DTOutput("judgmentsTable")
        ),

        # Show a plot of the generated distribution
        mainPanel(
            fluidRow(
                column(width = 3, offset = 1,
                       checkboxGroupInput("set_metrics", "Set-based metrics:",
                                          choiceValues = c("precision", "recall", "avg_precision"),
                                          selected = c("precision", "recall", "avg_precision"),
                                          choiceNames = c("Precision", "Recall", "Average precision")
                                          )
                       ),
                column(width = 3, offset = 1,
                       checkboxGroupInput("rank_metrics", "Rank-based metrics:",
                                          choiceValues = c("dcg", "ndcg"),
                                          choiceNames = c("DCG", "nDCG")
                                          )
                ),
                column(width = 3, offset = 1,
                      checkboxGroupInput("user_metrics", "User-based metrics:",
                                         choiceValues = c("rbp", "err"),
                                         choiceNames = c("RBP(p=0.8)", "ERR")
                                         )
                )
            ),
            fluidRow(
                plotlyOutput("setPlot")
            )
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    .at <- 5 # rank at which to perform evaluation metrics
    max_judge <- 4 # best relevance score possible
    
    rv <- reactiveValues()
    
    observeEvent(input$resample_btn, {
        docs <- sample(0:4, 10, replace = TRUE)
        rv$docs <- tibble(doc_id = paste0("doc", 1:length(docs)),
                          judgment = docs)
    }, ignoreNULL = FALSE)
    
    output$judgmentsTable <- DT::renderDT(server = FALSE, {
        DT::datatable(
            rv$docs,
            colnames = c("Rank" = 1, "Doc ID" = 2, "Relevance Score" = 3),
            extensions = "RowReorder",
            options = list(
                rowReorder = TRUE,
                order = list(list(0, 'asc')),
                dom = "t",
                pageLength = input$n
            ),
            callback=DT::JS(
                "table.on('row-reorder', function(e, details, changes) {
                    Shiny.onInputChange('table_row_reorder', JSON.stringify(details));
                });"
            ),
            selection = "none",
            )
        }
    )
    
    output$setPlot <- renderPlotly({
        req(rv$docs)
        
        docs <- rv$docs$judgment[input$judgmentsTable_rows_current]
        print(docs)
        
        dat <- tibble(n = 1:10) %>% 
            rowwise() %>% 
            mutate(
                precision = precision(n, docs > 2),
                recall = recall(n, docs > 2),
                avg_precision = avg_precision(n, docs > 2),
                cg = cg(n, docs),
                dcg = dcg(n, docs),
                ndcg = ndcg(n, docs),
                err = err(n, docs),
                sat = prob_satisfied(n, docs),
                rbp = rbp(n, docs, .8)
            ) %>% 
            pivot_longer(-n, "measure")
        
        
        dat %>%
            filter(measure %in% c(input$set_metrics, input$rank_metrics, input$user_metrics)) %>% 
            ggplotter()

    })
}

shinyApp(ui = ui, server = server)
