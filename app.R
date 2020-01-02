library(shinythemes)
library(plotly)
library(tidyverse)
library(shiny)

source("algo_scratch.R")

colores <- RColorBrewer::brewer.pal(8, "Dark2") %>% 
    set_names(c("Precision", "Recall", "Avg precision", "CG", "DCG", "nDCG", "RBP", "ERR"))

ggplotter <- function(data) {
    p <- ggplot(data, aes(n, value, color = metric)) +
        geom_path(size = 3) +
        scale_color_manual(values = colores, labels = color_labels()) +
        scale_x_continuous(breaks = 1:10) +
        theme_minimal()
    
    ggplotly(p)
}


# Define UI for application that draws a histogram
ui <- fluidPage(theme = shinytheme("paper"),

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
                                          choices = c("Precision", "Recall", "Avg precision"),
                                          selected = c("Precision", "Recall", "Avg precision")
                                          )
                       ),
                column(width = 3, offset = 1,
                       checkboxGroupInput("rank_metrics", "Rank-based metrics:",
                                          choices = c("DCG", "nDCG")
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
                Precision = precision(n, docs > 2),
                Recall = recall(n, docs > 2),
                `Average precision` = avg_precision(n, docs > 2),
                `CG`  = cg(n, docs),
                `DCG` = dcg(n, docs),
                `nDCG` = ndcg(n, docs),
                `ERR` = err(n, docs),
                sat = prob_satisfied(n, docs),
                `RBP(p=0.8)` = rbp(n, docs, .8)
            ) %>% 
            pivot_longer(-n, "metric")
        
        
        dat %>%
            filter(metric %in% c(input$set_metrics, input$rank_metrics, input$user_metrics)) %>% 
            ggplotter()

    })
}

shinyApp(ui = ui, server = server)
