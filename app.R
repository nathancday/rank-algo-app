library(tidyverse)
library(shiny)

source("algo_scratch.R")

# dat <- read_csv("queipid_detailed.csv") %>% 
#     janitor::clean_names() %>% 
#     select_if(~ !all(is.na(.))) %>% 
#     group_by(query_text) %>% 
#     mutate(judgement = sample (1:4, size = n(), replace = TRUE))

colores <- RColorBrewer::brewer.pal(6, "Dark2") %>% 
    set_names(c("p", "cg", "dcg", "ndcg", "rbp", "err"))

ggplotter <- function(data) {
    ggplot(data, aes(n, value, color = measure)) +
        geom_path(size = 3) +
        scale_color_manual(values = colores) +
        theme_minimal()
}


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Choose ya ranky-boi!!!"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("n",
                        "# of docs:",
                        min = 1,
                        max = 20,
                        value = 10),
            sliderInput("p",
                        "user persistance",
                        min = 0,
                        max = 1,
                        value = .9)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           DT::DTOutput("judgementsTable"),
           tabsetPanel(
               tabPanel("Set-based",
                        plotOutput("setPlot")
                        ),
               tabPanel("Rank-based",
                        plotOutput("rankPlot")
                        ),
               tabPanel("User-based",
                        plotOutput("userPlot"))
           )
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    .at <- 5 # rank at which to perform evaluation metrics
    max_judge <- 4 # best relevance score possible
    
    rv <- reactiveValues()
    
    observeEvent(c(input$n, input$p), {
        docs <- sample(0:4, input$n, replace = TRUE)
        rv$docs <- tibble(doc_id = paste0("doc", 1:length(docs)),
                          judgement = docs)
    })
    
    output$judgementsTable <- DT::renderDT(server = FALSE, {
        DT::datatable(
            rv$docs,
            colnames = c("Rank" = 1, "Doc ID" = 2, "Relevance Score" = 3),
            extensions = "RowReorder",
            options = list(
                rowReorder = TRUE,
                order = list(list(0, 'asc')),
                dom = "t",
                # ordering = FALSE,
                pageLength = input$n
            ),
            callback=DT::JS(
                "// pass on data to R
    table.on('row-reorder', function(e, details, changes) {
        Shiny.onInputChange('table_row_reorder', JSON.stringify(details));
    });"
            ),
            selection = "none",
            )
        }
    )
    
    observeEvent(input$table_row_reorder, {
        
        print(input$judgementsTable_rows_current)
        
        rv$dat <- tibble(n = 1:input$n) %>% 
            rowwise() %>% 
            mutate(
                p = precision(n, docs),
                cg = cg(n, docs),
                dcg = dcg(n, docs),
                err = err(n, docs),
                sat = prob_satisfied(n, docs),
                rbp = rbp(n, docs, input$p)
            ) %>% 
            pivot_longer(-n, "measure")
        
    }, )
    
    
    output$setPlot <- renderPlot({
        req(rv$dat)
        rv$dat %>%
            filter(measure %in% c("p", "cg")) %>% 
            ggplotter()

    })
    
    output$rankPlot <- renderPlot({
        rv$dat %>%
            filter(measure %in% c())
        ggplot(rv$dat, aes(n, value, color = measure)) +
            geom_path() +
            scale_color_brewer(palette = "Dark2") +
            theme_minimal()
    })
}

shinyApp(ui = ui, server = server)
