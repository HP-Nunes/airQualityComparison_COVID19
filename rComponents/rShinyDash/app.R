## app.R ##
library(shinydashboard)


dashboardPage(
    dashboardHeader(),
    dashboardSidebar(),
    dashboardBody()
)
###### HEADER
dropdownMenu(type = "messages",
             messageItem(
                 from = "Sales Dept",
                 message = "Sales are steady this month."
             ),
             messageItem(
                 from = "New User",
                 message = "How do I register?",
                 icon = icon("question"),
                 time = "13:45"
             ),
             messageItem(
                 from = "Support",
                 message = "The new server is ready.",
                 icon = icon("life-ring"),
                 time = "2014-12-01"
             )
)
output$messageMenu <- renderMenu({
    # Code to generate each of the messageItems here, in a list. This assumes
    # that messageData is a data frame with two columns, 'from' and 'message'.
    msgs <- apply(messageData, 1, function(row) {
        messageItem(from = row[["from"]], message = row[["message"]])
    })
    
    # This is equivalent to calling:
    #   dropdownMenu(type="messages", msgs[[1]], msgs[[2]], ...)
    dropdownMenu(type = "messages", .list = msgs)
})

header <- dashboardHeader(title = "PM2.5 Pollution in the Time of COVID-19",
                          dropdownMenuOutput("messageMenu")
                          )

##### SIDEBAR
sidebar <- dashboardSidebar()

body <- dashboardBody(
    # Boxes need to be put in a row (or column)
    fluidRow(
        box(plotOutput("plot1", height = 250)),
        
        box(
            title = "Controls",
            sliderInput("slider", "Number of observations:", 1, 100, 50)
        )
    )
)

ui <- dashboardPage(header, sidebar, body)


server <- function(input, output) {
    set.seed(122)
    histdata <- rnorm(500)
    
    output$plot1 <- renderPlot({
        data <- histdata[seq_len(input$slider)]
        hist(data)
    })
}

shinyApp(ui, server)
