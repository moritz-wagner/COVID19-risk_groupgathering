#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(plotly)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Risk of at least one COVID19 case by group gathering"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("pop.size",
                        "Population size:",
                        min = 100000,
                        max = 10000000,
                        value = 9000000,
                        round = 5),
            sliderInput("reporting.rate",
                        "Reporting rate:",
                        min = 0.1,
                        max = 1,
                        value = .5)
        ),

        # Show a plot of the generated distribution
        mainPanel(
            plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        n <- 1000
        
        prob.group <- function(group.size,cases.rep,n,reporting.rate,pop.size) {
            p.case.pop <- rpois(n,cases.rep/reporting.rate)/pop.size
            p.no.case.group <- (1-p.case.pop)^group.size
            p.case.group <- 1-p.no.case.group
            return(p.case.group)
        }
        
        group.size <- c(10,50,100,500,1000,5000,10000)
        cases.rep <- seq(0,10000,by = 200)
        group.cases <- expand.grid(group.size=group.size,cases.rep=cases.rep)
        res <- data.table::data.table(n = rep(1:n,nrow(group.cases)),
                                      cases.rep = rep(group.cases$cases.rep,each=n),
                                      group.size = rep(group.cases$group.size,each=n))
        res$prob <- c(mapply(prob.group,group.size=group.cases$group.size,cases.rep=group.cases$cases.rep,
                             MoreArgs=list(n=n,reporting.rate=input$reporting.rate,pop.size=input$pop.size)))
        
        res %>% 
            group_by(cases.rep,group.size) %>% 
            summarise(lwr=quantile(prob,.025),
                      mean=mean(prob),
                      upr=quantile(prob,.975)) %>% 
            ggplot(aes(x=cases.rep,y=mean,ymin=lwr,ymax=upr,fill=factor(group.size)))+
            geom_ribbon(alpha=.2)+geom_line(aes(color=factor(group.size)))+theme_minimal()
        # plotly::ggplotly(p)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
