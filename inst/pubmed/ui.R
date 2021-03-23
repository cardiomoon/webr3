library(shinybusy)
library(shinyjs)
library(webrSub)
library(shinythemes)

shinyUI(fluidPage(
    shinyjs::useShinyjs(),
    uiOutput("title"),
    add_busy_gif(src = "https://jeroen.github.io/images/banana.gif", height = 70, width = 70),
    singleton(
        tags$head(tags$script(src = "message-handler.js"))
    ),
    navbarPage( "Web-R.org",
                tabPanel("PubMedWordcloud",
                         pubmedModuleInput("pubmed")),

                id='main',
                theme=shinytheme("cerulean")
    ),
    uiOutput("showList"),
    verbatimTextOutput("table4")

)
)
