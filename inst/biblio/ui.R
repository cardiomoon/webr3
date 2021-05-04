library(shinybusy)
library(shinyjs)
library(webrSub)
library(shinythemes)
library(shinycssloaders)
library(rrtable)
library(visNetwork)

options(spinner.size=1, spinner.type=5)

shinyUI(fluidPage(
    shinyjs::useShinyjs(),
    add_busy_gif(src = "https://jeroen.github.io/images/banana.gif", height = 70, width = 70),
    singleton(
        tags$head(tags$script(src = "message-handler.js"))
    ),
    uiOutput("title"),
    navbarPage( "Web-R.org",
                tabPanel("Get Data",
                         uiOutput("biblio")),
                tabPanel("Analysis1",
                         uiOutput("Analysis1")),
                tabPanel("Analysis2",
                         uiOutput("Analysis2")),
                navbarMenu("Anlaysis3",
                           tabPanel("wordCloud",
                                    uiOutput("wordCloud")),
                           tabPanel("Three-Fields Plot",
                                    uiOutput("threeFieldsPlot")),
                           tabPanel("Thematic Evolution",
                                    uiOutput("themaEvol"))
                ),
                tabPanel("PPTxList",
                         pptxListInput("List1"),icon=icon("shopping-cart")),
                tabPanel("Citation",
                         citationModuleInput("citation")),
                id='main',
                theme=shinytheme("cerulean")
    ),
    uiOutput("showList"),
    verbatimTextOutput("table4")

)
)
