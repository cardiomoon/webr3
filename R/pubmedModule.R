#' UI function of mob module
#' @param id id
#' @importFrom shiny NS uiOutput
#' @export
pubmedModuleInput=function(id){
     ns <-NS(id)

     uiOutput(ns("pubmedModule"))
}


#' Server function of sTree shiny module
#'
#' @param input input
#' @param output output
#' @param session session
#' @param PPTdata PPTdata
#' @importFrom shiny selectInput checkboxInput reactiveValues req
#' @importFrom shiny textInput tagList hr fluidRow updateCheckboxInput
#' @importFrom shiny callModule numericInput conditionalPanel
#' @importFrom shiny observe observeEvent reactive renderUI verbatimTextOutput renderPrint
#' @importFrom webrSub dynamicShowData dynamicShowDataUI
#' @export
pubmedModule=function(input,output,session,PPTdata){

     ns <- session$ns

     savedPPT=reactiveValues(type=c(),title=c(),code=c())

     observeEvent(PPTdata(),{
          savedPPT$type<-PPTdata()$type
          savedPPT$title<-PPTdata()$title
          savedPPT$code<-PPTdata()$code
     })

     data=reactive({ df() })


     resultdf=reactive({
               df=data.frame(type=savedPPT$type,title=savedPPT$title,code=savedPPT$code,
                             stringsAsFactors = FALSE)

     })



output$pubmedModule=renderUI({
     tagList(
          fluidRow(
               textInput(ns("key"),"keyword",value=""),
               textInput(ns("author"),"author",value=""),
               numericInput(ns("start"),"startYear",value=2000),
               numericInput(ns("stop"),"endYear",value=2021),
               numericInput(ns("seed"),"seed",value=1234),
               numericInput(ns("limit"),"maximum number of abstract",value=200),
               numericInput(ns("max"),"maximum size of text",value=3),
               numericInput(ns("min"),"minimum size of text",value=0.3),
               numericInput(ns("min.freq"),"minimum frequwncy of words to be plotted",value=2),
               numericInput(ns("rot.per"),"proportion words with 90 degree rotation",value=0.35),
               selectInput(ns("palette"),"palette",choices=c("Accent", "Dark2", "Pastel1", "Pastel2", "Paired", "Set1", "Set2", "Set3"),
                           selected="Dark2"),
               numericInput(ns("no"),"number of plots to make",value=6),
               verbatimTextOutput(ns("text"))
          ),
          conditionalPanel("true==true",
                           checkboxInput(ns("plotOK"),"plotOK",value=FALSE),
                           conditionalPanel(condition=sprintf("input[['%s']]==true",ns("plotOK")),
                                            tagList(

                                              dynamicShowDataUI(ns("dynamicShowData"))

                                            )
                           )
                           )
     )
})


output$text=renderPrint({
    if(input$plotOK){
    df=makepubmedPPT()
    df
    }
})

observe({
     mode=0
     if(req(input$key)!="") mode=1
     if(req(input$author)!="") mode=1
      updateCheckboxInput(session,"plotOK",value=ifelse(mode==1,TRUE,FALSE))

})


makepubmedPPT=function(){


    makePPTList_pubmed(key=input$key,author=input$author,
                       start=input$start,stop=input$stop,
                       limit=input$limit,seed=input$seed,max=input$max,min=input$min,
                       min.freq=input$min.freq,rot.per=input$rot.per,
                       palette=input$palette,no=input$no)
  # makePPTList_pubmed(key=input$key,author=input$author)

}

resultDynamicShowData=callModule(dynamicShowData,"dynamicShowData",
                                 caption="PubMedWordcloud",
                                 data=reactive(makepubmedPPT()),
                                 plotheight="600px")

observeEvent(resultDynamicShowData(),{

        if(!is.null(resultDynamicShowData())){
                savedPPT$type<-resultDynamicShowData()$type
                savedPPT$title<-resultDynamicShowData()$title
                savedPPT$code<-resultDynamicShowData()$code
        }
})


return(resultdf)

}
