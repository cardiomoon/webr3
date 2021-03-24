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
#' @importFrom shiny selectInput checkboxInput reactiveValues req updateNumericInput wellPanel
#' @importFrom shiny textInput tagList hr fluidRow updateCheckboxInput updateTextInput
#' @importFrom shiny callModule numericInput conditionalPanel actionButton br
#' @importFrom shiny observe observeEvent reactive renderUI verbatimTextOutput renderPrint
#' @importFrom webrSub dynamicShowData dynamicShowDataUI myp
#' @importFrom editData numericInput3 textInput3 selectInput3
#' @export
pubmedModule=function(input,output,session,PPTdata){

     ns <- session$ns

     savedPPT=reactiveValues(type=c(),title=c(),code=c())

     RV=reactiveValues()

     observeEvent(PPTdata(),{
          savedPPT$type<-PPTdata()$type
          savedPPT$title<-PPTdata()$title
          savedPPT$code<-PPTdata()$code
     })


     resultdf=reactive({
               df=data.frame(type=savedPPT$type,title=savedPPT$title,code=savedPPT$code,
                             stringsAsFactors = FALSE)

     })



output$pubmedModule=renderUI({
     tagList(

              myp("Enter keyword and/or author to make query and press `make Query` button. "),
               wellPanel(
               textInput(ns("key"),"keyword(e.g. machine learning)",value="",width="240px"),
               textInput(ns("author"),"author(e.g. Moon KW)",value="",width="240px"),
               numericInput3(ns("start"),"startYear",value=2000,width=120),
               numericInput3(ns("stop"),"endYear",value=2021,width=120)
               ),

               actionButton(ns("makeQuery"),"make Query"),
               actionButton(ns("reset"),"reset"),
          hr(),
               textInput(ns("query"),"query",value="",width="300px"),
               verbatimTextOutput(ns("text")),
               numericInput3(ns("seed"),"seed",value=1234),
               hr(),


          conditionalPanel("true==false",
                           checkboxInput(ns("plotOK"),"plotOK",value=FALSE)),
          conditionalPanel(condition=sprintf("input[['%s']]==true",ns("plotOK")),
                                            tagList(
                                              myp("Press `PubMedWordcloud` button to make plots."),

                                              dynamicShowDataUI(ns("dynamicShowData"))

                                            )

           )

     )
})


output$text=renderPrint({
    if(input$plotOK){
      pmQueryTotalCount(input$query)
    }

})


observeEvent(input$makeQuery,{

      temp=makeQuery(key=input$key,author=input$author,start=input$start,stop=input$stop)
      updateTextInput(session,"query",value=temp)
      updateCheckboxInput(session,"plotOK",value=TRUE)
      # count=pmQueryTotalCount(input$query)$total_count
      # updateNumericInput(session,"count",value=count)


})

observeEvent(input$reset,{
     updateTextInput(session,"key",value="")
     updateTextInput(session,"author",value="")
     updateNumericInput(session,"start",value=2000)
     updateNumericInput(session,"stop",value=2021)
     updateTextInput(session,"query",value="")
     updateCheckboxInput(session,"plotOK",value=FALSE)
})

makepubmedPPT=function(){


    makePPTList_pubmed(query=input$query,
                      seed=input$seed)


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
