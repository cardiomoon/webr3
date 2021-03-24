library(shiny)
library(pubmedR)
library(wordcloud)
library(ggwordcloud)
library(PubMedWordcloud)
library(glue)
library(webr3)


shinyServer(function(input,output,session){

  menu=c("PubMedWordcloud","PubMedWordcloud")
  packages=c("pubmedR","PubMedWordcloud")
  note=c("gather content from PubMed database","PubMed Wordcloud")

  citationDf=data.frame(menu,packages,note)

  callModule(citationModule,"citation",data=citationDf)

    savedPPT=reactiveValues(type=c(),title=c(),code=c())


    resPubmed=callModule(pubmedModule,"pubmed",
                       PPTdata=reactive(PPTdata()))
    output$title=renderUI({

      tagList(
        h1("PubMedWordcloud - web-r.org"),
        hr(),
        # if(input$main=="DataSelect")
        myp("With this app, you can search `pubmed` abstracts with keyword and/or author and make wordcloud plot."),
        hr()
      )
    })

    pptdf=reactive({
        if(length(savedPPT$code)==0) {
            result=""
        } else{

            result<-data.frame(type=savedPPT$type,title=savedPPT$title,code=savedPPT$code,
                               stringsAsFactors = FALSE)
        }
        result
    })

    PPTdata=reactive({
        data.frame(type=savedPPT$type,title=savedPPT$title,code=savedPPT$code,
                   stringsAsFactors = FALSE)
    })


    observeEvent(resPubmed(),{
      savedPPT$type=resPubmed()$type
      savedPPT$title=resPubmed()$title
      savedPPT$code=resPubmed()$code

    })

    pptdf2=callModule(pptxList,"List1",data=reactive(pptdf()))



})
