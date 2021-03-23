##  PS matching app
library(shiny)
library(pubmedR)
library(PubMedWordcloud)
library(glue)
library(webr3)


shinyServer(function(input,output,session){


    savedPPT=reactiveValues(type=c(),title=c(),code=c())


    resPubmed=callModule(pubmedModule,"pubmed",
                       PPTdata=reactive(PPTdata()))


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



})
