## bibliometrix App

options(shiny.sanitize.errors = FALSE)

library(bibliometrix)
library(shiny)
library(webr3)
library(ggthemes)
library(ggplot2)
library(grid)
library(editData)


shinyServer(function(input,output,session){

    RV=reactiveValues()
    M<-NULL

    menu=c("PubMedWordcloud","PubMedWordcloud")
    packages=c("pubmedR","PubMedWordcloud")
    note=c("gather content from PubMed database","PubMed Wordcloud")

    citationDf=data.frame(menu,packages,note)

    callModule(citationModule,"citation",data=citationDf)

    savedPPT=reactiveValues(type=c(),title=c(),code=c())

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

    pptdf2=callModule(pptxList,"List1",data=reactive(pptdf()))

    output$title=renderUI({
        tagList(
        h3(paste0("Bibliometric Analysis with ",input$mydata," data"))
        )
    })

    output$biblio=renderUI({

           temp=list.files("./data")
           temp=gsub(".RDS","",temp)
           tagList(
               radioButtons("mydata","select data",choices=temp),
               actionButton("getData","Get Data",icon=icon("search-plus")),
               conditionalPanel("input[['getData']]>0",
                   br(),
                   editableDTUI("table")
               )
           )
    })

    observeEvent(input$getData,{
        RV$data=readRDS(paste0("./data/",input$mydata,".RDS"))
        RV$data$DI=ifelse(is.na(RV$data$DI),NA,paste0("<a href='http://www.doi.org/",RV$data$DI,"'>",RV$data$DI,"</a>"))
        RV$cols=match(c("AU","TI","J9","DA","DI"),names(RV$data))
        formatList=list(backgroundColor = 'white',fontSize = '80%')
        result=callModule(editableDT,"table",data=reactive(RV$data),length=50,
                          cols=RV$cols,escape=FALSE,rownames=FALSE,
                          class = 'cell-border compact stripe',formatList=formatList)
    })

    output$Analysis1=renderUI({
        tagList(
            h4("What do you want to analyze?"),
            checkboxInput3("all","all",value=FALSE),
            checkboxInput3("main1","main",value=TRUE),
            checkboxInput3("sources","sources",value=FALSE),
            checkboxInput3("authors","authors",value=FALSE),
            checkboxInput3("affiliations","affiliations",value=FALSE),
            checkboxInput3("country","country",value=FALSE),
            checkboxInput3("documents","documents",value=FALSE),
            checkboxInput3("keywords","keywords",value=FALSE),
            h4("Press Button when ready"),
            dynamicShowDataUI("biblio1")
        )
    })
    output$Analysis2=renderUI({

        tagList(
            fluidRow(
                column(6,
                       wellPanel(
            h4("Options for collaboration plot"),
            selectInput("field1","field",choices=c("Authors","Institutions","Countries"),width="200px"),
            numericInput3("n1","n",min=10,max=100,value=50),
            numericInput3("seed1","random seed",value=1234))),
            column(6,
                   wellPanel(
            h4("Options for co-citation plot"),
            selectInput("field2","field",choices=c("Authors","Papers","Sources"),width="200px"),
            numericInput3("n2","n",min=10,max=100,value=50),
            numericInput3("seed2","random seed",value=1234)))
            ),
            fluidRow(
                column(6,
                       wellPanel(
                       h4("Options for co-occurence plot"),
                       selectInput("field3","field",choices=c("Keywords-Plus"="ID","Author-Keyword"="DE",
                                                              "Title"="TI","Abstract"="AB"),width="200px"),
                       numericInput3("n3","n",min=10,max=100,value=50),
                       numericInput3("seed3","random seed",value=1234))),
                column(6,
                       wellPanel(
                       h4("Options for Factor Analysis"),
                       selectInput("method","method",choices=c("MCA","CA","MDS"),width="200px"),
                       selectInput("field4","field",choices=c("Keywords-Plus"="ID","Author-Keyword"="DE",
                                                              "Title"="TI","Abstract"="AB"),width="200px"),
                       numericInput("minDegree","minDegree",value=40,width="200px")))
            ),
            h4("Press Button when ready"),
            dynamicShowDataUI("biblio2")
        )

    })


    makeBiblioPPT=function(){
        result=makePPTList_biblio(filepath=paste0("./data/",input$mydata,".RDS"),
                                  all=input$all,
                                  main=input$main1,
                                  sources=input$sources,
                                  authors=input$authors,
                                  affiliations=input$affiliations,
                                  country=input$country,
                                  documents=input$documents,
                                  keywords=input$keywords
                                  )
        result

    }

    makeBiblioPPT2=function(){
        result=makePPTList_biblio2(filepath=paste0("./data/",input$mydata,".RDS"),
                                  field1=input$field1,
                                  n1=input$n1,
                                  seed1=input$seed1,
                                  field2=input$field2,
                                  n2=input$n2,
                                  seed2=input$seed2,
                                  field3=input$field3,
                                  n3=input$n3,
                                  seed3=input$seed3,
                                  method=input$method,
                                  field4=input$field4,
                                  minDegree=input$minDegree)
        result

    }

    resultDynamicShowData=callModule(dynamicShowData,"biblio1",
                                     caption="Bibliometric_Analysis",
                                     data=reactive(makeBiblioPPT()),
                                     plotheight="700px")

    resultDynamicShowData2=callModule(dynamicShowData,"biblio2",
                                     caption="Bibliometric_Analysis2",
                                     data=reactive(makeBiblioPPT2()),
                                     plotheight="700px")

    observeEvent(resultDynamicShowData(),{

        if(!is.null(resultDynamicShowData())){
            savedPPT$type<-resultDynamicShowData()$type
            savedPPT$title<-resultDynamicShowData()$title
            savedPPT$code<-resultDynamicShowData()$code
        }
    })

    observeEvent(resultDynamicShowData2(),{

        if(!is.null(resultDynamicShowData2())){
            savedPPT$type<-resultDynamicShowData2()$type
            savedPPT$title<-resultDynamicShowData2()$title
            savedPPT$code<-resultDynamicShowData2()$code
        }
    })




})
