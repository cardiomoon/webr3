## bibliometrix App

options(shiny.sanitize.errors = FALSE)

library(bibliometrix)
library(shiny)
library(webr3)
library(ggthemes)
library(ggplot2)
library(grid)
library(editData)
library(plotly)


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
               myp("Select data and press `Get Data` button to start"),
               radioButtons("mydata","Select Data",choices=temp),
               actionButton("getData","Get Data",icon=icon("search-plus")),
               conditionalPanel("input[['getData']]>0",
                   br(),
                   editableDTUI("table")
               )
           )
    })

    observeEvent(input$getData,{
        RV$M=readRDS(paste0("./data/",input$mydata,".RDS"))
        RV$M$DI=ifelse(is.na(RV$M$DI),NA,paste0("<a href='http://www.doi.org/",RV$M$DI,"'>",RV$M$DI,"</a>"))
        RV$cols=match(c("AU","TI","J9","DA","DI"),names(RV$M))
        formatList=list(backgroundColor = 'white',fontSize = '80%')
        result=callModule(editableDT,"table",data=reactive(RV$M),length=50,
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
            h4("Enter number of data to get"),
            numericInput("k","k",min=5,max=50,value=20),
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
        result=makePPTList_biblio(
                                  filepath=paste0("./data/",input$mydata,".RDS"),
                                  # M=RV$M,
                                  k=input$k,
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
        result=makePPTList_biblio2(
                                  filepath=paste0("./data/",input$mydata,".RDS"),
                                  # M=RV$M,
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

    output$wordCloud=renderUI({
         tagList(
             sidebarLayout(
                 # Sidebar with a slider and selection inputs
                 sidebarPanel(width=3,
                              h3(em(strong("WordCloud"))),
                              br(),
                              actionButton("applyWordCloud", "Apply!"),
                              br(),
                              "  ",
                              h4(em(strong("Graphical Parameters:"))),
                              " ",
                              selectInput("summaryTerms", "Field",
                                          choices = c("Keywords Plus" = "ID",
                                                      "Author's keywords" = "DE",
                                                      "Titles" = "TI",
                                                      "Abstracts" = "AB"),
                                          selected = "ID"),
                              conditionalPanel(condition = "input.summaryTerms == 'AB' |input.summaryTerms == 'TI'",
                                               selectInput("summaryTermsngrams",'N-Grams',
                                                           choices = c("Unigrams" = "1",
                                                                       "Bigrams" = "2",
                                                                       "Trigrams" = "3"),
                                                           selected = 1)),
                              hr(),
                              sliderInput("n_words", label = "Number of words", min = 10, max = 200, step = 5, value = 50),
                              selectInput("measure", "Word occurrence measure",
                                          choices = c("Frequency" = "freq",
                                                      "Square root" = "sqrt",
                                                      "Log" = "log",
                                                      "Log10" = "log10"),
                                          selected = "freq"),
                              selectInput("wcShape", "Shape",
                                          choices = c("Circle" = "circle",
                                                      "Cardiod" = "cardioid",
                                                      "Diamond" = "diamond",
                                                      "Pentagon" = "pentagon",
                                                      "Star" = "star",
                                                      "Triangle-forward" = "triangle-forward"
                                                      ,"Triangle" = "triangle"),
                                          selected = "circle"),
                              selectInput("font", label = "Font type",
                                          choices = c("Impact", "Comic Sans MS (No plz!)" = "Comic Sans MS",
                                                      "Arial", "Arial Black", "Tahoma", "Verdana", "Courier New",
                                                      "Georgia", "Times New Roman", "Andale Mono")),
                              selectInput("wcCol", "Text colors",
                                          choices = c("Random Dark" = "random-dark",
                                                      "Random Light" = "random-light"),
                                          selected = "random-dark"),
                              #colourpicker::colourInput("wcBGCol", label= "Backgroud color",value="white", showColour = "background", returnName=TRUE),
                              sliderInput("scale", label = "Font size", min=0.2,max=5,step=0.1,value=1),
                              sliderInput("ellipticity", label = "Ellipticity", min=0,max=1,step=0.05,value=0.65),
                              sliderInput("padding", label = "Padding", min = 0, max = 5, value = 1, step = 1),
                              sliderInput("rotate", label = "Rotate", min = 0, max = 20, value = 0, step = 1)
                 ),

                 # Show Word Cloud
                 mainPanel(
                     tabsetPanel(type = "tabs",
                                 tabPanel("Plot",
                                          wordcloud2::wordcloud2Output("wordcloud", height = "80vh") #height = "600px")
                                 ),
                                 tabPanel("Table",
                                          shinycssloaders::withSpinner(DT::DTOutput("wordTable"))
                                 ))

                 )
             ))

    })

    output$wordcloud <- wordcloud2::renderWordcloud2({

        input$applyWordCloud

        isolate({
            if (input$summaryTerms %in% c("TI","AB")){
                ngrams <- as.numeric(input$summaryTermsngrams)
            }else{
                ngrams <- 1
            }


        W=wordlist(M=RV$M, Field=input$summaryTerms, n=input$n_words, measure=input$measure, ngrams=ngrams)

        wordcloud2::wordcloud2(W$W, size = input$scale, minSize = 0, gridSize =  input$padding,
                               fontFamily = input$font, fontWeight = 'normal',
                               color = input$wcCol, backgroundColor = "white", #input$wcBGCol,
                               minRotation = 0, maxRotation = input$rotate/10, shuffle = TRUE,
                               rotateRatio = 0.7, shape = input$wcShape, ellipticity = input$ellipticity,
                               widgetsize = NULL, figPath = NULL, hoverFunction = NULL)
        })
    })

    output$wordTable <- DT::renderDT({
        input$applyWordCloud

        isolate({
            if (input$summaryTerms %in% c("TI","AB")){
                ngrams <- as.numeric(input$summaryTermsngrams)
            }else{
                ngrams <- 1
            }

            M<-readRDS(paste0("./data/",input$mydata,".RDS"))
            W=wordlist(M=M, Field=input$summaryTerms, n=input$n_words, measure=input$measure, ngrams=ngrams)

            DT::datatable(W$W, rownames = FALSE)
        })
    })

    output$threeFieldsPlot=renderUI({
        sidebarLayout(
            sidebarPanel(width=3,
                         "  ",
                         "  ",
                         h3(em(strong("Three-Fields Plot "))),
                         "  ",
                         actionButton("apply3F", "Apply!"),
                         br(),
                         selectInput("CentralField",
                                     label = "Middle Field",
                                     choices = c("Authors" = "AU",
                                                 "Affiliations" = "AU_UN",
                                                 "Countries"="AU_CO",
                                                 "Keywords" = "DE",
                                                 "Keywords Plus" = "ID",
                                                 "Titles" = "TI_TM",
                                                 "Abstract" = "AB_TM",
                                                 "Sources" = "SO",
                                                 "References" = "CR",
                                                 "Cited Sources" = "CR_SO"),
                                     selected = "AU"),
                         sliderInput("CentralFieldn",
                                     label=("Middle Field: Number of items"),
                                     min = 1, max = 50, step = 1, value = 20),
                         selectInput("LeftField",
                                     label = "Left Field",
                                     choices = c("Authors" = "AU",
                                                 "Affiliations" = "AU_UN",
                                                 "Countries"="AU_CO",
                                                 "Keywords" = "DE",
                                                 "Keywords Plus" = "ID",
                                                 "Titles" = "TI_TM",
                                                 "Abstract" = "AB_TM",
                                                 "Sources" = "SO",
                                                 "References" = "CR",
                                                 "Cited Sources" = "CR_SO"),
                                     selected = "CR"),
                         sliderInput("LeftFieldn",
                                     label=("Left Field: Number of items"),
                                     min = 1, max = 50, step = 1, value = 20),
                         selectInput("RightField",
                                     label = "Right Field",
                                     choices = c("Authors" = "AU",
                                                 "Affiliations" = "AU_UN",
                                                 "Countries"="AU_CO",
                                                 "Keywords" = "DE",
                                                 "Keywords Plus" = "ID",
                                                 "Titles" = "TI_TM",
                                                 "Abstract" = "AB_TM",
                                                 "Sources" = "SO",
                                                 "References" = "CR",
                                                 "Cited Sources" = "CR_SO"),
                                     selected = "DE"),
                         sliderInput("RightFieldn",
                                     label=("Right Field: Number of items"),
                                     min = 1, max = 50, step = 1, value = 20)
            ),
            mainPanel(
                #tabPanel("Plot",
                shinycssloaders::withSpinner(plotlyOutput(outputId = "3FieldsPlot", height = "90vh"))
                #shinycssloaders::withSpinner(networkD3::sankeyNetworkOutput(outputId = "ThreeFielsPlot",height = "80vh")) #height = "600px"))
                #            )
            )
        )
    })

    TFP <- eventReactive(input$apply3F,{

        fields=c(input$LeftField, input$CentralField, input$RightField)
        threeFieldsPlot(RV$M, fields=fields,n=c(input$LeftFieldn, input$CentralFieldn,input$RightFieldn))
    })

    output[["3FieldsPlot"]] <- renderPlotly({

        TFP()

    })

    output$themaEvol=renderUI({

                 sidebarLayout(
                     sidebarPanel(width=3,
                                  h3(em(strong("Thematic Evolution"))),
                                  br(),
                                  actionButton("applyTE", "Apply!"),
                                  br(),
                                  h4(em(strong("TE Parameters: "))),
                                  "  ",
                                  selectInput("TEfield",
                                              label = "Field",
                                              choices = c("Keywords Plus" = "ID",
                                                          "Author's Keywords" = "DE",
                                                          "Titles" = "TI",
                                                          "Abstracts
                                                           " = "AB"),
                                              selected = "ID"),
                                  conditionalPanel(condition = "input.TEfield == 'TI' | input.TEfield == 'AB'",
                                                   selectInput("TEngrams",'N-Grams',
                                                               choices = c("Unigrams" = "1",
                                                                           "Bigrams" = "2",
                                                                           "Trigrams" = "3"),
                                                               selected = 1)),

                                  sliderInput("nTE", label="Number of Words",value=250,min=50,max=5000,step=10),
                                  sliderInput("fTE", label="Min Cluster Frequency (per thousand docs)",value=5,min=1,max=100,step=1),
                                  selectInput("TEmeasure",
                                              label = "Weight index",
                                              choices = c("Inclusion Index" = "inclusion",
                                                          "Inclusion Index weighted by Word-Occurrences" = "weighted",
                                                          "Stability Index" = "stability"
                                              ),
                                              selected = "weighted"),
                                  sliderInput("minFlowTE", label="Min Weight Index",value=0.1,min=0.02,max=1,step=0.02),
                                  sliderInput("sizeTE", label="Label size",value=0.3,min=0.0,max=1,step=0.05),
                                  sliderInput("TEn.labels", label="Number of Labels (for each cluster)",value=1,min=1,max=5,step=1),
                                  br(),
                                  h4(em(strong("Time Slices: "))),
                                  numericInput("numSlices", label="Number of Cutting Points",min=1,max=4,value=1),
                                  "Please, write the cutting points (in year) for your collection",
                                  uiOutput("sliders")


                     ),
                     mainPanel("Thematic Evolution",

                               # tabsetPanel(type = "tabs",
                                           tabPanel("Thematic Evolution", tabsetPanel(type="tabs",
                                                                                      tabPanel("Map",
                                                                                               #shinycssloaders::withSpinner(networkD3::sankeyNetworkOutput(outputId = "TEPlot", height = "80vh"))  #height = "600px"))
                                                                                               shinycssloaders::withSpinner(plotlyOutput(outputId = "TEPlot", height = "80vh"))
                                                                                      ),
                                                                                      tabPanel("Table",
                                                                                               shinycssloaders::withSpinner(DT::DTOutput(outputId = "TETable"))
                                                                                      ))
                                           )
                               #)

                     )
                 )


    })

    output$sliders <- renderUI({
        numSlices <- as.integer(input$numSlices)
        v=quantile(RV$M$PY, seq(0,1,by=(1/(numSlices+1))), na.rm=TRUE)
        v=round(v[-c(1,length(v))],0)
        lapply(1:numSlices, function(i) {
            # sliderInput(inputId = paste0("Slice", i), label = paste("Cutting Year", i),
            #             min=1990,max=2018,value=1990)

            numericInput(inputId = paste0("Slice", i), label = paste("Cutting Year", i),value=v[i],min=min(RV$M$PY, na.rm = TRUE)+1,max=max(RV$M$PY, na.rm = TRUE)-1, step=1)
            #numericInput(inputId = paste0("Slice", i), label = paste("Cutting Year", i),value=median(RV$M$PY),min=min(RV$M$PY)+1,max=max(RV$M$PY)-1, step=1)
        })
    })


    TEMAP <- eventReactive(input$applyTE,{
        if (input$TEfield %in% c("TI","AB")){
            ngrams <- as.numeric(input$TEngrams)
        }else{
            ngrams <- 1
        }

        cat("TEMAP\n")

        yearSlices=c()
        for (i in 1:as.integer(input$numSlices)){
            if (length(input[[paste0("Slice", i)]])>0){yearSlices=c(yearSlices,input[[paste0("Slice", i)]])}
        }

        cat(yearSlices)

        if (length(yearSlices)>0){

            RV$nexus <- thematicEvolution(RV$M, field=input$TEfield, yearSlices, n = input$nTE, minFreq = input$fTE, size = input$sizeTE, n.labels=input$TEn.labels, repel=FALSE, ngrams=ngrams)

            validate(
                need(RV$nexus$check != FALSE, "\n\nNo topics in one or more periods. Please select a different set of parameters.")
            )

            plotThematicEvolution(Nodes = RV$nexus$Nodes,Edges = RV$nexus$Edges, measure = input$TEmeasure, min.flow = input$minFlowTE)
        }
    })

    output$TEPlot <- plotly::renderPlotly({

        TEMAP()


    })

    output$TETable <- DT::renderDT({
        TEMAP()
        TEData=RV$nexus$Data
        TEData=TEData[TEData$Inc_index>0,-c(4,8)]
        names(TEData)=c("From", "To", "Words", "Weighted Inclusion Index", "Inclusion Index", "Occurrences", "Stability Index")
        DT::datatable(TEData, escape = FALSE, rownames = FALSE)
    })








})


