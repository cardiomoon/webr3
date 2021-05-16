#' modify biblioData
#' @param M is a bibliographic data frame obtained by the converting function convert2df.
#' @importFrom bibliometrix metaTagExtraction termExtraction
#' @importFrom shiny isRunning Progress
modifyBiblioData=function(M){
    fields=c("CR_AU","CR_SO","AU_CO","AU1_CO","AU_UN","SR")
    if (shiny::isRunning()) {
        progress <- shiny::Progress$new()
        on.exit(progress$close())
        progress$set(message = "Extracting terms", value = 0)
    } else {
        cat("Extracting terms\n")
    }
    for(i in seq_along(fields)){
        if (shiny::isRunning()) {
            progress$inc(1/10, detail = paste("Doing part", i , "/", 10))
        } else { cat("processing",fields[i], "\n")}
        M = metaTagExtraction(M, fields[i])
    }
    fields=c("TI","AB","ID","DE")
    for(i in seq_along(fields)){
        if (shiny::isRunning()) {
            progress$inc(1/10, detail = paste("Doing part", i+6 , "/", 10))
        } else { cat("processing",fields[i], "\n")}

        M = termExtraction(M,  fields[i])
    }
    M
}



#' function to melt data
#' @param LM a matrix
meltx <- function(LM) {
    var1 <- rep((1:nrow(LM)), ncol(LM))
    var2 <- sort(var1)
    LMM <-
        data.frame(
            Var1 = rownames(LM)[var1],
            Var2 = colnames(LM)[var2],
            value = matrix(LM, length(LM), 1),
            stringsAsFactors = FALSE
        )
    return(LMM)
}

#' count dupicates
#' @param DF A data.frame
count.duplicates <- function(DF){
    x <- do.call('paste', c(DF, sep = '\r'))
    ox <- order(x)
    rl <- rle(x[ox])
    cbind(DF[ox[cumsum(rl$lengths)],,drop=FALSE],count = rl$lengths)
}

#' Make a world map plot
#' @param M is a bibliographic data frame obtained by the converting function convert2df.
#' @param label logical Whether or not show label
#' @param edgesize numeric
#' @param min.edges numeric
#' @importFrom ggplot2 ggplot geom_polygon scale_fill_continuous guides geom_curve labs scale_size_continuous theme
#' element_text element_blank map_data
#' @importFrom ggrepel geom_text_repel
#' @importFrom dplyr inner_join
#' @importFrom igraph ends E graph_from_adjacency_matrix
#' @importFrom Matrix `diag<-`
#' @importFrom bibliometrix biblioNetwork
#' @importFrom stats quantile
#' @importFrom utils data
#' @export
countrycollaboration <- function(M,label=FALSE,edgesize=2.5,min.edges=2){
    M=metaTagExtraction(M,"AU_CO")

    net=biblioNetwork(M,analysis="collaboration",network="countries")
    CO=data.frame(Tab=rownames(net),Freq=Matrix::diag(net),stringsAsFactors = FALSE)
    bsk.network=igraph::graph_from_adjacency_matrix(net,mode="undirected")
    COedges=as.data.frame(igraph::ends(bsk.network,igraph::E(bsk.network),names=TRUE),stringsAsFactors = FALSE)

    map.world <- map_data("world")
    map.world$region=toupper(map.world$region)
    map.world$region=gsub("UK","UNITED KINGDOM",map.world$region)
    map.world$region=gsub("SOUTH KOREA","KOREA",map.world$region)

    country.prod <- dplyr::left_join( map.world, CO, by = c('region' = 'Tab'))

    breaks=as.numeric(round(quantile(CO$Freq,c(0.2,0.4,0.6,0.8,1))))
    names(breaks)=breaks
    breaks=log(breaks)
    data("countries",envir=environment())
    names(countries)[1]="Tab"

    COedges=dplyr::inner_join(COedges,countries, by=c('V1'='Tab'))
    COedges=dplyr::inner_join(COedges,countries, by=c('V2'='Tab'))
    COedges=COedges[COedges$V1!=COedges$V2,]
    COedges=count.duplicates(COedges)
    tab=COedges
    COedges=COedges[COedges$count>=min.edges,]

    g=ggplot(country.prod, aes( x = .data$long, y = .data$lat, group = .data$group )) +
        geom_polygon(aes_string(fill = "log(Freq)")) +
        scale_fill_continuous(low='dodgerblue', high='dodgerblue4',breaks=breaks)+
        #guides(fill = guide_legend(reverse = T)) +
        guides(colour=FALSE, fill=FALSE)+
        geom_curve(data=COedges, aes(x = .data$Longitude.x , y = .data$Latitude.x, xend = .data$Longitude.y, yend = .data$Latitude.y,     # draw edges as arcs
                                     color = "firebrick4", size = .data$count, group=.data$continent.x),
                   curvature = 0.33,
                   alpha = 0.5) +
        labs(x = "Latitude", y = "Longitude")+
        scale_size_continuous(guide = FALSE, range = c(0.25, edgesize))+
        theme(text = element_text(color = '#333333')
              ,plot.title = element_text(size = 28)
              ,plot.subtitle = element_text(size = 14)
              ,axis.ticks = element_blank()
              ,axis.text = element_blank()
              ,panel.grid = element_blank()
              ,panel.background = element_rect(fill = '#FFFFFF')  #'#333333'
              ,plot.background = element_rect(fill = '#FFFFFF')
              ,legend.position = c(.18,.36)
              ,legend.background = element_blank()
              ,legend.key = element_blank()
        )
    if (isTRUE(label)){
        CO=dplyr::inner_join(CO,countries, by=c('Tab'='Tab'))
        g=g+
            ggrepel::geom_text_repel(data=CO, aes(x = .data$Longitude, y = .data$Latitude, label = .data$Tab, group=.data$continent),             # draw text labels
                                     hjust = 0, nudge_x = 1, nudge_y = 4,
                                     size = 3, color = "orange", fontface = "bold")
    }

    results=list(g=g,tab=tab)
    return(results)
}

#' Citation table
#'@param  x result of biblioSummary
#'@importFrom stats aggregate
#'@export
citationTable=function(x){
    Table2 = aggregate(x$TotalCitation, by = list(x$Years),
                       length)
    Table2$xx = aggregate(x$TotalCitation, by = list(x$Years),
                          mean)$x
    Table2$Annual = NA
    d = date()
    d = as.numeric(substring(d, nchar(d) - 3, nchar(d)))
    Table2$Years = d - Table2$Group.1
    Table2$Annual = Table2$xx/Table2$Years
    names(Table2) = c("Year", "N", "MeanTCperArt", "MeanTCperYear",
                      "CitableYears")
    YY = setdiff(seq(min(x$Years, na.rm = TRUE), max(x$Years,
                                                     na.rm = TRUE)), Table2$Year)
    if (length(YY > 0)) {
        YY = data.frame(YY, 0, 0, 0, 0)
        names(YY) = c("Year", "N", "MeanTCperArt", "MeanTCperYear",
                      "CitableYears")
        Table2 = rbind(Table2, YY)
        Table2 = Table2[order(Table2$Year), ]
        row.names(Table2) = Table2$Year
    }
    Table2
}

#'source growth Plot
#'@param ... arguments to be passed to sourceGrowth
#'@importFrom ggplot2 aes_string ggplot geom_line guides theme theme_bw
#'@importFrom bibliometrix sourceGrowth
#'@importFrom tidyr pivot_longer
#'@importFrom ggplot2 guide_legend margin
#'@export
#'@examples
#'#sourceGrowthPlot(M,top=5,cdf=FALSE)
sourceGrowthPlot=function(...){
    x=sourceGrowth(...)
    longdf=pivot_longer(x,-1,names_to="source")

    ggplot(longdf,aes_string(x="Year",y="value",color="source"))+
        geom_line()+
        guides(color=guide_legend(nrow=3,byrow=TRUE))+
        theme_bw()+
        theme(legend.position="bottom",legend.margin=margin())

}


#' get hindex data about author
#' @param M is a bibliographic data frame obtained by the converting function convert2df.
#' @param results Result of biblioAnalysis
#' @param k integer
#' @importFrom bibliometrix Hindex
#' @export
hindexAuthor=function(M,results,k=20){
    authors=gsub(',',' ',names(results$Authors)[1:k])
    Hindex(M, field = "author", elements = authors, years=Inf)$H
}

#' get hindex data of source
#' @param M is a bibliographic data frame obtained by the converting function convert2df.
#' @param k integer
#' @export
hindexSource=function(M,k=20){
    SO <- names(sort(table(M$SO),decreasing = TRUE))[1:k]
    Hindex(M, "source", elements = SO, years=Inf)$H
}


#'draw cleveland plot
#'@param data A data.frame
#'@param y numeric position of y-axis variable
#'@param x numeric position of x-axis variable
#'@param xname character Name of x-axis variable
#'@param yname character Name of y-axis variable
#'@param digits integer indicating the number of decimal places
#'@importFrom ggplot2 geom_col geom_text
#'@importFrom ggthemes theme_clean
#'@importFrom stats na.omit reorder
#'@export
myCLEplot=function(data,y=1,x=2,xname=NULL,yname=NULL,digits=2){
    # y=3;x=4;xname=NULL;yname=NULL;digits=2
    if(is.null(yname)) yname=names(data)[y]
    if(is.null(xname)) xname=names(data)[x]
    data=data[,c(x,y)]
    # data=na.omit(data)
    if(sum(is.na(data[[xname]]))>0) data=na.omit(data)
    if(!is.numeric(data[[xname]])) data[[xname]]=as.numeric(data[[xname]])
    if(!is.integer(data[[xname]])) data[[xname]]=round(data[[xname]],digits)
    colnames(data)=c("x","y")
    data$y1=nrow(data):1
    # ggplot(data,aes(x=x,y=reorder(y,x),xend=0,yend=reorder(y,x)))+
    #     geom_segment(aes(size=x,color=x))+
    #     geom_text(aes(label=x),hjust=-0.1)+
    #     guides(size=FALSE,color=FALSE,fill=FALSE)+
    #     labs(x=xname,y=yname)+ggthemes::theme_clean()
    ggplot(data,aes(x=x,y=reorder(y,x)))+
        geom_col(fill="blue",width=0.5,alpha=0.5)+
        geom_text(aes(label=x),hjust=-0.1)+
        guides(fill=FALSE)+
        labs(x=xname,y=yname)+ggthemes::theme_clean()



}

#' Most locally cited source
#' @param M is a data frame obtained by the converting function convert2df and modified with modifyBiblioData
#' @param k numeric
#' @importFrom bibliometrix tableTag
#' @export
MLCSources <- function(M,k=20){
    M=metaTagExtraction(M,"CR_SO")
    TAB=tableTag(M,"CR_SO")
    TAB=data.frame(Sources=names(TAB),Articles=as.numeric(TAB),stringsAsFactors = FALSE)
    TAB[1:k,]
}

#' Local citation score
#' @param M is a data frame obtained by the converting function convert2df and modified with modifyBiblioData
#' @param k numeric
#' @importFrom bibliometrix localCitations
#' @importFrom dplyr group_by mutate ungroup `%>%`
#' @importFrom rlang .data
#' @export
locCit=function(M,k=20){
    TAB <-localCitations(M)$Paper
    TAB <- TAB %>%
        group_by(.data$Year) %>%
        mutate(`LC/GC Ratio(%)` = .data$LCS/.data$GCS*100,
               `Normalized Local Citations` = .data$LCS/mean(.data$LCS),
               `Normalized Global Citations` = .data$GCS/mean(.data$GCS)) %>%
        ungroup() %>%
        as.data.frame()
    TAB[1:k,]
}
#' Keywords Count
#' @param M is a data frame obtained by the converting function convert2df and modified with modifyBiblioData
#'@param terms One of c("ID","DE","TI","AB")
#' @param k numeric
#' @export
wordCount=function(M,terms="ID",k=20){
    fields=c("ID","DE","TI","AB")
    select=grep(terms,fields)
    fieldnames=c("Keywords-Plus","Author_Keywords","Title","Abstract")
    for(i in seq_along(fields)){
        temp=data.frame(tableTag(M,terms)[1:k])
        colnames(temp)[1]=fieldnames[select]
        df=temp
    }
    df
}

#'Make wordDynamic Data
#'@param M is a data frame obtained by the converting function convert2df and modified with modifyBiblioData
#'@param terms One of c("ID","DE","TI","AB")
#'@param top numeric
#'@param cdf  logical Whrther or not cumulative
#'@importFrom bibliometrix KeywordGrowth
#'@export
wordDynamic=function(M,terms="ID",top=10,cdf=TRUE){
    if(terms=="TI"){
        M=termExtraction(M,Field = "TI", verbose=FALSE)
        terms="TI_TM"
    } else if(terms=="AB"){
        M=termExtraction(M,Field = "AB", verbose=FALSE)
        terms="AB_TM"
    }

    KW=KeywordGrowth(M, Tag = terms, sep = ";", top = top, cdf = cdf)
    KW=KW[,c(1,seq(1,top)+1)]
    KW

}



#' Make wordDynamic Plot
#'@param M is a data frame obtained by the converting function convert2df and modified with modifyBiblioData
#'@param terms One of c("ID","DE","TI","AB")
#'@param top numeric
#'@param cdf  logical Whrther or not cumulative
#'@export
wordDynamicPlot=function(M,terms="ID",top=10,cdf=TRUE){
    KW<-wordDynamic(M,terms=terms,top=top,cdf=cdf)
    term=names(KW)[-1]
    term=rep(term,each=dim(KW)[1])
    n=dim(KW)[1]*(dim(KW)[2]-1)
    freq=matrix(as.matrix(KW[,-1]),n,1)
    DF=data.frame(Year=rep(KW$Year,(dim(KW)[2]-1)),Term=term, Freq=freq, stringsAsFactors = TRUE)
    DF
    ggplot(DF,aes_string(x="Year",y="Freq",color="Term"))+
        geom_line()+
        guides(color=guide_legend(nrow=3,byrow=TRUE))+
        theme_bw()+
        theme(legend.position="bottom",legend.margin=margin())
}

#'Trend Tpoics
#'@param M is a data frame obtained by the converting function convert2df and modified with modifyBiblioData
#'@param terms One of c("ID","DE","TI","AB")
#'@param ... further arguments to be passed to fieldByYear
#'@importFrom bibliometrix fieldByYear
#'@export
TrendTopics <- function(M,terms="ID",...){

    if (terms %in% c("TI","AB")){
        M=termExtraction(M, Field = terms, stemming = FALSE, verbose = FALSE)
        field=paste(terms,"_TM",sep="")
    } else {field=terms}
    M<-M[!is.na(M$PY),]
    trendTopics <- fieldByYear(M, field = field, graph = FALSE,...)
    trendTopics

}


#' Make Powerpoint List with biblio data
#' @param M is a bibliographic data frame obtained by the converting function convert2df.
#' @param k numeric
#' @param all logical
#' @param main logical
#' @param sources logical
#' @param authors logical
#' @param affiliations logical
#' @param country logical
#' @param documents logical
#' @param keywords logical
#' @export
makePPTList_biblio=function(M,k=20,all=FALSE,main=FALSE,sources=FALSE,authors=FALSE,affiliations=FALSE,country=FALSE,documents=FALSE,keywords=FALSE){


    if(all==TRUE){
        main=TRUE
        sources=TRUE
        authors=TRUE
        affiliations=TRUE
        country=TRUE
        documents=TRUE
        keywords=TRUE
    }


    title=c("data","Bibliometric Analysis")
    type=c("out","out")
    code=c("M<-M","results<-biblioAnalysis(M)")

    title=c(title,"Summary")
    type=c(type,"out")
    code=c(code,paste0("sumResult<-summary(results,k=",k,",verbose=FALSE)"))

    title=c(title,"Plot summary")
    type=c(type,"Rcode")
    code=c(code,paste0("plotlist<-plot(results,k=",k,")"))

    if(main==TRUE){

        ## Main Information

        title=c(title,"Main Information About Data")
        type=c(type,"Rcode")
        code=c(code,"cat(sumResult[[1]])")

        title=c(title,"Annual Scientific Production")
        type=c(type,"data2")
        code=c(code,"sumResult[[3]]")

        title=c(title,"Annual Scientific Production")
        type=c(type,"ggplot")
        code=c(code,"plotlist[[3]]")

        title=c(title,"Annual Percentage Growth Rate")
        type=c(type,"Rcode")
        code=c(code,"cat(sumResult[[4]])")

        title=c(title,"Citation Table")
        type=c(type,"data2")
        code=c(code,"citationTable(results)")

        title=c(title,"Average Article Citations per Year")
        type=c(type,"ggplot")
        code=c(code,"plotlist[[4]]")

        title=c(title,"Average Total Citations per Year")
        type=c(type,"ggplot")
        code=c(code,"plotlist[[5]]")
    }

    ## Sources

    if(sources==TRUE){
        title=c(title,"Most Relevant Sources")
        type=c(type,"data2")
        code=c(code,"sumResult[[9]]")

        title=c(title,"Most Relevant Sources")
        type=c(type,"ggplot")
        code=c(code,"myCLEplot(sumResult[[9]])")

        title=c(title,"Most Locally Cited Source")
        type=c(type,"data2")
        code=c(code,paste0("MLCSources(M,k=",k,")"))

        title=c(title,"Most Locally Cited Source")
        type=c(type,"ggplot")
        code=c(code,paste0("myCLEplot(MLCSources(M,k=",k,"))"))

        title=c(title,"","")
        type=c(type,"input","input")
        code=c(code,"numericInput3('top','top',min=1,max=10,value=5)","checkboxInput3('cumulative','cumulative',value=TRUE)")

        title=c(title,"Source Growth")
        type=c(type,"data2")
        code=c(code,"sourceGrowth(M,top={input$top},cdf={input$cumulative})")

        title=c(title,"Source Growth Plot")
        type=c(type,"ggplot")
        code=c(code,"sourceGrowthPlot(M,top={input$top},cdf={input$cumulative})")

        title=c(title,"Source Local Impact")
        type=c(type,"data2")
        code=c(code,paste0("hindexSource(M,k=",k,")"))

        title=c(title,"")
        type=c(type,"input")
        code=c(code,"selectInput('xname','Impact Measure',choices=c('h_index'=2,'g_index'=3,'m_index'=4,'total Citation'=5),selected=2)")

        title=c(title,"Source Local Impact")
        type=c(type,"ggplot")
        code=c(code,paste0('myCLEplot(hindexSource(M,k=',k,'),y=1,x={input$xname})'))
    }

    ## Authors

    if(authors==TRUE){
        title=c(title,"Most Productive Author")
        type=c(type,"data2")
        code=c(code,"sumResult[[5]]")

        title=c(title,"Most Productive Author")
        type=c(type,"ggplot")
        code=c(code,"myCLEplot(sumResult[[5]])")

        title=c(title,"most frequent first authors")
        type=c(type,"data2")
        code=c(code,paste0("data.frame(citations(M, field = 'author')$Cited[1:",k,"])"))

        title=c(title,"most frequent first authors")
        type=c(type,"ggplot")
        code=c(code,paste0("myCLEplot(data.frame(citations(M, field = 'author')$Cited[1:",k,"]))"))

        title=c(title,"most local cited authors")
        type=c(type,"data2")
        code=c(code,paste0("localCitations(M)$Authors[1:",k,",]"))

        title=c(title,"most local cited authors")
        type=c(type,"ggplot")
        code=c(code,paste0("myCLEplot(localCitations(M)$Authors[1:",k,",])"))

        title=c(title,"Authors' Dominance ranking")
        type=c(type,"data2")
        code=c(code, paste0("dominance(results, k =",k,")"))

        title=c(title,"Author Impact")
        type=c(type,"data2")
        code=c(code,paste0("hindexAuthor(M,results,k=",k,")"))

        title=c(title,"")
        type=c(type,"input")
        code=c(code,"selectInput('xname2','Impact Measure',choices=c('h_index'=2,'g_index'=3,'m_index'=4,'total Citation'=5),selected=2)")

        title=c(title,"Source Local Impact")
        type=c(type,"ggplot")
        code=c(code,paste0('myCLEplot(hindexAuthor(M,results,k=',k,'),y=1,x={input$xname2})'))

        title=c(title,"Top Author's Production over the time")
        type=c(type,"ggplot")
        code=c(code,paste0("topAU <- authorProdOverTime(M, k = ",k,", graph = FALSE);removeGeoms(topAU$graph,geoms='GeomCustomAnn')+ggtitle('')"))

    }

    ## Affiliations
    if(affiliations==TRUE){

        title=c(title,"Most Relevant Affiliations")
        type=c(type,"data2")
        code=c(code,paste0("data.frame(results$Affiliations[1:",k,"])"))

        title=c(title,"Most Relevant Affiliations")
        type=c(type,"ggplot")
        code=c(code,paste0("myCLEplot(data.frame(results$Affiliations[1:",k,"]))"))
    }

    ## Country

    if(country==TRUE){
        title=c(title,"Most Productive Countries")
        type=c(type,"data2")
        code=c(code,"sumResult[[7]]")

        title=c(title,"Most Productive Countries")
        type=c(type,"ggplot")
        code=c(code,"plotlist[[2]]")

        title=c(title,"Total Citation per Country")
        type=c(type,"data2")
        code=c(code,"sumResult[[8]]")  ## Most Productive Countries (Corresponding Author's country) data.frame

        title=c(title,"Total Citation per Country")
        type=c(type,"ggplot")
        code=c(code,"myCLEplot(sumResult[[8]])")

        title=c(title,"Average Article Citations")
        type=c(type,"ggplot")
        code=c(code,"myCLEplot(sumResult[[8]],x=3)")

        title=c(title,"Country Scientific Production")
        type=c(type,"ggplot")
        code=c(code,"mapworld(M)")


    }

    if(documents==TRUE){

        title=c(title,"Most Global Cited Documents")
        type=c(type,"data2")
        code=c(code,"sumResult[[6]]")

        title=c(title,"")
        type=c(type,"input")
        code=c(code,"selectInput('xname3','Measure',choices=c('Total Citations'=3,'Total Citations per Year'=4,'Normalized Total Citations'=5),selected=3)")

        title=c(title,"Most Global Cited Documents")
        type=c(type,"ggplot")
        code=c(code,"myCLEplot(sumResult[[6]],x={input$xname3})")

        title=c(title,"Most Local Cited Documents")
        type=c(type,"data2")
        code=c(code,"locCit(M)")


        title=c(title,"")
        type=c(type,"input")
        code=c(code,"selectInput('xname4','Measure',choices=c('Local Citations'=4,'Global Citations'=5,'LC/GC ratio'=6,'Normalized LC'=7,'Normalized GC'=8),selected=3)")

        title=c(title,"Most Local Cited Documents")
        type=c(type,"ggplot")
        code=c(code,"myCLEplot(locCit(M),x={input$xname4})")
    }

    if(keywords==TRUE){

        title=c(title,"")
        type=c(type,"input")
        code=c(code,"selectInput('xname5','field',choices=c('Keywords-Plus'='ID','Author Keywords'='DE'),selected='ID')")

        title=c(title,"Most Relevant Keywords")
        type=c(type,"data2")
        code=c(code,paste0('wordCount(M,terms="{input$xname5}",k=',k,')'))

        title=c(title,"Most Relevant Keywords")
        type=c(type,"ggplot")
        code=c(code,paste0('myCLEplot(wordCount(M,terms="{input$xname5}",k=',k,'))'))

        title=c(title,"word cloud")
        type=c(type,"plot")
        code=c(code,"plotWC1(M)")

        title=c(title,"Tree Map")
        type=c(type,"ggplot")
        code=c(code,"treeMap(M)")


        title=c(title,"","","")
        type=c(type,rep("input",3))
        code=c(code,
               "pickerInput3('terms','terms',choices=c('Keywords-Plus'='ID','Author Keywords'='DE','Title'='TI','Abstract'='AB'),selected='ID',width=200)",
               "numericInput3('top','keyword numbers',min=1,max=50,value=10)",
               "checkboxInput3('cdf','cumulative',value=TRUE)")


        title=c(title,"Word Dynamic")
        type=c(type,"data2")
        code=c(code,'wordDynamic(M,terms="{input$terms}",top={input$top},cdf={input$cdf})')

        title=c(title,"Word Dynamic")
        type=c(type,"ggplot")
        code=c(code,'wordDynamicPlot(M,terms="{input$terms}",top={input$top},cdf={input$cdf})')


        title=c(title,"","","")
        type=c(type,rep("input",3))
        code=c(code,
               "pickerInput3('terms2','terms',choices=c('Keywords-Plus'='ID','Author Keywords'='DE','Title'='TI','Abstract'='AB'),selected='ID',width=200)",
               "numericInput3('min.freq','min frequency of the items to include',min=1,max=10,value=2,width=250)",
               "numericInput3('n.items','maximum number of items per year',min=1,max=50,value=5,width=250)")


        title=c(title,"Trend Topics")
        type=c(type,"data2")
        code=c(code,'TrendTopics(M,"{input$terms2}",min.freq={input$min.freq},n.items={input$n.items})$df_graph')

        title=c(title,"Trend Topics")
        type=c(type,"ggplot")
        code=c(code,'TrendTopics(M,"{input$terms2}",min.freq={input$min.freq},n.items={input$n.items})$graph %>% removeGeoms(geoms="GeomCustomAnn")')

    }

    result=data.frame(title,type,code)
    result
}



#' Make second Powerpoint List with biblio data
#' @param M is a bibliographic data frame obtained by the converting function convert2df.
#' @param field1 Character to be passed to collaboPlot
#' @param n1 numeric to be passed to collaboPlot
#' @param seed1 numeric random seed to be passed to collaboPlot
#' @param field2 Character to be passed to cocicPlot
#' @param n2 numeric to be passed to cocicPlot
#' @param seed2 numeric random seed to be passed to cocicPlot
#' @param field3 Character to be passed to cocPlot
#' @param n3 numeric to be passed to cocPlot
#' @param seed3 numeric random seed to be passed to cocPlot
#' @param method character factorial method. One of c("MCA","CA","MDS")
#' @param field4 character One of c("ID","DE","ID_TM","DE_TM","TI","AB")
#' @param minDegree numeric minimum occurrences of terms to analize and plot.
#' @importFrom glue glue
#' @export
makePPTList_biblio2=function(M,field1="Authors",n1=50,seed1=1234,
                             field2="Authors",n2=50,seed2=1234,
                             field3="ID",n3=50,seed3=1234,
                             method="MCA",field4="ID",minDegree=40){

    #title<-type<-code<-c()
    # title="Data"
    # type="out"
    # code=paste0("M<-readRDS('",filepath,"')")

    title="Data"
    type="out"
    code="M<-M"

    # title=c(title,"Rcode")
    # type=c(type,"Rcode")
    # code=c(code,"str(M)")

    # title<-type<-code<-c()

    title=c(title,"Collaboration Plot")
    type=c(type,"out")
    code=c(code,glue::glue('p1<-collaboPlot(M,fields="{field1}",n={n1},seed={seed1})'))

    title=c(title,"Collaboration Plot")
    type=c(type,"ggplot")
    code=c(code,'plot(p1$graph)')

    title=c(title,"Country Collaboration Map")
    type=c(type,"ggplot")
    code=c(code,'countrycollaboration(M)$g')

    title=c(title,"Co-Citation Plot")
    type=c(type,"out")
    code=c(code,glue::glue('p2<-cocicPlot(M,fields="{field2}",n={n2},seed={seed2})'))

    title=c(title,"Co-Citation Plot")
    type=c(type,"ggplot")
    code=c(code,'plot(p2$graph)')

    title=c(title,"Co-Occurence Network")
    type=c(type,"out")
    code=c(code,glue::glue('p3<-cocPlot(M,fields="{field3}",n={n3},seed={seed3})'))

    title=c(title,"Co-Occurence Network")
    type=c(type,"ggplot")
    code=c(code,'plot(p3$graph)')

    title=c(title,"Factorial Analysis")
    type=c(type,"out")
    code=c(code,glue::glue('CS<-conceptualStructure(M,method="{method}",field="{field4}", minDegree={minDegree},clust=0,k.max=8,labelsize=10,documents=5,graph=FALSE)'))


    title=c(title,"Conceptual Structure Map","Topic Dendrogram","Factorial map of documents with the highest contributes","Factorial map of the most cited documents")
    type=c(type,rep("ggplot",4))
    code=c(code,paste0("removeGeoms(CS[[",4:7,"]])"))

    # title=c(title,"Historical Direct Citation Network")
    # type=c(type,"out")
    # code=c(code,"p4<-removeGeoms(histPlot(histNetwork(M),size=4,labelsize=3,verbose=FALSE)$g,geoms='GeomCustomAnn')+ggtitle('')")
    #
    # title=c(title,"Historical Direct Citation Network")
    # type=c(type,"ggplot")
    # code=c(code,"print(p4)")
    result=data.frame(title,type,code)
    result

}

#' Create keyword collaboration network
#' @param M is a bibliographic data frame obtained by the converting function convert2df.
#' @param fields character One of c("Authors","Institutions","Countries")
#' @param n The number of items to select.
#' @param seed numeric random seed
#' @param ... Further arguments to be passed to networkPlot
#' @importFrom bibliometrix networkPlot
#' @export
#' @examples
#' \dontrun{
#' net=collaboPlot(M,"Authors",n=50,verbose=FALSE)
#' net=collaboPlot(M,"Institutions",n=50,verbose=FALSE)
#' net=collaboPlot(M,"Countries",n=30,verbose=FALSE)
#' plot(net$graph)
#' }
collaboPlot=function(M,fields="Authors",n=50,seed=1234,...){
    if(fields=="Authors") {
        network="authors"
        title="Author Collaboration Network"
    } else if(fields=="Institutions") {
        if(!("AU_UN" %in% names(M))){M=metaTagExtraction(M,Field="AU_UN", sep=";")}
        network="universities"
        title="University Collaboration network"
    } else {
        if(!("AU_CO" %in% names(M))){M=metaTagExtraction(M,Field="AU_CO", sep=";")}
        network="countries"
        title="Country Collaboration network"
    }
    NetMatrix <- biblioNetwork(M, analysis = "collaboration", network = network, sep = ";",n=n)
    set.seed(seed)
    networkPlot(NetMatrix, n = n, normalize="association",Title=title,
                type = "auto", labelsize=0.7,size=5,size.cex=TRUE,remove.multiple=TRUE,remove.isolates = TRUE,
                community.repulsion = 0.01,...)
}

#' Create Co-citation Network
#' @param M is a bibliographic data frame obtained by the converting function convert2df.
#' @param fields character One of c("Authors","Papers","Sources")
#' @param n The number of items to select.
#' @param seed numeric random seed
#' @param ... Further arguments to be passed to networkPlot
#' @export
#' @examples
#' \dontrun{
#' net=cocicPlot(M,"Authors",n=50,verbose=FALSE)
#' net=cocicPlot(M,"Papers",n=50,verbose=FALSE)
#' net=cocicPlot(M,"Sources",n=30,verbose=FALSE)
#' plot(net$graph)
#' }
cocicPlot=function(M,fields="Authors",n=50,seed=1234,...){
    if(fields=="Authors") {
        if(!("CR_AU" %in% names(M))){M=metaTagExtraction(M,Field="CR_AU", sep = ";")}
        M=M[M$CR_AU!="NA",]
        network="authors"
        title="Cited Authors Network"
    } else if(fields=="Papers") {
        if(!("AU_UN" %in% names(M))){M=metaTagExtraction(M,Field="AU_UN", sep=";")}
        network="references"
        title="Cited References Network"
    } else {
        if(!("CR_SO" %in% names(M))){M=metaTagExtraction(M,Field="CR_SO", sep = ";")}
        network="sources"
        title="Cited Sources Network"
    }
    NetMatrix <- biblioNetwork(M, analysis = "co-citation", network = network, sep = ";",n=n)
    set.seed(seed)
    # networkPlot(NetMatrix, n = n, normalize="association",Title=title,
    #             type = "auto", labelsize=0.7,size=5,size.cex=TRUE,remove.multiple=TRUE,remove.isolates = TRUE,
    #             community.repulsion = 0.01,...)
    networkPlot(NetMatrix,n = 50,Title = "Co-Citation Network", type = "auto",
                size=5,size.cex=T, remove.multiple=FALSE, labelsize=0.7,edgesize = 0.2,
                community.repulsion = 0.05,...)
}


#' Create keyword co-occurrences network
#' @param M is a bibliographic data frame obtained by the converting function convert2df.
#' @param fields character  One of c("ID","DE","TI","AB")
#' @param n The number of items to select.
#' @param seed numeric random seed
#' @param ... Further arguments to be passed to networkPlot
#' @export
#' @examples
#' \dontrun{
#' net=cocPlot(M,"ID",n=50,verbose=FALSE)
#' plot(net$graph)
#' }
cocPlot=function(M,fields="ID",n=50,seed=1234,...){
    if(fields=="TI") {
        M=termExtraction(M,Field="TI",verbose=FALSE)
        network="titles"
        title="Title Words Network"
    } else if(fields=="AB") {
        M=termExtraction(M,Field="TI",verbose=FALSE)
        network="abstracts"
        title="Abstract Words Network"
    } else if(fields=="ID"){
        network="keywords"
        title="Keywords Plus Network"
    } else {
        network="author_keywords"
        title="Authors' Keyword Network"
    }
    NetMatrix <- biblioNetwork(M, analysis = "co-occurrences", network = network, sep = ";",n=n)
    set.seed(seed)
    networkPlot(NetMatrix, n = n, normalize="association",Title=title,
                type = "auto", cluster="louvain",size=T,labelsize=0.7,edgesize = 0.5,
                community.repulsion = 0.05,...)
}

#' Extract data from co-occurence network plot
#' @param net result of networkPlot
#' @export
#' @examples
#' \dontrun{
#' net=cocPlot(M,"ID",n=50,verbose=FALSE)
#' net2Data(net)
#' }
net2Data=function(net){
    result=net$cluster_res
    names(result)=c("Node", "Cluster", "Betweenness", "Closeness", "PageRank")
    result
}

#'draw degree plot
#'@param net A list as a result of networkPlot
#'@importFrom ggplot2 aes geom_point element_rect element_line
#'@export
#'@examples
#'\dontrun{
#' net=cocPlot(M,"ID",n=50)
#' degreePlot(net)
#' }
degreePlot <- function(net){
    deg <- data.frame(node = names(net$nodeDegree), x= (1:length(net$nodeDegree)), y = net$nodeDegree)
    p <- ggplot(data = deg, aes(x=.data$x, y=.data$y,
                                text=paste("Node ",.data$x," - Degree ",.data$y, sep="")))+
        geom_point()+
        geom_line(aes(group="NA"),color = '#002F80', alpha = .5) +
        theme(text = element_text(color = "#444444")
              ,panel.background = element_rect(fill = '#EFEFEF')
              ,panel.grid.minor = element_line(color = '#FFFFFF')
              ,panel.grid.major = element_line(color = '#FFFFFF')
              ,plot.title = element_text(size = 24)
              ,axis.title = element_text(size = 14, color = '#555555')
              ,axis.title.y = element_text(vjust = 1, angle = 0)
              ,axis.title.x = element_text(hjust = 0)
        ) +
        labs(x = "Node", y="Degree", title = "Node Degrees")
    return(p)
}


#' Remove selected layers of ggplot
#' @param p A ggplot
#' @param choices numeric
#' @param geoms character
#' @param last logical
#' @export
removeGeoms <- function(p, choices=NULL,geoms=NULL, last = TRUE) {
    # Find layers that match the requested type.
    if(!is.null(choices)){
        choices=intersect(choices,1:length(p$layers))
    } else if(!is.null(geoms)){
        choices <- sapply(p$layers,
                          function(j) {
                              class(j$geom)[1] %in% geoms
                          })
    } else if(last) {
        choices <- length(p$layers)
    }

    p$layers[choices] <- NULL
    p
}
