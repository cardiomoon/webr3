#' Make query with keyword
#' @param key character keyword
#' @param author name of author
#' @param start integer start year
#' @param stop integer stop year
#' @export
#' @examples
#' makeQuery(key="",author="Moon KW")
#' makeQuery(key="machine learning",author="")
makeQuery=function(key="",author="",start=2000,stop=2021){
        #key="stomach ca";author="Moon KW";start=2000;stop=2021
        temp=c()
        if(key!="") temp=c(temp,paste0(key,"*[Title/Abstract]"))
        if(author!="") temp=c(temp,paste0(author,"[Author]"))
        temp=c(temp,paste0(start,":",stop,"[DP]"))
        temp
        paste0(temp,collapse=" AND ")
}


#' Make clean data from query
#' @param query character
#' @param limit integer
#' @importFrom pubmedR pmApiRequest pmApi2df
#' @importFrom PubMedWordcloud cleanAbstracts
#' @export
#' @examples
#' query=makeQuery(key="machine learning")
#' query2clean(query)
query2clean=function(query,limit=200){
    D <- pmApiRequest(query = query, limit = limit, api_key = NULL)
    M=pmApi2df(D,format="raw")
    PubMedWordcloud::cleanAbstracts(M$AB)
}


#' Make Wordcloud
#' @param df A data.frame
#' @param type integer
#' @param seed integer
#' @param ... further arguments
#' @importFrom PubMedWordcloud plotWordCloud
#' @importFrom wordcloud wordcloud
#' @importFrom ggplot2 ggplot aes_string theme_minimal scale_size_area
#' @importFrom ggwordcloud geom_text_wordcloud_area ggwordcloud ggwordcloud2
#' @export
#' @examples
#' query=makeQuery(key="machine learning")
#' df<-query2clean(query)
#' plotWC(df)
plotWC=function(df,type=1,seed=1234,...){

    df$color=factor(sample.int(10, nrow(df), replace = TRUE))
    df2<-df[1:100,]
    pal=c("#1B9E77","#D95F02","#7570B3","#E7298A","#66A61E","#E6AB02","#A6761D","#666666")
    set.seed(seed)
    if(type==1) {
        plotWordCloud(df,...)
    } else if(type==2){
    wordcloud(df2$word,df2$freq,rot.per=0.35,random.order = FALSE,colors=pal,...)
    } else if(type==3){
        ggplot(df2,aes_string(label="word",size="freq"))+
        geom_text_wordcloud_area(eccentricity = 0.35,...)+
        scale_size_area(max_size=10)+
        theme_minimal()
    } else if(type==4){
    ggplot(df2,aes_string(label="word",size="freq",color = "color"))+
        geom_text_wordcloud_area(eccentricity = 0.35,...)+
        scale_size_area(max_size=10)+
        theme_minimal()
    }else if(type==5){
    ggwordcloud(df2$word,df2$freq,rot.per=0.35,random.order = FALSE,colors=pal,...)
    } else {
    ggwordcloud2(df2,shape="circle",...)
    }
}

#' Make Powerpoint list for pubmed wordcloud
#' @param query character query
#' @param seed numeric seed number
#' @importFrom pubmedR pmQueryTotalCount
#' @export
#' @examples
#' query=makeQuery(key="machine learning")
#' result=makePPTList_pubmed(query=query)
makePPTList_pubmed=function(query="",seed=1234){

     title=c("Query","Total count","Make Clean Data","head of data","plot1","plot2","plot3","plot4","plot5","plot6")
     type=c("Rcode","Rcode","out","Rcode","plot","plot","ggplot","ggplot","ggplot","ggplot")
     code=c(paste0("query<-'",query,"';query"),
            "pmQueryTotalCount(query)",
            "out<-query2clean(query,limit=200)","head(out,10)",
            paste0("plotWC(out,type=",1:6,",seed=",seed,")"))
     data.frame(title,type,code)
}



