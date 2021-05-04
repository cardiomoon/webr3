#' Make query with keyword
#' @param key character keyword
#' @param author name of author
#' @param start integer start year
#' @param stop integer stop year
#' @export
#' @examples
#' query=makeQuery(key="",author="Moon KW")
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


#' retrieve pubmed data from query
#' @param query character
#' @param limit integer
#' @export
#' @examples
#' query=makeQuery(key="propensity score")
#' D <-query2metaData(query)
query2metaData=function(query,limit=200){
    D <- myPmApiRequest(query = query, limit = limit, api_key = NULL)
    D

}


#' Convert xml PubMed bibliographic data into a dataframe
#' @param D is a list following the xml PubMed structure, downloaded using the function pmApiRequest.
#' @param removeNoAbstract logical whether or not remove data with no abstract
#' @param addPMC logical wheter or not add PMCID
#' @importFrom pubmedR pmApi2df
#' @export
#' @examples
#' query=makeQuery(key="machine learning")
#' D <-query2metaData(query)
#' M <- metaData2df(D)
metaData2df=function(D,removeNoAbstract=TRUE,addPMC=FALSE){
    M=pmApi2df(D,format="raw")
    if(removeNoAbstract) M=M[M$AB!="NA",]
    if(addPMC) M<-addPMC(M)
    M
}

#' clean data
#' @param M A data.frame
#' @importFrom PubMedWordcloud cleanAbstracts
#' @export
df2clean=function(M){
    PubMedWordcloud::cleanAbstracts(M$AB)
}

#' retrieve pubmed data from query and make clean data
#' @param query character
#' @export
#' @examples
#' query=makeQuery(key="propensity score")
#' query2clean(query)
query2clean=function(query){
    D <-query2metaData(query)
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
#' D<-query2metaData(query)
#' M<-metaData2df(D)
#' df=df2clean(M)
#' plotWC(df)
plotWC=function(df,type=1,seed=1234,...){

    df$color=factor(sample.int(10, nrow(df), replace = TRUE))
    df2<-df[1:min(nrow(df),100),]
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

     title=c("Query","Get Query count","Query count","Get data from pubmed","convert metaData to df","Make Clean Data","head of data","plot1","plot2","plot3","plot4")
     type=c("Rcode","out","Rcode","out","out","out","Rcode","plot","plot","ggplot","ggplot")
     code=c(paste0("query<-'",query,"'"),
            "queryCount<-pmQueryTotalCount(query)",
            "queryCount",
            "D<-query2metaData(query)","M<-metaData2df(D)","queryResult<-df2clean(M)","head(queryResult,10)",
            paste0("plotWC(queryResult,type=",c(1,2,5,6),",seed=",seed,")"))
     data.frame(title,type,code)
}



