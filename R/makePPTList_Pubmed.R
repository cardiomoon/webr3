#' Make query with keyword
#' @param key character keyword
#' @param author name of author
#' @param start integer start year
#' @param stop integer stop year
#' @export
#' @examples
#' makeQuery(author="Moon KW")
#' makeQuery(key="machine learning")
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

#' Make Powerpoint list for pubmed wordcloud
#' @param key character keyword
#' @param author name of author
#' @param start integer start year
#' @param stop integer stop year
#' @param limit integer maximum number of abstracts
#' @param seed numeric seed number
#' @param max numeric maximum size of the words
#' @param min numeric minimum size of the words
#' @param min.freq words with frequency below min.freq will not be plotted
#' @param rot.per proportion words with 90 degree rotation
#' @param palette character one of c("Accent", "Dark2", "Pastel1", "Pastel2", "Paired", "Set1", "Set2", "Set3")
#' @param no integer number of plots
#' @importFrom glue glue
#' @export
#' @examples
#' result=makePPTList_pubmed(author="Moon KW")
makePPTList_pubmed=function(key="",author="",start=2000,stop=2021,limit=200,seed=1234,
                            max=3,min=0.3,min.freq=2,rot.per=0.35,palette="",no=6){

     title=c("Make Query","Total count","Make Clean Data","Set seed")
     type=c("Pre","Rcode","Rcode","Rcode")
     code=c(glue('query<-makeQuery(key="{key}",author="{author}",start={start},stop={stop})'),
            "pmQueryTotalCount(query)",
            glue('df<-query2clean(query,limit={limit});head(df,10)'),
            glue('set.seed({seed})'))
     for(i in 1:no){
             title=c(title,paste0("plot",i))
             type=c(type,"plot")
     if(palette=="") {
             temp=glue('plotWordCloud(df,scale=c({max},{min}),min.freq={min.freq},rot.per={rot.per})')
     } else{
             temp=glue('plotWordCloud(df,scale=c({max},{min}),min.freq={min.freq},colors=colSets("{palette}"),rot.per={rot.per})')
     }
     code=c(code,temp)
     }
     data.frame(title,type,code)
}

