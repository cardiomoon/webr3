#' Get word list
#' @param M is a bibliographic data frame obtained by the converting function convert2df.
#' @param Field character
#' @param n numeric
#' @param measure character
#' @param ngrams numeric
#' @export
wordlist <- function(M, Field="ID", n=50, measure="freq", ngrams=1){
    switch(Field,
           ID={v=tableTag(M,"ID")},
           DE={v=tableTag(M,"DE")},
           TI={
               if (!("TI_TM" %in% names(M))){
                   v=tableTag(M,"TI", ngrams=ngrams)
               }},
           AB={if (!("AB_TM" %in% names(M))){
               v=tableTag(M,"AB", ngrams=ngrams)
           }}
    )
    names(v)=tolower(names(v))
    #v=tableTag(values$M,"ID")
    n=min(c(n,length(v)))
    Words=data.frame(Terms=names(v)[1:n], Frequency=(as.numeric(v)[1:n]), stringsAsFactors = FALSE)
    W=Words
    switch(measure,
           identity={},
           sqrt={W$Frequency=sqrt(W$Frequency)},
           log={W$Frequency=log(W$Frequency+1)},
           log10={W$Frequency=log10(W$Frequency+1)}
    )

    results=list(v=v,W=W, Words=Words)
    return(results)
}

#' Make word cloud
#' @param M is a bibliographic data frame obtained by the converting function convert2df.
#' @param type integer
#' @export
plotWC1=function(M,type=1){
    df=wordlist(M)$W
    names(df)=c("word","freq")
    plotWC(df,type=type)
}

#' tree map plot
#' @param M is a bibliographic data frame obtained by the converting function convert2df.
#' @importFrom treemapify geom_treemap geom_treemap_text
#' @export
treeMap=function(M){
    wcdf=wordlist(M)$W
    wcdf$label=paste0(wcdf$Terms,"\n",wcdf$Frequency,"\n",round(wcdf$Frequency*100/sum(wcdf$Frequency)),"%")
    ggplot(wcdf,aes_string(area="Frequency",fill="Terms",label="label"))+
        geom_treemap(colour="white",size=1)+
        geom_treemap_text(colour="white",place="centre")+
        guides(fill=FALSE)
}

