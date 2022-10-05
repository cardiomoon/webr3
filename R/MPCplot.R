#' Make mpost productive countries plot
#'
#' @param sumResult A list as a result of summary of biblioAnalysis()
#' @param n integer Number of conuntries to plot
#' @param min minimum number of publication to label
#' @param legend.position character specifying legend position
#' @param title character title of plot
#' @return A ggplot
#' @export
#' @examples
#' data(scientometrics, package = "bibliometrixData")
#' results<-biblioAnalysis(scientometrics)
#' sumResult<-summary(results,k=20)
#' MPCplot(sumResult,n=10)
#' MPCplot(sumResult)
#' MPCplot(sumResult,n=30,min=10)
MPCplot=function(sumResult,n=20,min=2,legend.position="bottom",title="Most Productive Countries"){

    df1=sumResult$MostProdCountries[,c(1,4,5)]
    df1$order=1:nrow(df1)
    df1=df1[1:min(n,nrow(df1)),]
    df2<-df1 %>% pivot_longer(cols=2:3,names_to="Collaboration",values_to="No")
    df2$No=as.numeric(df2$No)
    df2$Country=as.character(df2$Country)
    df2$label=ifelse(df2$No<min,"",df2$No)

    ggplot(df2,aes(x=No,y=reorder(Country,-order),fill=Collaboration))+
        geom_col()+
        theme_bw()+
        geom_text(aes(label=label),position=position_stack(vjust=0.5))+
        labs(title=title,y="Countries",x="No. of Documents")+
        theme(legend.position=legend.position)

}
