#' Get pmc id with pmids
#' @param pmids String A character vector of pmids
#' @importFrom httr GET content
#' @export
#' @examples
#' pmids=c("10848627","111111111","10848628")
#' df=pmid2pmc(pmids)
pmid2pmc=function(pmids){

    service_root="https://www.ncbi.nlm.nih.gov/pmc/utils/idconv/v1.0/"
    pmidAddress="https://pubmed.ncbi.nlm.nih.gov/"
    PMCAddress="https://www.ncbi.nlm.nih.gov/pmc/articles/"

    # df=PMCids[match(pmids,PMCids$PMID),]
    # df$PDF=NA
    # df$PDF[!is.na(df$PMCID)]=paste0('<a href="',PMCAddress,df$PMCID[!is.na(df$PMCID)],'/pdf" target="_blank">PDF</a>')
    # df$PMID=paste0('<a href="',pmidAddress,pmids,'" target="_blank">',pmids,"</a>")
    # df$PMCID[!is.na(df$PMCID)]=paste0('<a href="',PMCAddress,df$PMCID[!is.na(df$PMCID)],'" target="_blank">',df$PMCID[!is.na(df$PMCID)],"</a>")
    # df

    no=length(pmids)
    count=(no %/%200)+1
    count
    if (shiny::isRunning()) {
        progress <- shiny::Progress$new()
        on.exit(progress$close())
        progress$set(message = "Add PMCID ", value = 0)
    }
    for(i in 1:count){
        start=(i-1)*200+1
        end=min(i*200,no)
        if (isRunning()) {
            progress$inc(200/no, detail = paste("Doing part",
                                                end, "/", no))
        } else{
            cat(end,"of",no,"\n")
        }
        pmid=paste0(pmids[start:end],collapse=",")
        uri=paste0(service_root,"?ids=",pmid,"&format=csv")
        response <- httr::GET(uri)
        temp=data.frame(httr::content(response))
        temp
        temp=temp[match(pmids[start:end],temp$PMID),]

        if(i==1){
            df=temp
        } else{
            df=rbind(df,temp)
        }
    }
    df=df[1:3]
    df$PDF=NA
    df$PDF[!is.na(df$PMCID)]=paste0('<a href="',PMCAddress,df$PMCID[!is.na(df$PMCID)],'/pdf" target="_blank">PDF</a>')
    df$PMID=paste0('<a href="',pmidAddress,df$PMID,'" target="_blank">',df$PMID,"</a>")
    # df$PMID=paste0('<a href="',pmidAddress,pmids,'" target="_blank">',pmids,"</a>")
    df$PMCID[!is.na(df$PMCID)]=paste0('<a href="',PMCAddress,df$PMCID[!is.na(df$PMCID)],'" target="_blank">',df$PMCID[!is.na(df$PMCID)],"</a>")
    df
}


#' Add PMCID to a data.frame with PMID
#' @param M A data.frame with a column 'PMID'
#' @export
addPMC=function(M){
    pmids=M$PMID
    result=pmid2pmc(pmids)
    no=grep("PMID",names(M))
    cbind(M[-no],result)
}
