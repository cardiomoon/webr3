#' Get pmc id with pmids
#' @param pmids String A character vector of pmids
#' @export
#' @examples
#' pmids=c("10848627","111111111","10848628")
#' df=pmid2pmc(pmids)
pmid2pmc=function(pmids){

    pmidAddress="https://pubmed.ncbi.nlm.nih.gov/"
    PMCAddress="https://www.ncbi.nlm.nih.gov/pmc/articles/"

    df=PMCids[match(pmids,PMCids$PMID),]
    df$PDF=NA
    df$PDF[!is.na(df$PMCID)]=paste0('<a href="',PMCAddress,df$PMCID[!is.na(df$PMCID)],'/pdf" target="_blank">PDF</a>')
    df$PMID=paste0('<a href="',pmidAddress,pmids,'" target="_blank">',pmids,"</a>")
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
