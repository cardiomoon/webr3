#' Gather bibliographic content from PubMed database using NCBI entrez APIs
#'
#' It gathers metadata about publications from the NCBI PubMed database. The use of NCBI PubMed APIs is entirely free, and doesn't necessary require an API key. The function pmApiRequest queries NCBI PubMed using an entrez query formulated through the function pmQueryBuild.
#' @param query is a character. It contains a search query formulated using the Entrez query language.
#' @param limit numeric. It indicates the max number of records to download.
#' @param api_key character. It contains a valid api key API keys for the NCBI E-utilities.
#' @importFrom rentrez entrez_fetch
#' @importFrom XML xmlToList
#' @importFrom shiny Progress isRunning
#' @export
myPmApiRequest=function (query, limit, api_key = NULL)
{
    res <- pmQueryTotalCount(query = query, api_key = api_key)
    n <- min(res$total_count, limit)
    step <- 200
    step <- min(limit, step)
    metadata <- list()
    stop <- FALSE
    s <- 1
    if (shiny::isRunning()) {
        progress <- shiny::Progress$new()
        on.exit(progress$close())
        progress$set(message = "Get File", value = 0)
    }
    while (!isTRUE(stop)) {
        if (isRunning()) {
            progress$inc(200/n, detail = paste("Doing part",
                                                            s+step-1, "/", n))
        } else{
        cat("Documents ", s + step - 1, " of ", n, "\n")
        }
        multi_summs <- entrez_fetch(db = "pubmed", web_history = res$web_history,
                                    retstart = s, retmax = step, rettype = "xml", parsed = T,
                                    api_key = api_key)
        multi_summs <- xmlToList(multi_summs, simplify = F)
        metadata <- c(metadata, multi_summs)
        if (n <= (s + step)) {
            stop <- TRUE
        }
        else {
            s <- s + step
            if ((s + step) > limit) {
                step <- (n - s + 1)
            }
        }
    }
    P <- list(data = metadata, query = query, query_translation = res$query_translation,
              records_downloaded = n, total_count = res$total_count)
    return(P)
}
