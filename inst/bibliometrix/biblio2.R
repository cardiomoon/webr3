require(webr3)
require(pubmedR)
query2metaData=function(query,limit=200){
    D <- myPmApiRequest(query = query, limit = limit, api_key = NULL)
    D

}
query=makeQuery(key="transradial",author="",start=1980,stop=2021)
pmQueryTotalCount(query)
D<-query2metaData(query,limit=2700)
M<-metaData2df(D,addPMC=TRUE)

M=pmApi2df(D,format="raw")
M=M[M$AB!="NA",]
str(M)

require(bibliometrix)
require(tidyverse)
results=biblioAnalysis(M,sep=";")


sep=";"
biblioAnalysis=function (M, sep = ";")
{
    Authors = NULL
    Authors_frac = NULL
    FirstAuthors = NULL
    PY = NULL
    FAffiliation = NULL
    Affiliation = NULL
    Affiliation_frac = NULL
    CO = rep(NA, dim(M)[1])
    TC = NULL
    TCperYear = NULL
    SO = NULL
    Country = NULL
    DE = NULL
    ID = NULL
    MostCitedPapers = NULL
    Tags <- names(M)
    if (!("SR" %in% Tags)) {
        M = metaTagExtraction(M, "SR")
    }
    if ("AU" %in% Tags) {
        listAU = strsplit(as.character(M$AU), sep)
        listAU = lapply(listAU, function(l) trim(l))
        nAU = unlist(lapply(listAU, length))
        fracAU = unlist(lapply(nAU, function(x) {
            rep(1/x, x)
        }))
        AU = unlist(listAU)
        Authors = sort(table(AU), decreasing = TRUE)
        Authors_frac = aggregate(fracAU, by = list(AU), "sum")
        names(Authors_frac) = c("Author", "Frequency")
        Authors_frac = Authors_frac[order(-Authors_frac$Frequency),
        ]
        FirstAuthors = unlist(lapply(listAU, function(l) {
            if (length(l) > 0) {
                l = l[[1]]
            }
            else {
                l = NA
            }
            return(l)
        }))
        AuSingleAuthoredArt = length(unique(FirstAuthors[nAU ==
                                                             1]))
        AuMultiAuthoredArt = length(Authors) - AuSingleAuthoredArt
    }
    if ("TC" %in% Tags) {
        TC = as.numeric(M$TC)
        PY = as.numeric(M$PY)
        CurrentYear = as.numeric(format(Sys.Date(), "%Y"))
        TCperYear = TC/(CurrentYear - PY + 1)
        if (!("DI" %in% names(M)))
            M$DI <- ""
        MostCitedPapers <- data.frame(M$SR, M$DI, TC, TCperYear,
                                      PY) %>% group_by(.data$PY) %>% mutate(NTC = .data$TC/mean(.data$TC)) %>%
            ungroup() %>% select(-.data$PY) %>% arrange(desc(.data$TC)) %>%
            as.data.frame()
        names(MostCitedPapers) = c("Paper         ", "DOI", "TC",
                                   "TCperYear", "NTC")
    }
    nReferences <- 0
    if ("CR" %in% Tags) {
        CR <- tableTag(M, "CR", sep)
        nReferences <- length(CR)
    }
    if ("ID" %in% Tags) {
        ID <- tableTag(M, "ID", sep)
    }
    if ("DE" %in% Tags) {
        DE = tableTag(M, "DE", sep)
    }
    if ("SO" %in% Tags) {
        SO = gsub(",", "", M$SO, fixed = TRUE)
        SO = sort(table(SO), decreasing = TRUE)
    }
    if (("C1" %in% Tags) & (sum(!is.na(M$C1)) > 0)) {
        if (!("AU_UN" %in% Tags)) {
            M = metaTagExtraction(M, Field = "AU_UN")
        }
        AFF = M$AU_UN
        listAFF = strsplit(AFF, sep, fixed = TRUE)
        nAFF = unlist(lapply(listAFF, length))
        listAFF[nAFF == 0] = "NA"
        fracAFF = unlist(sapply(nAFF, function(x) {
            if (x > 0) {
                x = rep(1/x, x)
            }
            else {
                x = 0
            }
        }))
        AFF = trim.leading(unlist(listAFF))
        Affiliation = sort(table(AFF), decreasing = TRUE)
        Affiliation_frac = aggregate(fracAFF, by = list(AFF),
                                     "sum")
        names(Affiliation_frac) = c("Affiliation", "Frequency")
        Affiliation_frac = Affiliation_frac[order(-Affiliation_frac$Frequency),
        ]
        FAffiliation = lapply(listAFF, function(l) l[1])
        data("countries", envir = environment())
        countries = as.character(countries[[1]])
        if (!("AU1_CO" %in% names(M))) {
            M = metaTagExtraction(M, Field = "AU1_CO", sep)
        }
        CO = M$AU1_CO
        Country = tableTag(M, "AU1_CO")
        SCP_MCP = countryCollaboration(M, Country, k = length(Country),
                                       sep)
    } else {
        M$AU1_CO = NA
        SCP_MCP = data.frame(Country = rep(NA, 1), SCP = rep(NA,
                                                             1))
    }
    if ("DT" %in% names(M)) {
        Documents = table(M$DT)
        n = max(nchar(names(Documents)))
        names(Documents) = substr(paste(names(Documents), "                                              ",
                                        sep = ""), 1, n + 5)
    } else {
        Documents = NA
    }
    results = list(Articles = dim(M)[1], Authors = Authors, AuthorsFrac = Authors_frac,
                   FirstAuthors = FirstAuthors, nAUperPaper = nAU, Appearances = sum(nAU),
                   nAuthors = dim(Authors), AuMultiAuthoredArt = AuMultiAuthoredArt,
                   AuSingleAuthoredArt = AuSingleAuthoredArt, MostCitedPapers = MostCitedPapers,
                   Years = PY, FirstAffiliation = unlist(FAffiliation),
                   Affiliations = Affiliation, Aff_frac = Affiliation_frac,
                   CO = CO, Countries = Country, CountryCollaboration = SCP_MCP,
                   TotalCitation = TC, TCperYear = TCperYear, Sources = SO,
                   DE = DE, ID = ID, Documents = Documents, nReferences = nReferences,
                   DB = M$DB[1])
    class(results) <- "bibliometrix"
    return(results)
}

k = length(Country)
countryCollaboration=function (M, Country, k, sep)
{
    if (!("AU_CO" %in% names(M))) {
        M = metaTagExtraction(M, Field = "AU_CO", sep)
    }
    M$SCP = 0
    M$SCP_CO = NA
    for (i in 1:dim(M)[1]) {
        if (!is.na(M$AU_CO[i])) {
            co = M$AU_CO[i]
            co = table(unlist(strsplit(co, ";")))
            if (length(co) == 1) {
                M$SCP[i] = 1
            }
            M$SCP_CO[i] = M$AU1_CO[i]
        }
        else {
            M$SCP[i] = NA
        }
    }
    CO = names(Country)[1:k]
    df = data.frame(Country = rep(NA, k), SCP = rep(0, k))
    for (i in 1:length(CO)) {
        co = CO[i]
        df$Country[i] = co
        df$SCP[i] = sum(M$SCP[M$SCP_CO == co], na.rm = T)
    }
    df$MCP = as.numeric(tableTag(M, "AU1_CO")[1:k]) - df$SCP
    return(df)
}

Field = "AU_CO"; sep = ";"; aff.disamb = TRUE

metaTagExtraction=function (M, Field = "CR_AU", sep = ";", aff.disamb = TRUE)
{
    if ("CR" %in% names(M)) {
        M$CR = gsub("DOI;", "DOI ", as.character(M$CR))
    }
    if (Field == "SR") {
        M <- SR(M)
    }
    if (Field == "CR_AU") {
        M <- CR_AU(M, sep)
    }
    if (Field == "CR_SO") {
        M <- CR_SO(M, sep)
    }
    if (Field == "AU_CO") {
        M <- AU_CO(M)
    }
    if (Field == "AU1_CO") {
        M <- AU1_CO(M, sep)
    }
    if (Field == "AU_UN") {
        if (isTRUE(aff.disamb)) {
            M <- AU_UN(M, sep)
        }
        else {
            M$AU_UN = gsub("\\[.*?\\] ", "", M$C1)
            M$AU1_UN = unlist(lapply(strsplit(M$RP, sep), function(l) {
                l = l[1]
                return(l)
            }))
            ind = regexpr("\\),", M$AU1_UN)
            a = which(ind > -1)
            M$AU1_UN[a] = trim(substr(M$AU1_UN[a], ind[a] + 2,
                                      nchar(M$AU1_UN[a])))
        }
    }
    return(M)
}

AU_CO=function (M)
{
    size = dim(M)[1]
    data("countries", envir = environment())
    countries = as.character(countries[[1]])
    if (M$DB[1] %in% c("ISI", "PUBMED")) {
        countries = as.character(sapply(countries, function(s) paste0(s,
                                                                      ".", collapse = "")))
    } else if (M$DB[1] == "SCOPUS") {
        countries = as.character(sapply(countries, function(s) paste0(s,
                                                                      ";", collapse = "")))
    }
    M$AU_CO = NA
    C1 = M$C1
    C1[which(is.na(C1))] = M$RP[which(is.na(C1))]
    C1 = unlist(lapply(C1, function(l) {
        l = unlist(strsplit(l, ";"))
        l = paste0(l, collapse = ";")
    }))
    C1 = gsub("\\[.*?\\] ", "", C1)
    C1[which(C1 == "NA")] = NA
    if (M$DB[1] == "ISI") {
        C1 = lastChar(C1, last = ".")
    }
    if (M$DB[1] == "SCOPUS") {
        C1 = lastChar(C1, last = ";")
    }
    RP = M$RP
    RP = paste(RP, ";", sep = "")
    # i=2
    for (i in 1:size[1]) {
        # cat("i=",i,"\n")
        if (!is.na(C1[i])) {
            ind = unlist(sapply(countries, function(l) (gregexpr(l,
                                                                 C1[i], fixed = TRUE))))
            if (sum(ind > -1,na.rm=TRUE) > 0) {
                M$AU_CO[i] = paste(unique(names(ind[ind > -1])),
                                   collapse = ";")
            }
        }
        if (is.na(M$AU_CO[i])) {
            ind = unlist(sapply(countries, function(l) (gregexpr(l,
                                                                 RP[i], fixed = TRUE))))

            if (sum(ind > -1,na.rm=TRUE) > 0) {
                M$AU_CO[i] = paste(unique(names(ind[ind > -1])),
                                   collapse = ";")
            }
        }
    }
    M$AU_CO = gsub("[[:digit:]]", "", M$AU_CO)
    M$AU_CO = gsub(".", "", M$AU_CO, fixed = TRUE)
    M$AU_CO = gsub(";;", ";", M$AU_CO, fixed = TRUE)
    M$AU_CO = gsub("UNITED STATES", "USA", M$AU_CO)
    M$AU_CO = gsub("TAIWAN", "CHINA", M$AU_CO)
    M$AU_CO = gsub("ENGLAND", "UNITED KINGDOM", M$AU_CO)
    M$AU_CO = gsub("SCOTLAND", "UNITED KINGDOM", M$AU_CO)
    M$AU_CO = gsub("WALES", "UNITED KINGDOM", M$AU_CO)
    M$AU_CO = gsub("NORTH IRELAND", "UNITED KINGDOM", M$AU_CO)
    if (M$DB[1] == "ISI") {
        M$AU_CO = removeLastChar(M$AU_CO, last = ".")
    }
    if (M$DB[1] == "SCOPUS") {
        M$AU_CO = removeLastChar(M$AU_CO, last = ";")
    }
    return(M)
}

S<-summary(results,k=10)
plotlist<-plot(results,k=10)
length(plotlist)
plotlist[[1]]
plotlist[[2]]
plotlist[[3]]
plotlist[[4]]
plotlist[[5]]

## Analysis of Cited References

CR <- citations(M, field = "article", sep = ";")
cbind(CR$Cited[1:10]) # Top-10 most cited references

localCR <- localCitations(M, results, sep = ";")
localCR$Authors[1:10,]  # Local citation analysis: Top-10 most cited authors
localCR$Papers[1:10,]

## Authors’ Dominance ranking

DF <- dominance(results, k = 10)
DF
authors = "KOSTOFF RN"
indices=Hindex(M, field="author",elements =authors, sep = ";",years=50)
indices$H
indices$CitationList

## Top-Authors’ Productivity over the Time

topAU <- authorProdOverTime(M, k = 10, graph = TRUE)
head(topAU$dfAU)
topAU$graph

head(topAU$dfAU)

## Lotka's Law Coefficient estimation

L <- lotka(results)
L$AuthorProd
L$Beta
L$C
L$R2
L$p.value
# Observed distribution
Observed=L$AuthorProd[,3]

# Theoretical distribution with Beta = 2
Theoretical=10^(log10(L$C)-2*log10(L$AuthorProd[,1]))

plot(L$AuthorProd[,1],Theoretical,type="l",col="red",ylim=c(0, 1), xlab="Articles",ylab="Freq. of Authors",main="Scientific Productivity")
lines(L$AuthorProd[,1],Observed,col="blue")
legend(x="topright",c("Theoretical (B=2)","Observed"),col=c("red","blue"),lty = c(1,1,1),cex=0.6,bty="n")

Articles=L$AuthorProd[,1]
Theoretical=10^(log10(L$C)-2*log10(L$AuthorProd[,1]))
Observed=L$AuthorProd[,3]

df=data.frame(Articles,Theoretical,Observed)
require(tidyr)
df1<-df %>% pivot_longer(cols=2:3)
require(ggplot2)
ggplot(df1,aes(x=Articles,y=value,color=name))+geom_line()+theme_bw()+
    theme(legend.position=c(0.7,0.9),legend.title=element_blank())+
    labs(y="Freqency of Authors",title="Scientific Productivity")


# Author Productivity. Empirical Distribution
L$AuthorProd

## bibliographic coupling

A <- cocMatrix(M, Field = "DE", sep = ";")

NetMatrix <- biblioNetwork (M, analysis = "coupling", network = "references", sep = ";")
networkPlot(NetMatrix, n = 30, type = "kamada", Title = "References",labelsize=0.7)
str(net)
net

## collaboration analysis

NetMatrix <- biblioNetwork(M, analysis = "collaboration",
                           network = "authors", sep = ";")
net <- networkPlot(NetMatrix, n = 30, type = "kamada", Title = "Collaboration",labelsize=0.7)

## Co-citation analysis

NetMatrix <- biblioNetwork(M, analysis = "co-citation",
                           network = "references", sep = ";")
net <- networkPlot(NetMatrix, n = 30, type = "kamada", Title = "Co-Citation",labelsize=0.5)

## Co-word Analysis

NetMatrix <- biblioNetwork(M, analysis = "co-occurrences", network = "keywords", sep = ";")
net <- networkPlot(NetMatrix, n = 30, type = "kamada", Title = "Co-word analysis",labelsize=0.5)



## Conceptual Map and keyword clusters
M <- termExtraction(M, Field = "TI", stemming=TRUE, language="english", verbose=TRUE)
CS <- conceptualStructure(M, field="ID", minDegree=5, k.max=5, stemming=FALSE)
length(CS)
CS[[1]]
CS[[2]]
CS[[3]]
CS[[4]] # Conceptual Structure Map - method: MCA
CS[[5]] # Topic dendrogram
CS[[6]] # factorial map of the documents with highest contributes
CS[[7]] # factorial map of the most cited documents
CS[[8]]

## Normalization

S <- normalizeSimilarity(NetMatrix, type = "association")
S <- normalizeSimilarity(NetMatrix, type = "inclusion")
S <- normalizeSimilarity(NetMatrix, type = "jaccard")
S <- normalizeSimilarity(NetMatrix, type = "salton")

## Network Mapping


COC <- biblioNetwork(M, analysis = "co-occurrences",network = "keywords", sep = ";")
networkPlot(COC, n=40, size=TRUE, remove.multiple = T, Title="Term co-occurrences", type="kamada", labelsize=0.5)

## Country collaboration network

M <- metaTagExtraction(M, Field = "AU_CO")
CC <- biblioNetwork(M, analysis = "collaboration", network = "countries", sep = ";")
networkPlot(CC, n=44, size=TRUE, remove.multiple = FALSE, Title="Country Collaboration", type="sphere")

## Historiographic Analysis
histResults <- histNetwork(M, n = 20, sep = ";")
histPlot(histResults, size=FALSE)

