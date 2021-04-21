# https://www.bibliometrix.org/vignettes/Introduction_to_bibliometrix.html
#  http://www.cultureplex.ca/wp-content/uploads/2018/01/1-s2.0-S1751157717300500-main.pdf
# https://github.com/massimoaria/bibliometrix

library(bibliometrix)

file <- "https://www.bibliometrix.org/datasets/savedrecs.bib"

M <- convert2df(file = file, dbsource = "isi", format = "bibtex")

results=biblioAnalysis(M,sep=";")
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
net <- networkPlot(NetMatrix, n = 30, type = "kamada", Title = "References",labelsize=0.5)
str(net)
net

## collaboration analysis

NetMatrix <- biblioNetwork(M, analysis = "collaboration",
                           network = "authors", sep = ";")
net <- networkPlot(NetMatrix, n = 30, type = "kamada", Title = "Collaboration",labelsize=0.5)

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




title=c("","","plot","network plot")
type=c("out","out","ggplot","plot")
code=c("M <- convert2df(file = file, dbsource = 'isi', format = 'bibtex')",
       "COC <- biblioNetwork(M, analysis = 'co-occurrences',network = 'keywords', sep = ';')",
       "topAU <- authorProdOverTime(M, k = 10, graph = FALSE);topAU$graph",
       "networkPlot(COC, n=40, size=TRUE, remove.multiple = T, Title='Term co-occurrences', type='kamada', labelsize=0.5)"
       )
result=data.frame(title,type,code)
out1=list(M=M,COC=COC)
names(out1)
str(out1)
webrSub::showList(result,out1=reactive(out1))
