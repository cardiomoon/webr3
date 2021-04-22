file="savedrecs(1).txt"
M1<-convert2df(file,dbsource="wos",format="plaintext")
dim(M1)
dim(M2)
dim(M3)
dim(M4)
dim(M5)
dim(M6)
file="savedrecs (2).txt"
M2<-convert2df(file,dbsource="wos",format="plaintext")
file="savedrecs (3).txt"
M3<-convert2df(file,dbsource="wos",format="plaintext")
file="savedrecs (4).txt"
M4<-convert2df(file,dbsource="wos",format="plaintext")
file="savedrecs (5).txt"
M5<-convert2df(file,dbsource="wos",format="plaintext")
file="savedrecs (6).txt"
M6<-convert2df(file,dbsource="wos",format="plaintext")

temp=setdiff(names(M3),names(M4))
temp
for(i in seq_along(temp)){
    M4[[temp[i]]]<-NA
}
setdiff(names(M4),names(M3))
M<-rbind(M1,M2,M3,M4,M5,M6)
str(M)
saveRDS(M,"M.RDS")
results=biblioAnalysis(M,sep=";")
summary(results,k=20)
plotlist=plot(results,k=20)
length(plotlist)
CR <- citations(M, field = "article", sep = ";")
cbind(CR$Cited[1:10])
CR <- citations(M, field = "author", sep = ";")
cbind(CR$Cited[1:10])
CR <- localCitations(M, sep = ";")
CR$Authors[1:10,]
CR$Papers[1:10,]
DF <- dominance(results, k = 20)
DF
indices=Hindex(M, field = "author", elements="CHA KS", sep = ";", years = 10)
indices$H
# Bornmann's citations
indices$CitationList

authors=gsub(","," ",names(results$Authors)[1:20])
indices <- Hindex(M, field = "author", elements=authors, sep = ";", years = 50)
indices$H

topAU <- authorProdOverTime(M, k = 20, graph = TRUE)
head(topAU$dfAU)
L <- lotka(results)
# Author Productivity. Empirical Distribution
L$AuthorProd
L$Beta
L$C
L$R2
L$p.value
Observed=L$AuthorProd[,3]

# Theoretical distribution with Beta = 2
Theoretical=10^(log10(L$C)-2*log10(L$AuthorProd[,1]))

plot(L$AuthorProd[,1],Theoretical,type="l",col="red",ylim=c(0, 1), xlab="Articles",ylab="Freq. of Authors",main="Scientific Productivity")
lines(L$AuthorProd[,1],Observed,col="blue")
legend(x="topright",c("Theoretical (B=2)","Observed"),col=c("red","blue"),lty = c(1,1,1),cex=0.6,bty="n")
A <- cocMatrix(M, Field = "SO", sep = ";")
sort(Matrix::colSums(A), decreasing = TRUE)[1:5]
A <- cocMatrix(M, Field = "CR", sep = ".  ")
A <- cocMatrix(M, Field = "AU", sep = ";")
M <- metaTagExtraction(M, Field = "AU_CO", sep = ";")
A <- cocMatrix(M, Field = "DE", sep = ";")
A <- cocMatrix(M, Field = "ID", sep = ";")
NetMatrix <- biblioNetwork(M, analysis = "coupling", network = "references", sep = ".  ")
net <- networkPlot(NetMatrix, n = 30, type = "kamada", Title = "Authors' Coupling",labelsize=0.5)

## collaboration analysis

NetMatrix <- biblioNetwork(M, analysis = "collaboration",
                           network = "authors", sep = ";")
net <- networkPlot(NetMatrix, n = 30, type = "kamada", Title = "Collaboration",labelsize=0.7)

## collaboration-country analysis
NetMatrix <- biblioNetwork(M, analysis = "collaboration", network = "countries", sep = ";")
net <- networkPlot(NetMatrix, n = 30, type = "kamada", Title = "Collaboration-Country",labelsize=0.7)

## Co-citation analysis

NetMatrix <- biblioNetwork(M, analysis = "co-citation",network = "references", sep = ";")
net <- networkPlot(NetMatrix, n = 30, type = "kamada", Title = "Co-Citation",labelsize=0.5)

## Co-word Analysis

NetMatrix <- biblioNetwork(M, analysis = "co-occurrences", network = "keywords", sep = ";")
net <- networkPlot(NetMatrix, n = 30, type = "kamada", Title = "Co-word analysis",labelsize=0.7)

# An example of a classical keyword co-occurrences network

NetMatrix <- biblioNetwork(M, analysis = "co-occurrences", network = "keywords", sep = ";")
netstat <- networkStat(NetMatrix)
names(netstat$network)
summary(netstat, k=10)

# Create a country collaboration network

M <- metaTagExtraction(M, Field = "AU_CO", sep = ";")
NetMatrix <- biblioNetwork(M, analysis = "collaboration", network = "countries", sep = ";")

# Plot the network
net=networkPlot(NetMatrix, n = dim(NetMatrix)[1], Title = "Country Collaboration", type = "circle", size=TRUE, remove.multiple=FALSE,labelsize=0.7,cluster="none")
net=networkPlot(NetMatrix, n = 30, Title = "Country Collaboration", type = "circle", size=TRUE, remove.multiple=FALSE,labelsize=0.7,cluster="none")

# Create a co-citation network

NetMatrix <- biblioNetwork(M, analysis = "co-citation", network = "references", sep = ";")

# Plot the network
net=networkPlot(NetMatrix, n = 30, Title = "Co-Citation Network", type = "fruchterman", size=T, remove.multiple=FALSE, labelsize=1,edgesize = 5)


# Create keyword co-occurrences network

NetMatrix <- biblioNetwork(M, analysis = "co-occurrences", network = "keywords", sep = ";")

# Plot the network
net=networkPlot(NetMatrix, normalize="association", weighted=T, n = 30, Title = "Keyword Co-occurrences", type = "fruchterman", size=T,edgesize = 5,labelsize=0.7)

## Co-Word Analysis: The conceptual structure of a field

CS <- conceptualStructure(M,field="ID", method="CA", minDegree=4, clust=5, stemming=FALSE, labelsize=10, documents=10)

## Historical Direct Citation Network
options(width=130)
histResults <- histNetwork(M, min.citations = 1, sep = ";")


# Plot a historical co-citation network
net <- histPlot(histResults, n=30, size = 5, labelsize=3)


## World map
WMmap=countrycollaboration(M,label=FALSE,edgesize=2.5,min.edges=2)
plot(WMmap$g)
Label=FALSE;edgesize=2.5;min.edges=2
countrycollaboration <- function(M,label,edgesize=2.5,min.edges=2){
    M=metaTagExtraction(M,"AU_CO")
    net=biblioNetwork(M,analysis="collaboration",network="countries")
    CO=data.frame(Tab=rownames(net),Freq=diag(net),stringsAsFactors = FALSE)
    bsk.network=igraph::graph_from_adjacency_matrix(net,mode="undirected")
    COedges=as.data.frame(igraph::ends(bsk.network,igraph::E(bsk.network),names=TRUE),stringsAsFactors = FALSE)

    map.world <- map_data("world")
    map.world$region=toupper(map.world$region)
    map.world$region=gsub("UK","UNITED KINGDOM",map.world$region)
    map.world$region=gsub("SOUTH KOREA","KOREA",map.world$region)

    country.prod <- dplyr::left_join( map.world, CO, by = c('region' = 'Tab'))

    breaks=as.numeric(round(quantile(CO$Freq,c(0.2,0.4,0.6,0.8,1))))
    names(breaks)=breaks
    breaks=log(breaks)
    data("countries",envir=environment())
    names(countries)[1]="Tab"

    COedges=dplyr::inner_join(COedges,countries, by=c('V1'='Tab'))
    COedges=dplyr::inner_join(COedges,countries, by=c('V2'='Tab'))
    COedges=COedges[COedges$V1!=COedges$V2,]
    COedges=count.duplicates(COedges)
    tab=COedges
    COedges=COedges[COedges$count>=min.edges,]

    g=ggplot(country.prod, aes( x = .data$long, y = .data$lat, group = .data$group )) +
        geom_polygon(aes(fill = log(Freq))) +
        scale_fill_continuous(low='dodgerblue', high='dodgerblue4',breaks=breaks)+
        #guides(fill = guide_legend(reverse = T)) +
        guides(colour=FALSE, fill=FALSE)+
        geom_curve(data=COedges, aes(x = .data$Longitude.x , y = .data$Latitude.x, xend = .data$Longitude.y, yend = .data$Latitude.y,     # draw edges as arcs
                                     color = "firebrick4", size = .data$count, group=.data$continent.x),
                   curvature = 0.33,
                   alpha = 0.5) +
        labs(title = "Country Collaboration Map", x = "Latitude", y = "Longitude")+
        scale_size_continuous(guide = FALSE, range = c(0.25, edgesize))+
        theme(text = element_text(color = '#333333')
              ,plot.title = element_text(size = 28)
              ,plot.subtitle = element_text(size = 14)
              ,axis.ticks = element_blank()
              ,axis.text = element_blank()
              ,panel.grid = element_blank()
              ,panel.background = element_rect(fill = '#FFFFFF')  #'#333333'
              ,plot.background = element_rect(fill = '#FFFFFF')
              ,legend.position = c(.18,.36)
              ,legend.background = element_blank()
              ,legend.key = element_blank()
        )
    if (isTRUE(label)){
        CO=dplyr::inner_join(CO,countries, by=c('Tab'='Tab'))
        g=g+
            ggrepel::geom_text_repel(data=CO, aes(x = .data$Longitude, y = .data$Latitude, label = .data$Tab, group=.data$continent),             # draw text labels
                                     hjust = 0, nudge_x = 1, nudge_y = 4,
                                     size = 3, color = "orange", fontface = "bold")
    }

    results=list(g=g,tab=tab)
    return(results)
}
