#'Make riverplot for thematic evolution
#'@param Nodes is a list of nodes obtained by thematicEvolution function.
#'@param Edges is a list of edges obtained by thematicEvolution function.
#'@param measure is a character. It can be measure=("inclusion","stability", "weighted")
#'@param nodewidth width of the node (relative to font size)
#'@param textcex size of text
#'@param ... further arguments to be passed to riverplot
#'@importFrom riverplot makeRiver riverplot
#'@importFrom graphics text
#'@export
#'@examples
#'library(bibliometrix)
#'library(riverplot)
#'data(scientometrics, package = "bibliometrixData")
#'years=c(2000,2012)
#'nexus <- thematicEvolution(scientometrics,field="ID",years=years,n=100,minFreq=2)
#'par(mfrow=c(1,1))
#'riverplotTE(nexus$Nodes,nexus$Edges)
riverplotTE=function(Nodes,Edges,measure="inclusion",nodewidth=10,textcex=1,...){

    # Nodes=nexus$Nodes; Edges=nexus$Edges; measure="inclusion";nodewidth=10;textcex=1
    ID=Nodes$name
    while(any(duplicated(ID))){
        ID[duplicated(ID)]=paste0(ID[duplicated(ID)]," ")
    }
    x=as.numeric(Nodes$slice)
    nodes1=data.frame(ID,x)

    N1=ID[Edges$from+1]
    N2=ID[Edges$to+1]
    if(measure %in% c("inclusion","Inclusion")) {
        measure="Inclusion"
    } else if(measure %in% c("stability","Stability")) {
        measure="Stability"
    } else measure="Inc_Weighted"
    Value=Edges[[measure]]
    edges1=data.frame(N1,N2,Value)
    color1=Nodes$color
    names(color1)=ID
    style <- sapply(ID, function(id)
        list(col=color1[id]), simplify=FALSE)
    r <- makeRiver(nodes=nodes1, edges=edges1,styles=style)
    # plot(r, plot_area=c(1,0.9), nodewidth=nodewidth, srt=0, textcex=textcex,gravity="c")
    riverplot(r, plot_area=c(1,0.9), nodewidth=nodewidth, srt=0, textcex=textcex,gravity="c",...)
    group=unique(Nodes$group)
    no=length(group)
    text(0,1,group[1],pos=4)
    text(1,1,group[no],pos=2)
    if(no>2){
        for(i in 2:(no-1)){
            text((i-1)/(no-1),1,group[i])
        }
    }

}
