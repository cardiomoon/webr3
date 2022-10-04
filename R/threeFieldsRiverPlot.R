#' Draw three fields river plot
#' @param res result of bibliometrix::threeFieldsPlot
#' @param nodewidth numeric The width of node
#' @param textcex numeric size of node label
#' @param ... further arguments to be passed to riverplot
#' @importFrom riverplot riverplot makeRiver
#' @export
#' @examples
#' library(bibliometrix)
#' library(riverplot)
#' data(scientometrics, package = "bibliometrixData")
#' res=threeFieldsPlot(scientometrics, fields=c("DE","AU","CR"),n=c(10,10,10))
#' threeFieldsRiverPlot(res,nodewidth=7)
threeFieldsRiverPlot=function(res,nodewidth=3,textcex=0.7,...){
    Nodes=res$x$attrs[[1]]$node
    Edges=res$x$attrs[[1]]$link

    ID=Nodes$label
    while(any(duplicated(ID))){
        ID[duplicated(ID)]=paste0(ID[duplicated(ID)]," ")
    }
    x=as.numeric(Nodes$x)
    nodes1=data.frame(ID,x)

    N1=ID[Edges$source+1]
    N2=ID[Edges$target+1]

    Value=Edges$value
    edges1=data.frame(N1,N2,Value)
    color1=Nodes$color
    names(color1)=ID
    style <- sapply(ID, function(id)
        list(col=color1[id]), simplify=FALSE)
    r <- makeRiver(nodes=nodes1, edges=edges1,styles=style)
    riverplot(r, plot_area=c(1,0.9), nodewidth=nodewidth, srt=0, textcex=textcex,gravity="c",edgecol="lightgray",
              usr=c(0,1,1,0),...)
    group=unique(as.character(res$x$layoutAttrs[[2]][[1]]$text))
    text(0,1,group[1],pos=4)
    text(1,1,group[3],pos=2)
    text(0.5,1,group[2])
}
