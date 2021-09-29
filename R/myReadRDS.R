#' Read RDS file with unique id
#' @param dataname string dataname
#' @param id Name of id field
#' @export
myReadRDS=function(dataname,id=""){
    temp=readRDS(dataname)
    if(id!=""){
        dup=which(duplicated(temp[[id]]))
        if(length(dup)>0){
            temp=temp[-dup,]
        }
    }
    temp
}
