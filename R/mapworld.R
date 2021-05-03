#' Make a world map plot
#' @param M is a bibliographic data frame obtained by the converting function convert2df.
#' @importFrom ggplot2 ggplot geom_polygon scale_fill_continuous guides labs scale_size_continuous theme
#' element_rect map_data
#' @importFrom dplyr anti_join left_join group_by summarise
#' @importFrom bibliometrix tableTag
#' @export
mapworld <- function(M){
    if (!("AU_CO" %in% names(M))){M=metaTagExtraction(M,"AU_CO")}
    CO=as.data.frame(tableTag(M,"AU_CO"),stringsAsFactors = FALSE)
    CO$Tab=gsub("UNITED KINGDOM","UK",CO$Tab)
    CO$Tab=gsub("KOREA","SOUTH KOREA",CO$Tab)

    map.world <- map_data("world")
    map.world$region=toupper(map.world$region)

    dplyr::anti_join(CO, map.world, by = c('Tab' = 'region'))

    country.prod <- dplyr::left_join( map.world, CO, by = c('region' = 'Tab'))

    tab=data.frame(country.prod %>%
                       dplyr::group_by(.data$region) %>%
                       dplyr::summarise(Freq=mean(.data$Freq)))

    tab=tab[!is.na(tab$Freq),]

    tab=tab[order(-tab$Freq),]

    breaks=as.numeric(round(quantile(CO$Freq,c(0.2,0.4,0.6,0.8,1))))
    names(breaks)=breaks
    breaks=log(breaks)

    g <- ggplot(country.prod, aes( x = .data$long, y = .data$lat, group=.data$group, text=paste("Country: ",.data$region,"\nN.of Documents: ",.data$Freq))) +
        geom_polygon(aes_string(fill = "log(Freq)", group="group")) +
        scale_fill_continuous(low='dodgerblue', high='dodgerblue4',breaks=breaks)+
        guides(fill = guide_legend(reverse = T)) +
        #geom_text(data=centroids, aes(label = centroids$Tab, x = centroids$long, y = centroids$lat, group=centroids$Tab)) +
        labs(fill = 'N.Documents'
             ,x = NULL
             ,y = NULL) +
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

    results=list(g=g,tab=tab)
    return(results)
}


