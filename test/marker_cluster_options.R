library(leaflet)
leaflet(quakes) %>% addTiles() %>% addMarkers(
  clusterOptions = markerClusterOptions(iconCreateFunction=JS("function (cluster) {    
    var childCount = cluster.getChildCount(); 
    var c = ' marker-cluster-';  
    if (childCount < 100) {  
      c += 'large';  
    } else if (childCount < 1000) {  
      c += 'medium';  
    } else { 
      c += 'small';  
    }    
    return new L.DivIcon({ html: '<div><span>' + childCount + '</span></div>', className: 'marker-cluster' + c, iconSize: new L.Point(40, 40) });

  }"))
)


clusterOptions = leaflet::markerClusterOptions(weight=3, radius=5,
                                               color = c("#28a745", "#007bff", "#ffc107", "#dc3545"),
                                               clusterOptions = markerClusterOptions(maxClusterRadius=20, 
                                                                                     spiderfyDistanceMultiplier=5),
                                               stroke=F, fillOpacity=0.5
), 




clusterOptions = leaflet::markerClusterOptions(iconCreateFunction =
                                                 JS("
                                          function(cluster) {
                                             return new L.DivIcon({
                                               html: '<div style=\"opacity: 0.8;background-color: #3e3e3e;color: #fff;\"><span>' + cluster.getChildCount() + '</div><span>',
                                               className: 'marker-cluster'
                                             });
                                           }", 
                                                    maxClusterRadius = 50)
                                               
                                               
                                               maxClusterRadius = 20, iconCreateFunction =
                                                 JS("
                                          function(cluster) {
                                             return new L.DivIcon({
                                               html: '<div style=\"opacity: 0.8;background-color: #3e3e3e;color: #fff;\"><span>' + cluster.getChildCount() + '</div><span>',
                                               className: 'marker-cluster'
                                             });
                                           }"
)
                                               
                                               
                                               
                                               clusterOptions = leaflet::markerClusterOptions(weight=3, radius=5,
                                                                                              color = c("#28a745", "#007bff", "#ffc107", "#dc3545"),
                                                                                              clusterOptions = markerClusterOptions(maxClusterRadius=20, 
                                                                                                                                    spiderfyDistanceMultiplier=5),
                                                                                              stroke=F, fillOpacity=0.5
                                               ),                                                