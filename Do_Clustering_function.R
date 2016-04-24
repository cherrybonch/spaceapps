#' Function to do clustering of rows in data.frame#'
#' @param data data.frame with numeric values, columns are features on which the clustering is based
#' @param grid_x_dim,grid_y_dim dimensions of the grid in somgrid
#' @param nclust amount of clusters needed
#' @param show_plot whether or not to show a clustering plot. Default is TRUE#'
#' @return a vector comprising number of cluster for each data row.
#' @export
Do.Clustering <- function(data, grid_x_dim = 10, grid_y_dim = 10, nclust = 10, show_plot = TRUE){
    for (package in c('kohonen')) {
        if (!require(package, character.only=T, quietly=T)) {
            install.packages(package)
            library(package)
        }
    }
    pretty_palette <- rainbow(nclust)
    data <- data.frame(apply(data,2,function(x){
        return(as.numeric(as.character(x)))}))
    data_train_matrix <- as.matrix(scale(data))
    som_grid <- somgrid(xdim = grid_x_dim, ydim = grid_y_dim, topo = "hexagonal")
    som_model <- som(data_train_matrix,grid=som_grid)
    som_cluster <- cutree(hclust(dist(som_model$codes)), nclust)
    if(show_plot){
        plot(som_model, type="mapping", bgcol = pretty_palette[som_cluster], main = "Clusters")
        add.cluster.boundaries(som_model, som_cluster)
    }
    nodes <- som_model$unit.classif
    clusters <- nodes
    for (i in seq(grid_x_dim * grid_y_dim)){
        clusters[nodes == i] <- as.numeric(som_cluster[i])
    }
    return(clusters)
}