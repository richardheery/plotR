library(ggplot2)
library(plotly)
library(scales)
library(RColorBrewer)

#' Calculate percentage of variation each PC in a PCA accounts for
#'
#' @param pca A prcomp or princomp object
#' @return A numeric vector with the percentage of variation explained by each PC
#' @export
pca_var_percent = function(pca) {
  if(!class(pca) %in% c("prcomp", "princomp")){simpleError("pca should be an object of class prcomp or princomp")}
  pca_var = pca$sdev ^ 2 / sum(pca$sdev^2) * 100
  return(pca_var)
}

#' Create a scree plot showing the percentage of variation each PC explains using ggplot2
#'
#' @param pca A prcomp or princomp object
#' @param number_of_pcs Number of PCs to display in the plot. Default is 10 or the total number of PCs, whichever is smaller.
#' @param cumulative Logical value indicating whether to show cumulative percentage of variation explained by PCs. Default is FALSE.
#' @param title Title for the plot. Default is "Percentage of Variance Explained by Top \code{number_of_pcs} PCs".
#' @param xlab Label for the x axis. Default is "PC".
#' @param ylab Label for the y axis. Default is "Percentage of Variance Explained".
#' @param rotate_x_axis_labels Whether to rotate the x labels by 90 degrees. Default is False
#' @param colour_palette Either of vector of colours with length equal to \code{number_of_pcs} or the name of a palette from RColorBrewer. Default is the "Oranges" palette.
#' @return A ggplot object
#' @export
scree_plot = function(pca, number_of_pcs = 10, cumulative = FALSE, title = NULL, 
  xlab = NULL, ylab = NULL, rotate_x_tick_labels = FALSE, colour_palette = "Oranges"){
  
  # Check for errors 
  if(!class(pca) %in% c("prcomp", "princomp")){simpleError("pca should be an object of class prcomp or princomp")}
  
  # Assign number_of_pcs to either number_of_pcs  or the total number of PCs, whichever is smaller
  number_of_pcs = min(number_of_pcs, ncol(pca$x))
  
  # Create palette if a palette name given
  if(length(colour_palette) == 1){
    if(colour_palette %in% rownames(RColorBrewer::brewer.pal.info)){
      colour_palette = suppressWarnings({rev(colorRampPalette(RColorBrewer::brewer.pal(12, colour_palette))(number_of_pcs))})
      if(cumulative){colour_palette = rev(colour_palette)}
    } else {
      simpleError("If giving the name palette it should be one of the available palettes from RColorBrewer")
    }
  } 
  
  # Assign labels and title
  if(is.null(xlab)){xlab = "PC"}
  if(is.null(ylab)){ylab = "Percentage of Variance"}
  if(is.null(title)){
    if(cumulative){
      title = paste("Cumulative percentage of Variance Explained by Top", number_of_pcs, "PCs")
    } else {
      title = paste("Percentage of Variance Explained by Top", number_of_pcs, "PCs")
    }
  }
  
  # Create plot
  pca_var = pca$sdev ^ 2 / sum(pca$sdev^2) * 100
  if(cumulative){pca_var = cumsum(pca_var)}
  scree_df = data.frame(variance = head(pca_var, number_of_pcs), pc = head(colnames(pca$x), number_of_pcs))
  scree_df$pc = factor(scree_df$pc, levels = scree_df$pc)
  scree_plot = ggplot(scree_df, aes(x = pc, y = variance)) +
    geom_bar(stat="identity", fill = colour_palette, color = "black") +
    scale_y_continuous(expand = c(0,0)) +
    labs(title = title, x = xlab, y = ylab) +
    theme_classic() +
    theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 24), 
       axis.title = element_text(size = 20), axis.text = element_text(size = 18)) +
    if(rotate_x_tick_labels) {theme(axis.text.x = element_text(angle = 90, hjust = 1))}
  return(scree_plot)
}

#' Create a PCA plot using ggplot2
#'
#' @param pca A prcomp or princomp object
#' @param PCs A numeric vector of length 2 specifying which PCs to use. Default are PCs 1 and 2
#' @param sample_subset An optional subset of samples to plot instead of all samples from the PCA
#' @param colour_groups An optional vector of the groups the observations belong to. Should be a character vector or a factor.
#' @param colours An optional vector of colours to use for the colour_groups if they are provided. 
#' @param shape_groups An optional vector of the groups the observations belong to. Should be a character vector or a factor.
#' @param labels An optional vector of labels for the observations. If used then labels will be shown instead of points. 
#' @param title A title for the plot. Default is no title. 
#' @param colour_legend_title The title of the colour legend for the plot. Default is no title. 
#' @param shape_legend_title The title of the shape legend for the plot. Default is no title. 
#' @param point_size = Size of the points. Default is 1.
#' @param alpha Alpha value for the points. Default is 1.
#' @param save_image_path An optional filepath where the image will be saved.
#' @param width If the image is saved, the width of the saved image. Default is 9 inches.
#' @param height If the image is saved, the height of the saved image. Default is 9 inches.
#' @return A ggplot object
#' @export
pca_ggplot = function(pca, PCs = c(1, 2), sample_subset = NULL, colour_groups = NULL, colours = NULL, shape_groups = NULL, labels = NULL, 
  title = NULL, colour_legend_title = NULL, shape_legend_title = NULL, point_size = 1, alpha = 1, save_image_path = NULL, width = 9, height = 9){
  
  # If colour_groups is not a character or factor, it will be converted to one
  if(!is(colour_groups, "factor")){colour_groups = factor(colour_groups)}
  
  if(is.null(colour_groups)){
    colour_vec = "black"
  } else {colour_vec = colour_groups
  }
  
  if(is.null(shape_groups)){
    shape_vec = "circle"
  } else {shape_vec = shape_groups
  }
  
  pca_data = data.frame(X = pca$x[,PCs[1]], Y = pca$x[,PCs[2]], colours = colour_vec, shape_vec = shape_vec)
  if(!is.null(sample_subset)){pca_data = pca_data[sample_subset, ]}
  if(!is.null(labels)){pca_data$labels = labels}
  pca_var_per <- round(pca$sdev ^ 2 / sum(pca$sdev^2) * 100, 1)
  pca_plot = ggplot(data = pca_data, aes(x = X, y = Y, label = labels, color = colours, shape = shape_vec)) + 
    {if(is.null(labels)){geom_point(alpha = alpha, size = point_size)}} + 
    {if(!is.null(labels)){geom_text(show.legend = FALSE, alpha = alpha, size = point_size)}} +
    {if(!is.null(labels)){geom_point(alpha = 0)}} +
    {if(!is.null(labels)){guides(colour = guide_legend(override.aes = list(alpha=1)))}} +
    xlab(paste(colnames(pca$x)[PCs[1]], " - ", pca_var_per[PCs[1]], "%", sep="")) + 
    ylab(paste(colnames(pca$x)[PCs[2]], " - ", pca_var_per[PCs[2]], "%", sep="")) +
    labs(colour = colour_legend_title, shape = shape_legend_title) +
    theme_classic() +
    ggtitle(title) +
    theme(plot.title = element_text(hjust = 0.5, size = 24), axis.text=element_text(size = 18), 
      axis.title=element_text(size = 20), legend.title = element_text(size = 20), legend.text = element_text(size = 18)) 
    if(is.null(colour_groups) & is.null(shape_groups)){pca_plot = pca_plot + theme(legend.position = "None")} 
    if(is.null(shape_groups)){pca_plot = pca_plot + guides(shape = "none")} 
    if(is.null(colour_groups)){pca_plot = pca_plot + guides(color = "none")}
    if(!is.null(colours)){pca_plot = pca_plot + scale_color_manual(values = colours)}
  if(!is.null(save_image_path)){ggsave(pca_plot, filename = save_image_path,  device = "jpeg", width = 9, height = 9, units = "in")}
  return(pca_plot)
}

pca_ggplot_continous_colour = function(pca, PCs = c(1, 2), sample_subset = NULL, colours = NULL, shape_groups = NULL, labels = NULL, 
  title = NULL, colour_legend_title = NULL, shape_legend_title = NULL, alpha = 1, save_image_path = NULL, width = 9, height = 9){
  
  if(is.null(shape_groups)){
    shape_vec = "circle"
  } else {shape_vec = shape_groups
  }
  
  pca_data = data.frame(X = pca$x[,PCs[1]], Y = pca$x[,PCs[2]], colours = colours, shape_vec = shape_vec)
  if(!is.null(sample_subset)){pca_data = pca_data[sample_subset, ]}
  if(!is.null(labels)){pca_data$labels = labels}
  pca_var_per <- round(pca$sdev ^ 2 / sum(pca$sdev^2) * 100, 1)
  pca_plot = ggplot(data = pca_data, aes(x = X, y = Y, label = labels, color = colours, shape = shape_vec)) + 
    {if(is.null(labels)){geom_point(alpha = alpha)}} + 
    {if(!is.null(labels)){geom_text(show.legend = FALSE, alpha = alpha)}} +
    {if(!is.null(labels)){geom_point(alpha = 0)}} +
    {if(!is.null(labels)){guides(colour = guide_legend(override.aes = list(alpha=1)))}} +
    xlab(paste(colnames(pca$x)[PCs[1]], " - ", pca_var_per[PCs[1]], "%", sep="")) + 
    ylab(paste(colnames(pca$x)[PCs[2]], " - ", pca_var_per[PCs[2]], "%", sep="")) +
    labs(colour = colour_legend_title, shape = shape_legend_title) +
    theme_classic() +
    ggtitle(title) +
    theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 24), axis.text=element_text(size = 20), 
      axis.title=element_text(size = 18)) 
    if(is.null(shape_groups)){pca_plot = pca_plot + theme(legend.position = "None")} 
    if(is.null(shape_groups)){pca_plot = pca_plot + guides(shape = "none")} 
  if(!is.null(save_image_path)){ggsave(pca_plot, filename = save_image_path,  device = "jpeg", width = 9, height = 9, units = "in")}
  return(pca_plot)
}
