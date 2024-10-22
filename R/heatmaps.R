#' Create a heatmap from a matrix without clustering
#'
#' @param mat A matrix or object which can ne coerced to a matrix.
#' @param colors A vector if colors to use for heatmap. Default is colorRampPalette(RColorBrewer::brewer.pal(n = 7, name = "Reds"))(100). 
#' Can also just supply a single colour and it will be interpolated using colorRampPalette().
#' @param reverse_colors A logical value indicating whether to reverse the supplied or created colors e.g. if all values are negative. Default is FALSE.
#' @param breaks Vector of breaks associated with colors. Default is NA.
#' @param mask_diagonal Logical value indicating whether main diagonal should be masked. Default is T.
#' @param decimal_places Number of decimal places to display. Default is 2.
#' @param title Title of the plot. Default is no title. 
#' @param row_labels Labels to use for rows. Default is to use row names of mat.
#' @param col_labels Labels to use for columns. Default is to use column names of mat.
#' @param angle_col Angle to rotate column labels. Default is 270. 
#' @param title_size Fontsize for the title. Default is 18.
#' @param label_size Fontsize for the labels. Default is 15.  
#' @param number_size Fontsize for numbers. Default is 18.
#' @param filename Optional filename to save plot with. 
#' @param file_dimensions A vector of length two with the width and height of the file in inches. 
#' Default is a width of 6 and height of 6.  
#' @param return_ggplot A logical value indicating whether the heatmap should be returned as a ggplot object
#' @return A pheatmap object or ggplot object dericed from it depending on 
#' @export
heatmap_without_clustering = function(mat, colors = colorRampPalette(RColorBrewer::brewer.pal(n = 7, name = "Reds"))(100),
  reverse_colors = F, breaks = NA, mask_diagonal = T, decimal_places = 2, title = "", row_labels = NULL, col_labels = NULL, 
  angle_col = 270, title_size = 18, label_size = 15, number_size = 18, filename = NA, file_dimensions = c(6, 6), return_ggplot = F){
  
  # Interpolate colors if just a single colour provided
  if(length(colors) == 1){
    colors = colorRampPalette(c("white", colors))(100)
  }
  
  # Reverse colurs if specified
  if(reverse_colors){colors = rev(colors)}
  
  # Ensure mat is a matrix
  mat = as.matrix(mat)
  
  # Round values to specified number of places and convert to a character
  char_mat = matrix(as.character(round(mat, decimal_places)), ncol = ncol(mat))
  
  # Mask the main diagonal if specified
  if(mask_diagonal){
    diag(mat) = NA
    diag(char_mat) = ""
  } 
  
  # Use row names of mat as row labels if none provided
  if(is.null(row_labels)){
    row_labels = row.names(mat)
  }
  
  # Use column names of mat as row labels if none provided
  if(is.null(col_labels)){
    col_labels = colnames(mat)
  }
  
  # Create heatmap
  heatmap = pheatmap::pheatmap(mat = mat, cluster_rows = F, cluster_cols = F, 
    color = colors, breaks = breaks, na_col = "white", legend = F, angle_col = angle_col, 
    display_numbers = char_mat, number_color = "black", border_color = "black", 
    main = title, labels_row = row_labels, labels_col = col_labels, 
    fontsize = title_size, fontsize_row = label_size, fontsize_col = label_size, fontsize_number = number_size, 
    filename = filename, width = file_dimensions[1], height = file_dimensions[2])
  
  # Reset graphics device if saving plot
  if(.Device == "pdf"){dev.off()}
  
  # Return heatmap
  if(return_ggplot){
    return(ggplotify::as.ggplot(heatmap$gtable))
  } else {
    return(heatmap)
  }
  
}
