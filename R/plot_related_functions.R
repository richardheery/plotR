#' #' Save a list of ggplot objects as a PDF
#' #'
#' #' @param plot_list A list of ggplot objects
#' #' @param file_name Name of the putput file
#' #' @param width Width for the plots in inches. default is 16.
#' #' @param height Height for the plots in inches. default is 9.
#' #' @return Invisibly returns TRUE
#' #' @export
#' pdf_save = function(plot_list, file_name, width = 16, height = 9){
#'   pdf(paste(file_name), width = width, height = height)
#'   for(plot in plot_list){
#'     print(plot)
#'   }
#'   dev.off()
#'   return(return(invisible(TRUE)))
#' }

# # Load required packages
# library(ggplot2)
# 
# sig_sym = function(p_values){
#   # Return significance symbol for p-values
#   return(as.character(symnum(p_values, corr = FALSE, na = FALSE, 
#   cutpoints = c(0, 0.001, 0.01, 0.05, 1), symbols = c("***", "**", "*", " "))))
# }
# 
# title_adjust = function(txt){
# # Convert a string to title case
#   tools::toTitleCase(gsub("_", " ", txt))
# }
# geom_bar_quick = function(data, x, y, fill, title = NULL, x_name = x, y_name = y, legend_name = fill, fill_levels = NULL, rows = NULL, 
#   xlabels = NULL, legend_labels = NULL, geom_hline = NULL, significance = NULL, save = NULL){
# # Make a dodged barplot from a dataframe using ggplot, specifying which columns to use as x, y and fill, the title, x-axis label, y-axis label, legend label and order of the fill levels.
# # Can optionally add a horizontal line at a specified height, custom x-labels and significance values above the bars (if they are present in the dataframe). 
# # Can also set a number of rows to split x axis groups across using facet_wrap()
# # Can save a file with the name specified using file_name
#   if(is.null(fill_levels)){fill_levels = unique(data[[fill]])}
#   data[[fill]] = factor(data[[fill]], levels = fill_levels)
#   plot = ggplot(data, aes_string(x = x, y = y, fill = fill)) +
#   geom_bar(position = "dodge", stat = "identity") +
#   geom_hline(yintercept = 1, linetype = "dashed") +
#   scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
#   labs(title = title, x = x_name, y = y_name, fill = legend_name) +
#   theme_classic() +
#   theme(plot.title = element_text(hjust = 0.5))
#   
#   if(!is.null(xlabels)){plot = plot + scale_x_discrete(labels = xlabels)}
#     else({plot = plot + theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank())})
#   if(!is.null(legend_labels)){plot = plot +  scale_fill_discrete(labels = legend_labels)}
#   if(!is.null(geom_hline)){plot = plot + geom_hline(yintercept = geom_hline, linetype = "dashed")}
#   if(!is.null(significance)){plot = plot + geom_text(aes_string(label = significance), position = position_dodge(width = 0.9), angle = 90, vjust = 0.75, hjust = 0)}
#   if(!is.null(rows)){plot = plot + facet_wrap(as.formula(paste("~", x)), scales = "free_x", nrow = rows)}
#   plot
#   if(!is.null(save)){ggsave(filename = save,  device = "jpeg", width = 16, height = 9, units = "in")}
#   plot
# }
# 
# geom_box_quick = function(data, x, y, title = NULL, x_name = x, y_name = y, rows = NULL,  xlabels = NULL, save = NULL){
# # Make boxplots from a dataframe using ggplot, specifying which columns to use as x and y, the title, x-axis label and y-axis label.
# # Can also set a number of rows to split x axis groups across using facet_wrap()
# # Can save a file with the name specified using file_name
#   plot = ggplot(data, aes_string(x = x, y = y)) +
#   geom_boxplot() +
#   scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
#   labs(title = title, x = x_name, y = y_name) +
#   theme_classic() +
#   theme(plot.title = element_text(hjust = 0.5))
#   
#   if(!is.null(xlabels)){plot = plot + scale_x_discrete(labels = xlabels)}
#     else({plot = plot + theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank())})
#   if(!is.null(rows)){plot = plot + facet_wrap(as.formula(paste("~", x)), scales = "free", nrow = rows)}
#   plot
#   if(!is.null(save)){ggsave(filename = save,  device = "jpeg", width = 16, height = 9, units = "in")}
#   plot
# }
# 
# 
# # Use ggplot to prepare a PCA plot  with first 2 PCs and associated text for each point for use with ggplotly
# pca_ggplotly_prepare = function(pca, title = NULL, col = NA, legend = NA, alpha = 1, text_df = NULL){
#   pca_data = data.frame(samp = rownames(pca$x), X=pca$x[,1], Y=pca$x[,2], col = sqrt(abs(col)*100) * sign(col))
#   pca_var_per <- round(pca$sdev^2 /sum(pca$sdev^2)*100, 1)
#   ggplot(data=pca_data, aes(x=X, y=Y, color = col, text = text_df)) + 
#     geom_point(alpha = alpha) + 
#     xlab(paste("PC1 - ", pca_var_per[1], "%", sep="")) + 
#     ylab(paste("PC2 - ", pca_var_per[2], "%", sep="")) +
#     labs(colour = legend) +
#     theme_bw() +
#     scale_color_gradient(low="#22B2B2", high="#B22222", breaks = c(-6, 6), labels = c("Hypo", "Hyper")) +
#     ggtitle(title) +
#     theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 16), axis.text=element_text(size=12), 
#           axis.title=element_text(size=12))
# }
# 
# 
# ggarranger = function(..., xlab = NULL, ylab = NULL, nrow = 1, ncol = NA, common.legend = T, legend = "right"){
#   plot_list = list(...)
#   plot_list = lapply(plot_list, function(x) x + rremove("xylab"))
#   annotate_figure(ggarrange(plotlist = plot_list, nrow = nrow, ncol = ncol, common.legend = common.legend, legend = legend),
#   bottom = text_grob(xlab, color = "black"),
#   left = text_grob(ylab, color = "black", rot = 90))
# }
# 
# 
# 
