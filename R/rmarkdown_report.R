#' Make an R Markdown report from a template
#'
#' @param rmd_template Path to an R Markdown template.
#' @param report_title Title of the report.
#' @param output_filepath Path to save output HTML document. 
#' @param show_report A logical value indicating whether or not to open the report after it's completion. Default is TRUE. 
#' @param ... Additional arguments required by the R Markdown template.
#' @return Silently returns TRUE
#' @export
make_rmd_html_report = function(rmd_template, report_title = NULL, output_file_name, show_report = T, ...){
  
  # Check that rmd_template has extension Rmd
  if(tools::file_ext(rmd_template) != "Rmd"){stop("rmd_template must be an R Markdown file with extension .Rmd")}
  
  # Store additional arguments in a list
  input_args = list(...)
  
  # Read in the markdown template
  rmd_txt = readLines(rmd_template)
  
  # Add the title to the report
  rmd_txt[grep("title:", rmd_txt)] = paste("title:", paste0("\"", report_title, "\""))
  rmd_txt[grep("# Title", rmd_txt)] = paste("#", report_title)
  
  # Knit the R Markdown document and return the resulting markdown file as a character vector
  knitted_txt = knitr::knit(text = rmd_txt)
  
  # Create HTML output from knitted_txt
  markdown::markdownToHTML(text = knitted_txt, output = output_file_name)
  
  # Display report in browser if show_report is TRUE.
  if(show_report){utils::browseURL(output_file_name)}
  return(invisible(TRUE))
} 
