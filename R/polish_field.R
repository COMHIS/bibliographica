#' @title Polish field
#' @description Polish a specified library catalogue field.
#' @param df data.frame that includes the given field
#' @param field Field to be preprocessed.
#' @param verbose verbose
#' @param mc.cores Number of cores for parallelization
#' @return Output of the polished field
#' @author Leo Lahti \email{leo.lahti@@iki.fi}
#' @references See citation("bibliographica")
#' @details A wrapper bringing together the separate field processing methods into a single function.
#' @examples \dontrun{a <- polish_field(df, "title")}
#' @export
#' @keywords utilities
polish_field <- function (df, field, verbose = TRUE, mc.cores = 1) {

  from <- till <- NULL

  conversions <- list(control_number = "control_number",
  	      	      subject_geography = "subject_geography",
		      publication_geography = "publication_geography",
		      subject_topic = "topic",
		      publication_topic = "topic_publication",
		      language = "language",
		      title = "title",
		      title_uniform = "title_uniform",
		      title_uniform2 = "title_uniform2",
		      publication_frequency = "publication_frequency",
		      publication_interval = "publication_interval",
		      row.index = "row.index",
		      original_row = "original_row",
		      estc_control_number = "estc_control_number"
  	      	 		     )

  # No preprocessing implemented for these fields
  # but the name may chaneg
  if (field %in% c("control_number",
                 "subject_geography",
		 "publication_geography",
		 "title_uniform",
		 "title_uniform2",
		 "publication_frequency",
		 "row_index",
		 "original_row",
		 "publication_interval",
		 "estc_control_number"		 
		 )) {
    df.tmp <- data.frame(df[[field]])
    names(df.tmp) <- conversions[[field]]
  }

  # Specific preprocessing implemented for these fields
  
  if (field == "language") {

    df.tmp <- mark_languages(df[[field]])

  } else if (field == "title") {

    df.tmp <- data.frame(title = polish_title(df[[field]]))

  } else if (field == "note_dissertation") {
   
    df.tmp <- mark_dissertations(df[[field]])
     
  } else if (field == "physical_extent") {

    df.tmp <- polish_physical_extent(df[[field]], verbose = verbose, mc.cores = mc.cores)

  } else if (field == "author_name") {

    # Full author name (Last, First) 
    author <- polish_author(df[[field]], verbose = verbose)
    df.tmp <- data.frame(author_name = author)

  } else if (field == "publication_place") {

    tab <- polish_place(df[[field]],
		remove.unknown = FALSE, verbose = verbose)
    df.tmp <- data.frame(publication_place = tab)

  } else if (field == "physical_dimension") {

    # FIXME: this includes filling entries which
    # should go after initial polishing (enrich section)

    # Fill in missing entries where estimates can be obtained:
    # area, width, height, gatherings
    # (also keep pure originals before fill in)
    df.tmp <- polish_dimensions(df[[field]],
		fill = FALSE, verbose = verbose)

  } else if (field == "publisher") {

    tab <- polish_publisher(df[[field]], verbose = verbose, mc.cores = mc.cores)
    df.tmp <- data.frame(publisher = tab)

  } else if (field == "corporate") {

    df.tmp <- data.frame(corporate = polish_corporate(df[[field]]))

  } else if (field == "note_granter") {

    # Use the university function for note_granter
    df.tmp <- data.frame(note_granter = polish_university(df[[field]]))

  } else if (field == "author_date") {

    # TODO make a tidy cleanup function to shorten the code here
    df.tmp <- polish_years(df[[field]], check = TRUE, verbose = verbose)
    df.tmp <- dplyr::rename(df.tmp, author_birth = from)
    df.tmp <- dplyr::rename(df.tmp, author_death = till)	
      	
  } else if (field == "publication_time") {
    
    tmp <- polish_years(df[[field]], check = TRUE)
      
    # Add to data.frame
    df.tmp <- data.frame(publication_year_from = tmp$from,
              		 publication_year_till = tmp$till
        )

  } else if (field == "control_number") {
  
    NULL

  } else {


    warning(paste("No info on how to preprocess field: ", field))
    #df.tmp <- NULL
    df.tmp <- data.frame(df[[field]])
    names(df.tmp) <- field
    
  }

  df.tmp

}