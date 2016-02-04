#' @title Polish field
#' @description Polish a specified library catalogue field.
#' @param df data.frame that includes the given field
#' @param field Field to be preprocessed.
#' @param verbose verbose
#' @return Output of the polished field
#' @author Leo Lahti \email{leo.lahti@@iki.fi}
#' @references See citation("bibliographica")
#' @details A generic wrapper bringing together the separate field processing methods into a single function.
#' @examples \dontrun{a <- polish_field(df, "title")}
#' @export
#' @keywords utilities
polish_field <- function (df, field, verbose = TRUE) {

  from <- till <- NULL

    if (field == "language") {

      df.tmp <- data.frame(language = mark_languages(df[[field]]))

    } else if (field == "title") {

      df.tmp <- data.frame(title = polish_title(df[[field]]))
      
     } else if (field == "publication_frequency") {
   
       df.tmp <- data.frame(publication_frequency = df[[field]])
     
     } else if (field == "publication_interval") {
   
       df.tmp <- data.frame(publication_interval = df[[field]])
     
     } else if (field == "note_dissertation") {
   
       df.tmp <- mark_dissertations(df[[field]])
     
      } else if (field == "physical_extent") {

      	df.tmp <- polish_physical_extent(df[[field]], verbose = verbose)

      } else if (field == "author_name") {

      	#print("Arrange author first and last names in a table")      
      	# Full author name (Last, First) 
      	author <- polish_author(df[[field]], validate = FALSE, verbose = verbose)
      	df.tmp <- data.frame(author_name = author$names$full)

      } else if (field == "publication_place") {

        tab <- polish_place(df[[field]],
		remove.unknown = TRUE, verbose = verbose)
      	df.tmp <- data.frame(publication_place = tab)

      } else if (field == "physical_dimension") {

        # FIXME: this includes filling entries which
        # should go after initial polishing (enrich section)

      	# Fill in missing entries where estimates can be obtained:
      	# area, width, height, gatherings
      	# (also keep pure originals before fill in)
      	df.tmp <- polish_dimensions(df[[field]],
		fill = FALSE, verbose = verbose)

      	# FIXME Filling should go in the enrichment section
      	# Remove the 'original' fields      
      	df.tmp <- df.tmp[, -grep("original", names(df.tmp))] 

      } else if (field == "publisher") {

      	tab <- polish_publisher(df[[field]], verbose = verbose)
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

     } else if (field == "row.index") {

        df.tmp <- data.frame(row.index = df[[field]])

     } else if (field == "original_row") {

        df.tmp <- data.frame(original_row = df[[field]])

     } else {
   
       stop(paste("No info on how to preprocess field: ", field))
     
     }

    df.tmp

}