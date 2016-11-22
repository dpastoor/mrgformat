annotation_spacing <- function(block_text, block_position, max_positions, i = 1) {
    if (length(block_position)) {
        value_start <- block_position[i, 1]
        add_spaces_before_value <- max_positions[i] - value_start
        new_spacing <- paste0(c(stringr::str_sub(block_text,1,value_start-1),
                                ifelse(add_spaces_before_value > 0, stringr::str_pad(" ", add_spaces_before_value), ""),
                                stringr::str_sub(block_text, value_start, nchar(block_text))),
                              collapse = "")
        return(new_spacing)
    }
    return(block_text)
}

#' normalize an annotated block
#' @param text the text to replace
#' @export
normalize_annotated_block <- function(text) {

    block_positions <- stringr::str_locate_all(text, ":")

    max_positions <- c(value_start = 0, annotation_start = 0)

    lapply(block_positions, function(block) {
        if (length(block)) {
            for (row in seq_along(1:nrow(block))) {
                if (max_positions[row] < block[row, 1]) {
                    max_positions[row] <<- block[row, 1]
                }
            }
        }
    })

    text <- purrr::map2(text, block_positions, annotation_spacing, max_positions, 1)
    # if full block with values will also have an annotation start
    if (max_positions[["annotation_start"]]) {
        block_positions <- stringr::str_locate_all(text, ":")

        max_positions <- c(value_start = 0, annotation_start = 0)

        lapply(block_positions, function(block) {
            if (length(block)) {
                for (row in seq_along(1:nrow(block))) {
                    if (max_positions[row] < block[row, 1]) {
                        max_positions[row] <<- block[row, 1]
                    }
                }
            }
        })
    text <- purrr::map2(text, block_positions, annotation_spacing, max_positions, 2)
    }
    return(unlist(text))

}

#' addin
#' @export
normalize_annotated_block_addin <- function() {
    context <- rstudioapi::getActiveDocumentContext()
    text <- context$content
    range_start <- context$selection[[1L]]$range$start[[1L]]
    range_end   <- context$selection[[1L]]$range$end[[1L]]

    new_text <- normalize_annotated_block(text[range_start:range_end])
    rstudioapi::insertText(paste0(new_text, collapse = "\n"))
}