sanitize_fortran_code <- function(.x) {
    cleaned_vals <- paste0(.x, collapse = "") %>%
        stringr::str_replace_all('[&"]', "") %>%
        stringr::str_replace(".*ITER_REPORT,", "") %>%
        stringr::str_replace_all("\\)\\s?,", "): ") %>%
        stringr::str_replace_all(",(?=\\s)", ": ") %>%
        stringr::str_replace_all(",(?=[A-Za-z])", ": ")%>%
        stringr::str_split(":") %>% .[[1]] %>%
        stringr::str_trim()

    glue::double_quote(cleaned_vals) %>%
        glue::glue_collapse(sep = ",") %>%
        glue::glue("c({val})", val = .)
}

#' addin
#' @export
sanitize_fortran <- function() {
    context <- rstudioapi::getActiveDocumentContext()
    text <- context$content
    range_start <- context$selection[[1L]]$range$start[[1L]]
    range_end   <- context$selection[[1L]]$range$end[[1L]]
    new_text <- sanitize_fortran_code(text[range_start:range_end])
    return(new_text)
}