#' Load Word2Vec Model
#'
#' @return A word2vec model
#' @export
load_word2vec_model <- function() {
  model_path <- system.file("extdata", "word2vec_model.RData", package = "WEMatch")
  load(model_path)
  return(model)
}
