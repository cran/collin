### welcome message
#' @importFrom utils packageDescription
.onAttach <- function(lib, pkg) {
  meta <- utils::packageDescription("collin")
  attachmsg <- paste0("\nThis is collin ", meta$Version,
                      ". For details, use:\n",
                      "> help(package = 'collin')\n",
                      "and\n",
                      "> browseVignettes('collin')\n\n",
                      "To cite the methods in the package use:\n",
                      "> citation('collin')\n")
  packageStartupMessage(attachmsg, domain = NULL, appendLF = TRUE)
}
