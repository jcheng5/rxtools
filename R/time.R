#' Run code after a delay
#'
#' @examples
#'
#' rx_session({
#'   rx_exec_interval(1, {
#'     print(sample.int(10, 1))
#'   })
#'
#'   rx_exec_later(5, {
#'     rx_stoploop()
#'   })
#'
#'   rx_runloop()
#' })
#'
#' @export
rx_exec_later <- function(delay = 0, expr, env = parent.frame(), quoted = FALSE,
  priority = 0, auto_destroy = TRUE) {

  shiny::installExprFunction(expr, "func", eval.env = env, quoted = quoted)

  expiration <- Sys.time() + delay
  waited <- FALSE

  task <- rx_exec({
    if (!waited && delay > 0) {
      waited <<- TRUE
      invalidateLater(max(0, expiration - Sys.time()) * 1000)
      return()
    }

    task$destroy()
    func()
  }, priority = priority, autoDestroy = auto_destroy)

  invisible(task)
}

#' @export
rx_exec_interval <- function(delay = 0, expr, env = parent.frame(), quoted = FALSE,
  priority = 0, auto_destroy = TRUE) {

  shiny::installExprFunction(expr, "func", eval.env = env, quoted = quoted)

  task <- rx_exec({
    on.exit(invalidateLater(delay * 1000))

    func()
  }, priority = priority, autoDestroy = auto_destroy)

  invisible(task)
}

#' Create or run within a reactive domain
#'
#' A reactive domain is an environment within which reactive constructs can run.
#' Some objects like \code{\link{rx_exec}} automatically register themselves to
#' be destroyed when the domain within which they were created is closed. This
#' makes cleanup easier and prevents tasks from one "session" from bleeding into
#' the next session.
#'
#' This function can be used to either create and return a domain; or to
#' evaluate an expression or code block under the domain, and close the domain
#' when complete.
#'
#' When passing \code{expr}, this function is reentrant.
#'
#' @param expr The expression or code block to evaluate under the domain. If
#'   provided, the domain will automatically be closed before returning.
#'
#' @return If \code{expr} is missing, a domain object; call the \code{close()}
#'   method on it to terminate the domain and clean up registered objects. If
#'   \code{expr} is provided, then the value of \code{expr} is returned.
#'
#' @examples
#'
#' @export
rx_domain <- function(expr) {
  domain <- create_domain()

  # If no-arg call, just create the domain and return it
  if (missing(expr))
    return(domain)

  # Otherwise, evaluate the expression using the domain, then close the domain
  on.exit({
    domain$close()
  })
  shiny::withReactiveDomain(domain, expr)
}
