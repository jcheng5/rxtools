#' Debounce a reactive expression
#'
#' Transforms a reactive expression by preventing its invalidation signals from
#' being sent until activity has stopped for the given number of milliseconds.
#' This lets you ignore a very "chatty" reactive expression until it becomes
#' idle, which is useful when the intermediate values don't matter as much as
#' the final value, and the downstream calculations that depend on the reactive
#' expression take a long time.
#'
#' This is not a true debounce in that it will not prevent \code{expr} from
#' being called many times (in fact it may be called more times than usual), but
#' rather, the reactive invalidation signal that is produced by expr is
#' debounced instead. This means that \code{rx_debounce} should be used when
#' \code{expr} is cheap but the things it will trigger (outputs and reactives
#' that use \code{expr}) are expensive.
#'
#' @param rxexpr A reactive expression that invalidates too often.
#' @param millis Number of milliseconds of inactivity to wait for before
#'   allowing invalidation to succeed.
#' @param domain See \link[shiny]{domains}.
#'
#' @export
rx_debounce <- function(rxexpr, millis, domain = shiny::getDefaultReactiveDomain()) {

  # TODO: make a nice label for the observer

  force(rxexpr)
  force(millis)

  v <- reactiveValues(
    trigger = NULL,
    when = NULL # the deadline for the timer to fire; NULL if not scheduled
  )

  # Responsible for tracking when f() changes.
  observeEvent(rxexpr(), {
    # The value changed. Start or reset the timer.
    v$when <- Sys.time() + millis/1000
  }, ignoreNULL = FALSE)

  # This observer is the timer. It rests until v$when elapses, then touches
  # v$trigger.
  observe({
    if (is.null(v$when))
      return()

    now <- Sys.time()
    if (now >= v$when) {
      v$trigger <- runif(1)
      v$when <- NULL
    } else {
      invalidateLater((v$when - now) * 1000, domain)
    }
  })

  # This is the actual reactive that is returned to the user. It returns the
  # value of f(), but only invalidates/updates when v$trigger is touched.
  eventReactive(v$trigger, {
    rxexpr()
  }, ignoreNULL = FALSE)
}
