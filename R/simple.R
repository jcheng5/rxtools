#' Reactive value
#'
#' Creates a reactive value object, whose value can be read (reactively) and
#' set. Setting the value invalidates any reactive conductors or endpoints that
#' had read the previous value (unless the new value is identical to the
#' previous value, in which case nothing happens).
#'
#' @param initial_value The starting value of the reactive value object.
#'
#' @return A function. To read the reactive value, call it with no arguments. To
#'   set the reactive value, call it with the desired value as an argument.
#'
#' @examples
#' a <- rx_value(10)
#'
#' r <- rx_calc({
#'   a()  # read the value
#' })
#'
#' a(20)  # set the value
#' @export
rx_value <- function(initial_value = NULL) {
  rv <- reactiveValues(v = initial_value)

  function(value) {
    if (missing(value)) {
      rv$v
    } else {
      rv$v <- value
    }
  }
}

#' Reactive trigger
#'
#' At times it can be useful to have an explicit mechanism to cause a reactive
#' expression to invalidate. Create a reactive trigger outside of the reactive
#' expression using \code{rx_trigger}, then have the reactive expression call
#' the reactive trigger like a function--this will take a reactive dependency on
#' the trigger. When you want to cause the reactive expression to invalidate,
#' call \code{rx_fire_trigger(trigger)}.
#'
#' \code{rx_trigger} is the constructor for the reactive trigger. The reactive
#' trigger object that it produces should be called like a function from
#' whatever reactive context wants to be triggered by it.
#'
#' To fire a reactive trigger, pass it to the \code{rx_fire_trigger} function.
#'
#' @param rxtrig The trigger to fire.
#'
#' @return \code{rx_trigger} returns a function-like object that you can call to
#'   take a dependency on the trigger.
#'
#' @examples
#' trig <- rx_trigger()
#'
#' now <- rx_exec({
#'   trig()
#'   print(Sys.time())
#' })
#'
#' rx_exec({
#'   shiny::invalidateLater(1000, NULL)
#'   rx_fire_trigger(trig)
#' })
#'
#' @export
rx_trigger <- function() {
  rv <- reactiveValues(v = 0)

  r <- reactive({
    rv$v
    invisible()
  })

  structure(
    r,
    trigger = function() { rv$v <- isolate(rv$v) %% 99999999999 + 1 },
    class = c("rx_trigger", class(r))
  )
}

#' @rdname rx_trigger
#' @export
rx_fire_trigger <- function(rxtrig) {
  if (!inherits(rxtrig, "rx_trigger")) {
    stop("Invalid rxtrig argument, must be rx_trigger")
  }

  attr(rxtrig, "trigger", exact = TRUE)()

  invisible()
}

#' Run reactive event loop
#'
#' Setting up reactive graphs does nothing until something actually causes
#' reactive events to be processed. This is handled for you when writing Shiny
#' apps, but it might also be useful to run reactives outside of Shiny, for
#' testing and experimentation if nothing else. This function runs an event loop
#' for reactivity and timer actions. From an interactive console, interrupt it
#' using \code{Escape} or \code{Ctrl+C} (or whatever the usual interrupt gesture
#' is in your R environment). You can also stop the loop programmatically by
#' calling \code{rx_stoploop}.
#'
#' @param ... Currently ignored. (Used to force other arguments to be passed by
#'   name, not by position.)
#' @param before_tick A no-argument callback function that will be called before
#'   work is done for each iteration.
#' @param after_tick A no-argument callback function that will be called after
#'   work is done for each iteration.
#'
#' @examples
#'
#' counter <- rx_value(0)
#'
#' rx_exec({
#'   print(counter())
#'   if (counter() == 10)
#'     rx_stoploop()
#' })
#'
#' rx_exec({
#'   invalidateLater(100)
#'   isolate({
#'     counter(counter() + 1)
#'   })
#' })
#'
#' rx_runloop()
#'
#' @export
rx_runloop <- function(..., before_tick = NULL, after_tick = NULL) {
  tryCatch(
    {
      while (TRUE) {
        if (!is.null(before_tick)) before_tick()
        shiny:::timerCallbacks$executeElapsed()
        shiny:::flushReact()
        if (!is.null(after_tick)) after_tick()
        Sys.sleep(min(0.1, shiny:::timerCallbacks$timeToNextEvent()))
      }
    },
    stoploop = function(e) {}
  )

  invisible()
}

#' @rdname rx_runloop
#' @export
rx_stoploop <- function() {
  stop(structure(list(), class = c("stoploop", "condition")))
}

create_domain <- function() {
  domain_callbacks <- shiny:::Callbacks$new()
  closed <- FALSE
  domain <- list(
    reactlog = function(...) {},
    onEnded = function(callback) {
      domain_callbacks$register(callback)
    },
    isClosed = function() {
      closed
    },
    isEnded = function() {
      closed
    },
    incrementBusyCount = function() {},
    decrementBusyCount = function() {},
    close = function() {
      closed <<- TRUE
      domain_callbacks$invoke()
    }
  )

  domain
}
