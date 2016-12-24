#' Reactive calculation with previous value
#'
#' Use this variant of \code{\link{rx_calc}} when your logic depends on the
#' previous value you generated. For example, you might want to accumulate some
#' state over each invalidation; or in some exceptional circumstances you may
#' want to return the previous value itself with no changes.
#'
#' @param func A function that takes a single parameter. Each time this function
#'   is called, it will be passed its previous return value.
#' @param initialValue The value to pass to the function the first time it is
#'   called.
#'
#' @return An \code{\link{rx_calc}} based on the function.
#'
#' @export
rx_calc_with_previous <- function(func, initialValue = NULL) {

  value <- initialValue

  rx_calc({
    value <<- func(value)
    value
  })
}

rx_reduce <- function(rxexpr, func, memo = NULL) {
  rxmemo <- rx_value(memo)

  rx_exec({
    val <- rxexpr()
    rxmemo(func(isolate(rxmemo()), val))
  })

  rx_calc({
    rxmemo()
  })
}
