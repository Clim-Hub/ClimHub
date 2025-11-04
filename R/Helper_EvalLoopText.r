#' @title Loop over a set numnber of iterations of a string to harmonise looping in package
#'
#' @description Iteraters over a set number of iterations using a loop-text string which gets evaluated at each iteration.
#'
#' @param loopText Character. String to be valuated in Loop.
#' @param iters Numeric. Sequence of iterator values for "Iter" object to take.
#' @param objects List. List of objects the evaluator needs access to. 
#' @param packages Character. Vector containing R package names which are needed for loopText evaluation.
#' @param cores Integer. Number of cores for parallelisation if desired.
#' @param verbose Logical. If progress should be displayed in the console.
#' 
#' @importFrom doSNOW registerDoSNOW
#' @importFrom parallel detectCores
#' @importFrom parallel makeCluster
#' @importFrom snow stopCluster
#' @importFrom foreach %dopar%
#' @importFrom foreach foreach
#'
#' @return A list of whatever the loopText returns per iteration
#' 
#' @author Erik Kusch
#'
#' @examples
#' ## objects which to use in the loops
#' Vec1 <- Vec2 <- 1:100
#' ## loopText to be evaluated
#' loopText <- "Sys.sleep(0.01); Vec1[Iter] + Vec2[Iter]"
#' 
#' ## Single-Core Exection
#' pb <- Helper_Progress(iterLength = length(Vec1), text = "TestLoop")
#' Helper_EvalLoopText(loopText = loopText, 
#'     iters = 1:length(Vec1), 
#'     objects = list("Vec1" = Vec1, "Vec2" = Vec2, "pb" = pb)
#'     )
#' 
#' ## Parallel Exection with progress
#' pb <- Helper_Progress(iterLength = length(Vec1), text = "TestLoop")
#' Helper_EvalLoopText(loopText = loopText, 
#'     iters = 1:length(Vec1), 
#'     objects = list("Vec1" = Vec1, "Vec2" = Vec2, "pb" = pb),
#'     cores = 2
#'     )
#' 
#' ## Parallel Exection without progress
#' pb <- Helper_Progress(iterLength = length(Vec1), text = "TestLoop")
#' Helper_EvalLoopText(loopText = loopText, 
#'     iters = 1:length(Vec1), 
#'     objects = list("Vec1" = Vec1, "Vec2" = Vec2, "pb" = pb),
#'     cores = 2,
#'     verbose = FALSE
#'     )
Helper_EvalLoopText <- function(loopText, iters, objects, packages = "", cores = 1, verbose = TRUE) {
    ## make objects available to function-internal environment ----
    list2env(objects, envir = environment()) ## make objects into environment within function

    ## loop over iters ----
    
    ### Single-Core ++
    if(cores == 1){
        Return <- list()
    for (Iter in iters) {
        IterOutcome <- eval(parse(text = loopText)) # evaluate the kriging specification per layer
        Return <- c(Return, IterOutcome)
        if(verbose){pb$tick(tokens = list(layer = Iter))}
    }
    }
    
    ## Multi-Core ++
    if (cores > 1) {
        cl <- makeCluster(cores)
        on.exit(snow::stopCluster(cl))
        doSNOW::registerDoSNOW(cl)
        progress <- function(n) {
            if(verbose){pb$tick(tokens = list(layer = n))}else{""}
        }
        ForeachObjects <- names(objects)
        Return <- foreach(
            Iter = iters,
            .packages = c("httr"),
            .export = ForeachObjects,
            .options.snow = list(progress = progress)
        ) %dopar% { # parallel loop'
            eval(parse(text = loopText))
        } 
    }# end of parallel loop

    ## return outcome ----
    return(Return)
}
