### LOOP TEXT EVALUATION ========================================================
#' Loop over a set numnber of iterations of a string to harmonise looping in package
#'
#' Iteraters over a set number of iterations using a loop-text string which gets evaluated at each iteration.
#'
#' @param LoopText Character. String to be valuated in Loop.
#' @param Iters Numeric. Sequence of iterator values for "Iter" object to take.
#' @param Objects List. List of objects the evaluator needs access to. 
#' @param Cores Integer. Number of cores for parallelisation if desired.
#' @param verbose Logical. If progress should be displayed in the console.
#' 
#' @importFrom doSNOW registerDoSNOW
#' @importFrom parallel detectCores
#' @importFrom parallel makeCluster
#' @importFrom snow stopCluster
#' @importFrom foreach %dopar%
#' @importFrom foreach foreach
#'
#' @return A list of whatever the LoopText returns per iteration
#'
#' @examples
#' ## objects which to use in the loops
#' Vec1 <- Vec2 <- 1:100
#' ## LoopText to be evaluated
#' LoopText <- "Sys.sleep(0.01); Vec1[Iter] + Vec2[Iter]"
#' 
#' ## Single-Core Exection
#' pb <- Helper.Progress(IterLength = length(Vec1), Text = "TestLoop")
#' Helper.EvalLoopText(LoopText = LoopText, 
#'     Iters = 1:length(Vec1), 
#'     Objects = list("Vec1" = Vec1, "Vec2" = Vec2, "pb" = pb)
#'     )
#' 
#' ## Parallel Exection with progress
#' pb <- Helper.Progress(IterLength = length(Vec1), Text = "TestLoop")
#' Helper.EvalLoopText(LoopText = LoopText, 
#'     Iters = 1:length(Vec1), 
#'     Objects = list("Vec1" = Vec1, "Vec2" = Vec2, "pb" = pb),
#'     Cores = 2
#'     )
#' 
#' ## Parallel Exection without progress
#' pb <- Helper.Progress(IterLength = length(Vec1), Text = "TestLoop")
#' Helper.EvalLoopText(LoopText = LoopText, 
#'     Iters = 1:length(Vec1), 
#'     Objects = list("Vec1" = Vec1, "Vec2" = Vec2, "pb" = pb),
#'     Cores = 2,
#'     verbose = FALSE
#'     )
#'
Helper.EvalLoopText <- function(LoopText, Iters, Objects, Cores = 1, verbose = TRUE) {
    ## make objects available to function-internal environment ----
    list2env(Objects, envir = environment()) ## make objects into environment within function

    ## loop over iters ----
    
    ### Single-Core ++
    if(Cores == 1){
        Return <- list()
    for (Iter in Iters) {
        IterOutcome <- eval(parse(text = LoopText)) # evaluate the kriging specification per layer
        Return <- c(Return, IterOutcome)
        if(verbose){pb$tick(tokens = list(layer = Iter))}
    }
    }
    
    ## Multi-Core ++
    if (Cores > 1) {
        cl <- makeCluster(Cores)
        on.exit(snow::stopCluster(cl))
        doSNOW::registerDoSNOW(cl)
        progress <- function(n) {
            if(verbose){pb$tick(tokens = list(layer = n))}else{""}
        }
        ForeachObjects <- names(Objects)
        Return <- foreach(
            Iter = Iters,
            # .packages = c("httr"),
            .export = ForeachObjects,
            .options.snow = list(progress = progress)
        ) %dopar% { # parallel loop'
            eval(parse(text = LoopText))
        } 
    }# end of parallel loop

    ## return outcome ----
    return(Return)
}