rcloud.enviewer.refresh <- function()
  rcloud.enviewer.on.change(.GlobalEnv)

## OCAP
rcloud.enviewer.view.dataframe <- function(expr)
  View(get(expr, .GlobalEnv))

## -- how to handle each group --
rcloud.enviewer.display.dataframe <- function(x)
  structure(list(command="view", object=x), class="data")


rcloud.enviewer.display.value <- function(val) {
is.POSIXobj <- function(POSIXobj)inherits(POSIXobj, "Date") || inherits(POSIXobj, "POSIXlt") || inherits(POSIXobj, "POSIXct") || inherits(POSIXobj, "POSIXt") 

vecORscalar <- function (val) {
         if (is.null(attributes(val))){ret <- typeof(val) %in% c('integer','complex','logical','double','NULL','character') 
            return(ret)} else {FALSE}}

type <- function(obj){
        tryCatch(
        if(vecORscalar(obj) || is.language(obj) || is.symbol(obj)) obj.class <- typeof(obj)
        else obj.class <- class(obj)[1],error = function(e) (obj.class <- "unknown"))
    return(obj.class)}

get.value <- function(val){
        if (is.POSIXobj(val)) return(format(val))
            else if (is.matrix(val)) return(paste0("1st Row: ", paste0(val[1,],collapse=", ")))
            else if (is.symbol(val)) return(as.character(sym))
            else if (is.language(val)) return(as.character(val)) 
            else if (is.list(val)){
                if(!is.null(names(val))) return(paste0("Named List: ", paste0(names(val),collapse=", ")))
                else return(capture.output(str(val)))}
            else if (vecORscalar(val)){
                if(length(val) == 0) return(as.character("empty"))
                else if (length(val) == 1) return(deparse(val))
                else if (length(val) > 1) return(as.character(paste0(val,collapse=", ")))
            }
            else return(paste(capture.output(str(val)), collapse=' '))
            }

get.dimensions <- function(val){if(is.null(dim(val))) paste0("[1:",length(val),"]")
     else paste0("[",paste0(dim(val),collapse=":"),"] ")}
            
str <- tryCatch(get.value(val),error = function(e){})
if (any(too.long <- (nchar(str) > 100)))
         str[too.long] <- paste(substr(str[too.long], 1, 100), "...")
     if (length(str) > 1L) str <- paste(str, collapse='\n')

dimensions <- try(get.dimensions(val),silent=TRUE)

structure(list(type=paste(type(val),dimensions), value=str), class="values")
}

rcloud.enviewer.display.function <- function(f)
    structure(list(type="function", value=deparse(args(f))[1]), class="functions")

## retrieve objects and format them
rcloud.enviewer.build <- function(vars, env) {
    ret <- lapply(vars, function(x) {
        val <- get(x, envir=env)
        if (is.data.frame(val)) {
            rcloud.enviewer.display.dataframe(x)
        } else if (is.function(val)) {
            rcloud.enviewer.display.function(val)
        } else
            rcloud.enviewer.display.value(val)
    })
    names(ret) <- vars
    ## re format to what the UI expects and split by group
    split(ret, factor(sapply(ret, class), levels=c("data", "functions", "values")))
}


rcloud.enviewer.on.change <- function(env)
{
    ret <- rcloud.enviewer.build(ls(envir=env), env)
    rcloud.enviewer.caps$display(ret)
}
