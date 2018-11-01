#Version 2018.11.01


prism_input <- function(value, type="guess", group="", default=NULL, range=c(NULL,NULL), title="", description="", control="")
{
  me <- list(
    value=value,
    type = type,
    default = default,
    range=range,
    title=title,
    description=description,
    control=control
  )

  if(is.vector(value)) {if(length(value)==1) me$type<-"scalar" else me$type<-"vector"}
  if(is.matrix(value)) me$type<-"matrix"

  ## Set the name for the class
  class(me) <- append(class(me),"prism_input")
  return(me)
}





summary <- function(x)
{
  UseMethod("summary",x)
}
summary.prism_input<-function(x)
{
  return(x$value)
}




print <- function(x)
{
  UseMethod("print",x)
}
print.prism_input<-function(x)
{
  print.listof(x)
}



Ops<-function(e1,e2)
{
  UseMethod("Ops",x)
}
Ops.prism_input<-function(e1,e2)
{
  source<-NULL;
  dest<-NULL;
  if(sum(class(e1)=="prism_input")>0) {source<-e1; e1<-e1$value}
  if(sum(class(e2)=="prism_input")>0) {dest<-e2; e2<-e2$value}
  val<-NextMethod(.Generic)
  if(is.null(source)) return(val) else {source$value<-val; return(source)}
}


Math<-function(x,...)
{
  UseMethod("Math",x,...)
}
Math.prism_input<-function(x,...)
{
  source<-NULL;
  dest<-NULL;
  if(sum(class(x)=="prism_input")>0) {source<-x; x<-x$value}
  val<-NextMethod(.Generic)
  if(is.null(source)) return(val) else {source$value<-val; return(source)}
}



Summary<-function(...,na.rm)
{
  UseMethod("Summary",...,na.rm)
}
Summary.prism_input<-function(...,na.rm)
{
  args<-list(...)
  args <- lapply(args, function(x) {
    if(sum(class(x)=="prism_input")>0) x$value
  })
  do.call(.Generic, c(args, na.rm = na.rm))
}



#' @export
canbe_prism_input<-function(...)
{
  y<-prism_input(0)
  xn<-sort(names(...))
  yn<-sort(names(y))
  if(length(xn)==length(yn) && sum(xn==yn)==length(xn)) return(T) else return(F)
}



to_prism_input<-function(x)
{
  if(is.list(x))
  {
    out<-prism_input(value=x$value)
    for(nm in names(out))
      if(!is.null(x[nm])) out[nm]<-x[nm]
      return(out)
  }
  return(prism_input(x))
}













#type=c("scalar","vector","matrix","data.frame","graphic/url","graphic/data","file/url","file/data")
prism_output <- function(title="", type="numeric", source="", group="", value=NULL, description="")
{
  me <- list(
    type = type,
    source = source,
    group=group,
    value=value,
    title=title,
    description=description
  )

  ## Set the name for the class
  class(me) <- append(class(me),"prism_output")
  return(me)
}






as.prism_output<-function(...)
{
  x<-list(...)[[1]]
  out<-prism_output()
  for(i in 1:length(x))
  {
    if(length(x[[i]])>0) out[names(x)[i]]<-x[[i]]
  }
  return(out)
}

canbe_prism_output<-function(...)
{
  y<-prism_output()
  xn<-sort(names(...))
  yn<-sort(names(y))
  if(length(xn)==length(yn) && sum(xn==yn)==length(xn)) return(T) else return(F)
}

print.prism_output<-function(x)
{
  if(length(x$value)>100) x$value=paste("[Data of length",length(x$value),"]")
  dput(unclass(x))
}




