prism_input <- function(type="", group="", default_value=NULL, limit=c(NULL,NULL), limit_type=c("range","single","multiple"), title="", description="", control="")
{
  me <- list(
    type = type,
    default_value = default_value,
    limit=limit,
    limit_type=limit_type,
    title=title,
    description=description,
    control=control
  )

  if(type=="")  me$type<-guess_prism_input_type(me)

  ## Set the name for the class
  class(me) <- append(class(me),"prism_input")
  return(me)
}


guess_prism_input_type<-function(p_inp)
{
  if(is.numeric(p_inp$value)) type="numeric" else type="string"
  if(is.vector(p_inp$value)) {if(length(p_inp$value)<=1) type<-paste(type,"/scalar",sep="") else type<-paste(type,"/vector",sep="")}
  if(is.matrix(p_inp$value)) {type<-paste(type,"/matrix",sep="")}
  return(type)
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


canbe_prism_input<-function(...)
{
  y<-prism_input(0)
  xn<-sort(names(...))
  yn<-sort(names(y))
  if(length(xn)==length(yn) && sum(xn==yn)==length(xn)) return(T) else return(F)
}



to_prism_input<-function(x)
#x is a list which hopefully has all that is needed!
{
  if(is.list(x))
  {
    out<-prism_input()
    for(nm in names(out)) out[[nm]]<-x[[nm]]
    return(out)
  }
  else
    out<-prism_input(default_value = x)
}



prism_output_types<-c("numeric/scalar","numeric/vector","numeric/matrix","string/scalar","string/vector","string/matrix","file/csv","graphics/url","graphics/data")
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


to_prism_output<-function(x)
{
  if(is.list(x))
  {
    out<-prism_output()
    for(nm in names(out))
      if(!is.null(x[nm])) out[nm]<-x[nm]
      return(out)
  }
  return(prism_output(x))
}


