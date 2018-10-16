current_model<-""
model_setting<-NULL
model_input<-NULL
model_output_ex<-NULL
address <- "localhost:5656"
last_token<-""

thisSession <- new.env()

options(stringsAsFactors = FALSE)

thisSession$current_model<-""
thisSession$model_setting<-NULL
thisSession$model_input<-NULL

#' Checks to see if model is available in PRISM
#'
#' @param model_name tment benefit at that marker value
#' @param address Server address. Default is "localhost:5656". Could be an IP address, for example: 122.103.54.12.
#' @return 0 for sucess and 1 for error
#' @export
connect_to_model<-function(model_name, address = "localhost:5656")
{
  thisSession$url <- address
  thisSession$current_model <- model_name
  call <- paste("http://", thisSession$url, "/ocpu/library/", thisSession$current_model,"/info", sep="")
  x<-POST(call)
  if(x$status_code!=200)
  {
    message("Error connecting to model");
    return(-1);
  }
  thisSession$model_setting<<-get_default_setting()
  thisSession$model_input<<-get_default_input()
  return(0)
}


#' @export
get_output<-function()
{
#TODO
  struct<-PRISM_call("get_default_output")
  out<-struct
  for (i in length(struct))
  {
    bp<-grep("$",struct[[i]])
    if(length(bp)>0)
    {
      out[[i]]<-fromJSON(PRISM_get_res_object(object = substring(element,1,bp-1)))[[substring(element,bp+1)]]
    } else
    {
      #if(substring())
      out[[i]]<-fromJSON(PRISM_get_res_object(object = substring(element,1,bp-1)))[[substring(element,bp+1)]]
    }
  }
}


#' Returns default PRISM model input
#'
#' @return 0 for sucess and 1 for error
#' @export
get_default_input<-function()
{
  message("Current model is ", thisSession$current_model)
  PRISM_call("get_default_input", parms="")
}


#' Returns default PRISM model settings
#'
#' @return 0 for sucess and 1 for error
#' @export
get_default_setting<-function()
{
  message("Current model is ", thisSession$current_model)
  PRISM_call("get_default_setting", parms="")
}


#' Sets PRISM model settings
#'
#' @return 0 for sucess and 1 for error
#' @export
set_model_setting<-function(setting)
{
  message("Current model is ", thisSession$current_model)
  thisSession$model_setting <- setting
}


#' Sets PRISM model inputs
#'
#' @return 0 for sucess and 1 for error
#' @export
set_model_input<-function(input)
{
  message("Current model is ", thisSession$current_model)
  thisSession$model_input <- input
}


#' Returns PRISM model settings
#'
#' @return 0 for sucess and 1 for error
#' @export
get_model_setting<-function()
{
  message("Current model is ", thisSession$current_model)
  return(thisSession$model_setting)
}


#' Returns PRISM model input
#'
#' @return 0 for sucess and 1 for error
#' @export
get_model_input<-function()
{
  message("Current model is ", thisSession$current_model)
  return(thisSession$model_input)
}




#' Returns default PRISM model output
#'
#' @return 0 for sucess and 1 for error
#' @export
get_default_output<-function()
{
  message("Current model is ", thisSession$current_model)
  x<-PRISM_call("get_default_output")
  for(i in 1:length(x))
  {
    if(canbe.prism_output(x[[i]])) x[[i]]<-as.prism_output(x[[i]])
  }
  return(x)
}





draw_plots<-function(plot_number=NULL)
{
  plots<-PRISM_filter_object_list(thisSession$model_output_ex,"graphics")
  if(!is.null(plot_number)) plots<-plots[plot_number]
  for(obj in plots)
  {
    par(new=F)
    plt_data<-PRISM_get_res_object(object=obj)
    plot.new()
    rasterImage(plt_data,0,0,1,1)
  }
}



#' Executes PRISM model
#'
#' @param parms required custom parameters for current model
#' @return 0 for sucess and 1 for error
#' @export
model_run<-function(parms="")
{
  res<-PRISM_call("model_run", parms1=thisSession$model_setting, parms2=thisSession$model_input)
  thisSession$model_output_ex<<-PRISM_get_object_list()
  message("Current model is ", thisSession$current_model)
  thisSession$results<<-res
  thisSession$session_id<<-last_token
  return(res)
}



PRISM_call<-function(func,...)
{
  call <- paste("http://", address, "/ocpu/library/prismServer/R/gateway_json",...length(),sep="")
  call <- paste("http://", thisSession$url, "/ocpu/library/prismServer/R/gateway_json",...length(),sep="")
  message(paste("call is ",call))
  arg<-list(func=func, parms=...)

  x<-POST(call,body=toJSON(arg), content_type_json())

  if(x$status_code!=200 && x$status_code!=201) stop(paste("Error:"),rawToChar(as.raw(strtoi(x$content, 16L))))

  #message(paste("x statargus is",x$status_code))

  token<-x$headers$'x-ocpu-session'
  last_token<<-token

  #message(paste("token is:",token))

  url<-paste("http://", address, "/ocpu/tmp/",token,"/R/.val",sep="")
  #message(url)
  url<-paste("http://", thisSession$url, "/ocpu/tmp/",token,"/R/.val",sep="")
  message(url)
  get <- url

  y<-GET (get)

  if(y$status_code!=200 && y$status_code!=201) stop(paste("Error:"),rawToChar(as.raw(strtoi(y$content, 16L))))

  res<-process_json(y)

  return(res)
}



PRISM_get_object_list<-function(token=last_token)
{

  call <- paste("http://", address, "/ocpu/tmp/",token,"/",sep="")
  #message(paste("call is ",call))

  x<-GET(call)

  if(x$status_code!=200 && x$status_code!=201) stop(paste("Error:"),rawToChar(as.raw(strtoi(x$content, 16L))))

  str<-content(x)
  con<-textConnection(str)
  lines<-readLines(con)
  close(con)

  return(lines)
}



PRISM_filter_object_list<-function(object_list,type="")
{
  if(type=="") return(l)
  return(object_list[which(substring(object_list,1,nchar(type))==type)])
}



PRISM_get_res_object<-function(token=last_token,object)
{
  call <- paste("http://", address, "/ocpu/tmp/",token,"/",object,sep="")
  #message(paste("call is ",call))

  x<-content(GET(call))

  #if(x$status_code!=200 && x$status_code!=201) stop(paste("Error:"),rawToChar(as.raw(strtoi(x$content, 16L))))

  return(x)
}




fetch_model_output<-function(po)
{
  if(substring(po$source,1,1)=="$")
  {
    po$value<-thisSession$results[[substring(po$source,2)]]
  }
  if(po$type=="graphic/url")
  {
    plots<-PRISM_filter_object_list(thisSession$model_output_ex,"graphics")
    plots<-plots[as.numeric(po$source)]
    po$value<-PRISM_get_res_object(token=thisSession$session_id, object=paste("graphics/",po$source,sep=""))
    par(new=F)
    plot.new()
    rasterImage(po$value,0,0,1,1)
  }

  return(po)
}



fetch_model_outputs<-function()
{
  pos<-get_default_output()
  for(i in 1:length(pos))
    pos[[i]]<-fetch_model_output(pos[[i]])
  return(pos)
}






#' @export
process_json<-function(y)
{
  return(fromJSON(content(y),simplifyMatrix=FALSE))
}










###################Prism objects#################
prism_input <- function(value, type="guess", group="", default=NULL, range=c(NULL,NULL), title="", description="")
{
  me <- list(
    value=value,
    type = type,
    default = default,
    range=range
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

canbe.prism_output<-function(...)
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






