library(httr)
library(jsonlite)

thisSession <- new.env()

on_load<-function()
{
  options(stringsAsFactors = FALSE)
  thisSession$current_model<-""
  thisSession$model_input<-NULL
}


#' Checks to see if model is available in PRISM
#'
#' @param model_name tment benefit at that marker value
#' @param address Server address. Default is "localhost:5656". Could be an IP address, for example: 122.103.54.12.
#' @return 0 for sucess and 1 for error
#' @export
connect_to_model<-function(model_name, address = "localhost:5656")
{
  on_load()
  thisSession$url <- address
  thisSession$current_model <- model_name
  call <- paste("http://", thisSession$url, "/ocpu/library/", thisSession$current_model,"/info", sep="")
  x<-POST(call)
  if(x$status_code!=200)
  {
    message("Error connecting to model");
    return(-1);
  }
  thisSession$model_input<-get_default_input()
  return(0)
}









#' Returns default PRISM model input
#'
#' @return 0 for sucess and 1 for error
#' @export
get_default_input<-function()
{
  message("Current model is ", thisSession$current_model)
  x<-PRISM_call("get_default_input", parms="")
  process_input(x)
}





process_input<-function(inp)
{
  if(canbe_prism_input(inp)) return(to_prism_input(inp))
  out<-list()
  for(nm in names(inp))
  {
    if(canbe_prism_input(inp[[nm]]))
      out[[nm]]<-to_prism_input(inp[[nm]])
    else
    {
      if(is.list(inp[[nm]])) out[[nm]]<-process_input(inp[[nm]]) else out[[nm]]<-inp[[nm]]
    }
  }
  return(out)
}





unprocess_input<-function(inp)
{
  if(inherits(inp,"prism_input")) return(inp$value)
  out<-list()
  for(nm in names(inp))
  {
    if(inherits(inp[[nm]],"prism_input"))
      out[[nm]]<-inp[[nm]]$value
    else
    {
      if(is.list(inp[[nm]])) out[[nm]]<-unprocess_input(inp[[nm]]) else out[[nm]]<-inp[[nm]]
    }
  }
  return(out)
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
    if(canbe_prism_output(x[[i]])) x[[i]]<-as.prism_output(x[[i]])
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
model_run<-function()
{
  res<-PRISM_call("model_run", parms1=unprocess_input(thisSession$model_input))
  thisSession$session_id<-thisSession$last_token
  thisSession$model_output_ex<-PRISM_get_object_list()
  message("Current model is ", thisSession$current_model)
  thisSession$results<-res
  return(res)
}





PRISM_call<-function(func,...)
{
  call <- paste("http://", thisSession$url, "/ocpu/library/prismServer/R/gateway_json",...length(),sep="")
  message(paste("call is ",call))
  arg<-list(func=func, parms=...)

  x<-POST(call,body=toJSON(arg), content_type_json())

  if(x$status_code!=200 && x$status_code!=201) stop(paste("Error:"),rawToChar(as.raw(strtoi(x$content, 16L))))

  #message(paste("x statargus is",x$status_code))

  token<-x$headers$'x-ocpu-session'
  thisSession$last_token<-token

  #message(paste("token is:",token))

  url<-paste("http://", thisSession$url, "/ocpu/tmp/",token,"/R/.val",sep="")
  message(url)
  get <- url

  y<-GET (get)

  if(y$status_code!=200 && y$status_code!=201) stop(paste("Error:"),rawToChar(as.raw(strtoi(y$content, 16L))))

  res<-process_json(y)

  return(res)
}





PRISM_call_s<-function(session,func,...)
{
  call <- paste("http://", address, "/ocpu/library/prismServer/R/gateway_json",...length(),sep="")
  call <- paste("http://", thisSession$url, "/ocpu/library/prismServer/R/gateway_json",...length(),"_s",sep="")
  message(paste("call is ",call))
  arg<-list(session=session, func=func, parms=...)

  x<-POST(call,body=toJSON(arg), content_type_json())

  if(x$status_code!=200 && x$status_code!=201) stop(paste("Error:"),rawToChar(as.raw(strtoi(x$content, 16L))))

  #message(paste("x statargus is",x$status_code))

  token<-x$headers$'x-ocpu-session'
  thisSession$last_token<-token

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





PRISM_get_object_list<-function(token=thisSession$session_id)
{

  call <- paste("http://", thisSession$url, "/ocpu/tmp/",token,"/",sep="")
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



PRISM_get_res_object<-function(token=thisSession$session_id,object)
{
  call <- paste("http://", thisSession$url, "/ocpu/tmp/",token,"/",object,sep="")
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
    #par(new=F)
    #plot.new()
    #rasterImage(po$value,0,0,1,1)
    class(po)<-"prism_output"
  }

  return(po)
}






fetch_model_outputs<-function()
{
  pos<-get_default_output()

  for(i in 1:length(pos))
  {
    if(canbe_prism_output(pos[[i]]))
    {
      pos[[i]]<-fetch_model_output(pos[[i]])
    }
    else
      if(is.list(pos[[i]]))
      {
        pos[[i]]<-fetch_model_outputs_l2(pos[[i]])
      }
      else pos[[i]]<-fetch_model_output(pos[[i]])
  }

  return(pos)
}


fetch_model_outputs_l2<-function(po)
{
  out<-po
  for(i in 1:length(out))
  {
    if(canbe_prism_output(out[[i]]))
    {
      out[[i]]<-fetch_model_output(out[[i]])
    }
    else
      if(is.list(out[[i]]))
      {
        out[[i]]<-fetch_model_outputs_l2(out[[i]])
      }
    else out[[i]]<-fetch_model_output(out[[i]])
  }

  return(out)
}






process_json<-function(y)
{
  return(fromJSON(content(y),simplifyMatrix=FALSE))
}



#' @export
get_session_info<-function()
{
  return(thisSession)
}


#' @export
temp<-function()
{
  return(getwd())
}









