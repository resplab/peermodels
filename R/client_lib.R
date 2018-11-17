library(httr)
library(jsonlite)

thisSession <- new.env()

on_load<-function()
{
  thisSession <- new.env()
  options(stringsAsFactors = FALSE)
  thisSession$current_model<-""
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
    return(NULL);
  }
  thisSession$default_input<-get_default_input()
  thisSession$output_structure<-get_output_structure()
  return(0)
}












#' Returns default PRISM model input
#'
#' @return 0 for sucess and 1 for error
#' @export
get_default_input<-function()
{
  message("Current model is ", thisSession$current_model)
  x<-PRISM_call("get_default_input")
  return(process_input(x))
}




#Evaluates if each item in the input can become a prism_input class
process_input<-function(inp)
{
  if(length(inp)==0) return(list())
  if(canbe_prism_input(inp)) return(to_prism_input(inp))
  out<-list()
  if(is.list(inp))
  {
    out<-list()
    for(i in 1:length(inp))
    {
      nm<-names(inp[i])
      element<-inp[[i]]
      if(canbe_prism_input(element))
        out[[i]]<-to_prism_input(element)
      else
      {
        if(is.list(inp[[nm]])) out[[nm]]<-process_input(inp[[nm]]) else out[[nm]]<-inp[[nm]]
      }
      names(out[i])<-nm
    }
    return(out)
  }
  else return(inp)
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
  thisSession$input <- input
}


#' Returns PRISM model input
#'
#' @return 0 for sucess and 1 for error
#' @export
get_model_input<-function()
{
  message("Current model is ", thisSession$current_model)
  return(thisSession$input)
}




#' Returns default PRISM model output
#'
#' @return 0 for sucess and 1 for error
#' @export
get_output_structure<-function()
{
  #message("Current model is ", thisSession$current_model)
  x<-PRISM_call("get_output_structure")

  if(is.null(x)) return(NULL)

  for(i in 1:length(x))
  {
    if(canbe_prism_output(x[[i]])) x[[i]]<-as.prism_output(x[[i]])
  }
  return(x)
}




show_output<-function(p_output)
{
  if(p_output$type=="graphic/data")
  {
    par(new=F)
    plot.new()
    rasterImage(p_output$value,0,0,1,1)
  }
  else
  {
    val<-p_output$value
    return(val)
  }
}


draw_plots<-function(plot_number=NULL)
{
  plots<-PRISM_filter_output_object_list(thisSession$model_output_ex,"graphics")
  if(!is.null(plot_number)) plots<-plots[plot_number]
  for(obj in plots)
  {
    par(new=F)
    plt_data<-PRISM_get_output_object(object=obj)
    plot.new()
    rasterImage(plt_data,0,0,1,1)
  }
}






#' Executes PRISM model
#'
#' @param parms required custom parameters for current model
#' @return 0 for sucess and 1 for error
#' @export
model_run<-function(input=NULL)
{
  if(is.null(input))
  {
    if(is.null(thisSession$input))
    {
      thisSession$input<-thisSession$default_input
    }
    input<-thisSession$input
  }
  else
  {
    thisSession$input<-input
  }

  res<-PRISM_call("model_run", parms1=unprocess_input(input))

  thisSession$session_id<-thisSession$last_token

  thisSession$result<-res

  thisSession$model_output_objects<-PRISM_get_output_object_list()

  if(is.null(thisSession$output_structure))
  {
    thisSession$output_structure<-generate_default_output_structure()
  }

  thisSession$output<-fetch_outputs()

  return(res)
}



generate_default_output_structure<-function()
{
  out<-list()

  for(i in 1:length(thisSession$result))
  {
    nm<-names(thisSession$result[i])
    element<-thisSession$result[[i]]

    if(is.list(element))
      out[[length(out)+1]]<-generate_default_output_structure_l2(out)
    else
    {
      if(is.null(dim(element)))
        if(length(element)==1) type<-"numeric" else type<-"vector"
        else type="matrix"
      out[[length(out)+1]]<-prism_output(title = nm, type = type, source =paste("$",nm,sep=""))
    }
    names(out)[length(out)]<-nm
  }

  plots<-PRISM_filter_output_object_list(thisSession$model_output_objects,"graphics")
  if(length(plots))
  {
    for(i in 1:length(plots))
    {
      out[[counter]]<-prism_output(title = paste("plot",i), type="graphic/url", source=toString(i))
      counter<-counter+1
    }
  }
  return(out)
}

generate_default_output_structure_l2<-function(root_element)
{
  out<-list()
  for(i in 1:length(root_element))
  {
    nm<-names(root_element[i])
    element<-root_element[[i]]

    if(is.list(element))
      out[[length(out)+1]]<-generate_default_output_structure_l2(out)
    else
    {
      if(is.null(dim(element)))
        if(length(element)==1) type<-"numeric" else type<-"vector"
      else type="matrix"
      out[length(out)+1]<-prism_output(title = nm, type = type, source =paste("$",nm,sep=""))
    }
    names(out)[length(out)]<-nm
  }






  plots<-PRISM_filter_output_object_list(thisSession$model_output_objects,"graphics")
  if(length(plots))
  {
    for(i in 1:length(plots))
    {
      out[[counter]]<-prism_output(title = paste("plot",i), type="graphic/url", source=toString(i))
      counter<-counter+1
    }
  }
  return(out)
}





PRISM_call<-function(func,...)
{
  call <- paste("http://", thisSession$url, "/ocpu/library/",thisSession$current_model,"/R/gateway_json",...length(),sep="")
  message(paste("call is ",call))
  arg<-list(func=func, parms=...)

  x<-POST(call,body=toJSON(arg), content_type_json())

  if(x$status_code!=200 && x$status_code!=201)
  {
    message(paste("Error:"),rawToChar(as.raw(strtoi(x$content, 16L))))
    thisSession$last_call_status<-status_code
    return(NULL)
  }

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
  call <- paste("http://", thisSession$url, "/ocpu/library/",thisSession$curent_model,"/R/gateway_json",...length(),"_s",sep="")
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





PRISM_get_output_object_list<-function(token=thisSession$session_id)
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



PRISM_filter_output_object_list<-function(object_list,type="")
{
  if(type=="") return(l)
  return(object_list[which(substring(object_list,1,nchar(type))==type)])
}



PRISM_get_output_object<-function(token=thisSession$session_id,object)
{
  call <- paste("http://", thisSession$url, "/ocpu/tmp/",token,"/",object,sep="")
  #message(paste("call is ",call))

  x<-content(GET(call))

  #if(x$status_code!=200 && x$status_code!=201) stop(paste("Error:"),rawToChar(as.raw(strtoi(x$content, 16L))))

  return(x)
}




fetch_output<-function(po)
{
  if(substring(po$source,1,1)=="$")
  {
    po$value<-thisSession$result[[substring(po$source,2)]]
  }
  if(po$type=="graphic/url")
  {
    plots<-PRISM_filter_output_object_list(thisSession$model_output_ex,"graphics")
    plots<-plots[as.numeric(po$source)]
    po$value<-PRISM_get_output_object(token=thisSession$session_id, object=paste("graphics/",po$source,sep=""))
    #par(new=F)
    #plot.new()
    #rasterImage(po$value,0,0,1,1)
    po$type<-"graphic/data"
    class(po)<-"prism_output"
  }

  return(po)
}






fetch_outputs<-function()
{
  pos<-thisSession$output_structure

  for(i in 1:length(pos))
  {
    if(canbe_prism_output(pos[[i]]))
    {
      pos[[i]]<-fetch_output(pos[[i]])
    }
    else
      if(is.list(pos[[i]]))
      {
        pos[[i]]<-fetch_outputs_l2(pos[[i]])
      }
      else pos[[i]]<-fetch_output(pos[[i]])
  }

  return(pos)
}


fetch_outputs_l2<-function(po)
{
  out<-po
  for(i in 1:length(out))
  {
    if(canbe_prism_output(out[[i]]))
    {
      out[[i]]<-fetch_output(out[[i]])
    }
    else
      if(is.list(out[[i]]))
      {
        out[[i]]<-fetch_outputs_l2(out[[i]])
      }
    else out[[i]]<-fetch_output(out[[i]])
  }

  return(out)
}






process_json<-function(y)
{
  return(fromJSON(content(y)))
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









