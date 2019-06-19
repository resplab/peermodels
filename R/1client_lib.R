thisSession <- new.env()



on_load<-function()
{
  thisSession <- new.env()
  options(stringsAsFactors = FALSE)
}




check_model<-function(model_name)
{
  call <- paste("http://", thisSession$url, "/ocpu/library/", thisSession$current_model,"/info", sep="")
  x<-POST(call)
  if(x$status_code!=200)
  {
    message("Error connecting to model");
    return(NULL);
  }
  else
  {
    return(0)
  }
}






#' Checks to see if model is available in PRISM
#'
#' @param model_name tment benefit at that marker value
#' @param address Server address. Default is "prism.resp.core.ubc.ca". Could be an IP address, for example: 122.103.54.12.
#' @return 0 for sucess and 1 for error
#' @export
connect_to_model<-function(model_name, api_key="", address = "prism.resp.core.ubc.ca")
#TODO: http:// at the beginning can be optional. Currently it must be absent otherwise error!;
{
  on_load()
  thisSession$api_key<-api_key
  thisSession$session_id<-NULL
  thisSession$url <- address
  thisSession$current_model <- model_name

  x<-PRISM_call("connect_to_model",api_key=api_key)

  res<-process_input(x)

  if(res$error_code!=0) {message("There was an error in connecting to the model."); return(res$error_code) }

  thisSession$session_id<-res$session_id

  message(res$description)

  return(res)

  #thisSession$default_input<-get_default_input()
  #thisSession$output_structure<-get_output_structure()
  #return(0)
}








#' @export
disconnect_from_model<-function()
{
  #fF this is a sessioned connection then the session id will be automatically passed so do not have to submit it!
  x<-PRISM_call("disconnect_from_model")

  on_load()

  res<-process_input(x)

  return(res)
}











#' Returns default PRISM model input
#'
#' @export
get_default_input<-function()
{
  x<-PRISM_call("get_default_input")
  return(x)
}





#' Returns default PRISM model input
#'
#' @export
get_default_input_style<-function()
{
  message("Current model is ", thisSession$current_model)
  x<-PRISM_call("get_default_input_style")
  out<-list()
  for (i in 1:length(x))
    out[[names(x[i])]]<-to_prism_input(x[[i]])
  return(out)
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






#' @export
get_plots<-function()
{
  if(is.null(thisSession$output_list)) thisSession$output_list<-PRISM_get_output_object_list()

  plots<-PRISM_filter_output_object_list(thisSession$output_list,"graphics")

  out<-list()
  counter<-1;
  for(obj in plots)
  {
    source<-paste("http://", thisSession$url, "/ocpu/tmp/",thisSession$output_location,"/",obj,sep="")
    out[[counter]]<-prism_output(type="graphics/url",source = source)
    counter<-counter+1
  }

  return(out)
}




#' @export
draw_plots<-function(plot_number=NULL)
{
  if(is.null(thisSession$output_list)) thisSession$output_list<-PRISM_get_output_object_list()

  plots<-PRISM_filter_output_object_list(thisSession$output_list,"graphics")

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
#' @param input required custom parameters for current model
#' @return 0 for sucess and 1 for error
#' @export
model_run<-function(input=NULL)
{
  thisSession$input<-input

  res<-PRISM_call("prism_model_run", model_input=input)

  thisSession$output_location<-thisSession$last_location
  thisSession$output_list<-NULL

  thisSession$result<-res

  #thisSession$model_output_objects<-PRISM_get_output_object_list()

  #if(is.null(thisSession$output_structure))
  #{
  #  thisSession$output_structure<-generate_default_output_structure()
  #}

  #thisSession$output<-fetch_outputs()

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




#' @export
PRISM_call<-function(func,...)
{
  call <- paste("http://", thisSession$url, "/ocpu/library/",thisSession$current_model,"/R/gateway/json",sep="")
  message("Current model is ", thisSession$current_model)

  arg<-list(func=func, parms=...)

  #If session id is available, use it; otherwise use API key itself.
  if(!is.null(thisSession$session_id) && thisSession$session_id!="")
  {
    arg<-c(session_id=thisSession$session_id,arg)
  }
  else
  {
    if(is.null(arg$api_key)) arg<-c(api_key=thisSession$api_key,arg)
  }

  message(paste("call is ", call))

  x<-POST(call,body=toJSON(arg), content_type_json())

  if(x$status_code!=200 && x$status_code!=201)
  {
    message(paste("Error:"),rawToChar(as.raw(strtoi(x$content, 16L))))
    thisSession$last_call_status<-status_code
    return(NULL)
  }

  location<-x$headers$'x-ocpu-session'
  thisSession$last_location<-location

  res<-fromJSON(content(x)[[1]])

  return(res)
}








PRISM_get_output_object_list<-function(location=thisSession$output_location)
{
  call <- paste("http://", thisSession$url, "/ocpu/tmp/",location,"/",sep="")
  message(paste("call is ",call))

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
  if(type=="") return(object_list)
  return(object_list[which(substring(object_list,1,nchar(type))==type)])
}



PRISM_get_output_object<-function(location=thisSession$output_location,object)
{
  call <- paste("http://", thisSession$url, "/ocpu/tmp/",location,"/",object,sep="")
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
    plots<-PRISM_filter_output_object_list(thisSession$output_list,"graphics")
    plots<-plots[as.numeric(po$source)]
    po$value<-PRISM_get_output_object(location=thisSession$output_location, object=paste("graphics/",po$source,sep=""))
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




#' @export
hand_wave<-function(server,model_name)
{
  call <- paste("http://", server, "/ocpu/library/",model_name,"/R/test",sep="")
  message(paste("call is ", call))

  x<-POST(call,body="", content_type_json())

  if(x$status_code!=200 && x$status_code!=201)
  {
    message(paste("Error:"),rawToChar(as.raw(strtoi(x$content, 16L))))
    return(FALSE)
  }

  return(x)
}





