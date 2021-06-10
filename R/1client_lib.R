thisSession <- new.env()


on_load<-function()
{
  thisSession <- new.env()
  options(stringsAsFactors = FALSE)
}



handshake<-function(model_name, api_key="", local_server = FALSE, bypass_router = FALSE, async = FALSE)
{
  model_name <- str_remove(model_name, "Prism")

  if (!local_server && !bypass_router)  {address <- paste0("https://prism.peermodelsnetwork.com/route/", model_name, "/run")
  addressObj <- paste0("https://prism.peermodelsnetwork.com/route/", model_name, "/tmp/")}

  if (!local_server && bypass_router)  {address <- paste0("http://model-", model_name, ".cp.prism-ubc.linaralabs.com/ocpu/library/", model_name, "Prism/R/gateway/json")
  addressObj <- paste0("http://model-", model_name, ".cp.prism-ubc.linaralabs.com/ocpu/tmp/")}

  if (local_server) {address <- paste0("http://localhost:5656/ocpu/library/", model_name,"Prism/R/gateway/json" )
  addressObj <- paste0("http://localhost:5656/ocpu","/tmp/" )}

  if (!local_server && async && bypass_router)  {address <- paste0("http://model-", model_name, ".cp.prism-ubc.linaralabs.com/ocpu/library/", model_name, "Prism/R/gatewayasync/json")
  addressObj <- paste0("http://model-", model_name, ".cp.prism-ubc.linaralabs.com/ocpu/tmp/")}

  if (!local_server && async && !bypass_router)  {address <- paste0("https://prism.peermodelsnetwork.com/route/", model_name, "/async/run")
  addressObj <- paste0("https://prism.peermodelsnetwork.com/route/", model_name, "/tmp/")} #TODO check addressObj for async

  if (local_server && async) {address <- paste0("http://localhost:5656/ocpu/library/", model_name,"Prism/R/gatewayasync/json" )
  addressObj <- paste0("http://localhost:5656/ocpu","/tmp/" )}


  on_load()
  thisSession$api_key<-api_key
  thisSession$session_id<-NULL
  thisSession$url <- address
  thisSession$urlObj <- addressObj
  thisSession$current_model <- model_name



  x<-PRISM_call("connect_to_model",api_key=api_key)

  res<-process_input(x)

  if(res$error_code!=0) {message("Error: Could not connect to the model."); return(res$error_code) }

  thisSession$session_id<-res$session_id

  message(res$description)

  return(res)

  #thisSession$default_input<-get_default_input()
  #thisSession$output_structure<-get_output_structure()
  #return(0)
}


#' Checks to see if model is available in PRISM
#'
#' @param model_name name of the model
#' @param api_key API key
#' @param local_server whether or not the call should be directed to the server on localhost. Default is FALSE.
#' @param bypass_router bypass server API router, for debugging purposes
#' @param async should the model be called in async mode?
#' @return 0 for success and 1 for error
#' @export
connect_to_model<-function(model_name, api_key="", local_server = FALSE, bypass_router = FALSE, async = FALSE)
{
  .Deprecated("model_run")
  model_name <- str_remove(model_name, "Prism")

  if (!local_server && !bypass_router)  {address <- paste0("https://prism.peermodelsnetwork.com/route/", model_name, "/run")
  addressObj <- paste0("https://prism.peermodelsnetwork.com/route/", model_name, "/tmp/")}

  if (!local_server && bypass_router)  {address <- paste0("http://model-", model_name, ".cp.prism-ubc.linaralabs.com/ocpu/library/", model_name, "Prism/R/gateway/json")
  addressObj <- paste0("http://model-", model_name, ".cp.prism-ubc.linaralabs.com/ocpu/tmp/")}

  if (local_server) {address <- paste0("http://localhost:5656/ocpu/library/", model_name,"Prism/R/gateway/json" )
  addressObj <- paste0("http://localhost:5656/ocpu","/tmp/" )}

  if (!local_server && async && bypass_router)  {address <- paste0("http://model-", model_name, ".cp.prism-ubc.linaralabs.com/ocpu/library/", model_name, "Prism/R/gatewayasync/json")
  addressObj <- paste0("http://model-", model_name, ".cp.prism-ubc.linaralabs.com/ocpu/tmp/")}

  if (!local_server && async && !bypass_router)  {address <- paste0("https://prism.peermodelsnetwork.com/route/", model_name, "/async/run")
  addressObj <- paste0("https://prism.peermodelsnetwork.com/route/", model_name, "/tmp/")} #TODO check addressObj for async

  if (local_server && async) {address <- paste0("http://localhost:5656/ocpu/library/", model_name,"Prism/R/gatewayasync/json" )
  addressObj <- paste0("http://localhost:5656/ocpu","/tmp/" )}


  on_load()
  thisSession$api_key<-api_key
  thisSession$session_id<-NULL
  thisSession$url <- address
  thisSession$urlObj <- addressObj
  thisSession$current_model <- model_name



  x<-PRISM_call("connect_to_model",api_key=api_key)

  res<-process_input(x)

  if(res$error_code!=0) {message("Error: Could not connect to the model."); return(res$error_code) }

  thisSession$session_id<-res$session_id

  message(res$description)

  return(res)

  #thisSession$default_input<-get_default_input()
  #thisSession$output_structure<-get_output_structure()
  #return(0)
}



#' Returns default PRISM model input
#'
#' @param model_name name of the model
#' @param api_key API key
#' @param local_server whether or not the call should be directed to the server on localhost. Default is FALSE.
#' @param bypass_router bypass server API router, for debugging purposes
#' @export
get_default_input<-function(model_name=NULL, api_key="", local_server=FALSE, bypass_router=FALSE)
{
  handshake(model_name = model_name, api_key = api_key, local_server = local_server, bypass_router = bypass_router, async = FALSE)
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



#' #' Sets PRISM model inputs
#' #'
#' #' @return 0 for success and 1 for error
#' #' @export
#' set_model_input<-function(input)
#' {
#'   message("Current model is ", thisSession$current_model)
#'   thisSession$input <- input
#' }

#'
#' #' Returns PRISM model input
#' #'
#' #' @return 0 for success and 1 for error
#' #' @export
#' get_model_input<-function()
#' {
#'   message("Current model is ", thisSession$current_model)
#'   return(thisSession$input)
#' }
#'



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





#' Retrieves plots generated by the model in R Session
#' @return URL of plots
#' @export
get_plots<-function()
{
  if(is.null(thisSession$output_list)) thisSession$output_list<-PRISM_get_output_object_list()

  plots<-PRISM_filter_output_object_list(thisSession$output_list,"graphics")

  out<-list()
  counter<-1;
  for(obj in plots)
  {
    source<-paste(thisSession$urlObj,thisSession$output_location,"/",obj,sep="")
    out[[counter]]<-prism_output(type="graphics/url",source = source)
    counter<-counter+1
  }

  return(out)
}



#' draws plots generated by the model in R Session
#' @param plot_number the number of the plot to be rendered
#' @return graphical object in R
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
#' @param model_name name of the model
#' @param api_key API key
#' @param local_server whether or not the call should be directed to the server on localhost. Default is FALSE.
#' @param bypass_router bypass server API router, for debugging purposes
#' @param async should the model be called in async mode?
#' @return 0 for success and 1 for error
#' @export
model_run<-function(input=NULL, model_name=NULL, api_key = "", local_server=FALSE, bypass_router=FALSE, async=FALSE)
{
  handshake(model_name = model_name, api_key = api_key, local_server = local_server, bypass_router = bypass_router, async = async)

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
  call <- thisSession$url
  message(paste0("Selected model is ", thisSession$current_model))

  arg<-list(func=func, parms=...)

  #If session id is available, use it; otherwise use API key itself.
  if(!is.null(thisSession$session_id) && thisSession$session_id!="")
  {
    arg<-c(session_id=thisSession$session_id,arg)
  }
  else
  {
    if(is.null(arg$api_key)) arg$api_key<-thisSession$api_key
  }

  message(paste0("Calling server at ", call))

  x<-POST(call, add_headers('x-prism-auth-user'=arg$api_key), body=toJSON(arg), content_type_json())

  if(x$status_code!=200 && x$status_code!=201)
  {
    message(paste("Error:"),rawToChar(as.raw(strtoi(x$content, 16L))))
    thisSession$last_call_status<-status_code
    return(NULL)
  }

  if (str_detect(thisSession$url, "https://admin-prism-api.cp.prism-ubc.linaralabs.com/route/")) {
    location<-x$headers$'x-prism-session-id'
    }

    else {
      (location<-x$headers$'x-ocpu-session')
    }

  thisSession$last_location<-location

  res<-fromJSON(content(x)[[1]])

  return(res)
}



PRISM_get_output_object_list<-function(location=thisSession$output_location)
{
  message(paste0("Session ID:", location))
  call <- paste0(thisSession$urlObj, location, "/")
  message(paste0("Calling server at ", call))

  x<-GET(call, add_headers('x-prism-auth-user'=thisSession$api_key))

  if(x$status_code!=200 && x$status_code!=201) stop(paste("Error:"),rawToChar(as.raw(strtoi(x$content, 16L))))
  #x$headers$`content-type`<- "text/plain; charset=utf-8"

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
  call <- paste0(thisSession$urlObj,location,"/",object)
  #message(paste("call is ",call))

  x<-content(GET(call, add_headers('x-prism-auth-user'=thisSession$api_key)))

  #if(x$status_code!=200 && x$status_code!=201) stop(paste("Error:"),rawToChar(as.raw(strtoi(x$content, 16L))))

  return(x)
}


#' returns PRISM server session info
#' @return session ID
#' @export
get_session_info<-function()
{
  return(thisSession)
}



