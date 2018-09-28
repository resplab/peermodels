#address <- "localhost:5656"
#address <- "142.103.58.49"
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
  return(0)
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



#' Executes PRISM model
#'
#' @param parms required custom parameters for current model
#' @return 0 for sucess and 1 for error
#' @export
model_run<-function(parms="")
{
  message("Current model is ", thisSession$current_model)
  return(PRISM_call("model_run", parms1=thisSession$model_setting, parms2=thisSession$model_input))
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

  url<-paste("http://", thisSession$url, "/ocpu/tmp/",token,"/R/.val",sep="")
  message(url)
  get <- url

  y<-GET (get)

  if(y$status_code!=200 && y$status_code!=201) stop(paste("Error:"),rawToChar(as.raw(strtoi(y$content, 16L))))

  res<-process_json(y)

  return(res)
}



process_json<-function(y)
{
  return(fromJSON(content(y),simplifyMatrix=FALSE))
}


#Depracated
process_R<-function(y)
{
  z<-rawToChar(as.raw(strtoi(y$content, 16L)))

  poss<-c(1,as.vector(gregexpr("\r\n",z)[[1]]))

  str<-""
  n_line<-length(poss)
  for(i in 1:(n_line-1))
  {
    line<-substring(z,poss[i],poss[i+1]-1)
    pos <- gregexpr('\"', line)
    line<-substr(line, pos[[1]][1]+1, pos[[1]][length(pos[[1]])]-1)
    line<-gsub( "\\", "", line, fixed=TRUE)
    str<-paste(str,line,sep="")
  }

  res<-eval(parse(text=str))
  return(res)
}















