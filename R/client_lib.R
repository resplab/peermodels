address <- "localhost:5656"
#address <- "142.103.58.49"
options(stringsAsFactors = FALSE)

current_model<-""
model_setting<-NULL
model_input<-NULL


#' @export
connect_to_model<-function(model_name)
{
  current_model<<-model_name
  call <- paste("http://", address, "/ocpu/library/",current_model,"/info",sep="")
  x<-POST(call)
  if(x$status_code!=200)
  {
    message("Error connecting to model"); return(-1);
  }
  return(0)
}


#' @export
get_default_input<-function()
{
  PRISM_call("get_default_input",parms="")
}


#' @export
get_default_setting<-function()
{
  PRISM_call("get_default_setting",parms="")
}


#' @export
set_model_setting<-function(setting)
{
  model_setting<<-setting
}


#' @export
set_model_input<-function(input)
{
  model_input<<-input
}



#' @export
get_model_setting<-function()
{
  return(model_setting)
}


#' @export
get_model_input<-function()
{
  return(model_input)
}



#' @export
model_run<-function(parms="")
{
  return(PRISM_call("model_run",parms1=model_setting,parms2=model_input))
}


#' @export
PRISM_call<-function(func,...)
{

  call <- paste("http://", address, "/ocpu/library/prismServer/R/gateway_json",...length(),sep="")
  message(paste("call is ",call))

  arg<-list(func=func,parms=...)

  x<-POST(call,body=toJSON(arg), content_type_json())

  if(x$status_code!=200 && x$status_code!=201) stop(paste("Error:"),rawToChar(as.raw(strtoi(x$content, 16L))))

  #message(paste("x statargus is",x$status_code))

  token<-x$headers$'x-ocpu-session'

  url<-paste("http://", address, "/ocpu/tmp/",token,"/R/.val",sep="")
  message(url)
  get <- url

  y<-GET (get)

  if(y$status_code!=200 && y$status_code!=201) stop(paste("Error:"),rawToChar(as.raw(strtoi(y$content, 16L))))

  res<-process_json(y)

  return(res)
}



#' @export
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















