
options(stringsAsFactors = FALSE)

current_model<-""
model_setting<-NULL
model_input<-NULL
model_output_ex<-NULL
address <- "localhost:5656"
last_token<-""


#' @export
connect_to_model<-function(model_name)
{
  current_model <- model_name
  call <- paste("http://", address, "/ocpu/library/",current_model,"/info",sep="")
  x<-POST(call)
  if(x$status_code!=200)
  {
    message("Error connecting to model"); return(-1);
  }
  model_setting<<-get_default_setting()
  model_input<<-get_default_input()
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


draw_plots<-function(plot_number=NULL)
{
  plots<-PRISM_filter_object_list(model_output_ex,"graphics")
  if(!is.null(plot_number)) plots<-plots[plot_number]
  for(obj in plots)
  {
    par(new=F)
    plt_data<-PRISM_get_res_object(object=obj)
    plot.new()
    rasterImage(plt_data,0,0,1,1)
  }
}



#' @export
model_run<-function(parms="")
{
  res<-PRISM_call("model_run", parms1=model_setting, parms2=model_input)
  model_output_ex<<-PRISM_get_object_list()
  return(res)
}


#' @export
PRISM_call<-function(func,...)
{

  call <- paste("http://", address, "/ocpu/library/prismServer/R/gateway_json",...length(),sep="")
  #message(paste("call is ",call))

  arg<-list(func=func,parms=...)

  x<-POST(call,body=toJSON(arg), content_type_json())

  if(x$status_code!=200 && x$status_code!=201) stop(paste("Error:"),rawToChar(as.raw(strtoi(x$content, 16L))))

  #message(paste("x statargus is",x$status_code))

  token<-x$headers$'x-ocpu-session'
  last_token<<-token

  #message(paste("token is:",token))

  url<-paste("http://", address, "/ocpu/tmp/",token,"/R/.val",sep="")
  #message(url)
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















