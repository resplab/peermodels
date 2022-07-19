this_session <- new.env()


on_load<-function()
{
  this_session <<- new.env()
  options(stringsAsFactors = FALSE)
}

is_response <- function(x) {
  class(x) == "httr2_response"
}

#' Returns the default server path for PRISM server
#'
#' @return the default server path for PRISM server
#' @export
default_server <- function()
{
  return("https://prism.peermodelsnetwork.com/route/")
}


make_url <- function(model_name, base_url, type=c("call","tmp","info"), async=FALSE)
{
  if(substring(base_url,nchar(base_url))=="/") base_url <- substring(base_url,1,nchar(base_url)-1)

  if(length(grep(pattern="/route",x=base_url))>0)
  {
    if(type=="call")
    {
      out <- paste0(base_url,"/",model_name,"/run")
      if (async) {out <- paste0(base_url,"/",model_name,"/async/run")}
    }
    if(type=="tmp")
    {
      out <- paste0(base_url,"/",model_name,"/tmp")
    }
    if(type=="info")
    {
      out <- paste0(base_url,"/",model_name,"")
    }
  }
  else
  {
    if(type=="call")
    {
      out <- paste0(base_url,"/library/", model_name, "/R/gateway/json")
      if (async) {out <- paste0(base_url,"/library/", model_name, "/R/gatewayasync/json")}
    }
    if(type=="tmp")
    {
      out <- paste0(base_url,"/tmp")
    }
    if(type=="info")
    {
      out <- paste0(base_url,"/library/", model_name, "/info")
    }
  }

  return(out)
}


#' returns session info
#'
#' @return returns current session variables
#' @export
get_session <- function()
{
  as.list(this_session)
}


#' resets session info and clears stored environmental variables such as API keys and server address.
#'
#' @export
reset_session <- function()
{
  rm(list=ls(this_session),envir = this_session)
}



#' checks to see if a model is available on the cloud server
#'
#' @param model_name name of the model
#' @param server base url of the server
#' @return TRUE if model is available on the server, FALSE otherwise
#' @examples
#' handshake("accept")
#' @export
handshake <- function(model_name, server=default_server())
{
  if (!has_internet()) {
    message("No internet connection.")
    return(invisible(NULL))
  }
  address <- make_url(model_name, base_url = server, type = "info")

  #fails gracefully to comply with CRAN policy
  res <- tryCatch(
    request(address) %>%
    req_error(is_error = function(resp) FALSE) %>%
    #req_throttle(10/60) %>%
    req_perform(),
    error = function(e) conditionMessage(e),
    warning = function(w) conditionMessage(w))

  # graceful fail for timeout errors
  if (!is_response(res)) {
         message(res)
         return(invisible(NULL))}

  found <- (res %>% resp_body_json())[1] == 100
  if (found) {
    return ("Model available for cloud access")

  } else {
    return ("Model not found on the server")
    }
}






#' Returns default PRISM model input
#'
#' @param model_name name of the model. If null, it will be set to the last call's value.
#' @param api_key API key. If null, it will be set to the last call's value.
#' @param server Server address. If null, it will be set to Peer Models Network PRISM server on the first run and to the last call's value on subsequent runs.
#' @return default model inputs, which can be changed and submitted to the model for a different run.
#' @examples
#' \dontrun{
#' sample_input <- get_default_input(model_name = "accept", api_key = "YOUR_API_KEY")
#' }
#' @export
get_default_input<-function(model_name=NULL, api_key=NULL, server=NULL)
{
  if(is.null(model_name)) {
    if (is.null(this_session$model_name)) stop("No model specified.")
    model_name <- this_session$model_name
    message(paste0("Calling last saved model: ", model_name))
  }

  if(is.null(api_key)) {
    if (is.null(this_session$api_key)) stop("No API key provided or saved in the session.")
    api_key <- this_session$api_key
    message ("Using stored API key.")
  }

  if(is.null(server)) server <- this_session$server
  if(is.null(server)) server <- default_server()
  this_session$server <- server

  url <- make_url(model_name,server,"call")
  default_inputs   <- prism_call(func="prism_get_default_input", base_url = url, api_key = api_key)

  this_session$model_name <- model_name
  this_session$api_key <- api_key
  this_session$server <- server

  return(default_inputs)
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









#' Retrieves plots generated by the model in R Session
#' @return URL of plots
#' @export
get_plots<-function()
{
  if(is.null(this_session$output_list)) this_session$output_list <- get_output_object_list()

  plots <- filter_output_object_list(this_session$output_list,"graphics")

  out<-list()
  counter<-1;
  for(obj in plots)
  {
    source<-paste(this_session$urlObj,this_session$output_location,"/",obj,sep="")
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
  if(is.null(this_session$output_list)) this_session$output_list<-get_output_object_list()

  plots<-filter_output_object_list(this_session$output_list,"graphics")

  if(!is.null(plot_number)) plots<-plots[plot_number]
  for(obj in plots)
  {
    par(new=F)
    plt_data<-get_output_object(object=obj)
    plot.new()
    img <- as.raster(plt_data)
    rasterImage(img,0,0,1,1)
    #require(png)
    #pic <- readPNG(plt_data)
    #plot(pic)
  }
}

validate_email <- function(email_address){
  if (is.null(email_address)) {return(FALSE)}
  return(grepl("^[[:alnum:]._-]+@[[:alnum:].-]+$", email_address))}

#' Executes PRISM model
#'
#' @param model_name name of the model
#' @param model_input required custom parameters for current model
#' @param api_key API key
#' @param server server address. Defaults to the Peer Models Network PRSIM repository.
#' @param async should the model be called in async mode?
#' @param email_address async results will be emailed to this address
#' @return 0 for success and 1 for error
#' @examples
#' \dontrun{
#' sample_input <- get_default_input(model_name = "epic", api_key = "YOUR_API_KEY")
#' sample_input$global_parameters.time_horizon <- 15
#' model_run ("epic", model_input = sample_input)
#' }
#' @export
model_run<-function(model_name=NULL, model_input=NULL, api_key = NULL, server = NULL, async=FALSE, email_address=NULL)
{
  if(is.null(model_name)) {
    if (is.null(this_session$model_name)) stop("No model specified.")
    model_name <- this_session$model_name
    message(paste0("Calling last saved model: ", model_name))
    }

  if(is.null(api_key)) {
    if (is.null(this_session$api_key)) stop("No API key provided or saved in the session.")
    api_key <- this_session$api_key
    message("Using stored API key.")
  }

  if(is.null(model_input)) message("No explicit model_input provided, the model might produce an error or revert to its default set of inputs.")

  if(is.null(server)) server <- this_session$server
  if(is.null(server)) server <- default_server()



  if(async && !validate_email(email_address)) {stop("You must provide a valid email address for asynchronous calls.")}

  address <- make_url(model_name, server, "call", async = async)

  if(is.null(email_address))
  {
    res<-prism_call("prism_model_run",  base_url = address, model_input=model_input, api_key = api_key)
  }
  else
  {
    res<-prism_call("prism_model_run",  base_url = address, model_input=model_input, api_key = api_key, email_address=email_address)
  }

  this_session$output_location<-this_session$last_location
  this_session$api_key<-api_key
  this_session$server <- server
  this_session$current_model <- model_name
  this_session$output_list <- NULL #This item needs to be retrieved explicitly by a separate call so we nullify it here

  return(res)
}





#' Generic call to PRISM server
#'
#' @param func function to call
#' @param base_url  the url to call
#' @param api_key API key
#' @param ... other parameters
#' @return processed (from JSON to R object result of the call)
#' @export
prism_call<-function(func, base_url, api_key = NULL, ...)
{
  if (!has_internet()) {
    message("No internet connection.")
    return(invisible(NULL))
  }
  if(is.null(api_key)) api_key <- this_session$api_key
  if (is.null(api_key)) stop ("No API key provided.")

  message(paste0("Calling server at ", base_url))
  arg <- list(func=func,param=...)

  res <- request(base_url) %>%
    req_headers("x-prism-auth-user"=api_key) %>%
    req_body_json(arg) %>%
    req_error(is_error = function(resp) FALSE) %>%
    req_throttle(10/60) %>%
    req_perform()

  this_session$last_location <- res$headers$'x-ocpu-session'
  if(!is.null(api_key)) this_session$api_key <- api_key
  resObject <-(res %>% resp_body_json())[[1]]
  if (!validate(as.character(resObject))) {stop("Non-standard response received from server.")} #handling error messages
  if (is.numeric(resObject)) { # error number is received from server
    stop((res %>% resp_body_json())$description)
  } else { #standard JSON is received
    res<-fromJSON(as.character(resObject))
}

  return(res)
}



#This is an internal function that is always run after model_run etc so OK for it to rely on this_session for info
get_output_object_list<-function(location=this_session$output_location)
{
  if (!has_internet()) {
    message("No internet connection.")
    return(invisible(NULL))
  }

  if (is.null(this_session$model_name)) {
    stop("No model specified. Run a model first with model_run before retrieving output objects.")

  }
  url <- paste0(make_url(this_session$model_name, this_session$server, type="tmp"),"/",location)
  message(paste0("Calling server at ", url))


  response <- request(url) %>%
    req_headers("x-prism-auth-user"=this_session$api_key) %>%
    req_error(is_error = function(resp) FALSE) %>%
    req_throttle(10/60) %>%
    req_perform()

  if (response$status_code!=200 && response$status_code!=201) { #TODO check if necessary
     message(paste("Error:"),rawToChar(as.raw(strtoi(response$content, 16L))))
     return(invisible(NULL))}

  str<-response %>% resp_body_string()


  con<-textConnection(str)
  lines<-readLines(con)
  close(con)

  return(lines)
}


filter_output_object_list<-function(object_list,type="")
{
  if(type=="") return(object_list)
  return(object_list[which(substring(object_list,1,nchar(type))==type)])
}


get_output_object<-function(location=this_session$output_location,object)
{
  if (!has_internet()) {
    message("No internet connection.")
    return(invisible(NULL))
  }
  url <- paste0(make_url(this_session$model_name,this_session$server,"tmp"),"/", location,"/",object)
  #message(paste("call is ",call))

  # res <- request(url) %>%
  #   req_headers("x-prism-auth-user"=this_session$api_key) %>%
  #   req_error(is_error = function(resp) FALSE) %>%
  #   req_throttle(10/60) %>%
  #   req_perform() %>%
  #   resp_body_string()


  res<-content(GET(url, add_headers('x-prism-auth-user'=this_session$api_key)))
  return(res)
}



#' Retrieves async results
#' @param model_name name of the model
#' @param api_key API key
#' @param server Server address. Defaults to PMN.
#' @param token async job token
#' @return processed (from JSON to R object result of the call)
#' @export
get_async_results <- function(model_name = NULL, token = NULL, api_key = NULL, server = NULL)
{

  if(is.null(token)) stop("Async job token not provided")
  if(is.null(model_name)) model_name <- this_session$model_name
  if(is.null(api_key)) api_key <- this_session$api_key
  if(is.null(server)) server <- this_session$server
  if(is.null(server)) server <- default_server()

  address <- make_url(model_name, server, "call")

  res <- prism_call("prism_get_async_results", base_url = address, api_key = api_key, token=token)

  return(res)
}




