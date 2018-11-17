#' @export


#Simple: a 'results UIouput on the ui side, and tagList within renderUI on the serverside. So no need for an output structure.
#' @export
make_app.simple<-function()
{
  input<-thisSession$default_input
  out<-"ui <- fluidPage(
  titlePanel( get_session_info()$current_model),

  # Sidebar layout with input and output definitions ----
  sidebarLayout(

  # Sidebar panel for inputs ----
  sidebarPanel(
  "

  out<-paste(out,crawl_input(input,"input"),sep="")

  out<-paste(out,"), mainPanel(uiOutput(outputId = 'results'))))\n\n",sep="")

  server_code<-paste("
    server <- function(input, output)
    {
      output$results<-renderUI(
      {
        for(nm in names(input))
        {
           p_input<-eval(parse(text=paste('thisSession$default_',gsub('[.]', '$', nm),sep='')))
           s_input<-eval(parse(text=paste('input$',nm,sep='')))
           value<-''
           if(canbe_prism_input(p_input))
           {
           if(substring(p_input$type,1,7)=='numeric')
           value<-as.numeric(unlist(strsplit(s_input)))
           else
           value<-s_input
           }
           else
           {
           if(is.numeric(s_input))
           value<-s_input
           else
           {
           value<-as.numeric(unlist(strsplit(s_input,',')))
           if(sum(is.na(value))>0) value<-s_input
           }
           }
           eval(parse(text=paste('thisSession$',gsub('[.]', '$', nm),'<-value',sep='')))
      }
model_run()
        tagList(",generate_prism_output_server_text_simple(),")
      })
    }")

  out<-paste(out,server_code,sep="")

  out<-gsub("),)",replacement = "))",x = out)

  return(out)
}



generate_prism_output_server_text_simple<-function()
{
  out<-""
  for(i in 1:length(thisSession$output_structure))
  {
    nm<-names(thisSession$output_structure[i])
    element<-thisSession$output_structure[[i]]
    if(canbe_prism_output(element))
    {
      if(element$type=="graphic/data" || element$type=="graphic/url")
        out<-paste(out,"renderPlot(show_output(thisSession$output[[",i,"]])),",sep="")
      else
        out<-paste(out,"renderText(show_output(thisSession$output[[",i,"]])),",sep="")
    }
  }

  out<-substring(out,1,nchar(out)-1)

  return(out)
}



#' @export
make_app<-function()
{
  input<-thisSession$default_input
  output<-thisSession$output_structure

  out<-"ui <- fluidPage(
  titlePanel( get_session_info()$current_model),

  # Sidebar layout with input and output definitions ----
  sidebarLayout(

  # Sidebar panel for inputs ----
  sidebarPanel(
  "

  out<-paste(out,crawl_input(input,"input"),sep="")

  out<-paste(out,"), mainPanel(",crawl_output(output,"output"),")))\n\n",sep="")

  server_code<-paste("
                 server <- function(input, output)
                 {
                    invoke<-reactive(
                    {
                      for(nm in names(input))
                      {
                        p_input<-eval(parse(text=paste('thisSession$default_',gsub('[.]', '$', nm),sep='')))
                        s_input<-eval(parse(text=paste('input$',nm,sep='')))
                        value<-''
                        if(canbe_prism_input(p_input))
                        {
                          if(substring(p_input$type,1,7)=='numeric')
                            value<-as.numeric(unlist(strsplit(s_input )))
                          else
                            value<-s_input
                        }
                        else
                        {
                          if(is.numeric(s_input))
                            value<-s_input
                          else
                          {
                            value<-as.numeric(unlist(strsplit(s_input,',')))
                            if(sum(is.na(value))>0) value<-s_input
                          }
                        }
                        eval(parse(text=paste('thisSession$',gsub('[.]', '$', nm),'<-value',sep='')))
                      }
                      model_run()
                  })\n", generate_prism_output_server_text(),
                "}")

  out<-paste(out,server_code,sep="")

  out<-gsub("),)",replacement = "))",x = out)
  out<-gsub(",,,",replacement = ",",x = out)
  out<-gsub(",,",replacement = ",",x = out)

  return(out)
}







crawl_input<-function(inp,id)
{
  out<-""

  if(inherits(inp,"prism_input")) return(render_prism_input(inp,id))

  if(is.list(inp))
  {
    if(length(inp)==0) browser()
    fe<-inp[[1]]
    if(inherits(fe,"prism_input") || !is.list(fe))
    {#The items in the current list terminal items, so open a tabpanel and list them here
      out<-paste(out,"tabPanel(title='",id,"',")
      first=T
      for(nm in names(inp))
      {
        out<-paste(out,ifelse(first,"",","),crawl_input(inp[[nm]],paste(id,".",nm,sep="")),sep="")
        first=F
      }
      out<-paste(out,"),")
    }
    else
    {
      out<-paste(out,"tabsetPanel(")
      first<-T
      for(nm in names(inp))
      {
        out<-paste(out,ifelse(first,"",""),crawl_input(inp[[nm]],paste(id,".",nm,sep="")),sep="")
        first<-F
      }
      out<-paste(out,")",sep="")
    }
  }
  else
  {
    #We are dealing with a terminal node here
    tmp<-to_prism_input(inp)
    tmp<-render_prism_input(tmp,id)
    return(tmp)
  }

  return(out)
}


render_prism_input<-function(inp,id)
{
  #Default control
  out<-paste("textInput(inputId = '",  id,"',
               label = '",ifelse(inp$title=="",id,inp$title),"',
               value = '", paste(inp$value,collapse=","),"')",sep="")

  if(inp$control=="dropdown")
  {
    out<-paste("selectInput(inputId = '",id,"',
               label = '",ifelse(inp$title=="",id,inp$title),"',
               choices = c('",paste(inp$range,collapse="','"),"'),
               selected = '", paste(inp$value,collapse=","),"')",sep="")
  }
  else
  {
    if(inp$type=="numeric/scalar")
    {
      if(is.null(inp$range))
      {
        inp$range<-c(inp$value/2,inp$value*2)
      }
      out<-paste("sliderInput(inputId = '",id,"',
                 label = '",ifelse(inp$title=="",id,inp$title),"',
                 min = ",inp$range[1],",
                 max = ",inp$range[2],",
                 value = ", paste(inp$value,collapse=","),")",sep="")
    }
    if(inp$type=="numeric/vector" || inp$type=="numeric/matrix")
    {
      out<-paste("textInput(inputId = '",id,"',
                 label = '",ifelse(inp$title=="",id,inp$title),"',
                 value = '", paste(inp$value,collapse=","),"')",sep="")
    }
    if(inp$type=="string/scalar" || inp$type=="string/array" || inp$type=="string/matrix")
    {
      out<-paste("textInput(inputId = '",id,"',
                 label = '",ifelse(inp$title=="",id,inp$title),"',
                 value = '", paste(inp$value,collapse=","),"')",sep="")
    }
  }

  return(paste(out,"\n"))
}










crawl_output<-function(outp,id)
{
  out<-""

  if(inherits(outp,"prism_output") || canbe_prism_output(outp)) return(generate_prism_output_ui_text(outp,id))

  if(is.list(outp))
  {
    fe<-outp[[1]]
    if(inherits(fe,"prism_output") || canbe_prism_output(fe) || !is.list(fe))
    {#The items in the current list terminal items, so open a tabpanel and list them here
      out<-paste(out,"tabPanel(title='",id,"',")
      first=T
      for(nm in names(outp))
      {
        out<-paste(out,ifelse(first,"",","),crawl_output(outp[[nm]],paste(id,".",nm,sep="")),sep="")
        first=F
      }
      out<-paste(out,"),")
    }
    else
    {
      out<-paste(out,"tabsetPanel(")
      first<-T
      for(nm in names(outp))
      {
        out<-paste(out,ifelse(first,"",""),crawl_output(outp[[nm]],paste(id,".",nm,sep="")),sep="")
        first<-F
      }
      out<-paste(out,")",sep="")
    }
  }
  else
  {
    #We are dealing with a terminal node here
    tmp<-to_prism_output(outp)
    return(generate_prism_output_ui_text(tmp,id))
  }

  return(out)
}






generate_prism_output_ui_text<-function(outp,id)
{
  out<-paste("span('",outp$title,"'),",sep="")
  if(outp$type=="graphic/url")
  {
    out<-paste(out, "plotOutput(outputId = '",id,"')",sep="")
  }
  if(outp$type=="numeric/vector" || outp$type=="numeric/matrix")
  {
    out<-paste(out, "tableOutput(outputId = '",id,"')",sep="")
  }
  if(outp$type=="numeric/scalar")
  {
    out<-paste(out, "textOutput(outputId = '",id,"')",sep="")
  }

  return(paste(out,",hr()"))
}



generate_prism_output_server_text<-function()
{
  out<-""
  x<-flatten_prism_output(thisSession$output_structure,"output")
  if(is.null(dim((x)))) browser()
  for(i in 1:dim(x)[1])
  {
    element<-x[i,]
    if(element$type=="graphic/url" || element$type=="graphic/data")
    {
      out<-paste(out,"output$",element$id,"<-renderPlot({invoke(); show_output(thisSession$",gsub(pattern = "\\.",x = element$id,replacement = "$"),")});",sep="")
    }

    if(element$type=="numeric/vector" || element$type=="numeric/matrix")
    {
      out<-paste(out,"output$",element$id,"<-renderTable({invoke(); show_output(thisSession$",gsub(pattern = "\\.",x = element$id,replacement = "$"),")});",sep="")
    }

    if(element$type=="numeric/scalar")
    {
      out<-paste(out,"output$",element$id,"<-renderText({invoke(); show_output(thisSession$",gsub(pattern = "\\.",x = element$id,replacement = "$"),")});",sep="")
    }
  }
  return(out)
}

















flatten_prism_output<-function(output,id)
{
  out<-list()
  counter<-1
  for(nm in names(output))
  {
    if(canbe_prism_output(output[[nm]]))
    {
      out<-rbind(out,c(id=paste(id,nm,sep="."),output[[nm]]))
    }
    else
      out<-rbind(out,flatten_prism_output(output[[nm]],id=paste(id,nm,sep=".")))
  }
  return(out)
}

