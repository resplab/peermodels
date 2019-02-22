





#' @export
make_app<-function(style, action_button=T)
#If style==FALSE: A minimalist web app. The model must only have get_default_input()
#This function requires an open connection to the model
{
  input<-prism::get_default_input()
  if(isFALSE(input)) {message("Error: the model had not exposed get_default_input()"); return(FALSE)}
  if(style)
  {
    input_style<-prism::get_default_input_style()
    if(isFALSE(style)) {message("Error: you requested an app with style but the model does not expose get_default_input_style()"); return(FALSE)}
  }

  out<-"ui <- fluidPage(
  titlePanel(get_session_info()$current_model),

  # Sidebar layout with input and output definitions ----
  sidebarLayout(

  # Sidebar panel for inputs ----
  sidebarPanel(actionButton('Run','Run'),
  "

  #out<-paste(out,"uiOutput(outputId='inputs')",sep="")

  out<-paste(out,ifelse(style,crawl_input.style(input,input_style),crawl_input.simple(input)),sep="")

  out<-paste(out,"), mainPanel(uiOutput(outputId = 'results')",sep="")

  if(action_button)
    out<-paste(out,"")

  out<-paste(out,")))\n\n")

  server_code<-paste("
    server <- function(input, output)
    {
      output$results<-renderUI(
      {

        model_input<-list()
        for(nm in names(input))
        {
            nm_correct<-sub(\"input.\",\"\", nm)
            model_input[[nm_correct]]<-as.numeric(strsplit(input[[nm]], split=\",\")[[1]])
        }

        model_output <- eventReactive(input$Run, {model_run(model_input)})

        output$results<-renderUI(eval(parse(text=generate_prism_output_server_text.simple(model_output))))
      })
    }

    generate_prism_output_server_text.simple<-function(model_output)
    {
      #browser()
      out<-\"tagList(\"
      for(i in 1:length(model_output))
      {
        nm<-names(model_output[i])
        element<-model_output[[i]]
        out<-paste(out,\"renderText(model_output[[\",i,\"]])\",sep=\"\")
        if(i<length(model_output)) out<-paste(out,\",\") else out<-paste(out,\")\")
      }

      return(out)
    }



    ")

  out<-paste(out,server_code,sep="")

  out<-gsub("),)",replacement = "))",x = out)

  return(out)
}





server.simple<-function(input,output)
{


  observeEvent (input$Run,
    {
      model_input<-list()
      for(nm in names(input))
      {
        if(substr(nm,1,6)=="input.")
        {
          nm_correct<-sub("input.","", nm)
          if(is.numeric(input[[nm]]))
            model_input[[nm_correct]]<-input[[nm]]
          else
          {
            tmp<-strsplit(input[[nm]], split=",")[[1]]
            tmp2<-suppressWarnings(as.numeric(tmp))
            if(is.na(tmp2[[1]]))
              model_input[[nm_correct]]<-tmp
            else
              model_input[[nm_correct]]<-tmp2
          }
        }
      }

      model_output<-model_run(model_input)

      output$results<-renderUI(
      {
        objects<-list()
        counter<-0
        if(is.list(model_output))
          for(i in 1:length(model_output))
          {
            counter<-counter+1
            nm<-names(model_output[i])
            objects[[counter]]<-renderText(paste(nm,model_output[[i]],sep=":"))
            hr()
          }
        else
          objects[[counter]]<-renderText(model_output)
        pl<-get_plots()
        for(i in length(pl))
          objects[[counter+1]]<-renderPlot(plot(pl[[i]]))
        tagList(objects)
      })
    })
}








crawl_input.simple<-function(inp)
{
  out<-""

  if(!is.list(inp)) {message("Error: input is not a list"); return(FALSE)}

  l<-length(inp)
  {
    for(i in 1:l)
    {
      item<-inp[[i]]
      item_name<-names(inp[i])
      out<-paste(out,render_prism_input.simple(item,id=paste("input",item_name,sep=".")))
      if(i<l) out<-paste(out,",")
    }
  }

  return(out)
}





render_prism_input.simple<-function(item,id)
{
  #Default control
  out<-paste("textInput(inputId = '",  id,"', label = '",id,"', value = '", paste(item,collapse=","),"')",sep="")

  return(paste(out,"\n"))
}





crawl_input.style<-function(inp,style)
{
  out<-""

  if(!is.list(inp)) {message("Error: input is not a list"); return(FALSE)}

  l<-length(inp)
  {
    for(i in 1:l)
    {
      item<-inp[[i]]
      item_name<-names(inp[i])
      item_style<-style[[item_name]]
      if(is.null(style))
        out<-paste(out,render_prism_input.simple(item,id=paste("input",item_name,sep=".")))
      else
        out<-paste(out,render_prism_input.style(item,item_style,id=paste("input",item_name,sep=".")))
      if(i<l) out<-paste(out,",")
    }
  }

  return(out)
}





render_prism_input.style<-function(item,style,id)
{
  #Default control

  if(style$control== prism::INPUT_CONTROL_TEXTBOX)
  {
    if(style$type==INPUT_TYPE_NUMERIC_SCALAR)
      out<-paste("numericInput(inputId = '",  id,"', label = '",id,"', value = '", paste(item,collapse=","),"')",sep="")
    if(style$type==INPUT_TYPE_NUMERIC_SCALAR)
      out<-paste("textInput(inputId = '",  id,"', label = '",id,"', value = '", paste(item,collapse=","),"')",sep="")
  }
  else
  if(style$control==prism::INPUT_CONTROL_SLIDER)
  {
     out<-paste("sliderInput(inputId = '",  id,"', label = '",id,"', min=",style$limit[1],",max=",style$limit[2],", value = '", paste(item,collapse=","),"')",sep="")
  }
  else
  if(style$control==prism::INPUT_CONTROL_RADIOBUTTON)
  {
    #browser()
    out<-paste("radioButtons(inputId = '",  id,"', label = '",id,"', choices=",deparse(style$limit),")",sep="")
  }
  else
  {
     out<-paste("textInput(inputId = '",  id,"', label = '",id,"', value = '", paste(item,collapse=","),"')",sep="")
  }
  return(paste(out,"\n"))
}






render_prism_input_DEPRECATED<-function(inp,id)
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



generate_prism_output_server_text_DEPRECATED<-function()
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

