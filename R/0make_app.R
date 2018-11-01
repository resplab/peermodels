make_app<-function(input,output)
{
  out<-"ui <- fluidPage(
    titlePanel('salaam'),

    # Sidebar layout with input and output definitions ----
    sidebarLayout(

      # Sidebar panel for inputs ----
      sidebarPanel(
  "

  out<-paste(out,crawl_input(input,"input"),sep="")

  out<-paste(out,"), mainPanel(uiOutput(outputId = 'results'))))\n\n",sep="")

  out<-paste(out,"server <- function(input, output) { PRISM_generic_shiny_server(input,output) }",sep="")

  out<-gsub("),)",replacement = "))",x = out)

  return(out)
}





crawl_input<-function(inp,id)
{
  out<-""

  if(inherits(inp,"prism_input")) return(render_prism_control(inp,id))

  if(is.list(inp))
  {
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
    return(render_prism_control(tmp,id))
  }

    return(out)
}





render_prism_control<-function(inp,id)
{
  out<-""
  if(inp$control=="dropdown")
  {
    out<-paste(out, "selectInput(inputId = '",id,"',
               label = '",ifelse(inp$title=="",id,inp$title),"',
               choices = c('",paste(inp$range,collapse="','"),"'),
               selected = '", inp$value,"')",sep="")
  }
  else
  {
    if(is.numeric(inp$value))
    {
      if(is.null(inp$range))
      {
        inp$range<-c(inp$value/2,inp$value*2)
      }
      out<-paste(out, "sliderInput(inputId = '",id,"',
               label = '",ifelse(inp$title=="",id,inp$title),"',
               min = ",inp$range[1],",
               max = ",inp$range[2],",
               value = ", inp$value,")",sep="")
    }
    else
    {
      out<-paste(out, "textInput(inputId = '",  id,"',
               label = '",ifelse(inp$title=="",id,inp$title),"',
               value = '", inp$value,"')",sep="")
    }
  }
}




PRISM_generic_shiny_server<-function(input,output)
{
  output$results<-renderUI(
    {
      #browser()
      for(nm in names(input))
      {
        text<-paste("thisSession$model_",gsub('[.]', '$', nm),"<-input$",nm,sep="")
        eval(parse(text=text))
      }
      tagList(model_run()$icer,renderPlot(draw_plots(1)),renderPlot(draw_plots(2)))
      #  tagList("hi")
    })
}




