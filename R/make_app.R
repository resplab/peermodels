make_app<-function(input,output)
{
  out<-"ui <- fluidPage(
    titlePanel('Hello Shiny!'),

    # Sidebar layout with input and output definitions ----
    sidebarLayout(

      # Sidebar panel for inputs ----
      sidebarPanel(
  "

  counter<-0

  for(name in names(input))
  {
    #browser()

    tmp<-input[[name]]
    if(!inherits(tmp,"prism_input"))
    {
      tmp<-to_prism_input(tmp)
    }
    if(is.null(tmp$range)) tmp$range<-c(tmp$value/2,tmp$value*2)

    if(counter>0) out<-paste(out,",")

    out<-paste(out, "sliderInput(inputId = '",  name,"',
                label = '",tmp$title,"',
                min = ",tmp$range[1],",
                max = ",tmp$range[2],",
                value = ", tmp$value,")"
    )
    counter<-counter+1
  }

  out<-paste(out,"), mainPanel() ) )\n\n")

  out<-paste(out,"server <- function(input, output) {
                 output$distPlot <- renderPlot({

                 x    <- faithful$waiting
                 bins <- seq(min(x), max(x), length.out = input$bins + 1)

                 hist(x, breaks = bins, col = '#75AADB', border = 'white',
                      xlab = 'Waiting time to next eruption (in mins)',
                      main = 'Histogram of waiting times')

               })

             }")


  return(out)
}



