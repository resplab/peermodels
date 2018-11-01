
connect_to_model("testModel")

model_run()

draw_plots(1)








#################
connect_to_model("testModel")
model_input<-get_default_input()
app<-make_app(model_input,"")
library(shiny)
eval(parse(text=app))
shinyApp(ui=ui,server=server)

