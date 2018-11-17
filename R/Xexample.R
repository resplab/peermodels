library(shiny)
connect_to_model("epicR")
model_run()

web_app<-make_app()

#fileConn<-file("./R/web_app.R")
#writeLines(web_app, fileConn)
#close(fileConn)

eval(parse(text=web_app))

shiny::runApp(list(ui=ui,server=server))
