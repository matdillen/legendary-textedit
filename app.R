library(shiny)
library(tidyverse)
library(shinyjs)
library(magrittr)

#read card text file with HTML styling
tooltext = read_tsv("data/tooltext.txt")
tooltext = arrange(tooltext,id)
tooltextlist = as.list(tooltext$id)
names(tooltextlist) = tooltext$id

types = dplyr::count(tooltext,type)
typeslist = as.list(types$type)
names(typeslist) = types$type

ui <- fluidPage(
  useShinyjs(),
  sidebarLayout(sidebarPanel(
    fluidRow(
      column(
        selectizeInput("cards",
                       "Card text",
                       tooltextlist),
        width=6),
      column(
        selectizeInput("type",
                       "Card type",
                       typeslist),
        width=6
      )),
    actionButton("nextc","Next"),
    textAreaInput("html_data",
                  "HTML INPUT",
                  width = "100%",
                  height = "600px"),
    actionButton("save",
                 "Save Edit"),
    width = 6
  ),
  mainPanel(
    HTML("<BR>"),
    uiOutput("result"),
    width = 6
  ))
)

server <- shinyServer(function(input, output, session) {
  
  tt = reactiveValues(data = tooltext)
  
  #When selecting a card, visualize the HTML code in the textbox for editing
  observeEvent(input$cards,{
    k = isolate(tt$data)
    text = filter(k,id==input$cards)$text
    if (length(text)>0) {
      updateTextInput(session, "html_data", value = text)
    }
  })
  
  observeEvent(input$type,{
    tooltextPart = grep(input$type,tooltext$type)
    filterlist = tooltextlist[tooltextPart]
    updateSelectizeInput(session, "cards",choices=filterlist)
  })
  
  #Next button for easier processing the whole file
  observeEvent(input$nextc,{
    part = filter(tooltext,type==input$type)
    id = grep(input$cards,
              part$id,
              fixed=T)
    nextn = part$id[id+1]
    updateSelectizeInput(session,"cards",selected = nextn)
  })
  
  #render the text as HTML to see if its good or bad
  output$result <- renderUI({
    HTML(paste0("<div>",input$html_data,"</div>"))
  })
  
  #save to the temporary data in the session
  #and also to file
  #currently saves to file in a different directory than the source file
  observeEvent(input$save,{
    k = isolate(tt$data)
    edit = input$html_data
    
    #replace LF within strings to CR for consistency
    edit = gsub("\n","\r",edit)
    
    #save changes for the card
    k %<>%
      mutate(text=replace(text,
                          id==input$cards&type==input$type,
                          edit))
    
    #save to file
    write_tsv(k,"tooltext.txt",na="")
    tt$data = k
  })
  
})

shinyApp(ui=ui,server=server)