#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

require(shiny)
require(visNetwork)
library(readxl)
require(shinydashboard)
library(igraph)



# visNetwork(nodes, df_sub, width = "100%") %>% 
#   visNodes(shapeProperties = list(useBorderWithImage = F),scaling = list(min = 5,max = 30)) %>%
#   visLayout(randomSeed = 19)

nodes <- nodes1

df <- get.data.frame(Bip_G)
df_sub<-df[!duplicated(t(apply(df[1:2], 1, sort))), ]

server <- function(input, output) {
  output$network_proxy_nodes <- renderVisNetwork({
    
    # visNetwork(nodes1, df_sub) %>%
    #   visInteraction(hover = TRUE) %>%
    #   visEvents(hoverNode = "function(nodes) {
    #             Shiny.onInputChange('current_node_id', nodes);
    #             ;}")
    
    visNetwork(nodes1[(nodes1$part == 1 | nodes1$part == 2),], df_sub) %>%
      visNodes(shapeProperties = list(useBorderWithImage = F),scaling = list(min = 5,max = 30)) %>%
      visInteraction(hover = TRUE) %>%
      visOptions(nodesIdSelection = TRUE) 
  })
  
  
  observe({
      print(input$part)
      if(input$part == "Part 1 minerals")
      {
        visNetworkProxy("network_proxy_nodes") %>%      
        visRemoveNodes(id = nodes1$id[nodes1$part == 3])
      }
      else
      {
        visNetworkProxy("network_proxy_nodes") %>%
        visUpdateNodes(nodes = nodes1[nodes1$part == 3,])  
      }
  })

}

ui <- fluidPage(
  fluidRow(
    column(
      width = 2,
      selectInput("part", "Part :",
                  c("Part 1 minerals","Part 1+2 minerals"))
    ),
    column(
      width = 8,
      visNetworkOutput("network_proxy_nodes", height = "1000px")
    )
  )
)

shinyApp(ui = ui, server = server)

