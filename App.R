library(dplyr)
library(stringr)
library(ggplot2)
library(shiny)
library(plotly)

source("analysis.R")

df <- read.csv("unified_dataset.csv")

ui <- fluidPage(
  navbarPage(
    "Celestial Chase",
    tabPanel("Intro",
             fluidPage(
               # Sidebar on the right with the information paragraph and data sources
               mainPanel(
                 # Add your introductory content, images, and links here
                 h1("Research About Meteorites Landings and Near-Earch Comets"),
                 
                 # Two spots for links at the bottom
                 fluidRow(
                   column(width = 4,
                          # Use HTML tags to create a hyperlink
                          p(HTML("Data Source 1: <a href='https://data.nasa.gov/Space-Science/Near-Earth-Comets-Orbital-Elements/b67r-rgxc'>NEC Orbital Elements</a>"))
                   ),
                   column(width = 4,
                          # Use HTML tags to create a hyperlink
                          p(HTML("Data Source 2: <a href='https://data.nasa.gov/Space-Science/Meteorite-Landings/gh4g-9sfh'>Meteorite Landings Data</a>"))
                   )
                 ),
                 
                 # New fluid row for the image, using the entire sixth column
                 fluidRow(
                   column(width = 6, 
                          p(style = "font-size: 16px; text-align: justify;",
  "In the year 2030, a brilliant astronomer named Dr. Elena Reyes made a groundbreaking discovery. She noticed a strange pattern in the trajectories of several near-earth comets. They seemed to be converging towards a single point in the night sky, which was unprecedented. Elena believed this convergence was more than a cosmic coincidence.

Elena shared her findings with her colleague, Dr. James Turner, a renowned geologist with a passion for meteorites. Together, they embarked on a daring mission to uncover the mystery behind these comets.

As they delved deeper into their research, they realized that the comets were on a collision course with Earth. Panic swept across the globe as news of the impending celestial threat spread. Governments around the world mobilized to prepare for a potential impact event.

By observing these incoming comets, Elena and James discovered an amazing fact: Not only are these comets very similar in shape and size, but the textures on their surfaces are also extremely similar, and they are even arranged one after another, very neatly. . It is possible that these comets were influenced by some outside presence. However, there is a high probability that the act of manipulating a comet to rush towards the earth is not a good intention, so humans must find a way to prevent the comet from colliding with the earth.

As the comet gets closer, Elena and James hatch a plan. They want to use an aerospace spacecraft to intercept a comet and study its structure, and then determine whether to deflect it from its orbit or destroy it directly. Surprisingly, when the spacecraft captured the comet, it was discovered that although the material of the comet was a normal comet, there were actually symbols similar to words appearing on its surface. This further confirms the conjecture that other intelligent civilizations control these comets.

After these comets were transported back to Earth, Elena and James studied them immediately. But no matter what methods they used, they could not damage the comet's surface. When people used a microscope to observe its surface structure, they found that the atoms that made up the comet were arranged very compactly, with no gaps at all. This is completely unimaginable technology for human technology. This also means that the technological level of the alien civilizations that control these comets far exceeds that of humans.

After the failure of launching nuclear bombs at comets and using fiber nets to intercept them, mankind fell into deep despair. People thought that the earth was about to be destroyed by these comets.

On the day when the comet entered the atmosphere, most people chose to stay at home and wait for death. But after the comet entered low-Earth orbit, it did not accelerate or change course as expected, but continued to fly at a constant speed. Eventually, the comets crashed in a desert. Some fall deeper and some fall shallower, very much like a three-dimensional coordinate system. Elena and James realized that this civilization might be sending humans the location of their own galaxy in the universe. Through searching past galaxy data, people discovered a galaxy 20 light years away from the earth that met the requirements of the coordinate information transmitted by the comet. So with hope, Elena and James used gravitational waves to send the first message to other civilizations."
                                               ),
                   # Shiny limits the columns that you can push since it has a locked 6,6 or 4,8 grid layout. I do not want to use Shiny layout templates because they don't fit with my project.
                 ),
  column(width = 6, 
         p(style = "font-size: 16px; text-align: justify;",
           
  "The Analysis of Recent Meteorite Landings and Near-Earth Comet Dataset was created to calculate some advanced data based on the movement patterns of near-Earth comets and the patterns of meteorites falling to the Earth. 

· Scientific Research:
Scientists use such datasets to analyze the characteristics of meteorite landings and near-Earth comets. This includes their trajectories, compositions, sizes, and impact locations.
Research based on these datasets can help in understanding the frequency and distribution of meteorite landings, which contributes to our knowledge of the solar system's dynamics.

· Astronomy and Astrophysics:
These datasets are valuable for astronomers studying the origins and evolution of celestial bodies. Insights gained from analyzing meteorites and near-Earth comets can provide clues about the early solar system and the formation of planets.

· Space Exploration Planning:
Knowledge about the frequency and distribution of meteorite landings is important for planning space missions. Understanding the risk of encountering debris or small objects is crucial for spacecraft safety."
         )
         # Shiny limits the columns that you can push since it has a locked 6,6 or 4,8 grid layout. I do not want to use Shiny layout templates because they don't fit with my project.
        )
               )
             ),
          )
    ),
    tabPanel("GenInfo",
             fluidPage(
                 # Sidebar on the left
               sidebarLayout(
                 sidebarPanel(
                        selectInput(inputId = "Name", label = "Choose a Name", choices = unique(df$name)),
                        htmlOutput(outputId = "Year"),
                        htmlOutput(outputId = "Mass"),
                        htmlOutput(outputId = "Location"),
                        htmlOutput(outputId = "Crush"),
                        htmlOutput(outputId = "Average Speed"),
                        htmlOutput(outputId = "Reclass"),
                 ),
                 mainPanel(h3("Year and Mass"),plotlyOutput(outputId = "scatter"))
                 ),
                 # Map on the right
                 
               )
    ),
    tabPanel("Filter",
             fluidPage(
               titlePanel("Filter All Meteorites by Class"),
               textInput(inputId = "Class", label = "Enter text to search for meteorites that belong to this class"),
               htmlOutput(outputId = "Meteorite"),
             )
    ),
    tabPanel("Research",
             fluidPage(
               splitLayout(
                 plotlyOutput(outputId = "Scatter1"),
                 plotlyOutput(outputId = "Scatter2"),
               )
             )
    ),
  )
)

server <- function(input, output) {
  output$Year <- renderUI({
    get_year(df,input$Name)
  })
  output$Mass <- renderUI({
    get_mass(df,input$Name)
  })
  output$Location <- renderUI({
    get_location(df,input$Name)
  })
  output$Crush <- renderUI({
    get_crush(df,input$Name)
  })
  output$Speed <- renderUI({
    get_speed(df,input$Name)
  })
  output$Class <- renderUI({
    get_class(df,input$Name)
  })
  output$scatter <- renderPlotly({
    temp <- df[df$name==input$Name,]
    p <-ggplot(df,aes(x=year,y=mass..g.,color=name,text=name))+
      geom_point(size=1)+
      geom_text(data=temp,aes(label=name,y=mass..g.+3),position = position_dodge(width = 1),size=3)+
      labs(x="Year",y="Mass of Meteorites and Comets")
    p = ggplotly(p,tooltip = "text")
    return(p)
  })
  output$Meteorite <- renderUI({
    input_class <- input$Class
    filtered_df <- get_class_special(input_class)
    return(make_names(filtered_df))
  })
  output$Scatter1 <- renderPlotly({
    temp <- df[df$name==input$Name,]
    p <-ggplot(df,aes(x=reclong,y=reclat,color=name,text=name))+
      geom_point(size=1)+
      geom_text(data=temp,aes(label=name,y=reclat+3),position = position_dodge(width = 1),size=3)+
      labs(x="Longitude",y="Latitude")
    p = ggplotly(p,tooltip = "text")
    return(p)
  })
  output$Scatter2 <- renderPlotly({
    temp <- df[df$name==input$Name,]
    p <-ggplot(df,aes(x=q..AU.,y=Q..AU.,color=name,text=name))+
      geom_point(size=1)+
      geom_text(data=temp,aes(label=name,y=Q..AU.+3),position = position_dodge(width = 1),size=3)+
      labs(x="Perihelion Distance",y="Aphelion Distance")
    p = ggplotly(p,tooltip = "text")
    return(p)
  })
}
shinyApp(ui = ui, server = server)