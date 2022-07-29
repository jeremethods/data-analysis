# Load Libraries ----
#install.packages("plotly")
library(shiny)
library(glue)
library(rvest)
library(stringr)
library(XML)
library(ggiraph)
library(ggplot2)
library(scales)
library(plotly)
library(htmlwidgets)
library(dplyr) 

makemodel <- {c(
  "Audi A3",
  "Audi A4",
  "Audi A6",
  "Audi Q2",
  "Audi Q7",
  "Audi R8",
  "Audi TT",
  "BMW i8",
  "BMW X1",
  "BMW X5",
  "Citroen C5",
  "Dacia Duster",
  "Fiat 500",
  "Ford Fiesta",
  "Ford Focus",
  "Ford Kuga",
  "Ford Mondeo",
  "Ford Mustang",
  "Ford Transit",
  "Honda Accord",
  "Honda Civic",
  "Honda CR-V",
  "Hyundai I20",
  "Hyundai I30",
  "Kia Rio",
  "Kia Sportage",
  "Mazda 3",
  "Mazda 6",
  "Mercedes Benz C",
  "Mitsubishi ASX",
  "Nissan GTR",
  "Nissan Qashqai",
  "Opel Astra",
  "Opel Insignia",
  "Opel Mokka",
  "Peugeot 3008",
  "Range Rover",
  "Renault Clio",
  "Seat Ateca",
  "Skoda Kodiaq",
  "Skoda Octavia",
  "Skoda Superb",
  "Subaru Forester",
  "Suzuki Vitara",
  "Tesla Model S",
  "Toyota Auris",
  "Toyota Avensis",
  "Toyota Corolla",
  "Toyota Rav4",
  "Toyota Yaris",
  "Volkswagen Golf",
  "Volkswagen Passat",
  "Volkswagen Polo",
  "Volvo S60",
  "Volvo S90",
  "Volvo V40",
  "Volvo V70",
  "Volvo V90",
  "Volvo XC60",
  "Volvo XC90")}


# Prepare Data ---
gdata <- function(make="audi", model="a4")
{
  
  a = NULL
  
  df <- matrix(nrow = 0, ncol = 4)
  df <- as.data.frame(df)
  names(df) <- c("price", "mileage", "year", "URL")
  
  for (i in 1:5)
  {
    url <- glue('https://autot.tori.fi/vaihtoautot', "/", make, "/", model, "?sivu=", i)
    #print(url)
    
    #html <- read_html("D:/ladatut/Audi A4.html")
    #html <- read_html("C:/users/jerek/Downloads/aaa.html")
    html <- read_html(url)
    
    
    a0 <- html %>% 
      html_nodes(".typography_subtitle2__nF6ow")
    
    if(length(a0) < 5) break
    
    for (i in 1:length(a0))
    {
      trytext <- gsub(" ", "", html_text(a0[i]))
      if(regexpr("[0-9]+[0-9]+[0-9]+???",trytext) == 1)
      {
        price <- gsub("???", "", trytext)
        
        parent <- html_nodes(a0[i], xpath="..")
        carpage <- html_nodes(a0[i], xpath="..") %>% html_attr("href")
        carpage <- glue("https://autot.tori.fi",carpage)
        nodes <- html_nodes(parent, ".typography_body2__fCRbo")
        
        desctext <- gsub(" ", "", html_text(nodes[2]))
        mileage <- gsub("km", "", regmatches(desctext,regexpr("[0-9]+[0-9]+[0-9]+km",desctext)))
        year <- gsub("km", "", regmatches(desctext,regexpr("km+[0-9]+[0-9]+[0-9]+[DBSH]",desctext)))
        year <- gsub("[DBSH]", "", year)
        #info <- glue("View: ", make, " ", model, ", ", mileage, " km, ", price)
        if(length(c(price, mileage, year)) == 3 && as.numeric(mileage) < 500000) df[nrow(df) + 1,] <- c(price, mileage, year, carpage)
      }
    }
  }
  df[,1] <- as.numeric(df[,1])
  df[,2] <- as.numeric(df[,2])
  df[,3] <- as.numeric(df[,3])
  return(df)
}


ui <- fluidPage(
  titlePanel("Car price analysis"),
  sidebarPanel(
    width=4,
    selectInput(
      inputId='model',
      label='Make and model',
      choices=makemodel,
      selected="Toyota Corolla",
    )
    
    #sliderInput(inputId="year", label="Year", min=1980, max=2022, value=c(1980, 2022), step = NULL, round = FALSE,
    #           ticks = TRUE, animate = FALSE,
    #           width = NULL, sep = "", pre = NULL, post = NULL, timeFormat = NULL,
    #          timezone = NULL, dragRange = TRUE)
    
    
  ),
  mainPanel(
    width=8,
    plotlyOutput('myplot')
  )
)
















renderPlotly2 <- function (expr, env = parent.frame(), quoted = FALSE){
  if (!quoted) {
    expr <- substitute(expr)
  }
  shinyRenderWidget(expr, plotlyOutput, env, quoted = TRUE)
}

js <- "
function(el, x) {
  el.on('plotly_click', function(d) {
    var point = d.points[0];
    var url = point.data.customdata[point.pointIndex];
    window.open(url);
  });
}"


server <- function(input, output){
  
  
  #mappings <- c('Audi' = 'audi', 'BMW' = 'bmw', 'Volvo' = 'volvo','A1' = 'a1', 'A4' = 'a4') 
  
  
  output$myplot <- renderPlotly2({
    
    #make <- mappings[input$make]
    #model <- mappings[input$model]
    
    modelstring <- tolower(input$model)
    #modelstring <- tolower("Tesla Model S")
    make <- strsplit(modelstring, " ")[[1]][1]
    model <- paste(strsplit(modelstring, " ")[[1]][-1], collapse="-")
    
    #yearmin <- input$year[1]
    #yearmax <- input$year[2]
    
    #print(modelstring)
    #print(model)
    #print(yearmin)
    #print(yearmax)
    
    #print(dat[1])
    dat <- gdata(make, model)
    #dat <- cbind(dat, as.numeric(between(dat$year, yearmin, yearmax)))
    #names(dat)[5] <- "opac"
    
    ll.smooth = loess(dat$price~dat$mileage, span=0.75)
    data.fmt = list(color=rgb(0.8,0.8,0.8,0.8), width=4)
    line.fmt = list(dash="solid", width = 1.5, color=NULL)
    
    p <- plot_ly(dat, type = "scatter", 
                 mode = "markers",
                 x = ~mileage, y = ~price, 
                 customdata = dat$URL,
                 color = ~year,
                 #marker = list(size = 5, opacity = dat$opac),
                 text = ~paste(price, " ???, ", mileage, " km, ", year,
                               "<br> View: ", URL),
                 hoverinfo = "text",
                 showlegend = F
    ) %>% 
      layout(
        xaxis = list(title = "Mileage km"),
        yaxis = list(title = 'Price ???'),
        title = glue(str_to_title(modelstring), " cars for sale")
      ) %>% hide_colorbar()
    
    p <- add_lines(p, x=dat$mileage, y=predict(ll.smooth), color=NULL, line=line.fmt, showlegend = F)
    
    
    as_widget(p) %>% onRender(js)
  })
}


shinyApp(ui = ui, server = server)