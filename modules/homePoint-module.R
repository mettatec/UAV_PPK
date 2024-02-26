##Modulo para crear el control de calidad al despegar el dron

HomePointModuleUI <- function(id) {
  ns <- NS(id)
  
  codigosEpsgs <- read.csv("www/epsgGeodesicUtm.csv")
  
  fluidRow(
    column(12,
           h4("Download Home Point data"),
           h3("Select a CRS"),
           selectInput(ns("srcInput"), label = "Select SRC",
                       choices = codigosEpsgs$SRC),
           br(),
           downloadButton(ns("downloadHomePoint"), label = "Download")
           )
  )
}



HomePointModuleServer <- function(input, output, session, dataArchivoPos) {
  ns <- session$ns
  
  output$downloadHomePoint<-  downloadHandler(
    filename = function() {
      paste("homePoint", ".csv", sep="") 
    },
    content = function(file) {
      
      ##Lee los CRS de la hoja y hace match con el indicado por el usuario
      codigosEpsgs <-  read.csv("www/epsgGeodesicUtm.csv")
      codigo <- codigosEpsgs[codigosEpsgs$SRC == input$srcInput, c("EPSG")]
      
      #Crea un objeto espacial y cambia el CRS de acuerdo al input del usuaior
      data <- st_as_sf(dataArchivoPos(), coords = c("longitude.deg.", "latitude.deg."), crs = 4326) %>%
        st_transform(codigo) 
      
      # Obtiene los primeros 200 datos con filtro de 1 del archivo pos y agrega una columna con las diferencia de alturas
      dataForHomePoint <- data %>% 
        cbind(st_coordinates(data), .) %>%
        cbind( . , dataArchivoPos()[, c("longitude.deg.", "latitude.deg.")]) %>%
        st_drop_geometry() %>% 
        filter(Q == 1) %>%
        slice(1:200) %>%
        mutate(heightDif = c(abs(diff(`height.m.`)), 0)) 
        
      #Identifica que valores son mayores a 10 cm, si hay valores los filtra si no mantiene toda la muestra
      heightUp10 <- which(dataForHomePoint$heightDif > 0.10)
      if(length(heightUp10) == 0){
        dataForHomePoint <- dataForHomePoint
      } else {
        dataForHomePoint <- dataForHomePoint[1:heightUp10[1],]
      }
       
      # Crea una tabla con los datos de los promedios
      homePoint <- data.frame(
        Name = "Homepoint",
        YMean <- mean(dataForHomePoint$Y),
        XMean <- mean(dataForHomePoint$X),
        heightMean <- mean(dataForHomePoint$`height.m.`),
        latitudeMean <- mean(dataForHomePoint$`latitude.deg.`),
        longitudeMean<- mean(dataForHomePoint$`longitude.deg.`),
        longitudeSD <- sd(dataForHomePoint$X),
        latitudeSD <- sd(dataForHomePoint$Y),
        heightSD <- sd(dataForHomePoint$`height.m.`)
        
      )
      names(homePoint) <- c("Name","North", "South", "Ellipsoidal Height","Latitude","Longitude",   "sdn", "sde", "sdu")
      write.csv(homePoint, file,  row.names = FALSE, quote = FALSE)
    }
  )
  
}