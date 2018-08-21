rootDir <- 'C:/Users/sea084/Dropbox/RossRCode/Git/SensorBackends'
deployDir <-'C:/Users/sea084/Dropbox/RossRCode/Git/SensorFederationWebAPI'
portNum <- 8070
server <- 'http://ternsoils.nexus.csiro.au'

source(paste0(rootDir, '/Backends/Backend_Config.R'))


DF <- getSensorLocations(usr='Public2', pwd='Public', siteID=NULL, sensorType='Rainfall', longitude=150, latitude=-36, radius_km=NULL, numToReturn=NULL)




b <- apiGetSensorLocations(usr='Public', pwd='Public', siteid=NULL, sensortype='Rainfall', longitude=150, latitude=-26, radius_km=1, numToReturn=100)
