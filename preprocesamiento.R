

# Requerimientos ---------------------------------------------------------------


source("requirements.R")


source("local_settings.R")

con <- DBI::dbConnect(RPostgres::Postgres(),
                      
                      
                      host = host,
                      
                      
                      dbname = dbname,
                      
                      
                      user = user,
                    
                      
                      password = password,
                      
                    
                      port = 5432 )

rnve <- dbReadTable(con, "rnve") # leer de la base de datos remota a tu compu, como si fuera una base local

registro_civil <- dbReadTable(con,"registro-civil")

write.csv(rnve, "data/rnve.csv", row.names = F)
write.csv(registro_civil, "data/registro_civil.csv", row.names = F)

dbDisconnect(con)