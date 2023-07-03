projectsDB <- function(){

  require(RPostgreSQL)
  require(stringi)

  dbConnection(type = "read")

  #projects
  df_projects <- dbGetQuery(con,"SELECT * FROM fotofallen.projekte")
  projects <- sort(unique(df_projects$projekt_name))

  assign("projectsDB_vector",projects, envir = .GlobalEnv)

  dbDisconnect(con)
}
