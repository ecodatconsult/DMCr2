% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/setDBLoginCredentials.R
\name{setDBLoginCredentials}
\alias{setDBLoginCredentials}
\title{Ändert oder erstellt die Datei mit Login-Information für die Datenbank}
\usage{
setDBLoginCredentials(user, pw, host, port, db)
}
\arguments{
\item{user}{Character, Name der Datenbankrolle}

\item{pw}{Character, Passwort für die Datenbankrolle}

\item{host}{Character, Adresse der Datenbank}

\item{port}{Numeric, Port der Datenbank}

\item{db}{Character, Name der Datenbank (i.d.R. "fotofallen")}
}
\value{
Nothing
}
\description{
Ändert oder erstellt die Datei mit Login-Information für die Datenbank
}
\examples{
setDBLoginCredentials("postgres", "postgres", "localhost", 5433, "fotofallen")

}
