all_cons <- dbListConnections(PostgreSQL())
for(con in all_cons){dbDisconnect(con)}
dbListConnections(PostgreSQL())


# rm(list=objects())
