# dump the data base
"pg_dump -U ifn tururu > tururu_dump.sql"
# scp the database
"scp tururu_dump.sql vgranda@158.109.46.23:~/"

# go to the server and drop the database
"DROP DATABASE tururu;"

# create an empty database
"CREATE DATABASE tururu;"

# load the dump in the new database
"psql -d tururu -f tururu_dump.sql"

# giving access to guest ####
"
GRANT CONNECT ON DATABASE tururu TO guest;
GRANT USAGE ON SCHEMA public TO guest;
GRANT SELECT ON ALL TABLES IN SCHEMA public TO guest;
GRANT SELECT ON ALL SEQUENCES IN SCHEMA public TO guest;

ALTER DEFAULT PRIVILEGES IN SCHEMA public
GRANT SELECT ON TABLES TO guest;
"
