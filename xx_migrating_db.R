# script to migrating the db to the server

## local
"pg_dump -C -c -W -U ifn tururu -F p > tururu_dump.sql"
"scp tururu_dump.sql vgranda@158.109.46.23:~/"

## remote
"psql tururu < tururu_dump.sql"

# giving access to guest ####
"GRANT CONNECT ON DATABASE tururu TO guest;
GRANT USAGE ON SCHEMA public TO guest;
GRANT SELECT ON ALL TABLES IN SCHEMA public TO guest;
GRANT SELECT ON ALL SEQUENCES IN SCHEMA public TO guest;

ALTER DEFAULT PRIVILEGES IN SCHEMA public
GRANT SELECT ON TABLES TO guest;"
