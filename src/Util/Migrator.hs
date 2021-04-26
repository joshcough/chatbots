module Util.Migrator (migrate) where

import           Protolude

import           Data.String.Conversions              (cs)
import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.Migration
import           NeatInterpolation                    (text)

migrations :: [Migration]
migrations =  [initialSchema, addUserToQuotes, addStreams]

migrate :: Text -> IO ()
migrate url = do
  con <- connectPostgreSQL (cs url)
  -- initialize the migrations table (idempotent)
  withTransaction con $ runMigration $ MigrationContext MigrationInitialization True con
  -- run all the migrations (also idempotent)
  res <- runMigrations' con migrations
  print res

runMigrations' :: Connection -> [Migration] -> IO [MigrationResult Text]
runMigrations' con migs =
  fmap (fmap (fmap cs)) $ withTransaction con $ forM migs $ \migration ->
    runMigration $ MigrationContext (toMigrationScript migration) True con
    where
    toMigrationScript m = MigrationScript (cs $ name m) (cs $ script m)

data Migration = Migration
  { name :: Text
  , script :: Text
  }

-- These were named:
--2019-07-06_23-32-00_initial-schema
--2019-10-20_12-21-00_add_user_to_quotes
--2019-10-26_21-21-00_add_streams
initialSchema :: Migration
initialSchema = Migration "initial_schema" $ [text|
    CREATE TABLE users (
        id SERIAL PRIMARY KEY,
        name character varying NOT NULL,
        email character varying NOT NULL,
        hashed_password character varying NOT NULL,
        admin boolean NOT NULL,
        Unique (email)
    );

    CREATE TABLE commands (
        id SERIAL PRIMARY KEY,
        channel character varying NOT NULL,
        name character varying NOT NULL,
        body character varying NOT NULL,
        Unique (channel, name)
    );

    CREATE TABLE questions (
        id SERIAL PRIMARY KEY,
        channel character varying NOT NULL,
        text character varying NOT NULL,
        qid bigint NOT NULL,
        Unique (channel, qid)
    );

    CREATE TABLE quotes (
        id SERIAL PRIMARY KEY,
        channel character varying NOT NULL,
        text character varying NOT NULL,
        qid bigint NOT NULL,
        Unique (channel, qid)
    );
  |]

addUserToQuotes :: Migration
addUserToQuotes = Migration "add_user_to_quotes" $ [text|
    ALTER table quotes
        add column user_id character varying;

    update quotes set user_id = 'trollabot';

    ALTER table quotes ALTER column user_id SET NOT NULL;
  |]

addStreams :: Migration
addStreams = Migration "add_streams" $ [text|
      CREATE TABLE streams (
          id SERIAL PRIMARY KEY,
          name character varying NOT NULL,
          Unique (name)
      );

      insert into streams values (0, 'daut');

      ALTER table quotes
          drop column channel;

      ALTER table quotes
          add column channel bigint REFERENCES streams (id);

      update quotes set channel = 0;
  |]
