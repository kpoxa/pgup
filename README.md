# pgup
epgsql database migrations

##how to build
  ```shell
  make escript
  ```

##how to run
1. files:
  * sql/
  * sql/001.create_table.**up**.sql
  ```sql
    CREATE TABLE sometable (id bigserial not null);
  ```
  * sql/001.some_description.**down**.sql
  ```sql
    DROP TABLE sometable;
  ```
  * db.config
  ```erlang
    [ { db, "my_db_name" },
      { host, "127.0.0.1" },
      { user, "postgres" },
      { password, "pass" },
      { dir, "./sql/" } ].
  ```
  
2. run:
  ```shell
  ./pgup db.config init   # creates _pgup table
  ./pgup db.config upgrade 
  ```
  
3. downgrade:
  ```shell
  ./pgup db.config downgrade 0 
  ```
