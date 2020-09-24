[![Build Status](https://travis-ci.org/hce/postgresql-session.svg?branch=master)](https://travis-ci.org/hce/postgresql-session)

Provides a PostgreSQL (9.5 or later) based session store for Network.Wai.Session.

See example/Main.hs for an example usage.


For running the test you need:

```
sudo -u postgres createuser demo
sudo -u postgres psql -c "ALTER USER demo WITH PASSWORD 'omed'"
sudo -u postgres createdb -Odemo demodb
```
