BEGIN TRANSACTION;

CREATE TABLE settings (
  name     TEXT    NOT NULL,
  upper    INTEGER NOT NULL,
  lower    INTEGER NOT NULL,
  interval INTEGER NOT NULL
);

CREATE TABLE events (
  id        INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT,
  kind      TEXT    NOT NULL,
  timestamp NUMERIC          DEFAULT (strftime('%Y-%m-%dT%H:%M:%S', 'now', 'localtime'))
);

CREATE TABLE measurements (
  id        INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT,
  kind      TEXT    NOT NULL,
  value     INTEGER NOT NULL,
  timestamp NUMERIC          DEFAULT (strftime('%Y-%m-%dT%H:%M:%S', 'now', 'localtime'))
);

COMMIT;
