BEGIN TRANSACTION;

CREATE TABLE settings (
  name     TEXT    NOT NULL,
  upper    INTEGER NOT NULL,
  lower    INTEGER NOT NULL,
  interval INTEGER NOT NULL,
  busy     BOOLEAN NOT NULL DEFAULT 0
);

CREATE TABLE events (
  id        INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT,
  type      TEXT    NOT NULL,
  timestamp NUMERIC          DEFAULT (strftime('%Y-%m-%dT%H:%M:%S', 'now', 'localtime'))
);

CREATE TABLE measurements (
  id        INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT,
  value     INTEGER NOT NULL,
  timestamp NUMERIC          DEFAULT (strftime('%Y-%m-%dT%H:%M:%S', 'now', 'localtime'))
);

COMMIT;
