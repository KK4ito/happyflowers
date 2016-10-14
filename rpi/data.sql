BEGIN TRANSACTION;

INSERT INTO settings (name, upper, lower, interval) VALUES
("Cecelia", 80, 40, 60);

INSERT INTO events (type, timestamp) VALUES
("automatic", "2016-10-03T05:10:01"),
("automatic", "2016-10-05T04:10:01"),
("manual", "2016-10-06T11:10:01"),
("automatic", "2016-10-07T11:10:01"),
("automatic", "2016-10-10T00:10:01"),
("automatic", "2016-10-12T06:10:01");

INSERT INTO measurements (value, timestamp) VALUES
(80, "2016-10-01T00:10:01"),
(78, "2016-10-01T05:10:01"),
(78, "2016-10-01T06:10:01"),
(77, "2016-10-01T08:10:01"),
(77, "2016-10-01T10:10:01"),
(76, "2016-10-01T11:10:01"),
(75, "2016-10-01T14:10:01"),
(74, "2016-10-01T14:10:01"),
(74, "2016-10-01T15:10:01"),
(73, "2016-10-01T16:10:01"),
(73, "2016-10-01T18:10:01"),
(72, "2016-10-01T20:10:01"),
(72, "2016-10-01T21:10:01"),
(71, "2016-10-01T22:10:01"),
(70, "2016-10-02T00:10:01"),
(70, "2016-10-02T02:10:01"),
(69, "2016-10-02T02:10:01"),
(68, "2016-10-02T03:10:01"),
(67, "2016-10-02T03:10:01"),
(67, "2016-10-02T04:10:01"),
(66, "2016-10-02T05:10:01"),
(64, "2016-10-02T05:10:01"),
(63, "2016-10-02T05:10:01"),
(62, "2016-10-02T06:10:01"),
(61, "2016-10-02T06:10:01"),
(60, "2016-10-02T07:10:01"),
(60, "2016-10-02T08:10:01"),
(60, "2016-10-02T09:10:01"),
(59, "2016-10-02T09:10:01"),
(59, "2016-10-02T09:10:01"),
(58, "2016-10-02T11:10:01"),
(57, "2016-10-02T11:10:01"),
(55, "2016-10-02T12:10:01"),
(55, "2016-10-02T14:10:01"),
(54, "2016-10-02T15:10:01"),
(53, "2016-10-02T15:10:01"),
(52, "2016-10-02T16:10:01"),
(51, "2016-10-02T16:10:01"),
(51, "2016-10-02T17:10:01"),
(50, "2016-10-02T17:10:01"),
(49, "2016-10-02T17:10:01"),
(49, "2016-10-02T17:10:01"),
(48, "2016-10-02T18:10:01"),
(48, "2016-10-02T20:10:01"),
(47, "2016-10-02T21:10:01"),
(46, "2016-10-02T22:10:01"),
(45, "2016-10-02T22:10:01"),
(43, "2016-10-02T23:10:01"),
(43, "2016-10-03T01:10:01"),
(42, "2016-10-03T03:10:01"),
(42, "2016-10-03T04:10:01"),
(41, "2016-10-03T04:10:01"),
(40, "2016-10-03T04:10:01"),
(38, "2016-10-03T05:10:01"),
(79, "2016-10-03T05:10:01"),
(79, "2016-10-03T06:10:01"),
(78, "2016-10-03T07:10:01"),
(77, "2016-10-03T08:10:01"),
(76, "2016-10-03T10:10:01"),
(75, "2016-10-03T10:10:01"),
(75, "2016-10-03T13:10:01"),
(74, "2016-10-03T13:10:01"),
(73, "2016-10-03T14:10:01"),
(73, "2016-10-03T15:10:01"),
(72, "2016-10-03T15:10:01"),
(71, "2016-10-03T16:10:01"),
(70, "2016-10-03T16:10:01"),
(69, "2016-10-03T18:10:01"),
(68, "2016-10-03T18:10:01"),
(68, "2016-10-03T18:10:01"),
(66, "2016-10-03T20:10:01"),
(66, "2016-10-03T20:10:01"),
(65, "2016-10-03T21:10:01"),
(65, "2016-10-03T22:10:01"),
(64, "2016-10-03T23:10:01"),
(64, "2016-10-04T00:10:01"),
(63, "2016-10-04T01:10:01"),
(62, "2016-10-04T01:10:01"),
(61, "2016-10-04T03:10:01"),
(60, "2016-10-04T04:10:01"),
(59, "2016-10-04T04:10:01"),
(58, "2016-10-04T06:10:01"),
(58, "2016-10-04T09:10:01"),
(57, "2016-10-04T13:10:01"),
(56, "2016-10-04T14:10:01"),
(55, "2016-10-04T14:10:01"),
(54, "2016-10-04T15:10:01"),
(53, "2016-10-04T16:10:01"),
(52, "2016-10-04T17:10:01"),
(51, "2016-10-04T17:10:01"),
(51, "2016-10-04T17:10:01"),
(50, "2016-10-04T18:10:01"),
(49, "2016-10-04T18:10:01"),
(48, "2016-10-04T18:10:01"),
(47, "2016-10-04T19:10:01"),
(47, "2016-10-04T19:10:01"),
(46, "2016-10-04T19:10:01"),
(44, "2016-10-04T21:10:01"),
(44, "2016-10-04T22:10:01"),
(43, "2016-10-04T22:10:01"),
(42, "2016-10-05T00:10:01"),
(41, "2016-10-05T01:10:01"),
(41, "2016-10-05T01:10:01"),
(40, "2016-10-05T04:10:01"),
(39, "2016-10-05T04:10:01"),
(81, "2016-10-05T07:10:01"),
(80, "2016-10-05T08:10:01"),
(79, "2016-10-05T09:10:01"),
(79, "2016-10-05T10:10:01"),
(78, "2016-10-05T11:10:01"),
(77, "2016-10-05T12:10:01"),
(75, "2016-10-05T12:10:01"),
(74, "2016-10-05T12:10:01"),
(74, "2016-10-05T13:10:01"),
(73, "2016-10-05T14:10:01"),
(72, "2016-10-05T14:10:01"),
(71, "2016-10-05T14:10:01"),
(71, "2016-10-05T18:10:01"),
(71, "2016-10-05T18:10:01"),
(70, "2016-10-05T19:10:01"),
(69, "2016-10-05T20:10:01"),
(68, "2016-10-05T20:10:01"),
(66, "2016-10-05T21:10:01"),
(65, "2016-10-05T21:10:01"),
(64, "2016-10-05T21:10:01"),
(63, "2016-10-05T21:10:01"),
(63, "2016-10-06T00:10:01"),
(62, "2016-10-06T02:10:01"),
(61, "2016-10-06T02:10:01"),
(60, "2016-10-06T02:10:01"),
(59, "2016-10-06T03:10:01"),
(58, "2016-10-06T07:10:01"),
(58, "2016-10-06T08:10:01"),
(57, "2016-10-06T10:10:01"),
(56, "2016-10-06T10:10:01"),
(55, "2016-10-06T11:10:01"),
(68, "2016-10-06T12:10:01"),
(68, "2016-10-06T14:10:01"),
(67, "2016-10-06T15:10:01"),
(65, "2016-10-06T17:10:01"),
(64, "2016-10-06T17:10:01"),
(63, "2016-10-06T18:10:01"),
(62, "2016-10-06T18:10:01"),
(62, "2016-10-06T19:10:01"),
(61, "2016-10-06T20:10:01"),
(61, "2016-10-06T20:10:01"),
(60, "2016-10-06T21:10:01"),
(59, "2016-10-06T22:10:01"),
(58, "2016-10-06T23:10:01"),
(57, "2016-10-06T23:10:01"),
(56, "2016-10-06T23:10:01"),
(55, "2016-10-06T23:10:01"),
(55, "2016-10-06T23:10:01"),
(54, "2016-10-07T00:10:01"),
(53, "2016-10-07T00:10:01"),
(52, "2016-10-07T03:10:01"),
(52, "2016-10-07T04:10:01"),
(51, "2016-10-07T04:10:01"),
(51, "2016-10-07T04:10:01"),
(50, "2016-10-07T04:10:01"),
(49, "2016-10-07T05:10:01"),
(48, "2016-10-07T05:10:01"),
(46, "2016-10-07T05:10:01"),
(46, "2016-10-07T06:10:01"),
(45, "2016-10-07T07:10:01"),
(43, "2016-10-07T07:10:01"),
(41, "2016-10-07T08:10:01"),
(41, "2016-10-07T10:10:01"),
(40, "2016-10-07T11:10:01"),
(39, "2016-10-07T11:10:01"),
(80, "2016-10-07T12:10:01"),
(78, "2016-10-07T13:10:01"),
(78, "2016-10-07T13:10:01"),
(77, "2016-10-07T13:10:01"),
(77, "2016-10-07T16:10:01"),
(76, "2016-10-07T16:10:01"),
(75, "2016-10-07T16:10:01"),
(74, "2016-10-07T17:10:01"),
(74, "2016-10-07T18:10:01"),
(73, "2016-10-07T21:10:01"),
(73, "2016-10-08T00:10:01"),
(72, "2016-10-08T01:10:01"),
(72, "2016-10-08T01:10:01"),
(71, "2016-10-08T02:10:01"),
(70, "2016-10-08T03:10:01"),
(70, "2016-10-08T04:10:01"),
(69, "2016-10-08T04:10:01"),
(68, "2016-10-08T06:10:01"),
(67, "2016-10-08T07:10:01"),
(67, "2016-10-08T08:10:01"),
(66, "2016-10-08T09:10:01"),
(64, "2016-10-08T13:10:01"),
(63, "2016-10-08T14:10:01"),
(62, "2016-10-08T17:10:01"),
(61, "2016-10-08T17:10:01"),
(60, "2016-10-08T17:10:01"),
(60, "2016-10-08T21:10:01"),
(60, "2016-10-08T22:10:01"),
(59, "2016-10-09T02:10:01"),
(59, "2016-10-09T03:10:01"),
(58, "2016-10-09T03:10:01"),
(57, "2016-10-09T04:10:01"),
(55, "2016-10-09T05:10:01"),
(55, "2016-10-09T06:10:01"),
(54, "2016-10-09T07:10:01"),
(53, "2016-10-09T08:10:01"),
(52, "2016-10-09T08:10:01"),
(51, "2016-10-09T09:10:01"),
(51, "2016-10-09T09:10:01"),
(50, "2016-10-09T10:10:01"),
(49, "2016-10-09T13:10:01"),
(49, "2016-10-09T13:10:01"),
(48, "2016-10-09T14:10:01"),
(48, "2016-10-09T15:10:01"),
(47, "2016-10-09T15:10:01"),
(46, "2016-10-09T18:10:01"),
(45, "2016-10-09T18:10:01"),
(43, "2016-10-09T18:10:01"),
(43, "2016-10-09T19:10:01"),
(42, "2016-10-09T20:10:01"),
(42, "2016-10-09T20:10:01"),
(41, "2016-10-09T21:10:01"),
(40, "2016-10-10T00:10:01"),
(38, "2016-10-10T00:10:01"),
(79, "2016-10-10T04:10:01"),
(79, "2016-10-10T05:10:01"),
(78, "2016-10-10T08:10:01"),
(77, "2016-10-10T09:10:01"),
(76, "2016-10-10T09:10:01"),
(75, "2016-10-10T10:10:01"),
(75, "2016-10-10T13:10:01"),
(74, "2016-10-10T13:10:01"),
(73, "2016-10-10T14:10:01"),
(73, "2016-10-10T15:10:01"),
(72, "2016-10-10T15:10:01"),
(71, "2016-10-10T15:10:01"),
(70, "2016-10-10T17:10:01"),
(69, "2016-10-10T17:10:01"),
(68, "2016-10-10T19:10:01"),
(68, "2016-10-10T19:10:01"),
(66, "2016-10-10T19:10:01"),
(66, "2016-10-10T20:10:01"),
(65, "2016-10-10T21:10:01"),
(65, "2016-10-10T23:10:01"),
(64, "2016-10-11T00:10:01"),
(64, "2016-10-11T02:10:01"),
(63, "2016-10-11T03:10:01"),
(62, "2016-10-11T03:10:01"),
(61, "2016-10-11T03:10:01"),
(60, "2016-10-11T03:10:01"),
(59, "2016-10-11T04:10:01"),
(58, "2016-10-11T04:10:01"),
(58, "2016-10-11T04:10:01"),
(57, "2016-10-11T04:10:01"),
(56, "2016-10-11T05:10:01"),
(55, "2016-10-11T06:10:01"),
(54, "2016-10-11T09:10:01"),
(53, "2016-10-11T11:10:01"),
(52, "2016-10-11T12:10:01"),
(51, "2016-10-11T12:10:01"),
(51, "2016-10-11T12:10:01"),
(50, "2016-10-11T12:10:01"),
(49, "2016-10-11T15:10:01"),
(48, "2016-10-11T15:10:01"),
(47, "2016-10-11T18:10:01"),
(47, "2016-10-11T18:10:01"),
(46, "2016-10-11T21:10:01"),
(44, "2016-10-11T22:10:01"),
(44, "2016-10-11T23:10:01"),
(43, "2016-10-11T23:10:01"),
(42, "2016-10-11T23:10:01"),
(41, "2016-10-12T02:10:01"),
(41, "2016-10-12T03:10:01"),
(40, "2016-10-12T06:10:01"),
(39, "2016-10-12T06:10:01"),
(81, "2016-10-12T07:10:01"),
(80, "2016-10-12T07:10:01"),
(79, "2016-10-12T07:10:01"),
(79, "2016-10-12T09:10:01"),
(78, "2016-10-12T10:10:01"),
(77, "2016-10-12T10:10:01"),
(75, "2016-10-12T10:10:01"),
(74, "2016-10-12T10:10:01"),
(74, "2016-10-12T11:10:01"),
(73, "2016-10-12T11:10:01"),
(72, "2016-10-12T11:10:01"),
(71, "2016-10-12T14:10:01"),
(71, "2016-10-12T15:10:01"),
(71, "2016-10-12T15:10:01"),
(70, "2016-10-12T15:10:01"),
(69, "2016-10-12T16:10:01"),
(68, "2016-10-12T16:10:01"),
(66, "2016-10-12T16:10:01"),
(65, "2016-10-12T17:10:01"),
(64, "2016-10-12T19:10:01"),
(63, "2016-10-12T20:10:01"),
(63, "2016-10-12T21:10:01"),
(62, "2016-10-12T21:10:01"),
(61, "2016-10-12T23:10:01"),
(60, "2016-10-13T00:10:01"),
(59, "2016-10-13T00:10:01"),
(58, "2016-10-13T00:10:01"),
(58, "2016-10-13T01:10:01"),
(57, "2016-10-13T02:10:01"),
(56, "2016-10-13T03:10:01"),
(55, "2016-10-13T03:10:01"),
(68, "2016-10-13T05:10:01"),
(68, "2016-10-13T05:10:01"),
(67, "2016-10-13T06:10:01"),
(65, "2016-10-13T06:10:01"),
(64, "2016-10-13T07:10:01"),
(63, "2016-10-13T08:10:01"),
(62, "2016-10-13T08:10:01"),
(62, "2016-10-13T08:10:01"),
(61, "2016-10-13T11:10:01"),
(61, "2016-10-13T11:10:01"),
(60, "2016-10-13T13:10:01"),
(59, "2016-10-13T14:10:01"),
(58, "2016-10-13T15:10:01"),
(57, "2016-10-13T15:10:01"),
(56, "2016-10-13T20:10:01"),
(55, "2016-10-13T20:10:01"),
(55, "2016-10-13T22:10:01"),
(54, "2016-10-13T23:10:01"),
(53, "2016-10-13T23:10:01"),
(52, "2016-10-14T01:10:01"),
(52, "2016-10-14T03:10:01"),
(51, "2016-10-14T04:10:01"),
(51, "2016-10-14T06:10:01"),
(50, "2016-10-14T09:10:01"),
(49, "2016-10-14T12:10:01"),
(48, "2016-10-14T12:10:01"),
(46, "2016-10-14T15:10:01"),
(46, "2016-10-14T17:10:01"),
(45, "2016-10-14T22:10:01"),
(43, "2016-10-14T22:10:01");

COMMIT;
