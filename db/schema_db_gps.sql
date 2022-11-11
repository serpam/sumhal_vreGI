BEGIN TRANSACTION;
CREATE TABLE IF NOT EXISTS "dicc_ganaderos" (
	"id_ganadero"	,
	"user_name"	,
	"localidad"	,
	"phone"	,
	"mail"	,
	PRIMARY KEY("id_ganadero")
);
CREATE TABLE IF NOT EXISTS "dicc_explotaciones" (
	"id_ganadero"	,
	"name_ganadero"	,
	"id_explotacion"	,
	FOREIGN KEY("id_ganadero") REFERENCES "dicc_ganaderos"("id_ganadero")
);
CREATE TABLE IF NOT EXISTS "dicc_dispositivos" (
	"id"	,
	"id_ganadero"	,
	"type"	,
	"codigo_gps"	,
	"user_name"	,
	PRIMARY KEY("codigo_gps"),
	FOREIGN KEY("id_ganadero") REFERENCES "dicc_ganaderos"("id_ganadero")
);
CREATE TABLE IF NOT EXISTS "datos_gps" (
	"id"	INTEGER,
	"codigo_gps"	,
	"id_itm"	INTEGER,
	"id_user"	INTEGER,
	"lat"	,
	"lng"	,
	"time_stamp"	DATETIME,
	"month"	INTEGER,
	PRIMARY KEY("id" AUTOINCREMENT),
	FOREIGN KEY("codigo_gps") REFERENCES "dicc_dispositivos"("codigo_gps")
);
COMMIT;
