create table PRODUCT (
	PRODUCT_ID			NUMERIC(10, 0) NOT NULL PRIMARY KEY,
	PRODUCT_NAME		NVARCHAR(100),
	PRODUCT_KANA_NAME	NVARCHAR(100),
	JAN_CODE			CHAR(13),
	PRODUCT_DESCRIPTION	NVARCHAR(100),
	INS_DATETIME		DATETIME,
	UPD_DATETIME		DATETIME,
	VERSION_NO			NUMERIC(10, 0)
)
;

create table PRODUCT_REGIST_WORK (
	PRODUCT_NAME		NVARCHAR(100),
	PRODUCT_KANA_NAME	NVARCHAR(100),
	JAN_CODE			CHAR(13),
	PRODUCT_DESCRIPTION	NVARCHAR(100),
	INS_DATETIME		DATETIME
)
;

create table COLUMN_TYPE_TEST (
	COL_VARCHAR			NVARCHAR(100),
	COL_CHAR			CHAR(10),
	COL_NUMERIC			NUMERIC(5, 2),
	COL_BOOLEAN			TEXT,
	COL_TIMESTAMP		DATETIME,
	COL_DATE			DATE,
	COL_TIME			TIME
)
;

