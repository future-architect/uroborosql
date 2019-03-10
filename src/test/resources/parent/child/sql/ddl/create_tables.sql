create table if not exists PRODUCT (
	PRODUCT_ID			NUMERIC(10, 0),
	PRODUCT_NAME		VARCHAR(100),
	PRODUCT_KANA_NAME	VARCHAR(100),
	JAN_CODE			CHAR(13),
	PRODUCT_DESCRIPTION	VARCHAR(100),
	INS_DATETIME		TIMESTAMP,
	UPD_DATETIME		TIMESTAMP,
	VERSION_NO			NUMERIC(10, 0),
	constraint PK_PRODUCT primary key (PRODUCT_ID)
)
;

create table if not exists PRODUCT_REGIST_WORK (
	PRODUCT_NAME		VARCHAR(100),
	PRODUCT_KANA_NAME	VARCHAR(100),
	JAN_CODE			CHAR(13),
	PRODUCT_DESCRIPTION	VARCHAR(100),
	INS_DATETIME		TIMESTAMP
)
;

create table if not exists COLUMN_TYPE_TEST (
	COL_VARCHAR			VARCHAR(100),
	COL_CHAR			CHAR(10),
	COL_NUMERIC			NUMERIC(4, 2),
	COL_BOOLEAN			BOOLEAN,
	COL_TIMESTAMP		TIMESTAMP,
	COL_DATE			DATE,
	COL_TIME			TIME
)
;

comment on table COLUMN_TYPE_TEST is 'column type test';
comment on column COLUMN_TYPE_TEST.COL_VARCHAR is 'column varchar';
comment on column COLUMN_TYPE_TEST.COL_CHAR is 'column char';
comment on column COLUMN_TYPE_TEST.COL_NUMERIC is 'column numeric';
comment on column COLUMN_TYPE_TEST.COL_BOOLEAN is 'column boolean';
comment on column COLUMN_TYPE_TEST.COL_TIMESTAMP is 'column timestamp';
comment on column COLUMN_TYPE_TEST.COL_DATE is 'column date';
comment on column COLUMN_TYPE_TEST.COL_TIME is 'column time';

create table if not exists COLUMN_TYPE_ARRAY (
	COL_ARRAY			ARRAY
)
;
