create table if not exists PRODUCT (
	PRODUCT_ID			NUMERIC(10, 0) NOT NULL,
	PRODUCT_NAME		VARCHAR(100),
	PRODUCT_KANA_NAME	VARCHAR(100),
	JAN_CODE			CHAR(13),
	PRODUCT_DESCRIPTION	VARCHAR(100),
	INS_DATETIME		TIMESTAMP(9),
	UPD_DATETIME		TIMESTAMP(9),
	VERSION_NO			NUMERIC(10, 0),
	constraint PK_PRODUCT primary key (PRODUCT_ID)
)
;

create index if not exists IX_PRODUCT ON PRODUCT (JAN_CODE)
;

create table if not exists PRODUCT_REGIST_WORK (
	PRODUCT_NAME		VARCHAR(100),
	PRODUCT_KANA_NAME	VARCHAR(100),
	JAN_CODE			CHAR(13),
	PRODUCT_DESCRIPTION	VARCHAR(100),
	INS_DATETIME		TIMESTAMP(9)
)
;

create table if not exists COLUMN_TYPE_TEST (
	COL_VARCHAR			VARCHAR(100),
	COL_CHAR			CHAR(10),
	COL_NUMERIC			NUMERIC(5, 2),
	COL_BOOLEAN			BOOLEAN,
	COL_TIMESTAMP		TIMESTAMP(9),
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
	COL_STR_ARRAY			ARRAY,
	COL_INT_ARRAY			ARRAY,
	COL_DATE_ARRAY			ARRAY,
	COL_TIMESTAMP_ARRAY		ARRAY
)
;
