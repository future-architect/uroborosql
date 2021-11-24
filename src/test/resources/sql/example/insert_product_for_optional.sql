INSERT
INTO
	PRODUCT
(
	PRODUCT_ID
/*IF product_name != null */
,	PRODUCT_NAME
/*END*/
/*IF product_kana_name != null */
,	PRODUCT_KANA_NAME
/*END*/
/*IF jan_code != null */
,	JAN_CODE
/*END*/
/*IF product_description != null */
,	PRODUCT_DESCRIPTION
/*END*/
,	INS_DATETIME
,	UPD_DATETIME
,	VERSION_NO
) VALUES (
	/*product_id*/
/*IF product_name != null */
,	/*product_name*/
/*END*/
/*IF product_kana_name != null */
,	/*product_kana_name*/
/*END*/
/*IF jan_code != null */
,	/*jan_code*/
/*END*/
/*IF product_description != null */
,	/*product_description*/
/*END*/
,	/*ins_datetime*/
,	/*upd_datetime*/
,	/*version_no*/
)