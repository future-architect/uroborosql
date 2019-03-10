SELECT /* _SQL_ID_ */
	*
FROM
	PRODUCT
WHERE 1 = 1
/*IF productIds != null */
AND	PRODUCT_ID	IN	/*productIds*/(0, 2)
/*END*/
/*IF SF.isNotEmpty(productName) */
AND	PRODUCT_NAME LIKE '%' || /*productName*/'' || '%'
/*END*/
ORDER BY PRODUCT_ID
