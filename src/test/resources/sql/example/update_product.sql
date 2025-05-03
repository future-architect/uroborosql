UPDATE PRODUCT
SET PRODUCT_NAME = CONCAT(PRODUCT_NAME, '_updated')
WHERE 1 = 1
/*IF SF.isNotEmpty(product_ids) */
AND PRODUCT_ID IN /*product_ids*/()
/*END*/