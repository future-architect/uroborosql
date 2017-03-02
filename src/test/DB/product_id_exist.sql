CREATE OR REPLACE FUNCTION product_id_exist
	  (prod_id    IN  product.product_id%TYPE)
	RETURN  NUMBER
	IS
	  p_id  product.product_id%TYPE;
	BEGIN
	  SELECT  product_id INTO p_id FROM product WHERE product_id = prod_id;
	  RETURN 1;
	EXCEPTION
	  WHEN NO_DATA_FOUND THEN
	    RETURN 0;
	END  product_id_exist;
/
