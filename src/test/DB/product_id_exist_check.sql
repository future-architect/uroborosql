CREATE OR REPLACE PROCEDURE product_id_exist_check
  (prod_id    IN  product.product_id%TYPE,
   check_out  out  NUMBER)
IS
  p_id  product.product_id%TYPE;
BEGIN
  SELECT  product_id INTO p_id FROM product WHERE product_id = prod_id;
  check_out := 1;
EXCEPTION
  WHEN NO_DATA_FOUND THEN
    check_out := 0;
END  product_id_exist_check;
/
