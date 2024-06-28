insert into product (
    product_id
,   product_name
,   product_kana_name
,   jan_code
,   product_description
,   ins_datetime
,   upd_datetime
,   version_no
)
select 
    /*product_id*/0
,   /*product_name*/''
,   product_kana_name
,   jan_code
,   product_description
,   ins_datetime
,   ins_datetime
,   0
from product_regist_work
where 
    jan_code = /*jan_code*/'1234567890123'
