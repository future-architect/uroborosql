package jp.co.future.uroborosql.event;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;

import java.time.LocalDate;
import java.time.ZoneId;
import java.util.Date;
import java.util.List;
import java.util.Map;
import java.util.stream.IntStream;

import org.junit.jupiter.api.Test;

import jp.co.future.uroborosql.AbstractDbTest;
import jp.co.future.uroborosql.enums.InsertsType;
import jp.co.future.uroborosql.event.subscriber.EventSubscriber;
import jp.co.future.uroborosql.model.Product;

public class SystemColumnTest extends AbstractDbTest {
	private static final Date FIXED_INS_DATE = Date
			.from(LocalDate.of(2023, 10, 11).atStartOfDay(ZoneId.systemDefault()).toInstant());
	private static final Date FIXED_UPD_DATE = Date
			.from(LocalDate.of(2023, 11, 12).atStartOfDay(ZoneId.systemDefault()).toInstant());

	@Test
	void testSystemColumnAutoUpdate() {
		truncateTable("PRODUCT");

		var es = new EventSubscriber() {

			@Override
			public void initialize() {
				beforeParseSqlListener(this::beforeParseSql);
				beforeEntityInsertListener(this::beforeEntityInsert);
				beforeEntityUpdateListener(this::beforeEntityUpdate);
				beforeEntityBatchInsertListener(this::beforeEntityBatchInsert);
				beforeEntityBatchUpdateListener(this::beforeEntityBatchUpdate);
				beforeEntityBulkInsertListener(this::beforeEntityBulkInsert);
			}

			void beforeParseSql(final BeforeParseSqlEvent event) {
				var ctx = event.getExecutionContext();
				switch (ctx.getSqlKind()) {
				case UPDATE:
				case BATCH_INSERT:
					ctx.paramIfAbsent("insDatetime", FIXED_INS_DATE);
					ctx.paramIfAbsent("updDatetime", FIXED_UPD_DATE);
					ctx.paramIfAbsent("versionNo", 0);
					break;
				default:
				}
			}

			void setInsertSystemColumns(final Product product) {
				product.setInsDatetime(FIXED_INS_DATE);
				product.setUpdDatetime(FIXED_INS_DATE);
				product.setVersionNo(0);
			}

			void setUpdateSystemColumns(final Product product) {
				product.setUpdDatetime(FIXED_UPD_DATE);
			}

			void beforeEntityInsert(final BeforeEntityInsertEvent event) {
				var product = (Product) event.getEntity();
				setInsertSystemColumns(product);
			}

			void beforeEntityUpdate(final BeforeEntityUpdateEvent event) {
				var product = (Product) event.getEntity();
				setUpdateSystemColumns(product);
			}

			void beforeEntityBatchInsert(final BeforeEntityBatchInsertEvent event) {
				var product = (Product) event.getEntity();
				setInsertSystemColumns(product);
			}

			void beforeEntityBatchUpdate(final BeforeEntityBatchUpdateEvent event) {
				var product = (Product) event.getEntity();
				setUpdateSystemColumns(product);
			}

			void beforeEntityBulkInsert(final BeforeEntityBulkInsertEvent event) {
				var product = (Product) event.getEntity();
				setInsertSystemColumns(product);
			}

		};

		config.getEventListenerHolder().addEventSubscriber(es);

		// SQL UPDATE
		{
			agent.update("example/insert_product_for_bean")
					.param("productId", 10)
					.param("productName", "商品10")
					.param("productKanaName", "ショウヒンカナ10")
					.param("janCode", "0123456789011")
					.param("productDescription", "商品説明10")
					.count();

			var product = agent.query("example/select_product_param_camel")
					.param("productIds", List.of(10))
					.findFirst(Product.class)
					.orElseThrow();

			assertThat(toLocalDate(product.getInsDatetime()), is(toLocalDate(FIXED_INS_DATE)));
			assertThat(toLocalDate(product.getUpdDatetime()), is(toLocalDate(FIXED_UPD_DATE)));
			assertThat(product.getVersionNo(), is(0));
		}

		truncateTable("PRODUCT");

		// SQL BATCH INSERT
		{
			agent.batch("example/insert_product_for_bean")
					.paramStream(IntStream.rangeClosed(1, 10)
							.mapToObj(idx -> Map.of("productId", idx, "productName", "商品" + idx, "productKanaName",
									"ショウヒンカナ" + idx, "janCode", "01234567890" + idx, "productDescription",
									"商品説明" + idx)))
					.count();

			var products = agent.query("example/select_product_param_camel")
					.collect(Product.class);

			assertThat(products.size(), is(10));

			products.forEach(product -> {
				assertThat(toLocalDate(product.getInsDatetime()), is(toLocalDate(FIXED_INS_DATE)));
				assertThat(toLocalDate(product.getUpdDatetime()), is(toLocalDate(FIXED_UPD_DATE)));
				assertThat(product.getVersionNo(), is(0));
			});
		}

		truncateTable("PRODUCT");

		// ENTITY INSERT/UPDATE
		{
			var count = agent.insert(new Product(10, "商品10", "ショウヒンカナ10", "0123456789011", "商品説明10", null, null, null));
			assertThat(count, is(1));

			var product = agent.query("example/select_product_param_camel")
					.param("productIds", List.of(10))
					.findFirst(Product.class)
					.orElseThrow();

			assertThat(toLocalDate(product.getInsDatetime()), is(toLocalDate(FIXED_INS_DATE)));
			assertThat(toLocalDate(product.getUpdDatetime()), is(toLocalDate(FIXED_INS_DATE)));
			assertThat(product.getVersionNo(), is(0));

			product.setProductDescription(product.getProductDescription() + "更新");
			agent.update(product);

			product = agent.query("example/select_product_param_camel")
					.param("productIds", List.of(10))
					.findFirst(Product.class)
					.orElseThrow();

			assertThat(toLocalDate(product.getInsDatetime()), is(toLocalDate(FIXED_INS_DATE)));
			assertThat(toLocalDate(product.getUpdDatetime()), is(toLocalDate(FIXED_UPD_DATE)));
			assertThat(product.getVersionNo(), is(1));
		}

		truncateTable("PRODUCT");

		// ENTITY BATCH INSERT/UPDATE
		{
			// ENTITY BATCH INSERT
			var count = agent.inserts(IntStream.rangeClosed(1, 10)
					.mapToObj(idx -> new Product(idx, "商品" + idx, "ショウヒンカナ" + idx, "01234567890" + idx, "商品説明" + idx,
							null, null, null)));
			assertThat(count, is(10));

			var products = agent.query("example/select_product_param_camel")
					.collect(Product.class);

			assertThat(products.size(), is(10));

			products.forEach(product -> {
				assertThat(toLocalDate(product.getInsDatetime()), is(toLocalDate(FIXED_INS_DATE)));
				assertThat(toLocalDate(product.getUpdDatetime()), is(toLocalDate(FIXED_INS_DATE)));
				assertThat(product.getVersionNo(), is(0));
			});

			// ENTITY BATCH UPDATE
			var updateCount = agent.updates(products.stream()
					.map(product -> {
						product.setProductDescription(product.getProductDescription() + "更新");
						return product;
					}));
			assertThat(updateCount, is(10));

			products = agent.query("example/select_product_param_camel")
					.collect(Product.class);
			products.forEach(product -> {
				assertThat(toLocalDate(product.getInsDatetime()), is(toLocalDate(FIXED_INS_DATE)));
				assertThat(toLocalDate(product.getUpdDatetime()), is(toLocalDate(FIXED_UPD_DATE)));
				assertThat(product.getVersionNo(), is(1));
			});
		}

		truncateTable("PRODUCT");

		// ENTITY BULK INSERT
		{
			// ENTITY BULK INSERT
			var count = agent.inserts(IntStream.rangeClosed(1, 10)
					.mapToObj(idx -> new Product(idx, "商品" + idx, "ショウヒンカナ" + idx, "01234567890" + idx, "商品説明" + idx,
							null, null, null)),
					InsertsType.BULK);
			assertThat(count, is(10));

			var products = agent.query("example/select_product_param_camel")
					.collect(Product.class);

			assertThat(products.size(), is(10));

			products.forEach(product -> {
				assertThat(toLocalDate(product.getInsDatetime()), is(toLocalDate(FIXED_INS_DATE)));
				assertThat(toLocalDate(product.getUpdDatetime()), is(toLocalDate(FIXED_INS_DATE)));
				assertThat(product.getVersionNo(), is(0));
			});
		}

		config.getEventListenerHolder().removeEventSubscriber(es);
	}

	private static final LocalDate toLocalDate(final Date date) {
		return date.toInstant().atZone(ZoneId.systemDefault()).toLocalDate();
	}

}
