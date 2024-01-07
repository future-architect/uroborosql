package jp.co.future.uroborosql;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.nullValue;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.fail;

import java.nio.file.Paths;
import java.sql.JDBCType;
import java.time.LocalDate;
import java.time.ZoneId;
import java.util.Date;
import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;
import java.util.stream.IntStream;
import java.util.stream.Stream;

import org.junit.jupiter.api.Test;

import jp.co.future.uroborosql.enums.GenerationType;
import jp.co.future.uroborosql.exception.OptimisticLockException;
import jp.co.future.uroborosql.mapping.annotations.GeneratedValue;
import jp.co.future.uroborosql.mapping.annotations.Id;
import jp.co.future.uroborosql.mapping.annotations.Table;
import jp.co.future.uroborosql.mapping.annotations.Version;

public class SqlEntityUpdateTest extends AbstractDbTest {

	@Test
	void testCountByEntitySingle() {
		// 事前条件
		cleanInsert(Paths.get("src/test/resources/data/setup", "testExecuteQuery.ltsv"));

		agent.required(() -> {
			assertThat(agent.update(Product.class).set("productName", "商品名_new").equal("productId", 1).count(), is(1));
			assertThat(agent.query(Product.class).equal("productId", 1).one().get().getProductName(), is("商品名_new"));
		});
	}

	@Test
	void testCountByEntitySingleSnakeCase() {
		// 事前条件
		cleanInsert(Paths.get("src/test/resources/data/setup", "testExecuteQuery.ltsv"));

		agent.required(() -> {
			assertThat(agent.update(Product.class).set("product_name", "商品名_new").equal("product_id", 1).count(),
					is(1));
			assertThat(agent.query(Product.class).equal("product_id", 1).one().get().getProductName(), is("商品名_new"));
		});
	}

	@Test
	void testCountByEntityMulti() {
		// 事前条件
		cleanInsert(Paths.get("src/test/resources/data/setup", "testExecuteQuery.ltsv"));

		agent.required(() -> {
			assertThat(
					agent.update(Product.class)
							.set("productName", "商品名_new")
							.set("productKanaName", "ショウヒンメイ_new")
							.greaterEqual("productId", 0)
							.count(),
					is(2));
			var product0 = agent.query(Product.class).equal("productId", 0).one().get();
			assertThat(product0.getProductName(), is("商品名_new"));
			assertThat(product0.getProductKanaName(), is("ショウヒンメイ_new"));
			var product1 = agent.query(Product.class).equal("productId", 1).one().get();
			assertThat(product1.getProductName(), is("商品名_new"));
			assertThat(product1.getProductKanaName(), is("ショウヒンメイ_new"));
		});
	}

	@Test
	void testCountByEntitySetSupplier() {
		// 事前条件
		cleanInsert(Paths.get("src/test/resources/data/setup", "testExecuteQuery.ltsv"));

		agent.required(() -> {
			assertThat(
					agent.update(Product.class).set("productName", ctx -> "商品名_new").greaterEqual("productId", 0)
							.count(),
					is(2));
			assertThat(agent.query(Product.class).equal("productId", 0).one().get().getProductName(), is("商品名_new"));
			assertThat(agent.query(Product.class).equal("productId", 1).one().get().getProductName(), is("商品名_new"));
		});
	}

	@Test
	void testCountByEntitySetSupplierSnakeCase() {
		// 事前条件
		cleanInsert(Paths.get("src/test/resources/data/setup", "testExecuteQuery.ltsv"));

		agent.required(() -> {
			assertThat(
					agent.update(Product.class).set("product_name", ctx -> "商品名_new").greaterEqual("product_id", 0)
							.count(),
					is(2));
			assertThat(agent.query(Product.class).equal("product_id", 0).one().get().getProductName(), is("商品名_new"));
			assertThat(agent.query(Product.class).equal("product_id", 1).one().get().getProductName(), is("商品名_new"));
		});
	}

	@Test
	void testCountByEntitySetIntType() {
		// 事前条件
		cleanInsert(Paths.get("src/test/resources/data/setup", "testExecuteQuery.ltsv"));

		agent.required(() -> {
			assertThat(
					agent.update(Product.class).set("productName", "商品名_new", JDBCType.VARCHAR.getVendorTypeNumber())
							.greaterEqual("productId", 0)
							.count(),
					is(2));
			assertThat(agent.query(Product.class).equal("productId", 0).one().get().getProductName(), is("商品名_new"));
			assertThat(agent.query(Product.class).equal("productId", 1).one().get().getProductName(), is("商品名_new"));
		});
	}

	@Test
	void testCountByEntitySetIntTypeSnakeCase() {
		// 事前条件
		cleanInsert(Paths.get("src/test/resources/data/setup", "testExecuteQuery.ltsv"));

		agent.required(() -> {
			assertThat(
					agent.update(Product.class).set("product_name", "商品名_new", JDBCType.VARCHAR.getVendorTypeNumber())
							.greaterEqual("product_id", 0)
							.count(),
					is(2));
			assertThat(agent.query(Product.class).equal("product_id", 0).one().get().getProductName(), is("商品名_new"));
			assertThat(agent.query(Product.class).equal("product_id", 1).one().get().getProductName(), is("商品名_new"));
		});
	}

	@Test
	void testCountByEntitySetType() {
		// 事前条件
		cleanInsert(Paths.get("src/test/resources/data/setup", "testExecuteQuery.ltsv"));

		agent.required(() -> {
			assertThat(
					agent.update(Product.class).set("productName", "商品名_new", JDBCType.VARCHAR)
							.greaterEqual("productId", 0)
							.count(),
					is(2));
			assertThat(agent.query(Product.class).equal("productId", 0).one().get().getProductName(), is("商品名_new"));
			assertThat(agent.query(Product.class).equal("productId", 1).one().get().getProductName(), is("商品名_new"));
		});
	}

	@Test
	void testCountByEntitySetTypeSnakeCase() {
		// 事前条件
		cleanInsert(Paths.get("src/test/resources/data/setup", "testExecuteQuery.ltsv"));

		agent.required(() -> {
			assertThat(
					agent.update(Product.class).set("product_name", "商品名_new", JDBCType.VARCHAR)
							.greaterEqual("product_id", 0)
							.count(),
					is(2));
			assertThat(agent.query(Product.class).equal("product_id", 0).one().get().getProductName(), is("商品名_new"));
			assertThat(agent.query(Product.class).equal("product_id", 1).one().get().getProductName(), is("商品名_new"));
		});
	}

	/**
	 * Entityを使ったDB更新処理のテストケース。
	 */
	@Test
	void testEntityUpdate() throws Exception {
		// 事前条件
		cleanInsert(Paths.get("src/test/resources/data/setup", "testExecuteBatch.ltsv"));

		agent.required(() -> {
			var product = new Product();
			product.setProductId(1);
			product.setProductName("商品名_new");
			assertThat(product.getVersionNo(), is(0));

			assertThat(agent.update(product), is(1));
			assertThat(product.getVersionNo(), is(1));
			assertThat(agent.find(Product.class, 1).get().getProductName(), is("商品名_new"));
			assertThat(agent.find(Product.class, 2).get().getVersionNo(), is(0));
		});
	}

	/**
	 * Entityを使ったDB更新処理のテストケース。
	 */
	@Test
	void testEntityUpdateAndReturn() throws Exception {
		// 事前条件
		cleanInsert(Paths.get("src/test/resources/data/setup", "testExecuteBatch.ltsv"));

		agent.required(() -> {
			var product = new Product();
			product.setProductId(1);
			product.setProductName("商品名_new");
			assertThat(product.getVersionNo(), is(0));

			assertThat(agent.updateAndReturn(product), is(product));
			assertThat(product.getVersionNo(), is(1));
			assertThat(agent.find(Product.class, 1).get().getProductName(), is("商品名_new"));
			assertThat(agent.find(Product.class, 2).get().getVersionNo(), is(0));
		});
	}

	/**
	 * Entityを使ったDB更新処理のテストケース。
	 */
	@Test
	void testEntityUpdatesAndReturnManyRecord() throws Exception {
		for (var row = 500; row <= 2000; row = row + 500) {
			// 事前条件
			truncateTable("PRODUCT");

			var boxSize = row + 1;

			agent.required(() -> {
				var now = Date.from(LocalDate.now().atStartOfDay(ZoneId.systemDefault()).toInstant());
				Stream<Product> insertedProduct = agent.insertsAndReturn(IntStream.range(1, boxSize).mapToObj(i -> {
					var product = new Product();
					product.setProductId(i);
					product.setProductName("商品名" + i);
					product.setProductKanaName("ショウヒンメイ" + i);
					product.setProductDescription("説明" + i);
					product.setJanCode("1111-" + i);
					product.setInsDatetime(now);
					product.setUpdDatetime(now);
					product.setVersionNo(i);
					return product;
				}));

				agent.updatesAndReturn(insertedProduct.map(p -> {
					p.setProductName(p.getProductName() + "_new");
					return p;
				})).forEach(p -> {
					assertThat(p.getVersionNo(), is(p.getProductId() + 1));
					assertThat(p.getProductName(), is("商品名" + p.getProductId() + "_new"));
				});

				agent.query(Product.class).stream().forEach(p -> {
					assertThat(p.getVersionNo(), is(p.getProductId() + 1));
					assertThat(p.getProductName(), is("商品名" + p.getProductId() + "_new"));
				});
			});
		}
	}

	/**
	 * Entityを使ったDB更新処理のテストケース。(楽観ロックエラー）
	 */
	@Test
	void testEntityUpdateThrowException() throws Exception {
		assertThrows(OptimisticLockException.class, () -> {
			// 事前条件
			cleanInsert(Paths.get("src/test/resources/data/setup", "testExecuteBatch.ltsv"));

			var product = new Product();
			product.setProductId(2);
			product.setProductName("商品名_new");
			product.setVersionNo(1);
			agent.update(product);
		});
	}

	/**
	 * Entityを使ったDB更新処理のテストケース。(Stream型引数エラー）
	 */
	@Test
	void testEntityUpdateStreamError() throws Exception {
		assertThrows(IllegalArgumentException.class, () -> {
			// 事前条件
			cleanInsert(Paths.get("src/test/resources/data/setup", "testExecuteBatch.ltsv"));

			var product = new Product();
			product.setProductId(2);
			product.setProductName("商品名_new");
			product.setVersionNo(1);
			agent.update(Stream.of(product));
		});
	}

	/**
	 * Entityを使った一括更新処理のテストケース。
	 */
	@Test
	void testEntityUpdates() throws Exception {
		// 事前条件
		cleanInsert(Paths.get("src/test/resources/data/setup", "testExecuteBatch.ltsv"));

		agent.required(() -> {

			List<Product> products = agent.query(Product.class).stream().map(p -> {
				p.setProductName(p.getProductName() + "_new");
				p.setProductKanaName(null);
				return p;
			}).collect(Collectors.toList());
			assertThat(agent.updates(products.stream()), is(2));

			assertThat(agent.find(Product.class, 1).get().getProductName(), is("商品名1_new"));
			assertThat(agent.find(Product.class, 1).get().getProductKanaName(), nullValue());
			assertThat(agent.find(Product.class, 2).get().getProductName(), is("商品名2_new"));
			assertThat(agent.find(Product.class, 2).get().getProductKanaName(), nullValue());
		});
	}

	/**
	 * Entityを使った一括更新処理のテストケース。
	 */
	@Test
	void testEntityUpdatesAndReturn() throws Exception {
		// 事前条件
		cleanInsert(Paths.get("src/test/resources/data/setup", "testExecuteBatch.ltsv"));

		agent.required(() -> {

			List<Product> products = agent.updatesAndReturn(agent.query(Product.class).stream().map(p -> {
				p.setProductName(p.getProductName() + "_new");
				p.setProductKanaName(null);
				return p;
			})).collect(Collectors.toList());

			assertThat(products.size(), is(2));

			assertThat(products.get(0).getProductName(), is("商品名1_new"));
			assertThat(products.get(0).getProductKanaName(), nullValue());
			assertThat(products.get(0).getVersionNo(), is(1));
			assertThat(products.get(1).getProductName(), is("商品名2_new"));
			assertThat(products.get(1).getProductKanaName(), nullValue());
			assertThat(products.get(1).getVersionNo(), is(1));
		});
	}

	/**
	 * Entityを使った一括更新処理のテストケース。
	 */
	@Test
	void testEntityUpdatesWithEntityType() throws Exception {
		// 事前条件
		cleanInsert(Paths.get("src/test/resources/data/setup", "testExecuteBatch.ltsv"));

		agent.required(() -> {

			List<Product> products = agent.query(Product.class).stream().map(p -> {
				p.setProductName(p.getProductName() + "_new");
				p.setProductKanaName(null);
				return p;
			}).collect(Collectors.toList());
			assertThat(agent.updates(Product.class, products.stream()), is(2));

			assertThat(agent.find(Product.class, 1).get().getProductName(), is("商品名1_new"));
			assertThat(agent.find(Product.class, 1).get().getProductKanaName(), nullValue());
			assertThat(agent.find(Product.class, 2).get().getProductName(), is("商品名2_new"));
			assertThat(agent.find(Product.class, 2).get().getProductKanaName(), nullValue());
		});
	}

	/**
	 * Entityを使った一括更新処理で楽観ロックエラーが発生するケース
	 */
	@Test
	void testEntityUpdatesOptimisticLockException() throws Exception {
		// 事前条件
		cleanInsert(Paths.get("src/test/resources/data/setup", "testExecuteBatch.ltsv"));

		agent.required(() -> {

			List<Product> products = agent.query(Product.class).stream().map(p -> {
				p.setProductName(p.getProductName() + "_new");
				p.setProductKanaName(null);
				return p;
			}).collect(Collectors.toList());

			// ロック番号を加算し、更新されないようにする
			var product1 = products.get(0);
			product1.setVersionNo(product1.getVersionNo() + 1);

			try {
				agent.updates(Product.class, products.stream());
				fail();
			} catch (OptimisticLockException ex) {
				var sql = String.format(
						"UPDATE /* mapping @ Product */ PUBLIC.PRODUCT SET %n\t  \"PRODUCT_ID\" = ?/*productId*/%n\t, \"PRODUCT_NAME\" = ?/*productName*/%n\t, \"PRODUCT_KANA_NAME\" = ?/*productKanaName*/%n\t, \"JAN_CODE\" = ?/*janCode*/%n\t, \"PRODUCT_DESCRIPTION\" = ?/*productDescription*/%n\t, \"INS_DATETIME\" = ?/*insDatetime*/%n\t, \"UPD_DATETIME\" = ?/*updDatetime*/%n\t, \"VERSION_NO\" = \"VERSION_NO\" + 1%nWHERE%n\t    \"PRODUCT_ID\" = ?/*productId*/%n\tAND \"VERSION_NO\" = ?/*versionNo*/");
				var entityCount = 2;
				var updateCount = 1;
				assertThat(ex.getMessage(), is(
						String.format(
								"An error occurred due to optimistic locking.%nExecuted SQL [%n%s]%nBatch Entity Count: %d, Update Count: %d.",
								sql, entityCount, updateCount)));
			} catch (Exception ex) {
				fail();
			}
		});
	}

	/**
	 * Entityを使った一括更新処理(IN句の上限を超える場合)のテストケース。
	 */
	@Test
	void testEntityUpdatesWithCondition() throws Exception {
		// 事前条件
		cleanInsert(Paths.get("src/test/resources/data/setup", "testExecuteBatch.ltsv"));

		agent.required(() -> {

			List<Product> products = agent.query(Product.class).stream().map(p -> {
				p.setProductName(p.getProductName() + "_new");
				p.setProductKanaName(null);
				return p;
			}).collect(Collectors.toList());
			assertThat(agent.updates(products.stream(), (ctx, count, e) -> count == 1), is(2));

			assertThat(agent.find(Product.class, 1).get().getProductName(), is("商品名1_new"));
			assertThat(agent.find(Product.class, 1).get().getProductKanaName(), nullValue());
			assertThat(agent.find(Product.class, 2).get().getProductName(), is("商品名2_new"));
			assertThat(agent.find(Product.class, 2).get().getProductKanaName(), nullValue());
		});
	}

	/**
	 * 複合キーを持つEntityを使った一括更新処理のテストケース。
	 */
	@Test
	void testEntityUpdatesMultiKey() throws Exception {
		agent.required(() -> {
			// テーブル作成
			agent.updateWith("drop table if exists test_entity_multi_key cascade").count();
			agent.updateWith(
					"create table if not exists test_entity_multi_key (id integer not null, end_at timestamp with time zone not null, name text not null, version integer not null, primary key (id, end_at))")
					.count();

			List<TestEntityMultiKey> entities = IntStream.range(1, 10)
					.mapToObj(i -> {
						var entity = new TestEntityMultiKey();
						entity.setId(i);
						entity.setEndAt(LocalDate.now().plusDays(i));
						entity.setName("名前" + i);
						entity.setVersion(i);
						return entity;
					}).collect(Collectors.toList());
			assertThat(agent.inserts(TestEntityMultiKey.class, entities.stream()), is(9));

			agent.updatesAndReturn(entities.stream().map(e -> {
				e.setName(e.getName() + "_new");
				return e;
			})).peek(e -> {
				assertThat(e.getName(), is("名前" + e.getId() + "_new"));
				assertThat(e.getVersion(), is(e.getId() + 1));
			}).count();

		});
	}

	/**
	 * @IdをもつEntityを使った更新処理のテストケース。
	 */
	@Test
	void testEntityUpdateWithId() throws Exception {
		agent.required(() -> {
			// テーブル作成
			agent.updateWith("drop table if exists test_entity cascade").count();
			agent.updateWith(
					"create table if not exists test_entity (id serial not null, name text not null, version integer not null, primary key (id))")
					.count();

			List<TestEntity> entities = IntStream.range(1, 10)
					.mapToObj(i -> {
						var entity = new TestEntity();
						entity.setName("名前" + i);
						entity.setVersion(0);
						return entity;
					}).collect(Collectors.toList());
			assertThat(agent.inserts(TestEntity.class, entities.stream()), is(9));

			var newId = 100;
			var count = agent.update(TestEntity.class)
					.set("id", newId)
					.equal("id", 3)
					.count();
			assertThat(count, is(1));

			Optional<TestEntity> entity = agent.find(TestEntity.class, 100);
			assertThat(entity.isPresent(), is(true));
		});
	}

	@Table(name = "test_entity")
	public static class TestEntity {
		@Id
		@GeneratedValue(strategy = GenerationType.IDENTITY)
		private int id;
		private String name;
		@Version
		private int version;

		public TestEntity() {
		}

		public int getId() {
			return id;
		}

		public void setId(final int id) {
			this.id = id;
		}

		public String getName() {
			return name;
		}

		public void setName(final String name) {
			this.name = name;
		}

		public int getVersion() {
			return version;
		}

		public void setVersion(final int version) {
			this.version = version;
		}
	}

	@Table(name = "test_entity_multi_key")
	public static class TestEntityMultiKey {
		private int id;
		private LocalDate endAt;
		private String name;
		@Version
		private int version;

		public TestEntityMultiKey() {
		}

		public int getId() {
			return id;
		}

		public void setId(final int id) {
			this.id = id;
		}

		public LocalDate getEndAt() {
			return endAt;
		}

		public void setEndAt(final LocalDate endAt) {
			this.endAt = endAt;
		}

		public String getName() {
			return name;
		}

		public void setName(final String name) {
			this.name = name;
		}

		public int getVersion() {
			return version;
		}

		public void setVersion(final int version) {
			this.version = version;
		}
	}

}
