package jp.co.future.uroborosql;

import static org.hamcrest.CoreMatchers.is;
import static org.junit.Assert.assertThat;

import java.nio.file.Paths;
import java.util.Collections;
import java.util.Date;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.junit.Test;

import jp.co.future.uroborosql.enums.InsertsType;

public class SqlEntityInsertTest extends AbstractDbTest {

	@Test
	public void testInsert() {
		truncateTable("PRODUCT");
		agent.required(() -> {
			var product = new Product(1, "商品1", "ショウヒン1", "1111-1", "商品-1", new Date(), new Date(), 1);
			agent.insert(product);

			assertThat(agent.find(Product.class, 1).get().getProductName(), is("商品1"));
		});
	}

	@Test
	public void testInsertAndReturn() {
		truncateTable("PRODUCT");
		agent.required(() -> {
			var product = new Product(1, "商品1", "ショウヒン1", "1111-1", "商品-1", new Date(), new Date(), 1);
			var insertedProduct = agent.insertAndReturn(product);

			assertThat(agent.find(Product.class, 1).get().getProductId(), is(insertedProduct.getProductId()));
			assertThat(agent.find(Product.class, 1).get().getProductName(), is("商品1"));
		});
	}

	@Test(expected = IllegalArgumentException.class)
	public void testInsertThrowException() {
		truncateTable("PRODUCT");
		agent.required(() -> {
			var product = new Product(1, "商品1", "ショウヒン1", "1111-1", "商品-1", new Date(), new Date(), 1);
			agent.insert(Stream.of(product));
		});
	}

	/**
	 * Entityを使った一括挿入処理のテストケース。
	 */
	@Test
	public void testInserts() throws Exception {
		// 事前条件
		cleanInsert(Paths.get("src/test/resources/data/setup", "testExecuteBatch.ltsv"));

		agent.setDefaultInsertsType(InsertsType.BATCH);
		agent.required(() -> {
			assertThat(agent.inserts(agent.query(Product.class).stream().map(e -> {
				e.setProductId(e.getProductId() + 10);
				e.setProductName(e.getProductName() + "_new");
				e.setProductKanaName(e.getProductKanaName() + "_new");
				e.setProductDescription(e.getProductDescription() + "_new");
				return e;
			})), is(2));
			assertThat(agent.find(Product.class, 11).get().getProductName(), is("商品名1_new"));
			assertThat(agent.find(Product.class, 12).get().getVersionNo(), is(0));
		});

		// 事前条件
		cleanInsert(Paths.get("src/test/resources/data/setup", "testExecuteBatch.ltsv"));

		agent.setDefaultInsertsType(InsertsType.BULK);
		agent.required(() -> {
			assertThat(agent.inserts(agent.query(Product.class).stream().map(e -> {
				e.setProductId(e.getProductId() + 10);
				e.setProductName(e.getProductName() + "_new");
				e.setProductKanaName(e.getProductKanaName() + "_new");
				e.setProductDescription(e.getProductDescription() + "_new");
				return e;
			})), is(2));
			assertThat(agent.find(Product.class, 11).get().getProductName(), is("商品名1_new"));
			assertThat(agent.find(Product.class, 12).get().getVersionNo(), is(0));
		});
	}

	/**
	 * Entityを使った一括挿入処理のテストケース（Streamが空の場合）。
	 */
	@Test
	public void testInsertsEmpty() throws Exception {
		// 事前条件
		cleanInsert(Paths.get("src/test/resources/data/setup", "testExecuteBatch.ltsv"));

		agent.setDefaultInsertsType(InsertsType.BATCH);
		agent.required(() -> {
			List<Product> emptyList = Collections.emptyList();
			assertThat(agent.inserts(emptyList.stream()), is(0));
		});

		// 事前条件
		cleanInsert(Paths.get("src/test/resources/data/setup", "testExecuteBatch.ltsv"));

		agent.setDefaultInsertsType(InsertsType.BULK);
		agent.required(() -> {
			List<Product> emptyList = Collections.emptyList();
			assertThat(agent.inserts(emptyList.stream()), is(0));
		});
	}

	/**
	 * Entityを使った一括挿入処理のテストケース。
	 */
	@Test
	public void testInsertsWithInsertsTypeBatch() throws Exception {
		// 事前条件
		cleanInsert(Paths.get("src/test/resources/data/setup", "testExecuteBatch.ltsv"));

		agent.setDefaultInsertsType(InsertsType.BATCH);
		agent.required(() -> {
			assertThat(agent.inserts(agent.query(Product.class).stream().map(e -> {
				e.setProductId(e.getProductId() + 10);
				e.setProductName(e.getProductName() + "_new");
				e.setProductKanaName(e.getProductKanaName() + "_new");
				e.setProductDescription(e.getProductDescription() + "_new");
				return e;
			}), InsertsType.BATCH), is(2));
			assertThat(agent.find(Product.class, 11).get().getProductName(), is("商品名1_new"));
			assertThat(agent.find(Product.class, 12).get().getVersionNo(), is(0));
		});

		// 事前条件
		cleanInsert(Paths.get("src/test/resources/data/setup", "testExecuteBatch.ltsv"));

		agent.setDefaultInsertsType(InsertsType.BULK);
		agent.required(() -> {
			assertThat(agent.inserts(agent.query(Product.class).stream().map(e -> {
				e.setProductId(e.getProductId() + 10);
				e.setProductName(e.getProductName() + "_new");
				e.setProductKanaName(e.getProductKanaName() + "_new");
				e.setProductDescription(e.getProductDescription() + "_new");
				return e;
			}), InsertsType.BATCH), is(2));
			assertThat(agent.find(Product.class, 11).get().getProductName(), is("商品名1_new"));
			assertThat(agent.find(Product.class, 12).get().getVersionNo(), is(0));
		});
	}

	/**
	 * Entityを使った一括挿入処理のテストケース。
	 */
	@Test
	public void testInsertsWithInsertsTypeBulk() throws Exception {
		// 事前条件
		cleanInsert(Paths.get("src/test/resources/data/setup", "testExecuteBatch.ltsv"));

		agent.setDefaultInsertsType(InsertsType.BATCH);
		agent.required(() -> {
			assertThat(agent.inserts(agent.query(Product.class).stream().map(e -> {
				e.setProductId(e.getProductId() + 10);
				e.setProductName(e.getProductName() + "_new");
				e.setProductKanaName(e.getProductKanaName() + "_new");
				e.setProductDescription(e.getProductDescription() + "_new");
				return e;
			}), InsertsType.BULK), is(2));
			assertThat(agent.find(Product.class, 11).get().getProductName(), is("商品名1_new"));
			assertThat(agent.find(Product.class, 12).get().getVersionNo(), is(0));
		});

		// 事前条件
		cleanInsert(Paths.get("src/test/resources/data/setup", "testExecuteBatch.ltsv"));

		agent.setDefaultInsertsType(InsertsType.BULK);
		agent.required(() -> {
			assertThat(agent.inserts(agent.query(Product.class).stream().map(e -> {
				e.setProductId(e.getProductId() + 10);
				e.setProductName(e.getProductName() + "_new");
				e.setProductKanaName(e.getProductKanaName() + "_new");
				e.setProductDescription(e.getProductDescription() + "_new");
				return e;
			}), InsertsType.BULK), is(2));
			assertThat(agent.find(Product.class, 11).get().getProductName(), is("商品名1_new"));
			assertThat(agent.find(Product.class, 12).get().getVersionNo(), is(0));
		});
	}

	/**
	 * Entityを使った一括挿入処理のテストケース。
	 */
	@Test
	public void testInsertsWithEntityType() throws Exception {
		// 事前条件
		cleanInsert(Paths.get("src/test/resources/data/setup", "testExecuteBatch.ltsv"));

		agent.setDefaultInsertsType(InsertsType.BATCH);
		agent.required(() -> {
			assertThat(agent.inserts(Product.class, agent.query(Product.class).stream().map(e -> {
				e.setProductId(e.getProductId() + 10);
				e.setProductName(e.getProductName() + "_new");
				e.setProductKanaName(e.getProductKanaName() + "_new");
				e.setProductDescription(e.getProductDescription() + "_new");
				return e;
			})), is(2));
			assertThat(agent.find(Product.class, 11).get().getProductName(), is("商品名1_new"));
			assertThat(agent.find(Product.class, 12).get().getVersionNo(), is(0));
		});

		// 事前条件
		cleanInsert(Paths.get("src/test/resources/data/setup", "testExecuteBatch.ltsv"));

		agent.setDefaultInsertsType(InsertsType.BULK);
		agent.required(() -> {
			assertThat(agent.inserts(Product.class, agent.query(Product.class).stream().map(e -> {
				e.setProductId(e.getProductId() + 10);
				e.setProductName(e.getProductName() + "_new");
				e.setProductKanaName(e.getProductKanaName() + "_new");
				e.setProductDescription(e.getProductDescription() + "_new");
				return e;
			})), is(2));
			assertThat(agent.find(Product.class, 11).get().getProductName(), is("商品名1_new"));
			assertThat(agent.find(Product.class, 12).get().getVersionNo(), is(0));
		});
	}

	/**
	 * Entityを使った一括挿入処理のテストケース。
	 */
	@Test
	public void testInsertsWithEntityTypeAndInsertsType() throws Exception {
		// 事前条件
		cleanInsert(Paths.get("src/test/resources/data/setup", "testExecuteBatch.ltsv"));

		agent.setDefaultInsertsType(InsertsType.BATCH);
		agent.required(() -> {
			assertThat(agent.inserts(Product.class, agent.query(Product.class).stream().map(e -> {
				e.setProductId(e.getProductId() + 10);
				e.setProductName(e.getProductName() + "_new");
				e.setProductKanaName(e.getProductKanaName() + "_new");
				e.setProductDescription(e.getProductDescription() + "_new");
				return e;
			}), InsertsType.BATCH), is(2));
			assertThat(agent.find(Product.class, 11).get().getProductName(), is("商品名1_new"));
			assertThat(agent.find(Product.class, 12).get().getVersionNo(), is(0));
		});

		// 事前条件
		cleanInsert(Paths.get("src/test/resources/data/setup", "testExecuteBatch.ltsv"));

		agent.setDefaultInsertsType(InsertsType.BATCH);
		agent.required(() -> {
			assertThat(agent.inserts(Product.class, agent.query(Product.class).stream().map(e -> {
				e.setProductId(e.getProductId() + 10);
				e.setProductName(e.getProductName() + "_new");
				e.setProductKanaName(e.getProductKanaName() + "_new");
				e.setProductDescription(e.getProductDescription() + "_new");
				return e;
			}), InsertsType.BULK), is(2));
			assertThat(agent.find(Product.class, 11).get().getProductName(), is("商品名1_new"));
			assertThat(agent.find(Product.class, 12).get().getVersionNo(), is(0));
		});

		// 事前条件
		cleanInsert(Paths.get("src/test/resources/data/setup", "testExecuteBatch.ltsv"));

		agent.setDefaultInsertsType(InsertsType.BULK);
		agent.required(() -> {
			assertThat(agent.inserts(Product.class, agent.query(Product.class).stream().map(e -> {
				e.setProductId(e.getProductId() + 10);
				e.setProductName(e.getProductName() + "_new");
				e.setProductKanaName(e.getProductKanaName() + "_new");
				e.setProductDescription(e.getProductDescription() + "_new");
				return e;
			}), InsertsType.BATCH), is(2));
			assertThat(agent.find(Product.class, 11).get().getProductName(), is("商品名1_new"));
			assertThat(agent.find(Product.class, 12).get().getVersionNo(), is(0));
		});

		// 事前条件
		cleanInsert(Paths.get("src/test/resources/data/setup", "testExecuteBatch.ltsv"));

		agent.setDefaultInsertsType(InsertsType.BULK);
		agent.required(() -> {
			assertThat(agent.inserts(Product.class, agent.query(Product.class).stream().map(e -> {
				e.setProductId(e.getProductId() + 10);
				e.setProductName(e.getProductName() + "_new");
				e.setProductKanaName(e.getProductKanaName() + "_new");
				e.setProductDescription(e.getProductDescription() + "_new");
				return e;
			}), InsertsType.BULK), is(2));
			assertThat(agent.find(Product.class, 11).get().getProductName(), is("商品名1_new"));
			assertThat(agent.find(Product.class, 12).get().getVersionNo(), is(0));
		});
	}

	/**
	 * Entityを使った一括挿入処理のテストケース。
	 */
	@Test
	public void testInsertsAndReturn() throws Exception {
		// 事前条件
		cleanInsert(Paths.get("src/test/resources/data/setup", "testExecuteBatch.ltsv"));

		agent.setDefaultInsertsType(InsertsType.BATCH);
		agent.required(() -> {
			List<Product> insertedEntities = agent.insertsAndReturn(agent.query(Product.class).stream().map(e -> {
				e.setProductId(e.getProductId() + 10);
				e.setProductName(e.getProductName() + "_new");
				e.setProductKanaName(e.getProductKanaName() + "_new");
				e.setProductDescription(e.getProductDescription() + "_new");
				return e;
			})).collect(Collectors.toList());

			assertThat(insertedEntities.get(0).getProductName(), is("商品名1_new"));
			assertThat(insertedEntities.get(1).getVersionNo(), is(0));
		});

		// 事前条件
		cleanInsert(Paths.get("src/test/resources/data/setup", "testExecuteBatch.ltsv"));

		agent.setDefaultInsertsType(InsertsType.BULK);
		agent.required(() -> {
			List<Product> insertedEntities = agent.insertsAndReturn(agent.query(Product.class).stream().map(e -> {
				e.setProductId(e.getProductId() + 10);
				e.setProductName(e.getProductName() + "_new");
				e.setProductKanaName(e.getProductKanaName() + "_new");
				e.setProductDescription(e.getProductDescription() + "_new");
				return e;
			})).collect(Collectors.toList());

			assertThat(insertedEntities.get(0).getProductName(), is("商品名1_new"));
			assertThat(insertedEntities.get(1).getVersionNo(), is(0));
		});
	}

	/**
	 * Entityを使った一括挿入処理のテストケース（Streamが空の場合）。
	 */
	@Test
	public void testInsertsAndReturnEmpty() throws Exception {
		// 事前条件
		cleanInsert(Paths.get("src/test/resources/data/setup", "testExecuteBatch.ltsv"));

		agent.setDefaultInsertsType(InsertsType.BATCH);
		agent.required(() -> {
			List<Product> emptyList = Collections.emptyList();
			assertThat(agent.insertsAndReturn(emptyList.stream()).collect(Collectors.toList()).size(), is(0));
		});

		// 事前条件
		cleanInsert(Paths.get("src/test/resources/data/setup", "testExecuteBatch.ltsv"));

		agent.setDefaultInsertsType(InsertsType.BULK);
		agent.required(() -> {
			List<Product> emptyList = Collections.emptyList();
			assertThat(agent.insertsAndReturn(emptyList.stream()).collect(Collectors.toList()).size(), is(0));
		});
	}

	/**
	 * Entityを使った一括挿入処理のテストケース。
	 */
	@Test
	public void testInsertsAndReturnWithInsertsTypeBatch() throws Exception {
		// 事前条件
		cleanInsert(Paths.get("src/test/resources/data/setup", "testExecuteBatch.ltsv"));

		agent.setDefaultInsertsType(InsertsType.BATCH);
		agent.required(() -> {
			List<Product> insertedEntities = agent.insertsAndReturn(agent.query(Product.class).stream().map(e -> {
				e.setProductId(e.getProductId() + 10);
				e.setProductName(e.getProductName() + "_new");
				e.setProductKanaName(e.getProductKanaName() + "_new");
				e.setProductDescription(e.getProductDescription() + "_new");
				return e;
			}), InsertsType.BATCH).collect(Collectors.toList());

			assertThat(insertedEntities.get(0).getProductName(), is("商品名1_new"));
			assertThat(insertedEntities.get(1).getVersionNo(), is(0));
		});

		// 事前条件
		cleanInsert(Paths.get("src/test/resources/data/setup", "testExecuteBatch.ltsv"));

		agent.setDefaultInsertsType(InsertsType.BULK);
		agent.required(() -> {
			List<Product> insertedEntities = agent.insertsAndReturn(agent.query(Product.class).stream().map(e -> {
				e.setProductId(e.getProductId() + 10);
				e.setProductName(e.getProductName() + "_new");
				e.setProductKanaName(e.getProductKanaName() + "_new");
				e.setProductDescription(e.getProductDescription() + "_new");
				return e;
			}), InsertsType.BATCH).collect(Collectors.toList());

			assertThat(insertedEntities.get(0).getProductName(), is("商品名1_new"));
			assertThat(insertedEntities.get(1).getVersionNo(), is(0));
		});
	}

	/**
	 * Entityを使った一括挿入処理のテストケース。
	 */
	@Test
	public void testInsertsAndReturnWithInsertsTypeBulk() throws Exception {
		// 事前条件
		cleanInsert(Paths.get("src/test/resources/data/setup", "testExecuteBatch.ltsv"));

		agent.setDefaultInsertsType(InsertsType.BATCH);
		agent.required(() -> {
			List<Product> insertedEntities = agent.insertsAndReturn(agent.query(Product.class).stream().map(e -> {
				e.setProductId(e.getProductId() + 10);
				e.setProductName(e.getProductName() + "_new");
				e.setProductKanaName(e.getProductKanaName() + "_new");
				e.setProductDescription(e.getProductDescription() + "_new");
				return e;
			}), InsertsType.BULK).collect(Collectors.toList());

			assertThat(insertedEntities.get(0).getProductName(), is("商品名1_new"));
			assertThat(insertedEntities.get(1).getVersionNo(), is(0));
		});

		// 事前条件
		cleanInsert(Paths.get("src/test/resources/data/setup", "testExecuteBatch.ltsv"));

		agent.setDefaultInsertsType(InsertsType.BULK);
		agent.required(() -> {
			List<Product> insertedEntities = agent.insertsAndReturn(agent.query(Product.class).stream().map(e -> {
				e.setProductId(e.getProductId() + 10);
				e.setProductName(e.getProductName() + "_new");
				e.setProductKanaName(e.getProductKanaName() + "_new");
				e.setProductDescription(e.getProductDescription() + "_new");
				return e;
			}), InsertsType.BULK).collect(Collectors.toList());

			assertThat(insertedEntities.get(0).getProductName(), is("商品名1_new"));
			assertThat(insertedEntities.get(1).getVersionNo(), is(0));
		});
	}

	/**
	 * Entityを使った一括挿入処理のテストケース。
	 */
	@Test
	public void testInsertsAndReturnWithType() throws Exception {
		// 事前条件
		cleanInsert(Paths.get("src/test/resources/data/setup", "testExecuteBatch.ltsv"));

		agent.setDefaultInsertsType(InsertsType.BATCH);
		agent.required(() -> {
			List<Product> insertedEntities = agent
					.insertsAndReturn(Product.class, agent.query(Product.class).stream().map(e -> {
						e.setProductId(e.getProductId() + 10);
						e.setProductName(e.getProductName() + "_new");
						e.setProductKanaName(e.getProductKanaName() + "_new");
						e.setProductDescription(e.getProductDescription() + "_new");
						return e;
					})).collect(Collectors.toList());

			assertThat(insertedEntities.get(0).getProductName(), is("商品名1_new"));
			assertThat(insertedEntities.get(1).getVersionNo(), is(0));
		});

		// 事前条件
		cleanInsert(Paths.get("src/test/resources/data/setup", "testExecuteBatch.ltsv"));

		agent.setDefaultInsertsType(InsertsType.BULK);
		agent.required(() -> {
			List<Product> insertedEntities = agent
					.insertsAndReturn(Product.class, agent.query(Product.class).stream().map(e -> {
						e.setProductId(e.getProductId() + 10);
						e.setProductName(e.getProductName() + "_new");
						e.setProductKanaName(e.getProductKanaName() + "_new");
						e.setProductDescription(e.getProductDescription() + "_new");
						return e;
					})).collect(Collectors.toList());

			assertThat(insertedEntities.get(0).getProductName(), is("商品名1_new"));
			assertThat(insertedEntities.get(1).getVersionNo(), is(0));
		});
	}

	/**
	 * Entityを使った一括挿入処理のテストケース。
	 */
	@Test
	public void testInsertsAndReturnWithTypeAndInsertsType() throws Exception {
		// 事前条件
		cleanInsert(Paths.get("src/test/resources/data/setup", "testExecuteBatch.ltsv"));

		agent.setDefaultInsertsType(InsertsType.BATCH);
		agent.required(() -> {
			List<Product> insertedEntities = agent
					.insertsAndReturn(Product.class, agent.query(Product.class).stream().map(e -> {
						e.setProductId(e.getProductId() + 10);
						e.setProductName(e.getProductName() + "_new");
						e.setProductKanaName(e.getProductKanaName() + "_new");
						e.setProductDescription(e.getProductDescription() + "_new");
						return e;
					}), InsertsType.BATCH)
					.collect(Collectors.toList());
			assertThat(insertedEntities.get(0).getProductName(), is("商品名1_new"));
			assertThat(insertedEntities.get(1).getVersionNo(), is(0));
		});

		// 事前条件
		cleanInsert(Paths.get("src/test/resources/data/setup", "testExecuteBatch.ltsv"));

		agent.setDefaultInsertsType(InsertsType.BATCH);
		agent.required(() -> {
			List<Product> insertedEntities = agent
					.insertsAndReturn(Product.class, agent.query(Product.class).stream().map(e -> {
						e.setProductId(e.getProductId() + 10);
						e.setProductName(e.getProductName() + "_new");
						e.setProductKanaName(e.getProductKanaName() + "_new");
						e.setProductDescription(e.getProductDescription() + "_new");
						return e;
					}), InsertsType.BULK)
					.collect(Collectors.toList());
			assertThat(insertedEntities.get(0).getProductName(), is("商品名1_new"));
			assertThat(insertedEntities.get(1).getVersionNo(), is(0));
		});

		// 事前条件
		cleanInsert(Paths.get("src/test/resources/data/setup", "testExecuteBatch.ltsv"));

		agent.setDefaultInsertsType(InsertsType.BULK);
		agent.required(() -> {
			List<Product> insertedEntities = agent
					.insertsAndReturn(Product.class, agent.query(Product.class).stream().map(e -> {
						e.setProductId(e.getProductId() + 10);
						e.setProductName(e.getProductName() + "_new");
						e.setProductKanaName(e.getProductKanaName() + "_new");
						e.setProductDescription(e.getProductDescription() + "_new");
						return e;
					}), InsertsType.BATCH)
					.collect(Collectors.toList());
			assertThat(insertedEntities.get(0).getProductName(), is("商品名1_new"));
			assertThat(insertedEntities.get(1).getVersionNo(), is(0));
		});

		// 事前条件
		cleanInsert(Paths.get("src/test/resources/data/setup", "testExecuteBatch.ltsv"));

		agent.setDefaultInsertsType(InsertsType.BULK);
		agent.required(() -> {
			List<Product> insertedEntities = agent
					.insertsAndReturn(Product.class, agent.query(Product.class).stream().map(e -> {
						e.setProductId(e.getProductId() + 10);
						e.setProductName(e.getProductName() + "_new");
						e.setProductKanaName(e.getProductKanaName() + "_new");
						e.setProductDescription(e.getProductDescription() + "_new");
						return e;
					}), InsertsType.BULK)
					.collect(Collectors.toList());
			assertThat(insertedEntities.get(0).getProductName(), is("商品名1_new"));
			assertThat(insertedEntities.get(1).getVersionNo(), is(0));
		});
	}

}
