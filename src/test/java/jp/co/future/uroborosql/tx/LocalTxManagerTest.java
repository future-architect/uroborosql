package jp.co.future.uroborosql.tx;

import static org.hamcrest.CoreMatchers.*;
import static org.junit.Assert.*;

import java.sql.JDBCType;
import java.sql.SQLException;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

import org.hamcrest.Matchers;
import org.junit.After;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

import jp.co.future.uroborosql.SqlAgent;
import jp.co.future.uroborosql.UroboroSQL;
import jp.co.future.uroborosql.config.SqlConfig;
import jp.co.future.uroborosql.enums.InsertsType;
import jp.co.future.uroborosql.exception.UroborosqlRuntimeException;
import jp.co.future.uroborosql.exception.UroborosqlTransactionException;
import jp.co.future.uroborosql.mapping.annotations.Table;

public class LocalTxManagerTest {
	/**
	 * SQL管理クラス
	 */
	SqlConfig config;

	@Before
	public void setUp() {
		config = UroboroSQL.builder("jdbc:h2:mem:LocalTxManagerTest;DB_CLOSE_DELAY=-1", "sa", null).build();
		try (SqlAgent agent = config.agent()) {
			agent.updateWith("create table if not exists emp ( id integer, name VARCHAR(30), PRIMARY KEY (id) )")
					.count();

			agent.required(() -> {
				del(agent, -1);
			});
		}
	}

	@Table(name = "emp")
	public static class Emp {
		private int id;
		private String name;

		public Emp() {
		}

		public Emp(final int id) {
			this.id = id;
		}

		public Emp(final int id, final String name) {
			this.id = id;
			this.name = name;
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

	}

	@After
	public void tearDown() throws Exception {
	}

	@Test
	public void testagentSample01_required() {

		try (SqlAgent agent = config.agent()) {
			agent.required(() -> {
				//トランザクション開始

				ins(agent, 1, "ABC");

				//トランザクション終了 commit
			});
			assertThat(select(agent), is(Arrays.asList("ABC")));

			agent.required(() -> {
				//トランザクション開始

				del(agent, 1);

				//トランザクション終了 commit
			});
			assertThat(select(agent), is(Arrays.asList()));
		}
	}

	@Test
	public void testagentSample02_requiresNew() {

		try (SqlAgent agent = config.agent()) {
			agent.required(() -> {
				//トランザクション開始

				ins(agent, 1, "ABC");

				assertThat(select(agent), is(Arrays.asList("ABC")));

				agent.requiresNew(() -> {
					//新しい トランザクション開始

					ins(agent, 2, "DEF");

					//上で登録した"ABC"は別Connectionのため入らない
					assertThat(select(agent), is(Arrays.asList("DEF")));

					//トランザクション終了 commit
				});

				//トランザクション終了 commit
			});

			assertThat(select(agent), is(Arrays.asList("ABC", "DEF")));

		}
	}

	@Test
	public void testagentSample03_rollback() {

		try (SqlAgent agent = config.agent()) {
			agent.required(() -> {
				//トランザクション開始

				ins(agent, 1, "ABC");

				agent.setRollbackOnly();//ロールバックを指示

				assertThat(select(agent), is(Arrays.asList("ABC")));//まだロールバックは行われない

				//トランザクション終了 rollback
			});

			assertThat(select(agent), is(Arrays.asList()));

		}
	}

	@Test
	public void testagentSample04_error_rollback() {

		try (SqlAgent agent = config.agent()) {
			try {
				agent.required(() -> {
					//トランザクション開始

					ins(agent, 1, "ABC");

					//エラーが起こった場合、ロールバックされる
					throw new IllegalArgumentException();//トランザクション終了 rollback
				});
			} catch (IllegalArgumentException e) {
			}

			assertThat(select(agent), is(Arrays.asList()));

		}

	}

	@Test
	public void testagentSample05_savepoint() {

		try (SqlAgent agent = config.agent()) {
			agent.required(() -> {
				ins(agent, 1, "A");
				ins(agent, 2, "B");
				agent.setSavepoint("sp");
				ins(agent, 3, "C");

				assertThat(select(agent), is(Arrays.asList("A", "B", "C")));

				agent.rollback("sp");//最後のinsertを取消

				assertThat(select(agent), is(Arrays.asList("A", "B")));

			});
		}
	}

	@Test
	public void testagent02() {

		try (SqlAgent agent = config.agent()) {
			agent.required(() -> {
				ins(agent, 1, "ABC");

				assertThat(select(agent), is(Arrays.asList("ABC")));

				assertThat(agent.required(() -> {
					//同Connection
					return select(agent);
				}), is(Arrays.asList("ABC")));

				agent.requiresNew(() -> {
					//別Connection
					assertThat(select(agent), is(Arrays.asList()));
				});

				assertThat(agent.requiresNew(() -> {
					//別Connection
					return select(agent);
				}), is(Arrays.asList()));

				agent.notSupported(() -> {
					//別Connection
					assertThat(select(agent), is(Arrays.asList()));
				});

				assertThat(agent.notSupported(() -> {
					//別Connection
					return select(agent);
				}), is(Arrays.asList()));
			});
		}
	}

	@Test
	public void testagent03() {

		try (SqlAgent agent = config.agent()) {
			agent.required(() -> {
				ins(agent, 1, "ABC");
			});//commit
			assertThat(select(agent), is(Arrays.asList("ABC")));

			agent.required(() -> {
				agent.notSupported(() -> {
					ins(agent, 2, "DEF");
				});//commitされないはず

				assertThat(select(agent), is(Arrays.asList("ABC")));
			});
		}
	}

	@Test
	public void testagent04() {

		try (SqlAgent agent = config.agent()) {
			agent.required(() -> {
				ins(agent, 1, "ABC");
				agent.setRollbackOnly();//ロールバックを予約
			});
			assertThat(select(agent), is(Arrays.asList()));

			try {
				agent.required(() -> {
					ins(agent, 1, "ABC");
					throw new UroborosqlRuntimeException();//エラーのためロールバック
				});
			} catch (UroborosqlRuntimeException e) {
			}
			assertThat(select(agent), is(Arrays.asList()));
		}
	}

	@Test
	public void testagent05() {

		try (SqlAgent agent = config.agent()) {
			agent.required(() -> {
				agent.setSavepoint("X");
				ins(agent, 1, "A");
				agent.setSavepoint("A");
				ins(agent, 2, "B");
				agent.setSavepoint("B");
				ins(agent, 3, "C");
				agent.setSavepoint("C");

				assertThat(select(agent), is(Arrays.asList("A", "B", "C")));

				agent.rollback("B");

				assertThat(select(agent), is(Arrays.asList("A", "B")));

				agent.rollback("A");
			});
			assertThat(select(agent), is(Arrays.asList("A")));

			agent.required(() -> {
				agent.setSavepoint("X");
				ins(agent, 2, "B");
				agent.setSavepoint("B");
				ins(agent, 3, "C");
				agent.setSavepoint("C");

				assertThat(select(agent), is(Arrays.asList("A", "B", "C")));

				agent.rollback("X");

				assertThat(select(agent), is(Arrays.asList("A")));

				agent.releaseSavepoint("B");

				ins(agent, 2, "B");
				agent.setSavepoint("B");
				ins(agent, 3, "C");
				agent.setSavepoint("C");

				agent.rollback("B");

				assertThat(select(agent), is(Arrays.asList("A", "B")));
			});
			assertThat(select(agent), is(Arrays.asList("A", "B")));
		}
	}

	@Test
	public void testSavepointScopeRunnable() {
		try (SqlAgent agent = config.agent()) {
			agent.required(() -> {
				agent.savepointScope(() -> {
					ins(agent, 1, "A");
					try {
						agent.savepointScope(() -> {
							ins(agent, 2, "B");
							try {
								agent.savepointScope(() -> {
									ins(agent, 3, "C");
									assertThat(select(agent), is(Arrays.asList("A", "B", "C")));
									throw new UroborosqlRuntimeException();
								});
							} catch (UroborosqlRuntimeException ex) {
								assertThat(select(agent), is(Arrays.asList("A", "B")));
								throw new UroborosqlRuntimeException();
							}
							fail();
						});
					} catch (UroborosqlRuntimeException ex) {
						assertThat(select(agent), is(Arrays.asList("A")));
					}
				});
				assertThat(select(agent), is(Arrays.asList("A")));
			});
		}
	}

	@Test
	public void testSavepointScopeSupplier() {
		try (SqlAgent agent = config.agent()) {
			agent.required(() -> {
				try {
					agent.savepointScope(() -> {
						ins(agent, 1, "A");
						assertThat(agent.savepointScope(() -> {
							ins(agent, 2, "B");
							return select(agent);
						}), is(Arrays.asList("A", "B")));
						throw new UroborosqlRuntimeException();
					});
				} catch (UroborosqlRuntimeException ex) {
					assertThat(select(agent), is(Matchers.emptyCollectionOf(String.class)));
				}
			});
		}
	}

	@Test
	public void testagentEx01() {

		try (SqlAgent agent = config.agent()) {
			agent.required(() -> {
				agent.notSupported(() -> {
					ins(agent, 1, "ABC");
				});
				agent.notSupported(() -> {
					ins(agent, 2, "DEF");
					agent.commit();//明示的にcommit
					//notSupportedは常に同じConnectionが利用されている
				});

				assertThat(select(agent), is(Arrays.asList("ABC", "DEF")));
			});

			agent.required(() -> {
				ins(agent, 3, "GHI");
				agent.commit();//Commit

				agent.requiresNew(() -> {
					//別ConnectionだけどCommitしたからデータが見れるはず
					assertThat(select(agent), is(Arrays.asList("ABC", "DEF", "GHI")));
				});

			});
		}
	}

	@Test
	public void testagentEx02() {

		try (SqlAgent agent = config.agent()) {
			agent.required(() -> {
				ins(agent, 1, "ABC");
				ins(agent, 2, "DEF");

				assertThat(select(agent), is(Arrays.asList("ABC", "DEF")));

				agent.rollback();//明示的にrollback

				assertThat(select(agent), is(Arrays.asList()));
			});

			ins(agent, 1, "ABC");
			ins(agent, 2, "DEF");

			assertThat(select(agent), is(Arrays.asList("ABC", "DEF")));

			agent.rollback();//明示的にrollback

			assertThat(select(agent), is(Arrays.asList()));
		}
	}

	@Test
	public void testSelectWithinTransaction() {
		config.getSqlAgentFactory().setForceUpdateWithinTransaction(true);

		try (SqlAgent agent = config.agent()) {
			agent.required(() -> {
				select(agent);
				assertThat(agent.query(Emp.class).collect().size(), is(0));
			});
		}
	}

	@Test
	public void testSelectWithinNewTransaction() {
		config.getSqlAgentFactory().setForceUpdateWithinTransaction(true);

		try (SqlAgent agent = config.agent()) {
			agent.requiresNew(() -> {
				select(agent);
				assertThat(agent.query(Emp.class).collect().size(), is(0));
			});
		}
	}

	@Test
	public void testSelectWithinNotSupportedTransaction() {
		config.getSqlAgentFactory().setForceUpdateWithinTransaction(true);

		try (SqlAgent agent = config.agent()) {
			agent.notSupported(() -> {
				select(agent);
				assertThat(agent.query(Emp.class).collect().size(), is(0));
			});
		}
	}

	@Test
	public void testSelectWithoutTransaction() {
		config.getSqlAgentFactory().setForceUpdateWithinTransaction(true);

		try (SqlAgent agent = config.agent()) {
			select(agent);
			assertThat(agent.query(Emp.class).collect().size(), is(0));
		}
	}

	@Test
	public void testInsertWithinTransaction() {
		config.getSqlAgentFactory().setForceUpdateWithinTransaction(true);

		try (SqlAgent agent = config.agent()) {
			agent.required(() -> {
				ins(agent, 1, "ABC");

				Emp emp = new Emp();
				emp.setId(2);
				emp.setName("DEF");
				agent.insert(emp);

				agent.batchWith("insert into emp (id, name) values (/*id*/, /*name*/)")
						.paramStream(IntStream.range(10, 20).mapToObj(i -> {
							Map<String, Object> map = new HashMap<>();
							map.put("id", i);
							map.put("name", "name" + String.valueOf(i));
							return map;
						})).count();

				agent.inserts(IntStream.range(21, 30).mapToObj(i -> new Emp(i, "name" + i)),
						InsertsType.BATCH);

				agent.inserts(IntStream.range(31, 40).mapToObj(i -> new Emp(i, "name" + i)),
						InsertsType.BULK);
			});
		}
	}

	@Test
	public void testInsertWithinNewTransaction() {
		config.getSqlAgentFactory().setForceUpdateWithinTransaction(true);

		try (SqlAgent agent = config.agent()) {
			agent.requiresNew(() -> {
				ins(agent, 1, "ABC");

				Emp emp = new Emp();
				emp.setId(2);
				emp.setName("DEF");
				agent.insert(emp);

				agent.batchWith("insert into emp (id, name) values (/*id*/, /*name*/)")
						.paramStream(IntStream.range(10, 20).mapToObj(i -> {
							Map<String, Object> map = new HashMap<>();
							map.put("id", i);
							map.put("name", "name" + String.valueOf(i));
							return map;
						})).count();

				agent.inserts(IntStream.range(21, 30).mapToObj(i -> new Emp(i, "name" + i)),
						InsertsType.BATCH);

				agent.inserts(IntStream.range(31, 40).mapToObj(i -> new Emp(i, "name" + i)),
						InsertsType.BULK);
			});
		}
	}

	@Test
	public void testInsertWithinNotSupportedTransaction() {
		config.getSqlAgentFactory().setForceUpdateWithinTransaction(true);

		try (SqlAgent agent = config.agent()) {
			agent.notSupported(() -> {
				try {
					ins(agent, 1, "ABC");
					Assert.fail();
				} catch (UroborosqlTransactionException ex) {
					// OK
				} catch (Throwable th) {
					Assert.fail();
				}

				try {
					Emp emp = new Emp();
					emp.setId(2);
					emp.setName("DEF");
					agent.insert(emp);
					Assert.fail();
				} catch (UroborosqlTransactionException ex) {
					// OK
				} catch (Throwable th) {
					Assert.fail();
				}

				try {
					agent.batchWith("insert into emp (id, name) values (/*id*/, /*name*/)")
							.paramStream(IntStream.range(10, 20).mapToObj(i -> {
								Map<String, Object> map = new HashMap<>();
								map.put("id", i);
								map.put("name", "name" + i);
								return map;
							}))
							.errorWhen((agt, ctx, ex) -> {
								throw (UroborosqlTransactionException) ex;
							}).count();
					Assert.fail();
				} catch (UroborosqlTransactionException ex) {
					// OK
				} catch (Throwable th) {
					Assert.fail();
				}

				try {
					agent.inserts(IntStream.range(1, 10).mapToObj(i -> new Emp(i, "name" + i)), InsertsType.BATCH);
					Assert.fail();
				} catch (UroborosqlTransactionException ex) {
					// OK
				} catch (Throwable th) {
					Assert.fail();
				}

				try {
					agent.inserts(IntStream.range(1, 10).mapToObj(i -> new Emp(i, "name" + i)), InsertsType.BULK);
					Assert.fail();
				} catch (UroborosqlTransactionException ex) {
					// OK
				} catch (Throwable th) {
					Assert.fail();
				}
			});
		}
	}

	@Test
	public void testInsertWithoutTransaction() {
		config.getSqlAgentFactory().setForceUpdateWithinTransaction(true);

		try (SqlAgent agent = config.agent()) {
			try {
				ins(agent, 1, "ABC");
				Assert.fail();
			} catch (UroborosqlTransactionException ex) {
				// OK
			} catch (Throwable th) {
				Assert.fail();
			}

			try {
				Emp emp = new Emp();
				emp.setId(2);
				emp.setName("DEF");
				agent.insert(emp);
				Assert.fail();
			} catch (UroborosqlTransactionException ex) {
				// OK
			} catch (Throwable th) {
				Assert.fail();
			}

			try {
				agent.batchWith("insert into emp (id, name) values (/*id*/, /*name*/)")
						.paramStream(IntStream.range(10, 20).mapToObj(i -> {
							Map<String, Object> map = new HashMap<>();
							map.put("id", i);
							map.put("name", "name" + i);
							return map;
						}))
						.errorWhen((agt, ctx, ex) -> {
							throw (UroborosqlTransactionException) ex;
						}).count();
				Assert.fail();
			} catch (UroborosqlTransactionException ex) {
				// OK
			} catch (Throwable th) {
				Assert.fail();
			}

			try {
				agent.inserts(IntStream.range(1, 10).mapToObj(i -> new Emp(i, "name" + i)), InsertsType.BATCH);
				Assert.fail();
			} catch (UroborosqlTransactionException ex) {
				// OK
			} catch (Throwable th) {
				Assert.fail();
			}

			try {
				agent.inserts(IntStream.range(1, 10).mapToObj(i -> new Emp(i, "name" + i)), InsertsType.BULK);
				Assert.fail();
			} catch (UroborosqlTransactionException ex) {
				// OK
			} catch (Throwable th) {
				Assert.fail();
			}
		}
	}

	@Test
	public void testUpdateWithinTransaction() {
		config.getSqlAgentFactory().setForceUpdateWithinTransaction(true);

		try (SqlAgent agent = config.agent()) {
			agent.required(() -> {
				agent.inserts(IntStream.range(1, 10).mapToObj(i -> new Emp(i, "name" + i)), InsertsType.BULK);

				assertThat(upd(agent, 1, "abc"), is(1));

				assertThat(agent.update(new Emp(2, "def")), is(1));
			});
		}
	}

	@Test
	public void testUpdateWithinNewTransaction() {
		config.getSqlAgentFactory().setForceUpdateWithinTransaction(true);

		try (SqlAgent agent = config.agent()) {
			agent.required(() -> {
				agent.inserts(IntStream.range(1, 10).mapToObj(i -> new Emp(i, "name" + i)), InsertsType.BULK);
			});

			agent.requiresNew(() -> {
				assertThat(upd(agent, 1, "abc"), is(1));

				assertThat(agent.update(new Emp(2, "def")), is(1));
			});
		}
	}

	@Test
	public void testUpdateWithinNotSupportedTransaction() {
		config.getSqlAgentFactory().setForceUpdateWithinTransaction(true);

		try (SqlAgent agent = config.agent()) {
			agent.required(() -> {
				agent.inserts(IntStream.range(1, 10).mapToObj(i -> new Emp(i, "name" + i)), InsertsType.BULK);
			});

			agent.notSupported(() -> {
				try {
					upd(agent, 1, "abc");
					Assert.fail();
				} catch (UroborosqlTransactionException ex) {
					// OK
				} catch (Throwable th) {
					Assert.fail();
				}

				try {
					agent.update(new Emp(2, "def"));
					Assert.fail();
				} catch (UroborosqlTransactionException ex) {
					// OK
				} catch (Throwable th) {
					Assert.fail();
				}
			});
		}
	}

	@Test
	public void testUpdateWithoutTransaction() {
		config.getSqlAgentFactory().setForceUpdateWithinTransaction(true);

		try (SqlAgent agent = config.agent()) {
			agent.required(() -> {
				agent.inserts(IntStream.range(1, 10).mapToObj(i -> new Emp(i, "name" + i)), InsertsType.BULK);
			});

			try {
				upd(agent, 1, "abc");
				Assert.fail();
			} catch (UroborosqlTransactionException ex) {
				// OK
			} catch (Throwable th) {
				Assert.fail();
			}

			try {
				agent.update(new Emp(2, "def"));
				Assert.fail();
			} catch (UroborosqlTransactionException ex) {
				// OK
			} catch (Throwable th) {
				Assert.fail();
			}
		}
	}

	@Test
	public void testDeleteWithinTransaction() {
		config.getSqlAgentFactory().setForceUpdateWithinTransaction(true);

		try (SqlAgent agent = config.agent()) {
			agent.required(() -> {
				agent.inserts(IntStream.range(1, 10).mapToObj(i -> new Emp(i, "name" + i)), InsertsType.BULK);

				assertThat(del(agent, 1), is(1));

				assertThat(agent.delete(new Emp(2)), is(1));
				assertThat(agent.delete(Emp.class, 3), is(1));
				assertThat(agent.delete(Emp.class).equal("name", "name4").count(), is(1));
			});
		}
	}

	@Test
	public void testDeleteWithinNewTransaction() {
		config.getSqlAgentFactory().setForceUpdateWithinTransaction(true);

		try (SqlAgent agent = config.agent()) {
			agent.required(() -> {
				agent.inserts(IntStream.range(1, 10).mapToObj(i -> new Emp(i, "name" + i)), InsertsType.BULK);
			});

			agent.requiresNew(() -> {
				assertThat(del(agent, 1), is(1));

				assertThat(agent.delete(new Emp(2)), is(1));
				assertThat(agent.delete(Emp.class, 3), is(1));
				assertThat(agent.delete(Emp.class).equal("name", "name4").count(), is(1));
			});
		}
	}

	@Test
	public void testDeleteWithinNotSupportedTransaction() {
		config.getSqlAgentFactory().setForceUpdateWithinTransaction(true);

		try (SqlAgent agent = config.agent()) {
			agent.required(() -> {
				agent.inserts(IntStream.range(1, 10).mapToObj(i -> new Emp(i, "name" + i)), InsertsType.BULK);
			});

			agent.notSupported(() -> {
				try {
					del(agent, 1);
					Assert.fail();
				} catch (UroborosqlTransactionException ex) {
					// OK
				} catch (Throwable th) {
					Assert.fail();
				}

				try {
					agent.delete(new Emp(2));
					Assert.fail();
				} catch (UroborosqlTransactionException ex) {
					// OK
				} catch (Throwable th) {
					Assert.fail();
				}

				try {
					agent.delete(Emp.class, 3);
					Assert.fail();
				} catch (UroborosqlTransactionException ex) {
					// OK
				} catch (Throwable th) {
					Assert.fail();
				}

				try {
					agent.delete(Emp.class).equal("name", "name4").count();
					Assert.fail();
				} catch (UroborosqlTransactionException ex) {
					// OK
				} catch (Throwable th) {
					Assert.fail();
				}
			});
		}
	}

	@Test
	public void testDeleteWithoutTransaction() {
		config.getSqlAgentFactory().setForceUpdateWithinTransaction(true);

		try (SqlAgent agent = config.agent()) {
			agent.required(() -> {
				agent.inserts(IntStream.range(1, 10).mapToObj(i -> new Emp(i, "name" + i)), InsertsType.BULK);
			});

			try {
				del(agent, 1);
				Assert.fail();
			} catch (UroborosqlTransactionException ex) {
				// OK
			} catch (Throwable th) {
				Assert.fail();
			}

			try {
				agent.delete(new Emp(2));
				Assert.fail();
			} catch (UroborosqlTransactionException ex) {
				// OK
			} catch (Throwable th) {
				Assert.fail();
			}

			try {
				agent.delete(Emp.class, 3);
				Assert.fail();
			} catch (UroborosqlTransactionException ex) {
				// OK
			} catch (Throwable th) {
				Assert.fail();
			}

			try {
				agent.delete(Emp.class).equal("name", "name4").count();
				Assert.fail();
			} catch (UroborosqlTransactionException ex) {
				// OK
			} catch (Throwable th) {
				Assert.fail();
			}
		}
	}

	@Test
	public void testCallStoredFunctionWithinTransaction() {
		config.getSqlAgentFactory().setForceUpdateWithinTransaction(true);

		try (SqlAgent agent = config.agent()) {
			agent.required(() -> {
				agent.updateWith("DROP ALIAS IF EXISTS MYFUNCTION").count();
				agent.updateWith("CREATE ALIAS MYFUNCTION AS $$\r\n" +
						"String toUpperCase(String lower) throws Exception {\r\n" +
						"    return lower.toUpperCase();\r\n" +
						"}\r\n" +
						"$$;").count();

				try {
					Map<String, Object> ans = agent.procWith("{/*ret*/ = call MYFUNCTION(/*param1*/)}")
							.outParam("ret", JDBCType.VARCHAR).param("param1", "test1").call();
					assertThat(ans.get("ret"), is("TEST1"));
				} catch (SQLException ex) {
					Assert.fail();
				}
			});
		}
	}

	@Test
	public void testCallStoredFunctionWithinNewTransaction() {
		config.getSqlAgentFactory().setForceUpdateWithinTransaction(true);

		try (SqlAgent agent = config.agent()) {
			agent.required(() -> {
				agent.updateWith("DROP ALIAS IF EXISTS MYFUNCTION").count();
				agent.updateWith("CREATE ALIAS MYFUNCTION AS $$\r\n" +
						"String toUpperCase(String lower) throws Exception {\r\n" +
						"    return lower.toUpperCase();\r\n" +
						"}\r\n" +
						"$$;").count();
			});

			agent.requiresNew(() -> {
				try {
					Map<String, Object> ans = agent.procWith("{/*ret*/ = call MYFUNCTION(/*param1*/)}")
							.outParam("ret", JDBCType.VARCHAR).param("param1", "test1").call();
					assertThat(ans.get("ret"), is("TEST1"));
				} catch (SQLException ex) {
					Assert.fail();
				}
			});
		}
	}

	@Test
	public void testCallStoredFunctionWithinNotSupportedTransaction() {
		config.getSqlAgentFactory().setForceUpdateWithinTransaction(true);

		try (SqlAgent agent = config.agent()) {
			agent.required(() -> {
				agent.updateWith("DROP ALIAS IF EXISTS MYFUNCTION").count();
				agent.updateWith("CREATE ALIAS MYFUNCTION AS $$\r\n" +
						"String toUpperCase(String lower) throws Exception {\r\n" +
						"    return lower.toUpperCase();\r\n" +
						"}\r\n" +
						"$$;").count();
			});

			agent.notSupported(() -> {
				try {
					agent.procWith("{/*ret*/ = call MYFUNCTION(/*param1*/)}")
							.outParam("ret", JDBCType.VARCHAR).param("param1", "test1").call();
					Assert.fail();
				} catch (UroborosqlTransactionException ex) {
					// OK
				} catch (SQLException ex) {
					Assert.fail();
				}
			});
		}
	}

	@Test
	public void testCallStoredFunctionWithoutTransaction() {
		config.getSqlAgentFactory().setForceUpdateWithinTransaction(true);

		try (SqlAgent agent = config.agent()) {
			agent.required(() -> {
				agent.updateWith("DROP ALIAS IF EXISTS MYFUNCTION").count();
				agent.updateWith("CREATE ALIAS MYFUNCTION AS $$\r\n" +
						"String toUpperCase(String lower) throws Exception {\r\n" +
						"    return lower.toUpperCase();\r\n" +
						"}\r\n" +
						"$$;").count();
			});

			try {
				agent.procWith("{/*ret*/ = call MYFUNCTION(/*param1*/)}")
						.outParam("ret", JDBCType.VARCHAR).param("param1", "test1").call();
				Assert.fail();
			} catch (UroborosqlTransactionException ex) {
				// OK
			} catch (SQLException ex) {
				Assert.fail();
			}
		}
	}

	private int ins(final SqlAgent agent, final int id, final String name) {
		return agent.updateWith("insert into emp (id, name) values (/*id*/0, /*name*/'A')")
				.param("id", id)
				.param("name", name)
				.count();
	}

	private int upd(final SqlAgent agent, final int id, final String name) {
		return agent.updateWith("update emp set name = /*name*/ where id = /*id*/")
				.param("id", id)
				.param("name", name)
				.count();
	}

	private int del(final SqlAgent agent, final int id) {
		return agent.updateWith("delete from emp where 1 = 1 /*IF id >= 0*/ and id = /*id*/0/*END*/")
				.param("id", id)
				.count();
	}

	private List<String> select(final SqlAgent agent) {
		return agent.queryWith("select name from emp order by id")
				.stream()
				.map(m -> m.get("NAME").toString())
				.collect(Collectors.toList());
	}
}
