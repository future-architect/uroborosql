package jp.co.future.uroborosql.tx;

import static org.hamcrest.CoreMatchers.instanceOf;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.fail;

import java.sql.JDBCType;
import java.sql.SQLException;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

import org.hamcrest.Matchers;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import jp.co.future.uroborosql.SqlAgent;
import jp.co.future.uroborosql.UroboroSQL;
import jp.co.future.uroborosql.config.SqlConfig;
import jp.co.future.uroborosql.connection.ConnectionContextBuilder;
import jp.co.future.uroborosql.enums.InsertsType;
import jp.co.future.uroborosql.exception.UroborosqlRuntimeException;
import jp.co.future.uroborosql.exception.UroborosqlTransactionException;
import jp.co.future.uroborosql.mapping.annotations.Table;

public class LocalTxManagerTest {
	/**
	 * SQL管理クラス
	 */
	SqlConfig config;

	@BeforeEach
	public void setUp() {
		config = UroboroSQL.builder("jdbc:h2:mem:LocalTxManagerTest;DB_CLOSE_DELAY=-1", "sa", null).build();
		try (var agent = config.agent()) {
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

	@AfterEach
	public void tearDown() throws Exception {
	}

	@Test
	void testagentSample01_required() {

		try (var agent = config.agent()) {
			agent.required(() -> {
				//トランザクション開始

				ins(agent, 1, "ABC");

				//トランザクション終了 commit
			});
			assertThat(select(agent), is(List.of("ABC")));

			agent.required(() -> {
				//トランザクション開始

				del(agent, 1);

				//トランザクション終了 commit
			});
			assertThat(select(agent), is(List.of()));
		}
	}

	@Test
	void testagentSample02_requiresNew() {

		try (var agent = config.agent()) {
			agent.required(() -> {
				//トランザクション開始

				ins(agent, 1, "ABC");

				assertThat(select(agent), is(List.of("ABC")));

				agent.requiresNew(() -> {
					//新しい トランザクション開始

					ins(agent, 2, "DEF");

					//上で登録した"ABC"は別Connectionのため入らない
					assertThat(select(agent), is(List.of("DEF")));

					//トランザクション終了 commit
				});

				//トランザクション終了 commit
			});

			assertThat(select(agent), is(List.of("ABC", "DEF")));

		}
	}

	@Test
	void testagentSample03_rollback() {

		try (var agent = config.agent()) {
			agent.required(() -> {
				//トランザクション開始

				ins(agent, 1, "ABC");

				agent.setRollbackOnly();//ロールバックを指示

				assertThat(select(agent), is(List.of("ABC")));//まだロールバックは行われない

				//トランザクション終了 rollback
			});

			assertThat(select(agent), is(List.of()));

		}
	}

	@Test
	void testagentSample04_error_rollback() {

		try (var agent = config.agent()) {
			try {
				agent.required(() -> {
					//トランザクション開始

					ins(agent, 1, "ABC");

					//エラーが起こった場合、ロールバックされる
					throw new IllegalArgumentException();//トランザクション終了 rollback
				});
			} catch (IllegalArgumentException e) {
			}

			assertThat(select(agent), is(List.of()));

		}

	}

	@Test
	void testagentSample05_savepoint() {

		try (var agent = config.agent()) {
			agent.required(() -> {
				ins(agent, 1, "A");
				ins(agent, 2, "B");
				agent.setSavepoint("sp");
				ins(agent, 3, "C");

				assertThat(select(agent), is(List.of("A", "B", "C")));

				agent.rollback("sp");//最後のinsertを取消

				assertThat(select(agent), is(List.of("A", "B")));

			});
		}
	}

	@Test
	void testagent02() {

		try (var agent = config.agent()) {
			agent.required(() -> {
				ins(agent, 1, "ABC");

				assertThat(select(agent), is(List.of("ABC")));

				assertThat(agent.required(() -> select(agent)), is(List.of("ABC")));

				agent.requiresNew(() -> {
					//別Connection
					assertThat(select(agent), is(List.of()));
				});

				assertThat(agent.requiresNew(() -> select(agent)), is(List.of()));

				agent.notSupported(() -> {
					//別Connection
					assertThat(select(agent), is(List.of()));
				});

				assertThat(agent.notSupported(() -> select(agent)), is(List.of()));
			});
		}
	}

	@Test
	void testagent03() {

		try (var agent = config.agent()) {
			agent.required(() -> {
				ins(agent, 1, "ABC");
			});//commit
			assertThat(select(agent), is(List.of("ABC")));

			agent.required(() -> {
				agent.notSupported(() -> {
					ins(agent, 2, "DEF");
				});//commitされないはず

				assertThat(select(agent), is(List.of("ABC")));
			});
		}
	}

	@Test
	void testagent04() {

		try (var agent = config.agent()) {
			agent.required(() -> {
				ins(agent, 1, "ABC");
				agent.setRollbackOnly();//ロールバックを予約
			});
			assertThat(select(agent), is(List.of()));

			try {
				agent.required(() -> {
					ins(agent, 1, "ABC");
					throw new UroborosqlRuntimeException();//エラーのためロールバック
				});
			} catch (UroborosqlRuntimeException e) {
			}
			assertThat(select(agent), is(List.of()));
		}
	}

	@Test
	void testagent05() {

		try (var agent = config.agent()) {
			agent.required(() -> {
				agent.setSavepoint("X");
				ins(agent, 1, "A");
				agent.setSavepoint("A");
				ins(agent, 2, "B");
				agent.setSavepoint("B");
				ins(agent, 3, "C");
				agent.setSavepoint("C");

				assertThat(select(agent), is(List.of("A", "B", "C")));

				agent.rollback("B");

				assertThat(select(agent), is(List.of("A", "B")));

				agent.rollback("A");
			});
			assertThat(select(agent), is(List.of("A")));

			agent.required(() -> {
				agent.setSavepoint("X");
				ins(agent, 2, "B");
				agent.setSavepoint("B");
				ins(agent, 3, "C");
				agent.setSavepoint("C");

				assertThat(select(agent), is(List.of("A", "B", "C")));

				agent.rollback("X");

				assertThat(select(agent), is(List.of("A")));

				agent.releaseSavepoint("B");

				ins(agent, 2, "B");
				agent.setSavepoint("B");
				ins(agent, 3, "C");
				agent.setSavepoint("C");

				agent.rollback("B");

				assertThat(select(agent), is(List.of("A", "B")));
			});
			assertThat(select(agent), is(List.of("A", "B")));
		}
	}

	@Test
	void testSavepointScopeRunnable() {
		try (var agent = config.agent()) {
			agent.required(() -> {
				agent.savepointScope(() -> {
					ins(agent, 1, "A");
					try {
						agent.savepointScope(() -> {
							ins(agent, 2, "B");
							try {
								agent.savepointScope(() -> {
									ins(agent, 3, "C");
									assertThat(select(agent), is(List.of("A", "B", "C")));
									throw new IllegalStateException();
								});
							} catch (Exception ex) {
								assertThat(select(agent), is(List.of("A", "B")));
								assertThat(ex, is(instanceOf(IllegalStateException.class)));
								throw ex;
							}
							fail();
						});
					} catch (Exception ex) {
						assertThat(select(agent), is(List.of("A")));
						assertThat(ex, is(instanceOf(IllegalStateException.class)));
					}
				});
				assertThat(select(agent), is(List.of("A")));
			});
		}
	}

	@Test
	void testSavepointScopeSupplier() {
		try (var agent = config.agent()) {
			agent.required(() -> {
				try {
					agent.savepointScope(() -> {
						ins(agent, 1, "A");
						assertThat(agent.savepointScope(() -> {
							ins(agent, 2, "B");
							return select(agent);
						}), is(List.of("A", "B")));
						throw new IllegalAccessError();
					});
				} catch (Throwable th) {
					assertThat(select(agent), is(Matchers.emptyCollectionOf(String.class)));
					assertThat(th, is(instanceOf(IllegalAccessError.class)));
				}
			});
		}
	}

	@Test
	void testSavepointScopeWithUnmanagedConnection() {
		try (var agent = config.agent()) {
			try {
				agent.savepointScope(() -> {
					throw new IllegalAccessError();
				});
			} catch (UroborosqlRuntimeException ex) {
				assertThat(ex.getMessage(), is("UnmanagedConnection cannot use savepoint."));
			}
		}
	}

	@Test
	void testagentEx01() {

		try (var agent = config.agent()) {
			agent.required(() -> {
				agent.notSupported(() -> {
					ins(agent, 1, "ABC");
				});
				agent.notSupported(() -> {
					ins(agent, 2, "DEF");
					agent.commit();//明示的にcommit
					//notSupportedは常に同じConnectionが利用されている
				});

				assertThat(select(agent), is(List.of("ABC", "DEF")));
			});

			agent.required(() -> {
				ins(agent, 3, "GHI");
				agent.commit();//Commit

				agent.requiresNew(() -> {
					//別ConnectionだけどCommitしたからデータが見れるはず
					assertThat(select(agent), is(List.of("ABC", "DEF", "GHI")));
				});

			});
		}
	}

	@Test
	void testagentEx02() {

		try (var agent = config.agent()) {
			agent.required(() -> {
				ins(agent, 1, "ABC");
				ins(agent, 2, "DEF");

				assertThat(select(agent), is(List.of("ABC", "DEF")));

				agent.rollback();//明示的にrollback

				assertThat(select(agent), is(List.of()));
			});

			ins(agent, 1, "ABC");
			ins(agent, 2, "DEF");

			assertThat(select(agent), is(List.of("ABC", "DEF")));

			agent.rollback();//明示的にrollback

			assertThat(select(agent), is(List.of()));
		}
	}

	@Test
	void testSelectWithinTransaction() {
		config.getSqlAgentProvider().setForceUpdateWithinTransaction(true);

		try (var agent = config.agent()) {
			agent.required(() -> {
				select(agent);
				assertThat(agent.query(Emp.class)
						.collect().size(), is(0));
			});
		}
	}

	@Test
	void testSelectWithinNewTransaction() {
		config.getSqlAgentProvider().setForceUpdateWithinTransaction(true);

		try (var agent = config.agent()) {
			agent.requiresNew(() -> {
				select(agent);
				assertThat(agent.query(Emp.class)
						.collect().size(), is(0));
			});
		}
	}

	@Test
	void testSelectWithinNotSupportedTransaction() {
		config.getSqlAgentProvider().setForceUpdateWithinTransaction(true);

		try (var agent = config.agent()) {
			agent.notSupported(() -> {
				select(agent);
				assertThat(agent.query(Emp.class)
						.collect().size(), is(0));
			});
		}
	}

	@Test
	void testSelectWithoutTransaction() {
		config.getSqlAgentProvider().setForceUpdateWithinTransaction(true);

		try (var agent = config.agent()) {
			select(agent);
			assertThat(agent.query(Emp.class)
					.collect().size(), is(0));
		}
	}

	@Test
	void testInsertWithinTransaction() {
		config.getSqlAgentProvider().setForceUpdateWithinTransaction(true);

		try (var agent = config.agent()) {
			agent.required(() -> {
				ins(agent, 1, "ABC");

				var emp = new Emp();
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
	void testInsertWithinNewTransaction() {
		config.getSqlAgentProvider().setForceUpdateWithinTransaction(true);

		try (var agent = config.agent()) {
			agent.requiresNew(() -> {
				ins(agent, 1, "ABC");

				var emp = new Emp();
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
	void testInsertWithinNotSupportedTransaction() {
		config.getSqlAgentProvider().setForceUpdateWithinTransaction(true);

		try (var agent = config.agent()) {
			agent.notSupported(() -> {
				try {
					ins(agent, 1, "ABC");
					Assertions.fail();
				} catch (UroborosqlTransactionException ex) {
					// OK
				} catch (Throwable th) {
					Assertions.fail();
				}

				try {
					var emp = new Emp();
					emp.setId(2);
					emp.setName("DEF");
					agent.insert(emp);
					Assertions.fail();
				} catch (UroborosqlTransactionException ex) {
					// OK
				} catch (Throwable th) {
					Assertions.fail();
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
					Assertions.fail();
				} catch (UroborosqlTransactionException ex) {
					// OK
				} catch (Throwable th) {
					Assertions.fail();
				}

				try {
					agent.inserts(IntStream.range(1, 10).mapToObj(i -> new Emp(i, "name" + i)), InsertsType.BATCH);
					Assertions.fail();
				} catch (UroborosqlTransactionException ex) {
					// OK
				} catch (Throwable th) {
					Assertions.fail();
				}

				try {
					agent.inserts(IntStream.range(1, 10).mapToObj(i -> new Emp(i, "name" + i)), InsertsType.BULK);
					Assertions.fail();
				} catch (UroborosqlTransactionException ex) {
					// OK
				} catch (Throwable th) {
					Assertions.fail();
				}
			});
		}
	}

	@Test
	void testInsertWithoutTransaction() {
		config.getSqlAgentProvider().setForceUpdateWithinTransaction(true);

		try (var agent = config.agent()) {
			try {
				ins(agent, 1, "ABC");
				Assertions.fail();
			} catch (UroborosqlTransactionException ex) {
				// OK
			} catch (Throwable th) {
				Assertions.fail();
			}

			try {
				var emp = new Emp();
				emp.setId(2);
				emp.setName("DEF");
				agent.insert(emp);
				Assertions.fail();
			} catch (UroborosqlTransactionException ex) {
				// OK
			} catch (Throwable th) {
				Assertions.fail();
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
				Assertions.fail();
			} catch (UroborosqlTransactionException ex) {
				// OK
			} catch (Throwable th) {
				Assertions.fail();
			}

			try {
				agent.inserts(IntStream.range(1, 10).mapToObj(i -> new Emp(i, "name" + i)), InsertsType.BATCH);
				Assertions.fail();
			} catch (UroborosqlTransactionException ex) {
				// OK
			} catch (Throwable th) {
				Assertions.fail();
			}

			try {
				agent.inserts(IntStream.range(1, 10).mapToObj(i -> new Emp(i, "name" + i)), InsertsType.BULK);
				Assertions.fail();
			} catch (UroborosqlTransactionException ex) {
				// OK
			} catch (Throwable th) {
				Assertions.fail();
			}
		}
	}

	@Test
	void testUpdateWithinTransaction() {
		config.getSqlAgentProvider().setForceUpdateWithinTransaction(true);

		try (var agent = config.agent()) {
			agent.required(() -> {
				agent.inserts(IntStream.range(1, 10).mapToObj(i -> new Emp(i, "name" + i)), InsertsType.BULK);

				assertThat(upd(agent, 1, "abc"), is(1));

				assertThat(agent.update(new Emp(2, "def")), is(1));
			});
		}
	}

	@Test
	void testUpdateWithinNewTransaction() {
		config.getSqlAgentProvider().setForceUpdateWithinTransaction(true);

		try (var agent = config.agent()) {
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
	void testUpdateWithinNotSupportedTransaction() {
		config.getSqlAgentProvider().setForceUpdateWithinTransaction(true);

		try (var agent = config.agent()) {
			agent.required(() -> {
				agent.inserts(IntStream.range(1, 10).mapToObj(i -> new Emp(i, "name" + i)), InsertsType.BULK);
			});

			agent.notSupported(() -> {
				try {
					upd(agent, 1, "abc");
					Assertions.fail();
				} catch (UroborosqlTransactionException ex) {
					// OK
				} catch (Throwable th) {
					Assertions.fail();
				}

				try {
					agent.update(new Emp(2, "def"));
					Assertions.fail();
				} catch (UroborosqlTransactionException ex) {
					// OK
				} catch (Throwable th) {
					Assertions.fail();
				}
			});
		}
	}

	@Test
	void testUpdateWithoutTransaction() {
		config.getSqlAgentProvider().setForceUpdateWithinTransaction(true);

		try (var agent = config.agent()) {
			agent.required(() -> {
				agent.inserts(IntStream.range(1, 10).mapToObj(i -> new Emp(i, "name" + i)), InsertsType.BULK);
			});

			try {
				upd(agent, 1, "abc");
				Assertions.fail();
			} catch (UroborosqlTransactionException ex) {
				// OK
			} catch (Throwable th) {
				Assertions.fail();
			}

			try {
				agent.update(new Emp(2, "def"));
				Assertions.fail();
			} catch (UroborosqlTransactionException ex) {
				// OK
			} catch (Throwable th) {
				Assertions.fail();
			}
		}
	}

	@Test
	void testDeleteWithinTransaction() {
		config.getSqlAgentProvider().setForceUpdateWithinTransaction(true);

		try (var agent = config.agent()) {
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
	void testDeleteWithinNewTransaction() {
		config.getSqlAgentProvider().setForceUpdateWithinTransaction(true);

		try (var agent = config.agent()) {
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
	void testDeleteWithinNotSupportedTransaction() {
		config.getSqlAgentProvider().setForceUpdateWithinTransaction(true);

		try (var agent = config.agent()) {
			agent.required(() -> {
				agent.inserts(IntStream.range(1, 10).mapToObj(i -> new Emp(i, "name" + i)), InsertsType.BULK);
			});

			agent.notSupported(() -> {
				try {
					del(agent, 1);
					Assertions.fail();
				} catch (UroborosqlTransactionException ex) {
					// OK
				} catch (Throwable th) {
					Assertions.fail();
				}

				try {
					agent.delete(new Emp(2));
					Assertions.fail();
				} catch (UroborosqlTransactionException ex) {
					// OK
				} catch (Throwable th) {
					Assertions.fail();
				}

				try {
					agent.delete(Emp.class, 3);
					Assertions.fail();
				} catch (UroborosqlTransactionException ex) {
					// OK
				} catch (Throwable th) {
					Assertions.fail();
				}

				try {
					agent.delete(Emp.class).equal("name", "name4").count();
					Assertions.fail();
				} catch (UroborosqlTransactionException ex) {
					// OK
				} catch (Throwable th) {
					Assertions.fail();
				}
			});
		}
	}

	@Test
	void testDeleteWithoutTransaction() {
		config.getSqlAgentProvider().setForceUpdateWithinTransaction(true);

		try (var agent = config.agent()) {
			agent.required(() -> {
				agent.inserts(IntStream.range(1, 10).mapToObj(i -> new Emp(i, "name" + i)), InsertsType.BULK);
			});

			try {
				del(agent, 1);
				Assertions.fail();
			} catch (UroborosqlTransactionException ex) {
				// OK
			} catch (Throwable th) {
				Assertions.fail();
			}

			try {
				agent.delete(new Emp(2));
				Assertions.fail();
			} catch (UroborosqlTransactionException ex) {
				// OK
			} catch (Throwable th) {
				Assertions.fail();
			}

			try {
				agent.delete(Emp.class, 3);
				Assertions.fail();
			} catch (UroborosqlTransactionException ex) {
				// OK
			} catch (Throwable th) {
				Assertions.fail();
			}

			try {
				agent.delete(Emp.class).equal("name", "name4").count();
				Assertions.fail();
			} catch (UroborosqlTransactionException ex) {
				// OK
			} catch (Throwable th) {
				Assertions.fail();
			}
		}
	}

	@Test
	void testCallStoredFunctionWithinTransaction() {
		config.getSqlAgentProvider().setForceUpdateWithinTransaction(true);

		try (var agent = config.agent()) {
			agent.required(() -> {
				agent.updateWith("DROP ALIAS IF EXISTS MYFUNCTION").count();
				agent.updateWith("CREATE ALIAS MYFUNCTION AS $$\r\n" +
						"String toUpperCase(String lower) throws Exception {\r\n" +
						"    return lower.toUpperCase();\r\n" +
						"}\r\n" +
						"$$;").count();

				try {
					var ans = agent.procWith("{/*ret*/ = call MYFUNCTION(/*param1*/)}")
							.outParam("ret", JDBCType.VARCHAR)
							.param("param1", "test1").call();
					assertThat(ans.get("ret"), is("TEST1"));
				} catch (SQLException ex) {
					Assertions.fail();
				}
			});
		}
	}

	@Test
	void testCallStoredFunctionWithinNewTransaction() {
		config.getSqlAgentProvider().setForceUpdateWithinTransaction(true);

		try (var agent = config.agent()) {
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
					var ans = agent.procWith("{/*ret*/ = call MYFUNCTION(/*param1*/)}")
							.outParam("ret", JDBCType.VARCHAR)
							.param("param1", "test1").call();
					assertThat(ans.get("ret"), is("TEST1"));
				} catch (SQLException ex) {
					Assertions.fail();
				}
			});
		}
	}

	@Test
	void testCallStoredFunctionWithinNotSupportedTransaction() {
		config.getSqlAgentProvider().setForceUpdateWithinTransaction(true);

		try (var agent = config.agent()) {
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
							.outParam("ret", JDBCType.VARCHAR)
							.param("param1", "test1").call();
					Assertions.fail();
				} catch (UroborosqlTransactionException ex) {
					// OK
				} catch (SQLException ex) {
					Assertions.fail();
				}
			});
		}
	}

	@Test
	void testCallStoredFunctionWithoutTransaction() {
		config.getSqlAgentProvider().setForceUpdateWithinTransaction(true);

		try (var agent = config.agent()) {
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
						.outParam("ret", JDBCType.VARCHAR)
						.param("param1", "test1").call();
				Assertions.fail();
			} catch (UroborosqlTransactionException ex) {
				// OK
			} catch (SQLException ex) {
				Assertions.fail();
			}
		}
	}

	@Test
	void testInsertRunnableWithinAutoCommit() {
		config.getSqlAgentProvider().setForceUpdateWithinTransaction(true);

		try (var agent = config.agent()) {
			agent.required(() -> {
				agent.autoCommitScope(() -> {
					agent.insert(new Emp(1, "name1")); // autoCommit=trueで実行される

					// 明示的にロールバックする
					agent.rollback();

					// ロールバックしてもautoCommitで実行されたEmpが登録されていることの確認
					agent.query(Emp.class)
							.equal("id", 1)
							.first()
							.orElseThrow(UroborosqlRuntimeException::new);
				});
				agent.query(Emp.class)
						.equal("id", 1)
						.first()
						.orElseThrow(UroborosqlRuntimeException::new);
			});
		}
	}

	@Test
	void testInsertRunnableWithinAutoCommitNoTransaction() {
		config.getSqlAgentProvider().setForceUpdateWithinTransaction(true);

		try (var agent = config.agent()) {
			agent.autoCommitScope(() -> {
				agent.insert(new Emp(1, "name1")); // autoCommit=trueで実行される

				// 明示的にロールバックする
				agent.rollback();

				// ロールバックしてもautoCommitで実行されたEmpが登録されていることの確認
				agent.query(Emp.class)
						.equal("id", 1)
						.first()
						.orElseThrow(UroborosqlRuntimeException::new);
			});
			agent.query(Emp.class)
					.equal("id", 1)
					.first()
					.orElseThrow(UroborosqlRuntimeException::new);
		}
	}

	@Test
	void testInsertSupplierWithinAutoCommit() {
		config.getSqlAgentProvider().setForceUpdateWithinTransaction(true);

		try (var agent = config.agent()) {
			var emp = agent.required(() -> agent.autoCommitScope(() -> {
				agent.insert(new Emp(1, "name1")); // autoCommit=trueで実行される

				// 明示的にロールバックする
				agent.rollback();

				// ロールバックしてもautoCommitで実行されたEmpが登録されていることの確認
				return agent.query(Emp.class)
						.equal("id", 1)
						.first()
						.orElseThrow(UroborosqlRuntimeException::new);
			}));
			assertThat(emp, notNullValue());
		}
	}

	@Test
	void testInsertSupplierWithinAutoCommitNoTransaction() {
		config.getSqlAgentProvider().setForceUpdateWithinTransaction(true);

		try (var agent = config.agent()) {
			var emp = agent.autoCommitScope(() -> {
				agent.insert(new Emp(1, "name1")); // autoCommit=trueで実行される

				// 明示的にロールバックする
				agent.rollback();

				// ロールバックしてもautoCommitで実行されたEmpが登録されていることの確認
				return agent.query(Emp.class)
						.equal("id", 1)
						.first()
						.orElseThrow(UroborosqlRuntimeException::new);
			});
			assertThat(emp, notNullValue());
		}
	}

	@Test
	void testUpdateSupplierWithinAutoCommitThrowException() {
		config.getSqlAgentProvider().setForceUpdateWithinTransaction(true);

		try (var agent = config.agent()) {
			var emp = agent.required(() -> {
				try {
					agent.autoCommitScope(() -> {
						agent.insert(new Emp(1, "name1")); // autoCommit=trueで実行される

						// 例外をスローする
						throw new IllegalStateException();
					});
				} catch (Exception ex) {
					// ここでは握りつぶす
				}
				// 例外がスローされる前にInsertされたレコードが登録されている
				return agent.query(Emp.class)
						.equal("id", 1)
						.first()
						.orElseThrow(UroborosqlRuntimeException::new);
			});
			assertThat(emp, notNullValue());
		}
	}

	@Test
	void testUpdateSupplierWithinAutoCommitBeforeAutoCommit() {
		config.getSqlAgentProvider().setForceUpdateWithinTransaction(true);

		try (var agent = config.agent(ConnectionContextBuilder
				.jdbc("jdbc:h2:mem:LocalTxManagerTest;DB_CLOSE_DELAY=-1", "sa", null).autoCommit(true))) {
			var emp = agent.required(() -> {
				try {
					agent.autoCommitScope(() -> {
						agent.insert(new Emp(1, "name1")); // autoCommit=trueで実行される

						// 例外をスローする
						throw new IllegalStateException();
					});
				} catch (Exception ex) {
					// ここでは握りつぶす
				}
				// 例外がスローされる前にInsertされたレコードが登録されている
				return agent.query(Emp.class)
						.equal("id", 1)
						.first()
						.orElseThrow(UroborosqlRuntimeException::new);
			});
			assertThat(emp, notNullValue());
		}
	}

	@Test
	void testUpdateRunnableWithinAutoCommitThrowExceptionNoTransaction() {
		config.getSqlAgentProvider().setForceUpdateWithinTransaction(true);

		try (var agent = config.agent()) {
			try {
				agent.autoCommitScope(() -> {
					agent.insert(new Emp(1, "name1")); // autoCommit=trueで実行される

					// 例外をスローする
					throw new IllegalStateException();
				});
			} catch (Exception ex) {
				// ここでは握りつぶす
			}
			// 例外がスローされる前にInsertされたレコードが登録されている
			var emp = agent.query(Emp.class)
					.equal("id", 1)
					.first()
					.orElseThrow(UroborosqlRuntimeException::new);
			assertThat(emp, notNullValue());
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
