package jp.co.future.uroborosql.tx;

import static org.hamcrest.CoreMatchers.*;
import static org.hamcrest.MatcherAssert.*;

import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;

import jp.co.future.uroborosql.SqlAgent;
import jp.co.future.uroborosql.UroboroSQL;
import jp.co.future.uroborosql.config.SqlConfig;
import jp.co.future.uroborosql.exception.UroborosqlRuntimeException;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;

public class LocalTxManagerTest {
	/**
	 * SQL管理クラス
	 */
	SqlConfig config;

	@Before
	public void setUp() {
		config = UroboroSQL.builder("jdbc:h2:mem:LocalTxManagerTest;DB_CLOSE_DELAY=-1", "sa", null).build();
		try (SqlAgent agent = config.agent()) {
			agent.updateWith("create table if not exists emp ( \n id VARCHAR(30) \n )").count();

			agent.required(() -> {
				del(agent);
			});
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

				ins(agent, "ABC");

				//トランザクション終了 commit
			});
			assertThat(select(agent), is(Arrays.asList("ABC")));

			agent.required(() -> {
				//トランザクション開始

				del(agent);

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

				ins(agent, "ABC");

				assertThat(select(agent), is(Arrays.asList("ABC")));

				agent.requiresNew(() -> {
					//新しい トランザクション開始

					ins(agent, "DEF");

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

				ins(agent, "ABC");

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

					ins(agent, "ABC");

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
				ins(agent, "A");
				ins(agent, "B");
				agent.setSavepoint("sp");
				ins(agent, "C");

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
				ins(agent, "ABC");

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
				ins(agent, "ABC");
			});//commit
			assertThat(select(agent), is(Arrays.asList("ABC")));

			agent.required(() -> {
				agent.notSupported(() -> {
					ins(agent, "DEF");
				});//commitされないはず

				assertThat(select(agent), is(Arrays.asList("ABC")));
			});
		}
	}

	@Test
	public void testagent04() {

		try (SqlAgent agent = config.agent()) {
			agent.required(() -> {
				ins(agent, "ABC");
				agent.setRollbackOnly();//ロールバックを予約
			});
			assertThat(select(agent), is(Arrays.asList()));

			try {
				agent.required(() -> {
					ins(agent, "ABC");
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
				ins(agent, "A");
				agent.setSavepoint("A");
				ins(agent, "B");
				agent.setSavepoint("B");
				ins(agent, "C");
				agent.setSavepoint("C");

				assertThat(select(agent), is(Arrays.asList("A", "B", "C")));

				agent.rollback("B");

				assertThat(select(agent), is(Arrays.asList("A", "B")));

				agent.rollback("A");
			});
			assertThat(select(agent), is(Arrays.asList("A")));

			agent.required(() -> {
				agent.setSavepoint("X");
				ins(agent, "B");
				agent.setSavepoint("B");
				ins(agent, "C");
				agent.setSavepoint("C");

				assertThat(select(agent), is(Arrays.asList("A", "B", "C")));

				agent.rollback("X");

				assertThat(select(agent), is(Arrays.asList("A")));

				agent.releaseSavepoint("B");

				ins(agent, "B");
				agent.setSavepoint("B");
				ins(agent, "C");
				agent.setSavepoint("C");

				agent.rollback("B");

				assertThat(select(agent), is(Arrays.asList("A", "B")));
			});
			assertThat(select(agent), is(Arrays.asList("A", "B")));
		}
	}

	@Test
	public void testagentEx01() {

		try (SqlAgent agent = config.agent()) {
			agent.required(() -> {
				agent.notSupported(() -> {
					ins(agent, "ABC");
				});
				agent.notSupported(() -> {
					ins(agent, "DEF");
					agent.commit();//明示的にcommit
					//notSupportedは常に同じConnectionが利用されている
				});

				assertThat(select(agent), is(Arrays.asList("ABC", "DEF")));
			});

			agent.required(() -> {
				ins(agent, "GHI");
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
				ins(agent, "ABC");
				ins(agent, "DEF");

				assertThat(select(agent), is(Arrays.asList("ABC", "DEF")));

				agent.rollback();//明示的にrollback

				assertThat(select(agent), is(Arrays.asList()));
			});

			ins(agent, "ABC");
			ins(agent, "DEF");

			assertThat(select(agent), is(Arrays.asList("ABC", "DEF")));

			agent.rollback();//明示的にrollback

			assertThat(select(agent), is(Arrays.asList()));
		}
	}

	private void ins(final SqlAgent agent, final String id) {
		agent.updateWith("insert into emp (id) values (/*id*/'A')").param("id", id).count();
	}

	private void del(final SqlAgent agent) {
		agent.updateWith("delete from emp").count();
	}

	private List<String> select(final SqlAgent agent) {
		return agent.queryWith("select id from emp order by id").stream().map(m -> m.get("ID").toString())
				.collect(Collectors.toList());
	}
}
