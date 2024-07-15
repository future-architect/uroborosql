package jp.co.future.uroborosql;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.contains;
import static org.hamcrest.Matchers.is;

import java.io.BufferedReader;
import java.io.StringReader;
import java.sql.DriverManager;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

import org.junit.jupiter.api.Test;

/**
 * エラーハンドリングのテスト
 *
 * @author ota
 */
public class SqlAgentSqlIdTest {

	/**
	 * デフォルト動作のテスト
	 *
	 * @throws SQLException SQL実行エラー
	 */
	@Test
	void testDefault() throws SQLException {
		List<List<String>> querys = new ArrayList<>();
		var config = UroboroSQL.builder(DriverManager.getConnection("jdbc:h2:mem:" + this.getClass().getSimpleName() + ";DB_CLOSE_DELAY=-1")).build();
		config.getEventListenerHolder().addAfterSqlQueryListener(evt -> {
			querys.add(toLines(evt.getExecutionContext().getExecutableSql()));
		});
		try (var agent = config.agent()) {
			agent.update("ddl/create_tables").count();
			agent.query("sqlid_test/select_product")
					.collect();
		}
		assertThat(querys, is(contains(
				List.of("SELECT /* sqlid_test/select_product */", "\t*", "FROM", "\tPRODUCT", "WHERE 1 = 1",
						"ORDER BY PRODUCT_ID"))));

	}

	/**
	 * デフォルト動作のテスト
	 *
	 * @throws SQLException SQL実行エラー
	 */
	@Test
	void testDefault2() throws SQLException {
		List<List<String>> querys = new ArrayList<>();
		var config = UroboroSQL.builder(DriverManager.getConnection("jdbc:h2:mem:" + this.getClass().getSimpleName() + ";DB_CLOSE_DELAY=-1")).build();
		config.getEventListenerHolder().addAfterSqlQueryListener(evt -> {
			querys.add(toLines(evt.getExecutionContext().getExecutableSql()));
		});
		try (var agent = config.agent()) {
			agent.update("ddl/create_tables").count();
			agent.query("sqlid_test/select_product_custom")
					.collect();
		}
		assertThat(querys, is(contains(
				List.of("SELECT /* _TESTSQL_ID_ */", "\t*", "FROM", "\tPRODUCT", "WHERE 1 = 1",
						"ORDER BY PRODUCT_ID"))));
	}

	/**
	 * 設定変更のテスト
	 *
	 * @throws SQLException SQL実行エラー
	 */
	@Test
	void testCustom() throws SQLException {
		List<List<String>> querys = new ArrayList<>();
		var config = UroboroSQL.builder(DriverManager.getConnection("jdbc:h2:mem:" + this.getClass().getSimpleName() + ";DB_CLOSE_DELAY=-1")).build();
		config.getSqlAgentProvider().setSqlIdKeyName("_TESTSQL_ID_");
		config.getEventListenerHolder().addAfterSqlQueryListener(evt -> {
			querys.add(toLines(evt.getExecutionContext().getExecutableSql()));
		});
		try (var agent = config.agent()) {
			agent.update("ddl/create_tables").count();
			agent.query("sqlid_test/select_product_custom")
					.collect();
		}
		assertThat(querys, is(contains(
				List.of("SELECT /* sqlid_test/select_product_custom */", "\t*", "FROM", "\tPRODUCT",
						"WHERE 1 = 1", "ORDER BY PRODUCT_ID"))));

	}

	private List<String> toLines(final String s) {
		return new BufferedReader(new StringReader(s)).lines().collect(Collectors.toList());
	}
}
