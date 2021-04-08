package jp.co.future.uroborosql;

import static org.hamcrest.MatcherAssert.*;
import static org.hamcrest.Matchers.*;

import java.io.BufferedReader;
import java.io.StringReader;
import java.sql.DriverManager;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.Arrays;
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
	public void testDefault() throws SQLException {
		List<List<String>> querys = new ArrayList<>();
		var config = UroboroSQL.builder(DriverManager.getConnection("jdbc:h2:mem:SqlAgentSqlIdTest"))
				.addSubscriber(s -> s.doOnQuery(e -> {
					querys.add(toLines(e.getExecutionContext().getExecutableSql()));
					return e.getResultSet();
				}))
				.build();

		try (var agent = config.agent()) {
			agent.update("ddl/create_tables").count();
			agent.query("sqlid_test/select_product").collect();
		}
		assertThat(querys, is(contains(
				Arrays.asList("SELECT /* sqlid_test/select_product */", "\t*", "FROM", "\tPRODUCT", "WHERE 1 = 1",
						"ORDER BY PRODUCT_ID"))));

	}

	/**
	 * デフォルト動作のテスト
	 *
	 * @throws SQLException SQL実行エラー
	 */
	@Test
	public void testDefault2() throws SQLException {
		List<List<String>> querys = new ArrayList<>();
		var config = UroboroSQL.builder(DriverManager.getConnection("jdbc:h2:mem:SqlAgentSqlIdTest"))
				.addSubscriber(s -> s.doOnQuery(e -> {
					querys.add(toLines(e.getExecutionContext().getExecutableSql()));
					return e.getResultSet();
				}))
				.build();

		try (var agent = config.agent()) {
			agent.update("ddl/create_tables").count();
			agent.query("sqlid_test/select_product_custom").collect();
		}
		assertThat(querys, is(contains(
				Arrays.asList("SELECT /* _TESTSQL_ID_ */", "\t*", "FROM", "\tPRODUCT", "WHERE 1 = 1",
						"ORDER BY PRODUCT_ID"))));
	}

	/**
	 * 設定変更のテスト
	 *
	 * @throws SQLException SQL実行エラー
	 */
	@Test
	public void testCustom() throws SQLException {
		List<List<String>> querys = new ArrayList<>();
		var config = UroboroSQL.builder(DriverManager.getConnection("jdbc:h2:mem:SqlAgentSqlIdTest"))
				.addSubscriber(s -> s.doOnQuery(e -> {
					querys.add(toLines(e.getExecutionContext().getExecutableSql()));
					return e.getResultSet();
				}))
				.build();
		config.getSqlAgentProvider().setSqlIdKeyName("_TESTSQL_ID_");

		try (var agent = config.agent()) {
			agent.update("ddl/create_tables").count();
			agent.query("sqlid_test/select_product_custom").collect();
		}
		assertThat(querys, is(contains(
				Arrays.asList("SELECT /* sqlid_test/select_product_custom */", "\t*", "FROM", "\tPRODUCT",
						"WHERE 1 = 1", "ORDER BY PRODUCT_ID"))));

	}

	private List<String> toLines(final String s) {
		return new BufferedReader(new StringReader(s)).lines().collect(Collectors.toList());
	}
}
