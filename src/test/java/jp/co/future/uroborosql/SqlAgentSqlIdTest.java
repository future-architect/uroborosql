package jp.co.future.uroborosql;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.contains;

import java.io.BufferedReader;
import java.io.StringReader;
import java.sql.DriverManager;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;

import jp.co.future.uroborosql.config.DefaultSqlConfig;
import jp.co.future.uroborosql.config.SqlConfig;
import jp.co.future.uroborosql.context.SqlContext;
import jp.co.future.uroborosql.filter.AbstractSqlFilter;

import org.junit.Test;

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
		SqlConfig config = DefaultSqlConfig.getConfig(DriverManager.getConnection("jdbc:h2:mem:SqlAgentSqlIdTest"));
		config.getSqlFilterManager().addSqlFilter(new AbstractSqlFilter() {

			@Override
			public ResultSet doQuery(final SqlContext sqlContext, final PreparedStatement preparedStatement, final ResultSet resultSet)
					throws SQLException {
				querys.add(toLines(sqlContext.getExecutableSql()));
				return super.doQuery(sqlContext, preparedStatement, resultSet);
			}

		});
		try (SqlAgent agent = config.createAgent()) {
			agent.update("ddl/create_tables").count();
			agent.query("sqlid_test/select_product").collect();
		}
		assertThat(querys, is(contains(
				Arrays.asList("SELECT /* sqlid_test/select_product */", "\t*", "FROM", "\tPRODUCT", "WHERE 1 = 1", "ORDER BY PRODUCT_ID"))));

	}

	/**
	 * デフォルト動作のテスト
	 *
	 * @throws SQLException SQL実行エラー
	 */
	@Test
	public void testDefault2() throws SQLException {
		List<List<String>> querys = new ArrayList<>();
		SqlConfig config = DefaultSqlConfig.getConfig(DriverManager.getConnection("jdbc:h2:mem:SqlAgentSqlIdTest"));
		config.getSqlFilterManager().addSqlFilter(new AbstractSqlFilter() {

			@Override
			public ResultSet doQuery(final SqlContext sqlContext, final PreparedStatement preparedStatement, final ResultSet resultSet)
					throws SQLException {
				querys.add(toLines(sqlContext.getExecutableSql()));
				return super.doQuery(sqlContext, preparedStatement, resultSet);
			}

		});
		try (SqlAgent agent = config.createAgent()) {
			agent.update("ddl/create_tables").count();
			agent.query("sqlid_test/select_product_custom").collect();
		}
		assertThat(querys, is(contains(
				Arrays.asList("SELECT /* _TESTSQL_ID_ */", "\t*", "FROM", "\tPRODUCT", "WHERE 1 = 1", "ORDER BY PRODUCT_ID"))));
	}

	/**
	 * 設定変更のテスト
	 *
	 * @throws SQLException SQL実行エラー
	 */
	@Test
	public void testCustom() throws SQLException {
		List<List<String>> querys = new ArrayList<>();
		SqlConfig config = DefaultSqlConfig.getConfig(DriverManager.getConnection("jdbc:h2:mem:SqlAgentSqlIdTest"));
		config.getSqlAgentFactory().setSqlIdKeyName("_TESTSQL_ID_");
		config.getSqlFilterManager().addSqlFilter(new AbstractSqlFilter() {

			@Override
			public ResultSet doQuery(final SqlContext sqlContext, final PreparedStatement preparedStatement, final ResultSet resultSet)
					throws SQLException {
				querys.add(toLines(sqlContext.getExecutableSql()));
				return super.doQuery(sqlContext, preparedStatement, resultSet);
			}

		});
		try (SqlAgent agent = config.createAgent()) {
			agent.update("ddl/create_tables").count();
			agent.query("sqlid_test/select_product_custom").collect();
		}
		assertThat(querys, is(contains(
				Arrays.asList("SELECT /* sqlid_test/select_product_custom */", "\t*", "FROM", "\tPRODUCT", "WHERE 1 = 1", "ORDER BY PRODUCT_ID"))));

	}

	private List<String> toLines(final String s) {
		return new BufferedReader(new StringReader(s)).lines().collect(Collectors.toList());
	}
}
