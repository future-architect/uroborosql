package jp.co.future.uroborosql.mapping;

import static org.hamcrest.CoreMatchers.*;
import static org.hamcrest.MatcherAssert.*;

import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;

import jp.co.future.uroborosql.SqlAgent;
import jp.co.future.uroborosql.UroboroSQL;
import jp.co.future.uroborosql.config.SqlConfig;
import jp.co.future.uroborosql.filter.AuditLogSqlFilter;
import jp.co.future.uroborosql.filter.SqlFilterManager;
import jp.co.future.uroborosql.mapping.annotations.Table;
import jp.co.future.uroborosql.mapping.mapper.PropertyMapper;
import jp.co.future.uroborosql.mapping.mapper.PropertyMapperManager;
import jp.co.future.uroborosql.parameter.mapper.BindParameterMapper;
import jp.co.future.uroborosql.parameter.mapper.BindParameterMapperManager;

import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;

/**
 * Mapperカスタム実装のテスト
 */
public class CustomMapperTest {
	public static class Name {
		private final String s;

		public Name(final String s) {
			this.s = s;
		}
	}

	@SuppressWarnings("unused")
	public static class TestEntity {
		private long id;
		private Name name;

		public TestEntity() {
		}

		public TestEntity(final long id, final String name) {
			this.id = id;
			this.name = new Name(name);
		}
	}

	private static class CustomMapper implements PropertyMapper<Name>, BindParameterMapper<Name> {
		@Override
		public Object toJdbc(final Name original, final Connection connection,
				final BindParameterMapperManager parameterMapperManager) {
			return "-" + original.s.toLowerCase() + "-";
		}

		@Override
		public Name getValue(final JavaType type, final ResultSet rs, final int columnIndex,
				final PropertyMapperManager mapperManager)
				throws SQLException {
			String s = rs.getString(columnIndex);
			return s != null ? new Name(s.toUpperCase().replaceAll("^-", "").replaceAll("-$", "")) : null;
		}
	}

	private static SqlConfig config;

	@BeforeClass
	public static void setUpBeforeClass() throws Exception {
		String url = "jdbc:h2:mem:CustomMapperTest;DB_CLOSE_DELAY=-1";
		String user = null;
		String password = null;

		try (Connection conn = DriverManager.getConnection(url, user, password)) {
			conn.setAutoCommit(false);
			// テーブル作成
			try (Statement stmt = conn.createStatement()) {
				stmt.execute(
						"drop table if exists test");
				stmt.execute(
						"create table if not exists test( id NUMERIC(4),name VARCHAR(10), primary key(id))");
			}
		}

		config = UroboroSQL.builder(url, user, password).build();

		// Mapperの登録
		CustomMapper customMapper = new CustomMapper();
		config.getSqlContextFactory().addBindParamMapper(customMapper);
		config.getEntityHandler().addPropertyMapper(customMapper);

		SqlFilterManager sqlFilterManager = config.getSqlFilterManager();
		sqlFilterManager.addSqlFilter(new AuditLogSqlFilter());
	}

	@Before
	public void setUpBefore() throws Exception {
		try (SqlAgent agent = config.agent()) {
			agent.required(() -> {
				agent.updateWith("delete from test").count();

				// 準備
				for (int i = 0; i < 24; i++) {
					TestEntity test = new TestEntity(i + 1, "NAME" + (i + 1));
					agent.insert(test);
				}
			});
			agent.commit();
		}
	}

	@SuppressWarnings("unused")
	@Table(name = "TEST")
	public static class Test2Entity {
		private long id;
		private String name;
	}

	@Test
	public void testFind() throws Exception {

		try (SqlAgent agent = config.agent()) {
			TestEntity data = agent.find(TestEntity.class, 2).orElse(null);
			assertThat(data.name.s, is("NAME2"));

			Test2Entity data2 = agent.find(Test2Entity.class, 2).orElse(null);
			assertThat(data2.name, is("-name2-"));

		}
	}
}
