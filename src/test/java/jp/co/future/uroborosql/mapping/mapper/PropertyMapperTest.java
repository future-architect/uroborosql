package jp.co.future.uroborosql.mapping.mapper;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;

import java.math.BigInteger;
import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.Statement;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.Optional;
import java.util.OptionalDouble;
import java.util.OptionalInt;
import java.util.OptionalLong;

import jp.co.future.uroborosql.SqlAgent;
import jp.co.future.uroborosql.config.DefaultSqlConfig;
import jp.co.future.uroborosql.config.SqlConfig;
import jp.co.future.uroborosql.filter.AuditLogSqlFilter;
import jp.co.future.uroborosql.filter.SqlFilterManager;
import jp.co.future.uroborosql.mapping.annotations.Table;

import org.apache.commons.lang3.builder.EqualsBuilder;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.apache.commons.lang3.builder.ToStringBuilder;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;

public class PropertyMapperTest {
	private static SqlConfig config;

	@BeforeClass
	public static void setUpBeforeClass() throws Exception {
		String url = "jdbc:h2:mem:PropertyMapperTest;DB_CLOSE_DELAY=-1";
		String user = null;
		String password = null;

		try (Connection conn = DriverManager.getConnection(url, user, password)) {
			conn.setAutoCommit(false);
			// テーブル作成
			try (Statement stmt = conn.createStatement()) {
				stmt.execute(
						"drop table if exists test");
				stmt.execute(
						"create table if not exists test( "
								+ "id NUMERIC(4)"
								+ ",name VARCHAR(10)"
								+ ",int_value NUMERIC(5)"
								+ ",long_value NUMERIC(5)"
								+ ",double_value NUMERIC(5,3)"
								+ ",date_value DATE"
								+ ",datetime_value DATETIME"
								+ ",enum_value VARCHAR(20)"
								+ ",big_int_value NUMERIC(10)"
								+ ",memo VARCHAR(500)"

								+ ", primary key(id))");
			}
		}

		config = DefaultSqlConfig.getConfig(url, user, password);

		SqlFilterManager sqlFilterManager = config.getSqlFilterManager();
		sqlFilterManager.addSqlFilter(new AuditLogSqlFilter());
	}

	@Before
	public void setUpBefore() throws Exception {
		try (SqlAgent agent = config.createAgent()) {
			agent.updateWith("delete from test").count();
			agent.commit();
		}
	}

	public static enum TesEnum {
		A_VALUE, B_VALUE,
	}

	@Table(name = "TEST")
	public static class PropertyMapperTestEntity {
		private long id;
		private Optional<String> name;
		private OptionalInt intValue;
		private OptionalLong longValue;
		private OptionalDouble doubleValue;
		private LocalDate dateValue;
		private LocalDateTime datetimeValue;
		private TesEnum enumValue;
		private BigInteger bigIntValue;

		public PropertyMapperTestEntity() {
		}

		public PropertyMapperTestEntity(final int id) {
			this.id = id;
			this.name = Optional.of("name");
			this.intValue = OptionalInt.of(2);
			this.longValue = OptionalLong.of(3);
			this.doubleValue = OptionalDouble.of(4.5);
			this.dateValue = LocalDate.now();
			this.datetimeValue = LocalDateTime.now();
			this.enumValue = TesEnum.B_VALUE;
			this.bigIntValue = BigInteger.TEN;
		}

		@Override
		public int hashCode() {
			return HashCodeBuilder.reflectionHashCode(this, true);
		}

		@Override
		public boolean equals(final Object obj) {
			return EqualsBuilder.reflectionEquals(this, obj, true);
		}

		@Override
		public String toString() {
			return ToStringBuilder.reflectionToString(this);
		}
	}

	@Test
	public void test01() throws Exception {

		try (SqlAgent agent = config.createAgent()) {
			agent.required(() -> {
				PropertyMapperTestEntity test1 = new PropertyMapperTestEntity(1);
				agent.insert(test1);
				PropertyMapperTestEntity data = agent.getFromKey(PropertyMapperTestEntity.class, 1).orElse(null);
				assertThat(data, is(test1));
			});
		}
	}

	@Test
	public void test02() throws Exception {

		try (SqlAgent agent = config.createAgent()) {
			agent.required(() -> {
				PropertyMapperTestEntity test1 = new PropertyMapperTestEntity(1);
				test1.intValue = OptionalInt.empty();
				test1.longValue = OptionalLong.empty();
				test1.doubleValue = OptionalDouble.empty();
				test1.dateValue = null;
				test1.datetimeValue = null;
				test1.enumValue = null;
				test1.bigIntValue = null;
				agent.insert(test1);
				PropertyMapperTestEntity data = agent.getFromKey(PropertyMapperTestEntity.class, 1).orElse(null);
				assertThat(data, is(test1));
			});
		}
	}
}
