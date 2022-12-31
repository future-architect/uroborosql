package jp.co.future.uroborosql.mapping.mapper;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;

import java.math.BigInteger;
import java.sql.DriverManager;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.Objects;
import java.util.Optional;
import java.util.OptionalDouble;
import java.util.OptionalInt;
import java.util.OptionalLong;

import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;

import jp.co.future.uroborosql.UroboroSQL;
import jp.co.future.uroborosql.config.SqlConfig;
import jp.co.future.uroborosql.filter.AuditLogSqlFilter;
import jp.co.future.uroborosql.mapping.annotations.Table;

public class PropertyMapperTest {
	private static SqlConfig config;

	@BeforeClass
	public static void setUpBeforeClass() throws Exception {
		var url = "jdbc:h2:mem:PropertyMapperTest;DB_CLOSE_DELAY=-1";
		String user = null;
		String password = null;

		try (var conn = DriverManager.getConnection(url, user, password)) {
			conn.setAutoCommit(false);
			// テーブル作成
			try (var stmt = conn.createStatement()) {
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
								+ ",datetime_value TIMESTAMP(9)"
								+ ",enum_value VARCHAR(20)"
								+ ",big_int_value NUMERIC(10)"
								+ ",memo VARCHAR(500)"

								+ ", primary key(id))");
			}
		}

		config = UroboroSQL.builder(url, user, password).build();

		var sqlFilterManager = config.getSqlFilterManager();
		sqlFilterManager.addSqlFilter(new AuditLogSqlFilter());
	}

	@Before
	public void setUpBefore() throws Exception {
		try (var agent = config.agent()) {
			agent.updateWith("delete from test").count();
			agent.commit();
		}
	}

	public enum TesEnum {
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
			return Objects.hash(bigIntValue, dateValue, datetimeValue, doubleValue, enumValue, id, intValue, longValue,
					name);
		}

		@Override
		public boolean equals(final Object obj) {
			if (this == obj) {
				return true;
			}
			if (obj == null || getClass() != obj.getClass()) {
				return false;
			}
			var other = (PropertyMapperTestEntity) obj;
			if (!Objects.equals(bigIntValue, other.bigIntValue) || !Objects.equals(dateValue, other.dateValue)
					|| !Objects.equals(datetimeValue, other.datetimeValue)
					|| !Objects.equals(doubleValue, other.doubleValue)) {
				return false;
			}
			if (enumValue != other.enumValue) {
				return false;
			}
			if (id != other.id) {
				return false;
			}
			if (!Objects.equals(intValue, other.intValue)) {
				return false;
			}
			if (!Objects.equals(longValue, other.longValue)) {
				return false;
			}
			if (!Objects.equals(name, other.name)) {
				return false;
			}
			return true;
		}

		@Override
		public String toString() {
			return "PropertyMapperTestEntity [id=" + id + ", name=" + name + ", intValue=" + intValue + ", longValue="
					+ longValue + ", doubleValue=" + doubleValue + ", dateValue=" + dateValue + ", datetimeValue="
					+ datetimeValue + ", enumValue=" + enumValue + ", bigIntValue=" + bigIntValue + "]";
		}
	}

	@Test
	public void test01() throws Exception {

		try (var agent = config.agent()) {
			agent.required(() -> {
				var test1 = new PropertyMapperTestEntity(1);
				agent.insert(test1);
				var data = agent.find(PropertyMapperTestEntity.class, 1).orElse(null);
				assertThat(data, is(test1));
			});
		}
	}

	@Test
	public void test02() throws Exception {

		try (var agent = config.agent()) {
			agent.required(() -> {
				var test1 = new PropertyMapperTestEntity(1);
				test1.intValue = OptionalInt.empty();
				test1.longValue = OptionalLong.empty();
				test1.doubleValue = OptionalDouble.empty();
				test1.dateValue = null;
				test1.datetimeValue = null;
				test1.enumValue = null;
				test1.bigIntValue = null;
				agent.insert(test1);
				var data = agent.find(PropertyMapperTestEntity.class, 1).orElse(null);
				assertThat(data, is(test1));
			});
		}
	}
}
