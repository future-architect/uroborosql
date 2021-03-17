package jp.co.future.uroborosql.mapping.mapper;

import static org.hamcrest.CoreMatchers.*;
import static org.hamcrest.MatcherAssert.*;

import java.math.BigInteger;
import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.Statement;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.temporal.ChronoUnit;
import java.util.Optional;
import java.util.OptionalDouble;
import java.util.OptionalInt;
import java.util.OptionalLong;

import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;

import jp.co.future.uroborosql.SqlAgent;
import jp.co.future.uroborosql.UroboroSQL;
import jp.co.future.uroborosql.config.SqlConfig;
import jp.co.future.uroborosql.filter.AuditLogSqlFilter;
import jp.co.future.uroborosql.filter.SqlFilterManager;
import jp.co.future.uroborosql.mapping.annotations.Table;

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

		config = UroboroSQL.builder(url, user, password).build();

		SqlFilterManager sqlFilterManager = config.getSqlFilterManager();
		sqlFilterManager.addSqlFilter(new AuditLogSqlFilter());
	}

	@Before
	public void setUpBefore() throws Exception {
		try (SqlAgent agent = config.agent()) {
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
			final int prime = 31;
			int result = 1;
			result = prime * result + (bigIntValue == null ? 0 : bigIntValue.hashCode());
			result = prime * result + (dateValue == null ? 0 : dateValue.hashCode());
			result = prime * result + (datetimeValue == null ? 0 : datetimeValue.hashCode());
			result = prime * result + (doubleValue == null ? 0 : doubleValue.hashCode());
			result = prime * result + (enumValue == null ? 0 : enumValue.hashCode());
			result = prime * result + (int) (id ^ id >>> 32);
			result = prime * result + (intValue == null ? 0 : intValue.hashCode());
			result = prime * result + (longValue == null ? 0 : longValue.hashCode());
			result = prime * result + (name == null ? 0 : name.hashCode());
			return result;
		}

		@Override
		public boolean equals(final Object obj) {
			if (this == obj) {
				return true;
			}
			if (obj == null) {
				return false;
			}
			if (getClass() != obj.getClass()) {
				return false;
			}
			PropertyMapperTestEntity other = (PropertyMapperTestEntity) obj;
			if (bigIntValue == null) {
				if (other.bigIntValue != null) {
					return false;
				}
			} else if (!bigIntValue.equals(other.bigIntValue)) {
				return false;
			}
			if (dateValue == null) {
				if (other.dateValue != null) {
					return false;
				}
			} else if (!dateValue.equals(other.dateValue)) {
				return false;
			}
			if (datetimeValue == null) {
				if (other.datetimeValue != null) {
					return false;
				}
			} else if (!datetimeValue.equals(other.datetimeValue)) {
				return false;
			}
			if (doubleValue == null) {
				if (other.doubleValue != null) {
					return false;
				}
			} else if (!doubleValue.equals(other.doubleValue)) {
				return false;
			}
			if (enumValue != other.enumValue) {
				return false;
			}
			if (id != other.id) {
				return false;
			}
			if (intValue == null) {
				if (other.intValue != null) {
					return false;
				}
			} else if (!intValue.equals(other.intValue)) {
				return false;
			}
			if (longValue == null) {
				if (other.longValue != null) {
					return false;
				}
			} else if (!longValue.equals(other.longValue)) {
				return false;
			}
			if (name == null) {
				if (other.name != null) {
					return false;
				}
			} else if (!name.equals(other.name)) {
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

		try (SqlAgent agent = config.agent()) {
			agent.required(() -> {
				PropertyMapperTestEntity test1 = new PropertyMapperTestEntity(1);
				agent.insert(test1);
				test1.datetimeValue = test1.datetimeValue.truncatedTo(ChronoUnit.MILLIS);
				PropertyMapperTestEntity data = agent.find(PropertyMapperTestEntity.class, 1).orElse(null);
				data.datetimeValue = data.datetimeValue.truncatedTo(ChronoUnit.MILLIS);
				assertThat(data, is(test1));
			});
		}
	}

	@Test
	public void test02() throws Exception {

		try (SqlAgent agent = config.agent()) {
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
				PropertyMapperTestEntity data = agent.find(PropertyMapperTestEntity.class, 1).orElse(null);
				assertThat(data, is(test1));
			});
		}
	}
}
