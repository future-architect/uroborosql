package jp.co.future.uroborosql.dialect;

import static org.hamcrest.CoreMatchers.*;
import static org.junit.Assert.*;

import java.math.BigDecimal;
import java.sql.JDBCType;
import java.sql.Ref;
import java.sql.SQLXML;
import java.sql.Timestamp;
import java.time.LocalTime;
import java.time.OffsetTime;
import java.time.ZonedDateTime;

import org.junit.Before;
import org.junit.Test;

import jp.co.future.uroborosql.mapping.JavaType;

public class DefaultDialectTest {
	private Dialect dialect;

	@Before
	public void setUp() throws Exception {
		dialect = new DefaultDialect();
	}

	@Test
	public void testGetDatabaseName() {
		assertThat(dialect.getDatabaseName(), is("default"));
	}

	@Test
	public void testAccept() {
		assertThat(dialect.accept(null), is(true));
	}

	@Test
	public void testIsRemoveTerminator() {
		assertThat(dialect.isRemoveTerminator(), is(true));
	}

	@Test
	public void testIsRollbackToSavepointBeforeRetry() {
		assertThat(dialect.isRollbackToSavepointBeforeRetry(), is(false));
	}

	@Test
	public void testSupportsBulkInsert() {
		assertThat(dialect.supportsBulkInsert(), is(false));
	}

	@Test
	public void testSupportsLimitClause() {
		assertThat(dialect.supportsBulkInsert(), is(false));
	}

	@Test
	public void testEscapeLikePattern() {
		assertThat(dialect.escapeLikePattern(""), is(""));
		assertThat(dialect.escapeLikePattern("pattern"), is("pattern"));
		assertThat(dialect.escapeLikePattern("%pattern"), is("$%pattern"));
		assertThat(dialect.escapeLikePattern("_pattern"), is("$_pattern"));
		assertThat(dialect.escapeLikePattern("pat%tern"), is("pat$%tern"));
		assertThat(dialect.escapeLikePattern("pat_tern"), is("pat$_tern"));
		assertThat(dialect.escapeLikePattern("pattern%"), is("pattern$%"));
		assertThat(dialect.escapeLikePattern("pattern_"), is("pattern$_"));
		assertThat(dialect.escapeLikePattern("pat[]tern"), is("pat[]tern"));
		assertThat(dialect.escapeLikePattern("pat％tern"), is("pat％tern"));
		assertThat(dialect.escapeLikePattern("pat＿tern"), is("pat＿tern"));
	}

	@Test
	public void testGetDatabaseType() {
		assertThat(dialect.getDatabaseType(), is("default"));
	}

	@Test
	public void testGetLimitClause() {
		Dialect dialect = new MySqlDialect();
		assertThat(dialect.getLimitClause(3, 5), is("LIMIT 3 OFFSET 5" + System.lineSeparator()));
		assertThat(dialect.getLimitClause(0, 5), is("OFFSET 5" + System.lineSeparator()));
		assertThat(dialect.getLimitClause(3, 0), is("LIMIT 3 " + System.lineSeparator()));
		assertThat(dialect.getLimitClause(0, 0), is(""));
	}

	@Test
	public void getJavaType() {
		assertEquals(dialect.getJavaType(JDBCType.CHAR, "").getClass(), JavaType.of(String.class).getClass());
		assertEquals(dialect.getJavaType(JDBCType.VARCHAR, "").getClass(), JavaType.of(String.class).getClass());
		assertEquals(dialect.getJavaType(JDBCType.LONGVARCHAR, "").getClass(), JavaType.of(String.class).getClass());
		assertEquals(dialect.getJavaType(JDBCType.NCHAR, "").getClass(), JavaType.of(String.class).getClass());
		assertEquals(dialect.getJavaType(JDBCType.NVARCHAR, "").getClass(), JavaType.of(String.class).getClass());
		assertEquals(dialect.getJavaType(JDBCType.LONGNVARCHAR, "").getClass(), JavaType.of(String.class).getClass());

		assertEquals(dialect.getJavaType(JDBCType.NUMERIC, "").getClass(), JavaType.of(BigDecimal.class).getClass());
		assertEquals(dialect.getJavaType(JDBCType.DECIMAL, "").getClass(), JavaType.of(BigDecimal.class).getClass());

		assertEquals(dialect.getJavaType(JDBCType.BIT, "").getClass(), JavaType.of(Boolean.class).getClass());
		assertEquals(dialect.getJavaType(JDBCType.BOOLEAN, "").getClass(), JavaType.of(Boolean.class).getClass());
		assertEquals(dialect.getJavaType(JDBCType.TINYINT, "").getClass(), JavaType.of(byte.class).getClass());
		assertEquals(dialect.getJavaType(JDBCType.SMALLINT, "").getClass(), JavaType.of(Short.class).getClass());
		assertEquals(dialect.getJavaType(JDBCType.INTEGER, "").getClass(), JavaType.of(Integer.class).getClass());
		assertEquals(dialect.getJavaType(JDBCType.BIGINT, "").getClass(), JavaType.of(Long.class).getClass());
		assertEquals(dialect.getJavaType(JDBCType.REAL, "").getClass(), JavaType.of(Float.class).getClass());
		assertEquals(dialect.getJavaType(JDBCType.FLOAT, "").getClass(), JavaType.of(Double.class).getClass());
		assertEquals(dialect.getJavaType(JDBCType.DOUBLE, "").getClass(), JavaType.of(Double.class).getClass());

		assertEquals(dialect.getJavaType(JDBCType.BINARY, "").getClass(), JavaType.of(byte[].class).getClass());
		assertEquals(dialect.getJavaType(JDBCType.VARBINARY, "").getClass(), JavaType.of(byte[].class).getClass());
		assertEquals(dialect.getJavaType(JDBCType.LONGVARBINARY, "").getClass(), JavaType.of(byte[].class).getClass());

		assertEquals(dialect.getJavaType(JDBCType.DATE, "").getClass(), JavaType.of(ZonedDateTime.class).getClass());
		assertEquals(dialect.getJavaType(JDBCType.TIME, "").getClass(), JavaType.of(LocalTime.class).getClass());
		assertEquals(dialect.getJavaType(JDBCType.TIMESTAMP, "").getClass(), JavaType.of(Timestamp.class).getClass());
		assertEquals(dialect.getJavaType(JDBCType.TIME_WITH_TIMEZONE, "").getClass(),
				JavaType.of(OffsetTime.class).getClass());
		assertEquals(dialect.getJavaType(JDBCType.TIMESTAMP_WITH_TIMEZONE, "").getClass(),
				JavaType.of(ZonedDateTime.class).getClass());

		assertEquals(dialect.getJavaType(JDBCType.CLOB, "").getClass(), JavaType.of(String.class).getClass());
		assertEquals(dialect.getJavaType(JDBCType.BLOB, "").getClass(), JavaType.of(byte[].class).getClass());
		assertEquals(dialect.getJavaType(JDBCType.NCLOB, "").getClass(), JavaType.of(String.class).getClass());
		assertEquals(dialect.getJavaType(JDBCType.REF, "").getClass(), JavaType.of(Ref.class).getClass());
		assertEquals(dialect.getJavaType(JDBCType.SQLXML, "").getClass(), JavaType.of(SQLXML.class).getClass());

		assertEquals(dialect.getJavaType(JDBCType.ARRAY, "").getClass(), JavaType.of(Object[].class).getClass());

		assertEquals(dialect.getJavaType(JDBCType.OTHER, "").getClass(), JavaType.of(Object.class).getClass());

		assertEquals(dialect.getJavaType(JDBCType.OTHER.getVendorTypeNumber(), "").getClass(),
				JavaType.of(Object.class).getClass());
		assertEquals(dialect.getJavaType(999, "error type").getClass(),
				JavaType.of(Object.class).getClass());
	}
}
