package jp.co.future.uroborosql.dialect;

import static org.hamcrest.CoreMatchers.*;
import static org.junit.Assert.*;

import java.math.BigDecimal;
import java.sql.JDBCType;
import java.sql.Ref;
import java.sql.SQLType;
import java.sql.SQLXML;
import java.sql.Timestamp;
import java.time.LocalTime;
import java.time.OffsetTime;
import java.time.ZonedDateTime;
import java.util.ArrayList;
import java.util.List;

import org.junit.Before;
import org.junit.Test;

import jp.co.future.uroborosql.enums.ForUpdateType;
import jp.co.future.uroborosql.exception.UroborosqlRuntimeException;
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

	@Test(expected = UroborosqlRuntimeException.class)
	public void testGetSequenceNextValSql() {
		dialect.getSequenceNextValSql("test_sequence");
	}

	@Test
	public void testEscapeLikePattern() {
		assertThat(dialect.escapeLikePattern(""), is(""));
		assertThat(dialect.escapeLikePattern(null), nullValue());
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
	public void testGetEscapeChar() {
		assertThat(dialect.getEscapeChar(), is('$'));
	}

	@Test
	public void testSupport() {
		assertThat(dialect.supportsBulkInsert(), is(false));
		assertThat(dialect.supportsLimitClause(), is(false));
		assertThat(dialect.supportsNullValuesOrdering(), is(false));
		assertThat(dialect.supportsIdentity(), is(true));
		assertThat(dialect.supportsSequence(), is(false));
		assertThat(dialect.isRemoveTerminator(), is(true));
		assertThat(dialect.isRollbackToSavepointBeforeRetry(), is(false));
		assertThat(dialect.supportsForUpdate(), is(true));
		assertThat(dialect.supportsForUpdateNoWait(), is(true));
		assertThat(dialect.supportsForUpdateWait(), is(false));
		assertThat(dialect.supportsOptimizerHints(), is(false));
		assertThat(dialect.supportsEntityBulkUpdateOptimisticLock(), is(true));
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
	public void getJavaTypeWithSQLType() {
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
		assertEquals(dialect.getJavaType(new SQLType() {

			@Override
			public Integer getVendorTypeNumber() {
				return 999;
			}

			@Override
			public String getVendor() {
				return "dummy";
			}

			@Override
			public String getName() {
				return "dummy";
			}
		}, "").getClass(), JavaType.of(Object.class).getClass());
	}

	@Test
	public void getJavaTypeWithVendorTypeNumber() {
		assertEquals(dialect.getJavaType(JDBCType.CHAR.getVendorTypeNumber(), "").getClass(),
				JavaType.of(String.class).getClass());
		assertEquals(dialect.getJavaType(JDBCType.VARCHAR.getVendorTypeNumber(), "").getClass(),
				JavaType.of(String.class).getClass());
		assertEquals(dialect.getJavaType(JDBCType.LONGVARCHAR.getVendorTypeNumber(), "").getClass(),
				JavaType.of(String.class).getClass());
		assertEquals(dialect.getJavaType(JDBCType.NCHAR.getVendorTypeNumber(), "").getClass(),
				JavaType.of(String.class).getClass());
		assertEquals(dialect.getJavaType(JDBCType.NVARCHAR.getVendorTypeNumber(), "").getClass(),
				JavaType.of(String.class).getClass());
		assertEquals(dialect.getJavaType(JDBCType.LONGNVARCHAR.getVendorTypeNumber(), "").getClass(),
				JavaType.of(String.class).getClass());

		assertEquals(dialect.getJavaType(JDBCType.NUMERIC.getVendorTypeNumber(), "").getClass(),
				JavaType.of(BigDecimal.class).getClass());
		assertEquals(dialect.getJavaType(JDBCType.DECIMAL.getVendorTypeNumber(), "").getClass(),
				JavaType.of(BigDecimal.class).getClass());

		assertEquals(dialect.getJavaType(JDBCType.BIT.getVendorTypeNumber(), "").getClass(),
				JavaType.of(Boolean.class).getClass());
		assertEquals(dialect.getJavaType(JDBCType.BOOLEAN.getVendorTypeNumber(), "").getClass(),
				JavaType.of(Boolean.class).getClass());
		assertEquals(dialect.getJavaType(JDBCType.TINYINT.getVendorTypeNumber(), "").getClass(),
				JavaType.of(byte.class).getClass());
		assertEquals(dialect.getJavaType(JDBCType.SMALLINT.getVendorTypeNumber(), "").getClass(),
				JavaType.of(Short.class).getClass());
		assertEquals(dialect.getJavaType(JDBCType.INTEGER.getVendorTypeNumber(), "").getClass(),
				JavaType.of(Integer.class).getClass());
		assertEquals(dialect.getJavaType(JDBCType.BIGINT.getVendorTypeNumber(), "").getClass(),
				JavaType.of(Long.class).getClass());
		assertEquals(dialect.getJavaType(JDBCType.REAL.getVendorTypeNumber(), "").getClass(),
				JavaType.of(Float.class).getClass());
		assertEquals(dialect.getJavaType(JDBCType.FLOAT.getVendorTypeNumber(), "").getClass(),
				JavaType.of(Double.class).getClass());
		assertEquals(dialect.getJavaType(JDBCType.DOUBLE.getVendorTypeNumber(), "").getClass(),
				JavaType.of(Double.class).getClass());

		assertEquals(dialect.getJavaType(JDBCType.BINARY.getVendorTypeNumber(), "").getClass(),
				JavaType.of(byte[].class).getClass());
		assertEquals(dialect.getJavaType(JDBCType.VARBINARY.getVendorTypeNumber(), "").getClass(),
				JavaType.of(byte[].class).getClass());
		assertEquals(dialect.getJavaType(JDBCType.LONGVARBINARY.getVendorTypeNumber(), "").getClass(),
				JavaType.of(byte[].class).getClass());

		assertEquals(dialect.getJavaType(JDBCType.DATE.getVendorTypeNumber(), "").getClass(),
				JavaType.of(ZonedDateTime.class).getClass());
		assertEquals(dialect.getJavaType(JDBCType.TIME.getVendorTypeNumber(), "").getClass(),
				JavaType.of(LocalTime.class).getClass());
		assertEquals(dialect.getJavaType(JDBCType.TIMESTAMP.getVendorTypeNumber(), "").getClass(),
				JavaType.of(Timestamp.class).getClass());
		assertEquals(dialect.getJavaType(JDBCType.TIME_WITH_TIMEZONE.getVendorTypeNumber(), "").getClass(),
				JavaType.of(OffsetTime.class).getClass());
		assertEquals(dialect.getJavaType(JDBCType.TIMESTAMP_WITH_TIMEZONE.getVendorTypeNumber(), "").getClass(),
				JavaType.of(ZonedDateTime.class).getClass());

		assertEquals(dialect.getJavaType(JDBCType.CLOB.getVendorTypeNumber(), "").getClass(),
				JavaType.of(String.class).getClass());
		assertEquals(dialect.getJavaType(JDBCType.BLOB.getVendorTypeNumber(), "").getClass(),
				JavaType.of(byte[].class).getClass());
		assertEquals(dialect.getJavaType(JDBCType.NCLOB.getVendorTypeNumber(), "").getClass(),
				JavaType.of(String.class).getClass());
		assertEquals(dialect.getJavaType(JDBCType.REF.getVendorTypeNumber(), "").getClass(),
				JavaType.of(Ref.class).getClass());
		assertEquals(dialect.getJavaType(JDBCType.SQLXML.getVendorTypeNumber(), "").getClass(),
				JavaType.of(SQLXML.class).getClass());

		assertEquals(dialect.getJavaType(JDBCType.ARRAY.getVendorTypeNumber(), "").getClass(),
				JavaType.of(Object[].class).getClass());

		assertEquals(dialect.getJavaType(JDBCType.OTHER.getVendorTypeNumber(), "").getClass(),
				JavaType.of(Object.class).getClass());
		assertEquals(dialect.getJavaType(999, "error type").getClass(),
				JavaType.of(Object.class).getClass());
	}

	@Test
	public void testAddForUpdateClause() {
		StringBuilder sql = new StringBuilder("SELECT * FROM test WHERE 1 = 1 ORDER id").append(System.lineSeparator());
		assertThat(dialect.addForUpdateClause(sql, ForUpdateType.NORMAL, -1).toString(),
				is("SELECT * FROM test WHERE 1 = 1 ORDER id" + System.lineSeparator() + "FOR UPDATE"));
		assertThat(dialect.addForUpdateClause(sql, ForUpdateType.NOWAIT, -1).toString(),
				is("SELECT * FROM test WHERE 1 = 1 ORDER id" + System.lineSeparator() + "FOR UPDATE NOWAIT"));
		assertThat(dialect.addForUpdateClause(sql, ForUpdateType.WAIT, 10).toString(),
				is("SELECT * FROM test WHERE 1 = 1 ORDER id" + System.lineSeparator() + "FOR UPDATE WAIT 10"));
	}

	@Test(expected = IllegalStateException.class)
	public void testAddOptimizerHints() {
		StringBuilder sql = new StringBuilder("SELECT * FROM test WHERE 1 = 1 ORDER id").append(System.lineSeparator());
		List<String> hints = new ArrayList<>();
		hints.add("USE_NL");
		dialect.addOptimizerHints(sql, hints);
	}

	@Test
	public void testGetPessimisticLockingErrorCodes() {
		assertThat(dialect.getPessimisticLockingErrorCodes().isEmpty(), is(true));
	}
}
