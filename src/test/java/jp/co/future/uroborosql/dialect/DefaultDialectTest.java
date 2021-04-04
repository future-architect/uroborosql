package jp.co.future.uroborosql.dialect;

import static org.hamcrest.MatcherAssert.*;
import static org.hamcrest.Matchers.*;
import static org.junit.jupiter.api.Assertions.*;

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

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import jp.co.future.uroborosql.enums.ForUpdateType;
import jp.co.future.uroborosql.exception.UroborosqlRuntimeException;
import jp.co.future.uroborosql.mapping.JavaType;

public class DefaultDialectTest {
	private Dialect dialect;

	@BeforeEach
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
	public void testGetSequenceNextValSql() {
		assertThrows(UroborosqlRuntimeException.class, () -> dialect.getSequenceNextValSql("test_sequence"));
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
		assertThat(dialect.getJavaType(JDBCType.CHAR, "").getClass(), is(JavaType.of(String.class).getClass()));
		assertThat(dialect.getJavaType(JDBCType.VARCHAR, "").getClass(), is(JavaType.of(String.class).getClass()));
		assertThat(dialect.getJavaType(JDBCType.LONGVARCHAR, "").getClass(), is(JavaType.of(String.class).getClass()));
		assertThat(dialect.getJavaType(JDBCType.NCHAR, "").getClass(), is(JavaType.of(String.class).getClass()));
		assertThat(dialect.getJavaType(JDBCType.NVARCHAR, "").getClass(), is(JavaType.of(String.class).getClass()));
		assertThat(dialect.getJavaType(JDBCType.LONGNVARCHAR, "").getClass(), is(JavaType.of(String.class).getClass()));

		assertThat(dialect.getJavaType(JDBCType.NUMERIC, "").getClass(), is(JavaType.of(BigDecimal.class).getClass()));
		assertThat(dialect.getJavaType(JDBCType.DECIMAL, "").getClass(), is(JavaType.of(BigDecimal.class).getClass()));

		assertThat(dialect.getJavaType(JDBCType.BIT, "").getClass(), is(JavaType.of(Boolean.class).getClass()));
		assertThat(dialect.getJavaType(JDBCType.BOOLEAN, "").getClass(), is(JavaType.of(Boolean.class).getClass()));
		assertThat(dialect.getJavaType(JDBCType.TINYINT, "").getClass(), is(JavaType.of(byte.class).getClass()));
		assertThat(dialect.getJavaType(JDBCType.SMALLINT, "").getClass(), is(JavaType.of(Short.class).getClass()));
		assertThat(dialect.getJavaType(JDBCType.INTEGER, "").getClass(), is(JavaType.of(Integer.class).getClass()));
		assertThat(dialect.getJavaType(JDBCType.BIGINT, "").getClass(), is(JavaType.of(Long.class).getClass()));
		assertThat(dialect.getJavaType(JDBCType.REAL, "").getClass(), is(JavaType.of(Float.class).getClass()));
		assertThat(dialect.getJavaType(JDBCType.FLOAT, "").getClass(), is(JavaType.of(Double.class).getClass()));
		assertThat(dialect.getJavaType(JDBCType.DOUBLE, "").getClass(), is(JavaType.of(Double.class).getClass()));

		assertThat(dialect.getJavaType(JDBCType.BINARY, "").getClass(), is(JavaType.of(byte[].class).getClass()));
		assertThat(dialect.getJavaType(JDBCType.VARBINARY, "").getClass(), is(JavaType.of(byte[].class).getClass()));
		assertThat(dialect.getJavaType(JDBCType.LONGVARBINARY, "").getClass(),
				is(JavaType.of(byte[].class).getClass()));

		assertThat(dialect.getJavaType(JDBCType.DATE, "").getClass(), is(JavaType.of(ZonedDateTime.class).getClass()));
		assertThat(dialect.getJavaType(JDBCType.TIME, "").getClass(), is(JavaType.of(LocalTime.class).getClass()));
		assertThat(dialect.getJavaType(JDBCType.TIMESTAMP, "").getClass(), is(JavaType.of(Timestamp.class).getClass()));
		assertThat(dialect.getJavaType(JDBCType.TIME_WITH_TIMEZONE, "").getClass(),
				is(JavaType.of(OffsetTime.class).getClass()));
		assertThat(dialect.getJavaType(JDBCType.TIMESTAMP_WITH_TIMEZONE, "").getClass(),
				is(JavaType.of(ZonedDateTime.class).getClass()));

		assertThat(dialect.getJavaType(JDBCType.CLOB, "").getClass(), is(JavaType.of(String.class).getClass()));
		assertThat(dialect.getJavaType(JDBCType.BLOB, "").getClass(), is(JavaType.of(byte[].class).getClass()));
		assertThat(dialect.getJavaType(JDBCType.NCLOB, "").getClass(), is(JavaType.of(String.class).getClass()));
		assertThat(dialect.getJavaType(JDBCType.REF, "").getClass(), is(JavaType.of(Ref.class).getClass()));
		assertThat(dialect.getJavaType(JDBCType.SQLXML, "").getClass(), is(JavaType.of(SQLXML.class).getClass()));

		assertThat(dialect.getJavaType(JDBCType.ARRAY, "").getClass(), is(JavaType.of(Object[].class).getClass()));

		assertThat(dialect.getJavaType(JDBCType.OTHER, "").getClass(), is(JavaType.of(Object.class).getClass()));
		assertThat(dialect.getJavaType(new SQLType() {

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
		}, "").getClass(), is(JavaType.of(Object.class).getClass()));
	}

	@Test
	public void getJavaTypeWithVendorTypeNumber() {
		assertThat(dialect.getJavaType(JDBCType.CHAR.getVendorTypeNumber(), "").getClass(),
				is(JavaType.of(String.class).getClass()));
		assertThat(dialect.getJavaType(JDBCType.VARCHAR.getVendorTypeNumber(), "").getClass(),
				is(JavaType.of(String.class).getClass()));
		assertThat(dialect.getJavaType(JDBCType.LONGVARCHAR.getVendorTypeNumber(), "").getClass(),
				is(JavaType.of(String.class).getClass()));
		assertThat(dialect.getJavaType(JDBCType.NCHAR.getVendorTypeNumber(), "").getClass(),
				is(JavaType.of(String.class).getClass()));
		assertThat(dialect.getJavaType(JDBCType.NVARCHAR.getVendorTypeNumber(), "").getClass(),
				is(JavaType.of(String.class).getClass()));
		assertThat(dialect.getJavaType(JDBCType.LONGNVARCHAR.getVendorTypeNumber(), "").getClass(),
				is(JavaType.of(String.class).getClass()));

		assertThat(dialect.getJavaType(JDBCType.NUMERIC.getVendorTypeNumber(), "").getClass(),
				is(JavaType.of(BigDecimal.class).getClass()));
		assertThat(dialect.getJavaType(JDBCType.DECIMAL.getVendorTypeNumber(), "").getClass(),
				is(JavaType.of(BigDecimal.class).getClass()));

		assertThat(dialect.getJavaType(JDBCType.BIT.getVendorTypeNumber(), "").getClass(),
				is(JavaType.of(Boolean.class).getClass()));
		assertThat(dialect.getJavaType(JDBCType.BOOLEAN.getVendorTypeNumber(), "").getClass(),
				is(JavaType.of(Boolean.class).getClass()));
		assertThat(dialect.getJavaType(JDBCType.TINYINT.getVendorTypeNumber(), "").getClass(),
				is(JavaType.of(byte.class).getClass()));
		assertThat(dialect.getJavaType(JDBCType.SMALLINT.getVendorTypeNumber(), "").getClass(),
				is(JavaType.of(Short.class).getClass()));
		assertThat(dialect.getJavaType(JDBCType.INTEGER.getVendorTypeNumber(), "").getClass(),
				is(JavaType.of(Integer.class).getClass()));
		assertThat(dialect.getJavaType(JDBCType.BIGINT.getVendorTypeNumber(), "").getClass(),
				is(JavaType.of(Long.class).getClass()));
		assertThat(dialect.getJavaType(JDBCType.REAL.getVendorTypeNumber(), "").getClass(),
				is(JavaType.of(Float.class).getClass()));
		assertThat(dialect.getJavaType(JDBCType.FLOAT.getVendorTypeNumber(), "").getClass(),
				is(JavaType.of(Double.class).getClass()));
		assertThat(dialect.getJavaType(JDBCType.DOUBLE.getVendorTypeNumber(), "").getClass(),
				is(JavaType.of(Double.class).getClass()));

		assertThat(dialect.getJavaType(JDBCType.BINARY.getVendorTypeNumber(), "").getClass(),
				is(JavaType.of(byte[].class).getClass()));
		assertThat(dialect.getJavaType(JDBCType.VARBINARY.getVendorTypeNumber(), "").getClass(),
				is(JavaType.of(byte[].class).getClass()));
		assertThat(dialect.getJavaType(JDBCType.LONGVARBINARY.getVendorTypeNumber(), "").getClass(),
				is(JavaType.of(byte[].class).getClass()));

		assertThat(dialect.getJavaType(JDBCType.DATE.getVendorTypeNumber(), "").getClass(),
				is(JavaType.of(ZonedDateTime.class).getClass()));
		assertThat(dialect.getJavaType(JDBCType.TIME.getVendorTypeNumber(), "").getClass(),
				is(JavaType.of(LocalTime.class).getClass()));
		assertThat(dialect.getJavaType(JDBCType.TIMESTAMP.getVendorTypeNumber(), "").getClass(),
				is(JavaType.of(Timestamp.class).getClass()));
		assertThat(dialect.getJavaType(JDBCType.TIME_WITH_TIMEZONE.getVendorTypeNumber(), "").getClass(),
				is(JavaType.of(OffsetTime.class).getClass()));
		assertThat(dialect.getJavaType(JDBCType.TIMESTAMP_WITH_TIMEZONE.getVendorTypeNumber(), "").getClass(),
				is(JavaType.of(ZonedDateTime.class).getClass()));

		assertThat(dialect.getJavaType(JDBCType.CLOB.getVendorTypeNumber(), "").getClass(),
				is(JavaType.of(String.class).getClass()));
		assertThat(dialect.getJavaType(JDBCType.BLOB.getVendorTypeNumber(), "").getClass(),
				is(JavaType.of(byte[].class).getClass()));
		assertThat(dialect.getJavaType(JDBCType.NCLOB.getVendorTypeNumber(), "").getClass(),
				is(JavaType.of(String.class).getClass()));
		assertThat(dialect.getJavaType(JDBCType.REF.getVendorTypeNumber(), "").getClass(),
				is(JavaType.of(Ref.class).getClass()));
		assertThat(dialect.getJavaType(JDBCType.SQLXML.getVendorTypeNumber(), "").getClass(),
				is(JavaType.of(SQLXML.class).getClass()));

		assertThat(dialect.getJavaType(JDBCType.ARRAY.getVendorTypeNumber(), "").getClass(),
				is(JavaType.of(Object[].class).getClass()));

		assertThat(dialect.getJavaType(JDBCType.OTHER.getVendorTypeNumber(), "").getClass(),
				is(JavaType.of(Object.class).getClass()));
		assertThat(dialect.getJavaType(999, "error type").getClass(), is(JavaType.of(Object.class).getClass()));
	}

	@Test
	public void testAddForUpdateClause() {
		var sql = new StringBuilder("SELECT * FROM test WHERE 1 = 1 ORDER id").append(System.lineSeparator());
		assertThat(dialect.addForUpdateClause(sql, ForUpdateType.NORMAL, -1).toString(),
				is("SELECT * FROM test WHERE 1 = 1 ORDER id" + System.lineSeparator() + "FOR UPDATE"));
		assertThat(dialect.addForUpdateClause(sql, ForUpdateType.NOWAIT, -1).toString(),
				is("SELECT * FROM test WHERE 1 = 1 ORDER id" + System.lineSeparator() + "FOR UPDATE NOWAIT"));
		assertThat(dialect.addForUpdateClause(sql, ForUpdateType.WAIT, 10).toString(),
				is("SELECT * FROM test WHERE 1 = 1 ORDER id" + System.lineSeparator() + "FOR UPDATE WAIT 10"));
	}

	@Test
	public void testAddOptimizerHints() {
		var sql = new StringBuilder("SELECT * FROM test WHERE 1 = 1 ORDER id").append(System.lineSeparator());
		List<String> hints = new ArrayList<>();
		hints.add("USE_NL");
		assertThrows(IllegalStateException.class, () -> dialect.addOptimizerHints(sql, hints));
	}

	@Test
	public void testGetPessimisticLockingErrorCodes() {
		assertThat(dialect.getPessimisticLockingErrorCodes().isEmpty(), is(true));
	}
}
