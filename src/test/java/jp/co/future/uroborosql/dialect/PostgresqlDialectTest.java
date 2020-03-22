package jp.co.future.uroborosql.dialect;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.nullValue;
import static org.hamcrest.Matchers.*;
import static org.hamcrest.Matchers.instanceOf;
import static org.junit.Assert.*;

import java.sql.Connection;
import java.sql.JDBCType;
import java.sql.Time;
import java.sql.Timestamp;
import java.time.OffsetTime;
import java.time.ZonedDateTime;
import java.util.ArrayList;
import java.util.List;
import java.util.ServiceLoader;
import java.util.stream.StreamSupport;

import org.junit.Test;

import jp.co.future.uroborosql.connection.ConnectionSupplier;
import jp.co.future.uroborosql.enums.ForUpdateType;
import jp.co.future.uroborosql.mapping.JavaType;

/**
 * PostgresqlDialectの個別実装部分のテストケース
 *
 * @author H.Sugimoto
 *
 */
public class PostgresqlDialectTest {
	private final Dialect dialect = new PostgresqlDialect();

	@Test
	public void testAccept12() {
		ConnectionSupplier supplier = new ConnectionSupplier() {

			@Override
			public Connection getConnection(final String alias) {
				return null;
			}

			@Override
			public Connection getConnection() {
				return null;
			}

			@Override
			public String getDatabaseName() {
				return "PostgreSQL";
			}
		};

		Dialect dialect = StreamSupport.stream(ServiceLoader.load(Dialect.class).spliterator(), false)
				.filter(d -> d.accept(supplier)).findFirst().orElseGet(DefaultDialect::new);

		assertThat(dialect, instanceOf(PostgresqlDialect.class));
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
	public void testGetEscapeChar() {
		assertThat(dialect.getEscapeChar(), is('$'));
	}

	@Test
	public void testSupports() {
		assertThat(dialect.supportsBulkInsert(), is(true));
		assertThat(dialect.supportsLimitClause(), is(true));
		assertThat(dialect.supportsNullValuesOrdering(), is(true));
		assertThat(dialect.isRemoveTerminator(), is(true));
		assertThat(dialect.isRollbackToSavepointBeforeRetry(), is(true));
		assertThat(dialect.supportsForUpdate(), is(true));
		assertThat(dialect.supportsForUpdateNoWait(), is(true));
		assertThat(dialect.supportsForUpdateWait(), is(false));
	}

	@Test
	public void testGetLimitClause() {
		assertThat(dialect.getLimitClause(3, 5), is("LIMIT 3 OFFSET 5" + System.lineSeparator()));
		assertThat(dialect.getLimitClause(0, 5), is("OFFSET 5" + System.lineSeparator()));
		assertThat(dialect.getLimitClause(3, 0), is("LIMIT 3 " + System.lineSeparator()));
		assertThat(dialect.getLimitClause(0, 0), is(""));
	}

	@Test
	public void testGetJavaType() {
		assertEquals(dialect.getJavaType(JDBCType.OTHER, "json").getClass(), JavaType.of(String.class).getClass());
		assertEquals(dialect.getJavaType(JDBCType.OTHER, "jsonb").getClass(), JavaType.of(String.class).getClass());
		assertEquals(dialect.getJavaType(JDBCType.OTHER, "other").getClass(), JavaType.of(Object.class).getClass());
		assertEquals(dialect.getJavaType(JDBCType.TIMESTAMP, "timestamptz").getClass(),
				JavaType.of(ZonedDateTime.class).getClass());
		assertEquals(dialect.getJavaType(JDBCType.TIMESTAMP, "timestamp").getClass(),
				JavaType.of(Timestamp.class).getClass());
		assertEquals(dialect.getJavaType(JDBCType.TIME, "timetz").getClass(),
				JavaType.of(OffsetTime.class).getClass());
		assertEquals(dialect.getJavaType(JDBCType.TIME, "time").getClass(),
				JavaType.of(Time.class).getClass());
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

	@Test
	public void testAddOptimizerHints1() {
		StringBuilder sql = new StringBuilder("SELECT")
				.append(System.lineSeparator())
				.append(" * FROM test")
				.append(System.lineSeparator())
				.append("WHERE 1 = 1 ORDER id")
				.append(System.lineSeparator());
		List<String> hints = new ArrayList<>();
		hints.add("INDEX (test test_ix)");
		hints.add("USE_NL");

		assertThat(dialect.addOptimizerHints(sql, hints).toString(),
				is("/*+" + System.lineSeparator() + "\t" + "INDEX (test test_ix)"
						+ System.lineSeparator() + "\t"
						+ "USE_NL"
						+ System.lineSeparator() + " */"
						+ System.lineSeparator() + "SELECT"
						+ System.lineSeparator()
						+ " * FROM test"
						+ System.lineSeparator()
						+ "WHERE 1 = 1 ORDER id"
						+ System.lineSeparator()));
	}

	@Test
	public void testAddOptimizerHints2() {
		StringBuilder sql = new StringBuilder("SELECT")
				.append(System.lineSeparator())
				.append(" * FROM PUBLIC.TEST_1");
		List<String> hints = new ArrayList<>();
		hints.add("INDEX (PUBLIC.TEST_1 test_ix)");
		hints.add("USE_NL");

		assertThat(dialect.addOptimizerHints(sql, hints).toString(),
				is("/*+" + System.lineSeparator() + "\t" + "INDEX (PUBLIC.TEST_1 test_ix)"
						+ System.lineSeparator() + "\t"
						+ "USE_NL"
						+ System.lineSeparator() + " */"
						+ System.lineSeparator() + "SELECT"
						+ System.lineSeparator()
						+ " * FROM PUBLIC.TEST_1"));
	}

	@Test
	public void testGetPessimisticLockingErrorCodes() {
		assertThat(dialect.getPessimisticLockingErrorCodes(), is(containsInAnyOrder("55P03")));
	}

}
