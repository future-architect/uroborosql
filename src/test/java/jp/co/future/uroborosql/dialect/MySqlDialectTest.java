package jp.co.future.uroborosql.dialect;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.nullValue;
import static org.hamcrest.Matchers.instanceOf;
import static org.junit.Assert.*;

import java.sql.Connection;
import java.util.ServiceLoader;
import java.util.stream.StreamSupport;

import org.junit.Test;

import jp.co.future.uroborosql.connection.ConnectionSupplier;
import jp.co.future.uroborosql.enums.ForUpdateType;
import jp.co.future.uroborosql.exception.UroborosqlRuntimeException;

/**
 * MySqlDialectの個別実装部分のテストケース
 *
 * @author H.Sugimoto
 *
 */
public class MySqlDialectTest {
	private final Dialect dialect = new MySqlDialect();

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
				return "MySQL";
			}
		};

		Dialect dialect = StreamSupport.stream(ServiceLoader.load(Dialect.class).spliterator(), false)
				.filter(d -> d.accept(supplier)).findFirst().orElseGet(DefaultDialect::new);

		assertThat(dialect, instanceOf(MySqlDialect.class));
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
	public void testGetEscapeChar() {
		assertThat(dialect.getEscapeChar(), is('$'));
	}

	@Test
	public void testSupports() {
		assertThat(dialect.supportsBulkInsert(), is(true));
		assertThat(dialect.supportsLimitClause(), is(true));
		assertThat(dialect.supportsNullValuesOrdering(), is(false));
		assertThat(dialect.supportsIdentity(), is(true));
		assertThat(dialect.supportsSequence(), is(false));
		assertThat(dialect.isRemoveTerminator(), is(true));
		assertThat(dialect.isRollbackToSavepointBeforeRetry(), is(false));
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
	public void testAddForUpdateClause() {
		StringBuilder sql = new StringBuilder("SELECT * FROM test WHERE 1 = 1 ORDER id").append(System.lineSeparator());
		assertThat(dialect.addForUpdateClause(sql, ForUpdateType.NORMAL, -1).toString(),
				is("SELECT * FROM test WHERE 1 = 1 ORDER id" + System.lineSeparator() + "FOR UPDATE"));
		assertThat(dialect.addForUpdateClause(sql, ForUpdateType.NOWAIT, -1).toString(),
				is("SELECT * FROM test WHERE 1 = 1 ORDER id" + System.lineSeparator() + "FOR UPDATE NOWAIT"));
		assertThat(dialect.addForUpdateClause(sql, ForUpdateType.WAIT, 10).toString(),
				is("SELECT * FROM test WHERE 1 = 1 ORDER id" + System.lineSeparator() + "FOR UPDATE WAIT 10"));
	}
}
