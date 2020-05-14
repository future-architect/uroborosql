package jp.co.future.uroborosql.dialect;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.nullValue;
import static org.hamcrest.Matchers.*;
import static org.hamcrest.Matchers.instanceOf;
import static org.junit.Assert.*;

import java.sql.Connection;
import java.util.ArrayList;
import java.util.List;
import java.util.ServiceLoader;
import java.util.stream.StreamSupport;

import org.junit.Test;

import jp.co.future.uroborosql.connection.ConnectionContext;
import jp.co.future.uroborosql.connection.ConnectionSupplier;
import jp.co.future.uroborosql.enums.ForUpdateType;

/**
 * H2Dialectの個別実装部分のテストケース
 *
 * @author H.Sugimoto
 *
 */
public class MsSqlDialectTest {
	private final Dialect dialect = new MsSqlDialect();

	@Test
	public void testAccept12() {
		ConnectionSupplier supplier = new ConnectionSupplier() {

			@Override
			public Connection getConnection() {
				return null;
			}

			@Override
			public Connection getConnection(final ConnectionContext ctx) {
				return null;
			}

			@Override
			public String getDatabaseName() {
				return "Microsoft SQL Server";
			}
		};

		Dialect dialect = StreamSupport.stream(ServiceLoader.load(Dialect.class).spliterator(), false)
				.filter(d -> d.accept(supplier)).findFirst().orElseGet(DefaultDialect::new);

		assertThat(dialect, instanceOf(MsSqlDialect.class));
	}

	@Test
	public void testGetSequenceNextValSql() {
		assertThat(dialect.getSequenceNextValSql("test_sequence"), is("next value for test_sequence"));
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
		assertThat(dialect.supportsBulkInsert(), is(false));
		assertThat(dialect.supportsLimitClause(), is(false));
		assertThat(dialect.supportsNullValuesOrdering(), is(false));
		assertThat(dialect.supportsIdentity(), is(true));
		assertThat(dialect.supportsSequence(), is(true));
		assertThat(dialect.isRemoveTerminator(), is(false));
		assertThat(dialect.isRollbackToSavepointBeforeRetry(), is(false));
		assertThat(dialect.supportsForUpdate(), is(true));
		assertThat(dialect.supportsForUpdateNoWait(), is(true));
		assertThat(dialect.supportsForUpdateWait(), is(false));
		assertThat(dialect.supportsOptimizerHints(), is(true));
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
		StringBuilder sql = new StringBuilder("SELECT")
				.append(System.lineSeparator())
				.append(" * FROM test")
				.append(System.lineSeparator())
				.append("WHERE 1 = 1 ORDER id")
				.append(System.lineSeparator());
		assertThat(dialect.addForUpdateClause(sql, ForUpdateType.NORMAL, -1).toString(),
				is("SELECT"
						+ System.lineSeparator()
						+ " * FROM test WITH (UPDLOCK, ROWLOCK)"
						+ System.lineSeparator()
						+ "WHERE 1 = 1 ORDER id"
						+ System.lineSeparator()));
		assertThat(dialect.addForUpdateClause(sql, ForUpdateType.NOWAIT, -1).toString(),
				is("SELECT"
						+ System.lineSeparator()
						+ " * FROM test WITH (UPDLOCK, ROWLOCK, NOWAIT)"
						+ System.lineSeparator()
						+ "WHERE 1 = 1 ORDER id"
						+ System.lineSeparator()));
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
				is("SELECT"
						+ System.lineSeparator()
						+ " * FROM test WITH (INDEX (test test_ix), USE_NL)"
						+ System.lineSeparator()
						+ "WHERE 1 = 1 ORDER id"
						+ System.lineSeparator()));
		assertThat(
				dialect.addOptimizerHints(dialect.addForUpdateClause(sql, ForUpdateType.NOWAIT, -1), hints).toString(),
				is("SELECT"
						+ System.lineSeparator()
						+ " * FROM test WITH (UPDLOCK, ROWLOCK, NOWAIT, INDEX (test test_ix), USE_NL)"
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
				is("SELECT"
						+ System.lineSeparator()
						+ " * FROM PUBLIC.TEST_1 WITH (INDEX (PUBLIC.TEST_1 test_ix), USE_NL)"
						+ System.lineSeparator()));
		assertThat(
				dialect.addOptimizerHints(dialect.addForUpdateClause(sql, ForUpdateType.NOWAIT, -1), hints).toString(),
				is("SELECT"
						+ System.lineSeparator()
						+ " * FROM PUBLIC.TEST_1 WITH (UPDLOCK, ROWLOCK, NOWAIT, INDEX (PUBLIC.TEST_1 test_ix), USE_NL)"
						+ System.lineSeparator()));
	}

	@Test
	public void testGetPessimisticLockingErrorCodes() {
		assertThat(dialect.getPessimisticLockingErrorCodes(), is(containsInAnyOrder("1222")));
	}

}
