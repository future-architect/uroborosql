package jp.co.future.uroborosql.dialect;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.nullValue;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.containsInAnyOrder;
import static org.hamcrest.Matchers.instanceOf;
import static org.hamcrest.Matchers.not;

import java.sql.Connection;
import java.util.ArrayList;
import java.util.List;
import java.util.ServiceLoader;
import java.util.stream.StreamSupport;

import org.junit.jupiter.api.Test;

import jp.co.future.uroborosql.connection.ConnectionContext;
import jp.co.future.uroborosql.connection.ConnectionSupplier;
import jp.co.future.uroborosql.enums.ForUpdateType;

/**
 * Oracle21Dialectの個別実装部分のテストケース
 *
 * @author H.Sugimoto
 *
 */
public class Oracle21DialectTest {
	private final Dialect dialect = new Oracle21Dialect();

	@Test
	void testAccept21() {
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
				return "Oracle-21.1";
			}
		};

		var dialect = StreamSupport.stream(ServiceLoader.load(Dialect.class).spliterator(), false)
				.filter(d -> d.accept(supplier)).findFirst().orElseGet(DefaultDialect::new);

		assertThat(dialect, instanceOf(Oracle21Dialect.class));
	}

	@Test
	void testAcceptUnder21() {
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
				return "Oracle-19.1";
			}
		};

		var dialect = StreamSupport.stream(ServiceLoader.load(Dialect.class).spliterator(), false)
				.filter(d -> d.accept(supplier)).findFirst().orElseGet(DefaultDialect::new);

		assertThat(dialect, not(instanceOf(Oracle21Dialect.class)));
	}

	@Test
	void testAcceptOver21() {
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
				return "Oracle-23.1";
			}
		};

		var dialect = StreamSupport.stream(ServiceLoader.load(Dialect.class).spliterator(), false)
				.filter(d -> d.accept(supplier)).findFirst().orElseGet(DefaultDialect::new);

		assertThat(dialect, not(instanceOf(Oracle21Dialect.class)));
	}

	@Test
	void testGetSequenceNextValSql() {
		assertThat(dialect.getSequenceNextValSql("test_sequence"), is("test_sequence.nextval"));
	}

	@Test
	void testEscapeLikePattern() {
		assertThat(dialect.escapeLikePattern(""), is(""));
		assertThat(dialect.escapeLikePattern(null), nullValue());
		assertThat(dialect.escapeLikePattern("pattern"), is("pattern"));
		assertThat(dialect.escapeLikePattern("%pattern"), is("\\%pattern"));
		assertThat(dialect.escapeLikePattern("_pattern"), is("\\_pattern"));
		assertThat(dialect.escapeLikePattern("pat%tern"), is("pat\\%tern"));
		assertThat(dialect.escapeLikePattern("pat_tern"), is("pat\\_tern"));
		assertThat(dialect.escapeLikePattern("pattern%"), is("pattern\\%"));
		assertThat(dialect.escapeLikePattern("pattern_"), is("pattern\\_"));
		assertThat(dialect.escapeLikePattern("pat[]tern"), is("pat[]tern"));
		assertThat(dialect.escapeLikePattern("pat％tern"), is("pat\\％tern"));
		assertThat(dialect.escapeLikePattern("pat＿tern"), is("pat\\＿tern"));
	}

	@Test
	void testGetEscapeChar() {
		assertThat(dialect.getEscapeChar(), is('\\'));
	}

	@Test
	void testSupports() {
		assertThat(dialect.supportsBulkInsert(), is(false));
		assertThat(dialect.supportsLimitClause(), is(true));
		assertThat(dialect.supportsNullValuesOrdering(), is(true));
		assertThat(dialect.supportsIdentity(), is(true));
		assertThat(dialect.supportsSequence(), is(true));
		assertThat(dialect.isRemoveTerminator(), is(true));
		assertThat(dialect.isRollbackToSavepointBeforeRetry(), is(false));
		assertThat(dialect.supportsForUpdate(), is(true));
		assertThat(dialect.supportsForUpdateNoWait(), is(true));
		assertThat(dialect.supportsForUpdateWait(), is(true));
		assertThat(dialect.supportsOptimizerHints(), is(true));
		assertThat(dialect.supportsEntityBulkUpdateOptimisticLock(), is(true));
		assertThat(dialect.supportsUpdateChained(), is(false));
		assertThat(dialect.needsStrictSqlTypeForNullSetting(), is(false));
	}

	@Test
	void testGetLimitClause() {
		assertThat(dialect.getLimitClause(3, 5), is("OFFSET 5 ROWS FETCH FIRST 3 ROWS ONLY" + System.lineSeparator()));
		assertThat(dialect.getLimitClause(0, 5), is("OFFSET 5 ROWS" + System.lineSeparator()));
		assertThat(dialect.getLimitClause(3, 0), is("FETCH FIRST 3 ROWS ONLY" + System.lineSeparator()));
		assertThat(dialect.getLimitClause(0, 0), is(""));
	}

	@Test
	void testAddForUpdateClause() {
		var sql = new StringBuilder("SELECT * FROM test WHERE 1 = 1 ORDER id").append(System.lineSeparator());
		assertThat(dialect.addForUpdateClause(sql, ForUpdateType.NORMAL, -1).toString(),
				is("SELECT * FROM test WHERE 1 = 1 ORDER id" + System.lineSeparator() + "FOR UPDATE"));
		assertThat(dialect.addForUpdateClause(sql, ForUpdateType.NOWAIT, -1).toString(),
				is("SELECT * FROM test WHERE 1 = 1 ORDER id" + System.lineSeparator() + "FOR UPDATE NOWAIT"));
		assertThat(dialect.addForUpdateClause(sql, ForUpdateType.WAIT, 10).toString(),
				is("SELECT * FROM test WHERE 1 = 1 ORDER id" + System.lineSeparator() + "FOR UPDATE WAIT 10"));
	}

	@Test
	void testAddOptimizerHints1() {
		var sql = new StringBuilder("SELECT")
				.append(System.lineSeparator())
				.append(" * FROM test WHERE 1 = 1 ORDER id")
				.append(System.lineSeparator());
		List<String> hints = new ArrayList<>();
		hints.add("INDEX (test test_ix)");
		hints.add("USE_NL");

		assertThat(dialect.addOptimizerHints(sql, hints).toString(),
				is("SELECT /*+ INDEX (test test_ix) USE_NL */" + System.lineSeparator()
						+ " * FROM test WHERE 1 = 1 ORDER id" + System.lineSeparator()));
	}

	@Test
	void testAddOptimizerHints2() {
		var sql = new StringBuilder("SELECT /* SQL_ID */")
				.append(System.lineSeparator())
				.append(" * FROM PUBLIC.TEST_1");
		List<String> hints = new ArrayList<>();
		hints.add("INDEX (PUBLIC.TEST_1 test_ix)");
		hints.add("USE_NL");

		assertThat(dialect.addOptimizerHints(sql, hints).toString(),
				is("SELECT /* SQL_ID */ /*+ INDEX (PUBLIC.TEST_1 test_ix) USE_NL */" + System.lineSeparator()
						+ " * FROM PUBLIC.TEST_1"));
	}

	@Test
	void testGetPessimisticLockingErrorCodes() {
		assertThat(dialect.getPessimisticLockingErrorCodes(), is(containsInAnyOrder("54", "30006")));
	}

}
