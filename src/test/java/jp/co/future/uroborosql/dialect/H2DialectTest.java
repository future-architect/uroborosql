package jp.co.future.uroborosql.dialect;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.nullValue;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.containsInAnyOrder;
import static org.hamcrest.Matchers.instanceOf;

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
 * H2Dialectの個別実装部分のテストケース
 *
 * @author H.Sugimoto
 *
 */
public class H2DialectTest {
	private final Dialect dialect = new H2Dialect();

	@Test
	void testAccept12() {
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
				return "H2";
			}
		};

		var dialect = StreamSupport.stream(ServiceLoader.load(Dialect.class).spliterator(), false)
				.filter(d -> d.accept(supplier)).findFirst().orElseGet(DefaultDialect::new);

		assertThat(dialect, instanceOf(H2Dialect.class));
	}

	@Test
	void testGetSequenceNextValSql() {
		assertThat(dialect.getSequenceNextValSql("test_sequence"), is("nextval('test_sequence')"));
	}

	@Test
	void testEscapeLikePattern() {
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
	void testGetEscapeChar() {
		assertThat(dialect.getEscapeChar(), is('$'));
	}

	@Test
	void testSupports() {
		assertThat(dialect.supportsBulkInsert(), is(true));
		assertThat(dialect.supportsLimitClause(), is(true));
		assertThat(dialect.supportsNullValuesOrdering(), is(true));
		assertThat(dialect.supportsIdentity(), is(true));
		assertThat(dialect.supportsSequence(), is(true));
		assertThat(dialect.isRemoveTerminator(), is(true));
		assertThat(dialect.isRollbackToSavepointBeforeRetry(), is(false));
		assertThat(dialect.supportsForUpdate(), is(true));
		assertThat(dialect.supportsForUpdateNoWait(), is(false));
		assertThat(dialect.supportsForUpdateWait(), is(false));
		assertThat(dialect.supportsOptimizerHints(), is(true));
		assertThat(dialect.supportsEntityBulkUpdateOptimisticLock(), is(true));
		assertThat(dialect.supportsUpdateChained(), is(true));
		assertThat(dialect.needsStrictSqlTypeForNullSetting(), is(false));
	}

	@Test
	void testGetLimitClause() {
		assertThat(dialect.getLimitClause(3, 5), is("LIMIT 3 OFFSET 5" + System.lineSeparator()));
		assertThat(dialect.getLimitClause(0, 5), is("OFFSET 5" + System.lineSeparator()));
		assertThat(dialect.getLimitClause(3, 0), is("LIMIT 3 " + System.lineSeparator()));
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
		var sql = new StringBuilder("SELECT /* SQL_ID */")
				.append(System.lineSeparator())
				.append(" * FROM test WHERE 1 = 1 ORDER id")
				.append(System.lineSeparator());
		List<String> hints = new ArrayList<>();
		hints.add("test1_ix");
		hints.add("test2_ix");

		assertThat(dialect.addOptimizerHints(sql, hints).toString(),
				is("SELECT /* SQL_ID */" + System.lineSeparator()
						+ " * FROM test USE INDEX (test1_ix, test2_ix)"
						+ System.lineSeparator()
						+ "WHERE 1 = 1 ORDER id"
						+ System.lineSeparator()));
	}

	@Test
	void testAddOptimizerHints2() {
		var sql = new StringBuilder("SELECT /* SQL_ID */")
				.append(System.lineSeparator())
				.append(" * FROM PUBLIC.TEST_1");
		List<String> hints = new ArrayList<>();
		hints.add("test1_ix");
		hints.add("test2_ix");

		assertThat(dialect.addOptimizerHints(sql, hints).toString(),
				is("SELECT /* SQL_ID */" + System.lineSeparator()
						+ " * FROM PUBLIC.TEST_1 USE INDEX (test1_ix, test2_ix)"
						+ System.lineSeparator()));
	}

	@Test
	void testGetPessimisticLockingErrorCodes() {
		assertThat(dialect.getPessimisticLockingErrorCodes(), is(containsInAnyOrder("50200")));
	}

}
