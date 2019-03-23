package jp.co.future.uroborosql.dialect;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.nullValue;
import static org.hamcrest.Matchers.instanceOf;
import static org.hamcrest.Matchers.not;
import static org.junit.Assert.*;

import java.sql.Connection;
import java.util.ServiceLoader;
import java.util.stream.StreamSupport;

import org.junit.Test;

import jp.co.future.uroborosql.connection.ConnectionSupplier;

/**
 * Oracle10Dialectの個別実装部分のテストケース
 *
 * @author H.Sugimoto
 *
 */
public class Oracle12DialectTest {
	private final Dialect dialect = new Oracle12Dialect();

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
				return "Oracle-12.1";
			}
		};

		Dialect dialect = StreamSupport.stream(ServiceLoader.load(Dialect.class).spliterator(), false)
				.filter(d -> d.accept(supplier)).findFirst().orElseGet(DefaultDialect::new);

		assertThat(dialect, instanceOf(Oracle12Dialect.class));
	}

	@Test
	public void testAcceptUnder12() {
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
				return "Oracle-11.1";
			}
		};

		Dialect dialect = StreamSupport.stream(ServiceLoader.load(Dialect.class).spliterator(), false)
				.filter(d -> d.accept(supplier)).findFirst().orElseGet(DefaultDialect::new);

		assertThat(dialect, not(instanceOf(Oracle12Dialect.class)));
	}

	@Test
	public void testAcceptOver12() {
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
				return "Oracle-13.1";
			}
		};

		Dialect dialect = StreamSupport.stream(ServiceLoader.load(Dialect.class).spliterator(), false)
				.filter(d -> d.accept(supplier)).findFirst().orElseGet(DefaultDialect::new);

		assertThat(dialect, instanceOf(Oracle12Dialect.class));
	}

	@Test
	public void testEscapeLikePattern() {
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
	public void testGetEscapeChar() {
		assertThat(dialect.getEscapeChar(), is('\\'));
	}

	@Test
	public void testSupports() {
		assertThat(dialect.supportsBulkInsert(), is(false));
		assertThat(dialect.supportsLimitClause(), is(true));
		assertThat(dialect.supportsNullValuesOrdering(), is(true));
		assertThat(dialect.isRemoveTerminator(), is(true));
		assertThat(dialect.isRollbackToSavepointBeforeRetry(), is(false));
	}

	@Test
	public void testGetLimitClause() {
		assertThat(dialect.getLimitClause(3, 5), is("OFFSET 5 ROWS FETCH FIRST 3 ROWS ONLY" + System.lineSeparator()));
		assertThat(dialect.getLimitClause(0, 5), is("OFFSET 5 ROWS" + System.lineSeparator()));
		assertThat(dialect.getLimitClause(3, 0), is("FETCH FIRST 3 ROWS ONLY" + System.lineSeparator()));
		assertThat(dialect.getLimitClause(0, 0), is(""));
	}

}
