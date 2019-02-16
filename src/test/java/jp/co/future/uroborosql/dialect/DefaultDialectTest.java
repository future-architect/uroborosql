package jp.co.future.uroborosql.dialect;

import static org.hamcrest.CoreMatchers.*;
import static org.junit.Assert.*;

import org.junit.Before;
import org.junit.Test;

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

}
